  # - libraries

import pandas as pd
import numpy as np
import torch.nn as nn
import torch
import torch.utils.data as data
import random
import sklearn.metrics as metrics
import sklearn.model_selection as cv
import torch.nn.utils.rnn as rnn_utils

# gpu?

device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")

# - functions

def encode_word(word, char_dict):
    word = list(word)
    chars = [char_dict[char] for char in word]
    return chars

def pad_word(word, max_length):
    if len(word) < max_length:
        pad = [0 for i in range(0, max_length - len(word))]
        word = pad + word
    return torch.tensor(word, dtype=torch.long)


def int_to_one_hot(integer, n_features):
    onehot = torch.zeros(n_features)
    onehot[integer] = torch.ones(1)
    return onehot

def test_function(listofsamples):
    data = [sample[0] for sample in listofsamples]
    labels = [sample[1] for sample in listofsamples]
    lengths = torch.LongTensor([len(word) for word in data])
    sorted_lengths, indices = torch.sort(lengths, descending=True)
    data = [data[index] for index in indices]
    labels = torch.LongTensor([labels[index] for index in indices])
    data = rnn_utils.pad_sequence(data)
    data = rnn_utils.pack_padded_sequence(data, sorted_lengths, batch_first=False)
    return [data, labels]

# - classes

class Dataset(data.Dataset):
    def __init__(self, ids, labels, all_data):
        self.labels = labels
        self.ids = ids
        self.all_data = all_data

    def __len__(self):
        return len(self.ids)

    def __getitem__(self, index):
        id = self.ids[index]
        x = self.all_data[int(id)]
        y = self.labels[id]
        return x, y

class RNNConcept(nn.Module):

    def __init__(self, hidden_dim, vocab_size):
        super(RNNConcept, self).__init__()
        self.hidden_dim = hidden_dim
        self.vocab_size = vocab_size
        self.rnn = nn.RNN(input_size=vocab_size, hidden_size=hidden_dim,
                          nonlinearity='relu', batch_first=True)
        for name, param in self.rnn.named_parameters():
            if 'weight_ih' in name:
                torch.nn.init.normal_(param.data, mean=0, std=0.001)  # based on https://arxiv.org/pdf/1504.00941.pdf
            elif 'weight_hh' in name:
                torch.nn.init.eye_(param.data)  # based on https://arxiv.org/pdf/1504.00941.pdf
                param.data = param.data * 0.01
            elif 'bias' in name:
                param.data.fill_(0)  # # based on https://arxiv.org/pdf/1504.00941.pdf
        self.classifier = nn.Linear(hidden_dim, 2)  # binary output layer
        self.softmax = nn.LogSoftmax(dim = 1)

    def forward(self, word):
        rnn_out, rnn_hidden = self.rnn(word)
        rnn_hidden = rnn_hidden.view(rnn_hidden.size()[1], rnn_hidden.size()[2])
        class_space = self.classifier(rnn_hidden)
        class_scores = self.softmax(class_space)
        return class_scores

def get_network_performance(language_data):
    # encode word sequences
    n_words = len(language_data.index)
    char_dict = "".join(language_data['phon'].tolist())  # make a giant string
    char_dict = list(set(char_dict))  # list of the unique characters
    char_dict = dict(zip(char_dict, [i for i in range(0, (len(char_dict)))]))  # Dictionary where every character key has an integer as value
    encoded_words = [encode_word(word, char_dict) for word in language_data['phon'].tolist()]
    encoded_words = [torch.stack([int_to_one_hot(char, len(char_dict)) for char in word]) for word in encoded_words]
    classes = language_data['ontological.category'].tolist()
    class_dict = {'Action': 1, 'Thing': 0}  # save dummy encoding
    classes = torch.tensor([class_dict[pos] for pos in classes],
                           dtype=torch.float)  # encode classes into 0 and 1 torch float tensors
    classes = torch.unsqueeze(classes, 1)  # add dimension for pytorch. nsamples x 1
    all_id = [str(i) for i in range(len(classes))]  # word ids: indices of language_data as strings to access dictionary
    all_classes = dict(zip(all_id,
                           classes))  # stores a dictionary where the "id" of a word (string of index in matrix) maps to its class

    # training parameters and folds
    max_epochs = 300 # maximum number of epochs if early stopping doesn't trigger
    training_sets = cv.StratifiedShuffleSplit(n_splits=100, train_size=50, test_size=n_words - 50)
    training_sets_data = training_sets.split(language_data, classes)

    # storage for performance data
    folds_test_accuracy = []
    folds_action_accuracy = []
    folds_things_accuracy = []
    folds_test_f1 = []
    folds_test_matthews = []
    folds_predictions = {}
    folds_confidence = {}

    # network loop through k-fold data
    for train_indices, test_indices in training_sets_data:
        print("doing new fold")
        # - allocate data
        all_test = [ind for ind in test_indices]
        train_id = [str(ind) for ind in train_indices]  # for accessing dict of classes
        test_id = [str(ind) for ind in test_indices]
        partition = {'train': train_id, 'test': test_id}
        loss = nn.NLLLoss()

        # - create datasets and loaders
        training_set = Dataset(ids=partition['train'], labels=all_classes, all_data=encoded_words)
        training_loader = data.DataLoader(training_set, batch_size= 32,
                                          shuffle=True, collate_fn=test_function)
        # - create network and optimizer
        model = RNNConcept(hidden_dim=10, vocab_size=len(char_dict))
        model = model.to(device)
        optim = torch.optim.Adam(model.parameters(), lr=0.001, weight_decay=0.01)

        # - early stopping variables

        # - training loop
        for epoch in range(max_epochs):
            # -- loop through training mini batches
            for batch, batch_labels in training_loader:
                batch = batch.to(device)
                batch_labels = batch_labels.to(device)
                prediction_scores = model(batch)  # prediction for training
                epoch_loss = loss(prediction_scores, batch_labels)  # training BCE loss
                model.zero_grad()  # reset gradient after each batch
                epoch_loss.backward()  # back propagate gradient
                optim.step()  # step in optimizer

        # - performance for this fold
        with torch.no_grad():
            test_data = [encoded_words[index] for index in all_test]

            sorted_lengths = torch.LongTensor([len(word) for word in test_data])
            sorted_lengths, indices = torch.sort(sorted_lengths, descending=True)
            test_data = [test_data[index] for index in indices]
            test_data = rnn_utils.pad_sequence(test_data)
            test_data = rnn_utils.pack_padded_sequence(test_data, sorted_lengths, batch_first=False)
            test_data = test_data.to(device)
            test_scores = model(test_data).cpu()
            test_confidence = torch.max(test_scores, 1)[0]
            test_confidence = test_confidence.exp().tolist()
            test_ground = [all_classes[id] for id in test_id]
            test_ground = [test_ground[index] for index in indices]
            test_id = [test_id[index] for index in indices]
            test_prediction = (torch.max(test_scores, 1)[1]).tolist()
            test_matthews = metrics.matthews_corrcoef(test_ground, test_prediction)
            test_accuracy = metrics.balanced_accuracy_score(test_ground, test_prediction)
            test_f1 = metrics.f1_score(test_ground, test_prediction)
            tn, fp, fn, tp = metrics.confusion_matrix(test_ground, test_prediction).ravel()
            acc_action = tp / (tp + fn)
            folds_action_accuracy.append(acc_action)
            acc_thing = tn / (tn + fp)
            folds_things_accuracy.append(acc_thing)
            folds_test_matthews.append(test_matthews)
            folds_test_f1.append(test_f1)
            folds_test_accuracy.append(test_accuracy)
    # aggregate fold performance and return dataframe
    all_results = pd.DataFrame({'Matthews': folds_test_matthews,
                                'Accuracy': folds_test_accuracy, 'F1': folds_test_f1,
                                'ActionAccuracy': folds_action_accuracy, 'ThingAccuracy': folds_things_accuracy})
    print(all_results)
    return [all_results]


def get_repeated_performance_rnn(all_data, language_name, times=1):
    print(language_name)
    language_data = all_data[all_data['language'] == language_name]
    language_data = language_data[language_data['ontological.category'] != 'Other']
    print(language_data.shape)
    all_performance = []
    for i in range(times):
        print(i)
        all_performance.append(get_network_performance(language_data))
    all_measures = [result[0] for result in all_performance]
    all_measures = pd.concat(all_measures)
    return [all_measures]


def save_repeated_measures(list_of_results, language_name):
    filename_performance = "Results/Spurt/" + f"{language_name}" + "_spurt_performance.csv"
    list_of_results[0].to_csv(filename_performance)

random.seed(1)
np.random.seed(1)
torch.manual_seed(1)

language_data = pd.read_csv("Data/Processed/all_phon.csv", keep_default_na=False)
language_data = language_data[language_data['ontological.category'] != 'Other']
all_languages = sorted(set(language_data["language"]))

for language_name in all_languages[36:]:
    language_performance = get_repeated_performance_rnn(language_data, language_name, times=1)
    save_repeated_measures(language_performance, language_name)

# best performing = amsgrad true, lr = 0.001, adam. HU = 5 gives stable performance. weight decay 0.001 stabilizes results.
