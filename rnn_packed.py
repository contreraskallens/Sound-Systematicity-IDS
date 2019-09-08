  # - libraries

import pandas as pd
import numpy as np
import torch.nn as nn
import torch
import torch.utils.data as data
import random
import sklearn.metrics as metrics
import sklearn.model_selection as cv
import time
import torch.nn.utils.rnn as rnn_utils

# gpu?

device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")


# print(torch.cuda.get_device_name(torch.cuda.current_device()))
# if device.type != 'cpu':
# device = "cpu"
# cores = 7
# else:
#     cores = 0

# print(cores)

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

    # def __init__(self, hidden_dim, vocab_size):
    #     super(RNNConcept, self).__init__()
    #     self.hidden_dim = hidden_dim
    #     self.vocab_size = vocab_size
    #     self.rnn = nn.RNN(input_size=vocab_size, hidden_size=hidden_dim,
    #                       nonlinearity='relu', batch_first=True)
    #     for name, param in self.rnn.named_parameters():
    #         if 'weight_ih' in name:
    #             torch.nn.init.normal_(param.data, mean=0, std=0.001)  # based on https://arxiv.org/pdf/1504.00941.pdf
    #         elif 'weight_hh' in name:
    #             torch.nn.init.eye_(param.data)  # based on https://arxiv.org/pdf/1504.00941.pdf
    #             param.data = param.data * 0.01
    #         elif 'bias' in name:
    #             param.data.fill_(0)  # # based on https://arxiv.org/pdf/1504.00941.pdf
    #     self.classifier = nn.Linear(hidden_dim, 2)  # binary output layer
    #     self.softmax = nn.LogSoftmax(dim = 1)
    def __init__(self, hidden_dim, vocab_size):
        super(RNNConcept, self).__init__()
        self.hidden_dim = hidden_dim
        self.vocab_size = vocab_size
        self.rnn = nn.LSTM(input_size=vocab_size, hidden_size=hidden_dim, batch_first=True)
        self.classifier = nn.Linear(hidden_dim, 2)  # binary output layer
        self.softmax = nn.LogSoftmax(dim = 1)

    def forward(self, word):
        rnn_out, rnn_hidden = self.rnn(word)
        rnn_hidden = rnn_hidden[0]
        # print(rnn_hidden)
        rnn_hidden = rnn_hidden.view(rnn_hidden.size()[1], rnn_hidden.size()[2])
        # print(rnn_hidden.size())
        # rnn_out = rnn_utils.pad_packed_sequence(rnn_out)
        # rnn_out = rnn_out[0]
        class_space = self.classifier(rnn_hidden)
        # class_scores = torch.sigmoid(class_space)  # binary
        class_scores = self.softmax(class_space)
        return class_scores


def get_network_performance(language_data):
    # encode word sequences
    char_dict = "".join(language_data['phon'].tolist())  # make a giant string
    char_dict = list(set(char_dict))  # list of the unique characters
    char_dict = dict(zip(char_dict, [i for i in range(0, (len(char_dict)))]))  # Dictionary where every character key has an integer as value
    max_length = max([len(word) for word in
                      language_data['phon'].tolist()])  # obtains the longest word in the vocabulary for padding

    # encoded_words = [pad_word(encode_word(word, char_dict), max_length) for word in language_data['phon'].tolist()]  # first encodes words from character to integer, then pads with 0 until length = max_length
    # encoded_words = [torch.stack([int_to_one_hot(char, (len(char_dict) + 1)) for char in word]) for word in encoded_words]  # transforms the characters (ints) into one-hot encoding and then stacks them into a pytorch tensor matrix
    # encoded_words = torch.stack(encoded_words).permute(1, 0, 2) # sequence x nsamples x dimensions (vocabulary size)
    encoded_words = [encode_word(word, char_dict) for word in language_data['phon'].tolist()]
    encoded_words = [torch.stack([int_to_one_hot(char, len(char_dict)) for char in word]) for word in encoded_words]
    classes = language_data['ontologicalCategory'].tolist()
    # classes = language_data['englishPOS'].tolist()
    class_dict = {'Action': 1, 'Thing': 0}  # save dummy encoding
    # class_dict = {'verb': 1, 'noun': 0}  # save dummy encoding
    classes = torch.tensor([class_dict[pos] for pos in classes],
                           dtype=torch.float)  # encode classes into 0 and 1 torch float tensors
    classes = torch.unsqueeze(classes, 1)  # add dimension for pytorch. nsamples x 1
    all_id = [str(i) for i in range(len(classes))]  # word ids: indices of language_data as strings to access dictionary
    all_classes = dict(zip(all_id,
                           classes))  # stores a dictionary where the "id" of a word (string of index in matrix) maps to its class

    # training parameters and folds
    max_epochs = 400  # maximum number of epochs if early stopping doesn't trigger
    patience = 40  # number of epochs without improvement of test_loss that triggers early stopping
    # loss = nn.BCELoss()  # define loss function
    k_folds = cv.StratifiedKFold(n_splits=3, shuffle=True)  # make k-folds. choose number of splits.
    k_fold_data = k_folds.split(language_data, classes)

    # storage for performance data
    folds_test_accuracy = []
    folds_test_f1 = []
    folds_test_matthews = []
    folds_test_auc = []
    folds_predictions = {}
    folds_confidence = {}

    # network loop through k-fold data
    for train_indices, test_indices in k_fold_data:
        print("doing new fold")
        # - allocate data
        # all_train = [ind for ind in train_indices]  # indexing
        all_test = [ind for ind in test_indices]
        train_id = [str(ind) for ind in train_indices]  # for accessing dict of classes
        test_id = [str(ind) for ind in test_indices]
        partition = {'train': train_id, 'test': test_id}
        # proportion = pd.crosstab(language_data.iloc[test_indices, 2], columns="count").apply(lambda c: c / c.sum(),
        #                                                                                      axis=0)
        # proportion = torch.FloatTensor(proportion["count"].tolist())
        loss = nn.NLLLoss()
        # Action_proportion = proportion["count"][1]
        # - create datasets and loaders
        training_set = Dataset(ids=partition['train'], labels=all_classes, all_data=encoded_words)
        training_loader = data.DataLoader(training_set, batch_size=128,
                                          shuffle=True, collate_fn=test_function)
        test_set = Dataset(labels=all_classes, ids=partition['test'], all_data=encoded_words)
        test_loader = data.DataLoader(test_set, batch_size=128,
                                      shuffle=True, collate_fn=test_function)
        # - create network and optimizer
        model = RNNConcept(hidden_dim=10, vocab_size=len(char_dict))
        model = model.to(device)
        # optim = torch.optim.Adam(model.parameters(), lr=0.001, weight_decay=0.001, amsgrad=True)
        optim = torch.optim.Adam(model.parameters(), lr=0.001, weight_decay=0.01)

        # - early stopping variables
        min_loss = np.Inf  # keep track of the minimum loss to this point
        patience_counter = 0  # keep track of epochs without improvement

        # - training loop
        for epoch in range(max_epochs):

            # -- loop through training mini batches
            for batch, batch_labels in training_loader:
                # batch = batch.to(device), batch_labels.to(device)
                batch = batch.to(device)
                batch_labels = batch_labels.to(device)
                prediction_scores = model(batch)  # prediction for training
                epoch_loss = loss(prediction_scores, batch_labels)  # training BCE loss
                model.zero_grad()  # reset gradient after each batch
                epoch_loss.backward()  # back propagate gradient
                optim.step()  # step in optimizer
            # print(epoch_loss)
            # -- early stopping loop through test minibatches
            val_loss = []
            for batch, batch_labels in test_loader:
                with torch.no_grad():  # no tracking for test loss calculation
                    # batch = batch.permute(1, 0, 2)
                    # batch, batch_labels = batch.to(device), batch_labels.to(device)
                    batch = batch.to(device)
                    batch_labels = batch_labels.to(device)
                    val_prediction = model(batch)
                    batch_loss = loss(val_prediction, batch_labels)  # store loss for each batch
                    val_loss.append(batch_loss)

            val_loss = torch.stack(val_loss)
            val_loss = torch.Tensor.mean(val_loss)  # calculate mean loss for each of the test batches
            # print(val_loss)
            if (
                    val_loss < min_loss):  # if mean batch loss is the minimum yet, store it in place and reset patience counter
                min_loss = val_loss
                patience_counter = 0
            else:  # if its larger or equal than the minimum yet, add one to the patience counter
                patience_counter += 1
            if patience_counter == patience:  # when there have been `patience` epochs without improvement, stop training and report the epochs of training
                print("converged at epoch " + str(epoch))
                break
        # print(time.perf_counter() - start)
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
            test_prediction = (torch.max(test_scores, 1)[1]).tolist()
            test_ground = [all_classes[id] for id in test_id]
            test_ground = [test_ground[index] for index in indices]
            test_id = [test_id[index] for index in indices]
            test_prediction = (torch.max(test_scores, 1)[1]).tolist()
            # test_prediction = [0 if predscore < Action_proportion else 1 for predscore in
            #                    test_scores]  # using class weight as treshold.
            test_matthews = metrics.matthews_corrcoef(test_ground, test_prediction)
            # test_auc = metrics.roc_auc_score(test_ground, test_scores)
            test_accuracy = metrics.balanced_accuracy_score(test_ground, test_prediction)
            test_f1 = metrics.f1_score(test_ground, test_prediction)
            folds_test_matthews.append(test_matthews)
            # folds_test_auc.append(test_auc)
            folds_test_f1.append(test_f1)
            folds_test_accuracy.append(test_accuracy)
            folds_confidence = {**folds_confidence, **dict(zip(test_id,
                                                                 test_confidence))}  # create dictionary with id:confidence. id = index of word in its language dataframe
            folds_predictions = {**folds_predictions, **dict(zip(test_id,
                                                                 test_prediction))}  # create dictionary with id:prediction. id = index of word in its language dataframe

    # aggregate fold performance and return dataframe
    all_results = pd.DataFrame({'Matthews': folds_test_matthews,
                                'Accuracy': folds_test_accuracy, 'F1': folds_test_f1})
    print(all_results)
    return [all_results, folds_predictions, folds_confidence]
    # return [all_results, folds_predictions]


def get_repeated_performance_rnn(all_data, language_name, times=1):
    print(language_name)
    language_data = all_data[all_data['language'] == language_name]
    language_data = language_data[language_data['ontologicalCategory'] != 'Other']
    # language_data = language_data[language_data['englishPOS'] != 'other']
    print(language_data.shape)
    all_performance = []
    for i in range(times):
        print(i)
        all_performance.append(get_network_performance(language_data))
    all_measures = [result[0] for result in all_performance]
    all_measures = pd.concat(all_measures)
    all_predictions = []
    all_confidence = [result[2] for result in all_performance]
    conf_dataframes = []
    for i in range(times):
        conf_dataframe = pd.DataFrame.from_dict(all_confidence[i], orient='index')
        conf_dataframe.index = conf_dataframe.index.astype(int)
        conf_dataframe.sort_index(inplace=True)
        pred_dataframe = pd.DataFrame.from_dict(all_predictions[i], orient='index')
        pred_dataframe.index = pred_dataframe.index.astype(int)
        pred_dataframe.sort_index(inplace=True)
        conf_dataframe['Prediction'] = pred_dataframe.iloc[:,0]
        print(conf_dataframe)
        conf_dataframes.append(conf_dataframe)
    conf_dataframes = pd.concat(conf_dataframes, axis=1)
    conf_dataframes['Form'] = language_data['Form'].tolist()
    return [all_measures, conf_dataframes]
    # for prediction in [result[1] for result in all_performance]:
    #     pred_dataframe = pd.DataFrame.from_dict(prediction, orient='index')
    #     pred_dataframe.index = pred_dataframe.index.astype(int)
    #     pred_dataframe.sort_index(inplace=True)
    #     all_predictions.append(pred_dataframe)
    # all_predictions = pd.concat(all_predictions, axis=1)
    # all_predictions['Form'] = language_data['Form'].tolist()
    # return [all_measures, all_predictions]


def save_repeated_measures(list_of_results, language_name):
    filename_performance = "Results/Exp/" + f"{language_name}" + "_lstm_performance.csv"
    filename_confidence = "Results/Exp/" + f"{language_name}" + "_lstm_confidence.csv"
    list_of_results[0].to_csv(filename_performance)
    list_of_results[1].to_csv(filename_confidence, header=False, index=False)


random.seed(1)
# lang_data = pd.read_csv("Data/Processed/allPhonFormsConcepticon.csv")
lang_data = pd.read_csv("Data/Processed/allPhonFormsConcepticon2.csv", keep_default_na=False)
# language_data = lang_data[lang_data['language'] == "English"]
# language_data = language_data[language_data['ontologicalCategory'] != 'Other']

# all_languages = ["Spanish", "French", "German", "Danish", "Hawaiian", "Persian", "Avar", "Maori", "Hindi", "Mapudungun"]
# all_languages = sorted(set(lang_data["language"]))
# print(all_languages.index("Sirionó"))
# all_languages = ["Pacaas Novos", "Vietnamese", "Wichí", "Rotuman", "Tongan", "Ese Ejja", "Maori", "Cayuvava", "Pilagá", "Sirionó", "Guaraní", "Seri"]
all_languages = ["Danish", "English", "Hungarian", "French", "Mapudungun"]
all_languages = ["Hungarian"]
for language_name in all_languages:
    language_performance = get_repeated_performance_rnn(lang_data, language_name, times=10)
    save_repeated_measures(language_performance, language_name)

# best performing = amsgrad true, lr = 0.001, adam. HU = 5 gives stable performance. weight decay 0.001 stabilizes results.

# TODO: CV -> DONE
# TODO: compare embedding with no embedding (one-hot) -> DONE
# TODO: decide on model (n units, identity * x) -> DONE
# TODO: early stopping script to compare final performance -> done
# TODO: functionalize so it can be run on number of languages. -> done
# TODO: get performance for word and join with word dataframe and get to R for plotting. -> done
# TODO: decide CV process and start getting data. -> DONE
# TODO: export languages as CSV with appropriate filenames (interpolation). -> DONE
# TODO: test that everything's working as expected -> DONE
# TODO: repeat with MLP and compare. -> DONE
# TODO: MC permutations: labels and sequence
