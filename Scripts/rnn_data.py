# - Libraries and preliminaries

import pandas as pd
import numpy as np
import torch.nn as nn
import torch
import torch.utils.data as data
import random
import sklearn.metrics as metrics
import sklearn.model_selection as cv
import torch.nn.utils.rnn as rnn_utils

# Use CUDA if available
device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")

# If true, then use use spurt model
# If false, use 10-fold cross validation
spurt_model = False

# - Supporting Functions

def encode_word(word, char_dict):
    """Returns a word encoded as a list of integers.

    :param word: a word represented as a string.
    :param char_dict: A dictionary where keys are the unique characters of a language,
    and values are the integers associated with each character.
    :return: the word represented as a list of integers.
    """
    word = list(word)
    chars = [char_dict[char] for char in word]
    return chars


def int_to_one_hot(integer, n_features):
    """One-hot encodes a single integer.

    :param integer: an integer, hopefully obtained from encode_word.
    :param n_features: the total number of features,
                        i.e. the length of the char_dict used in encode_word.
    :return: a pytorch tensor with the input integer one-hot encoded.
    """
    onehot = torch.zeros(n_features)
    onehot[integer] = torch.ones(1)
    return onehot


def pack_word(list_of_samples):
    """Collate function for the training samples in the pytorch dataset.
    Packs the sequences of different length to avoid presenting a padded version to the RNN.
    For more information on packing, see
    https://pytorch.org/docs/stable/nn.html?highlight=pack#torch.nn.utils.rnn.pack_padded_sequence
    In the script, only used as the collate_fn parameter in the Training Dataset.

    The function sorts the labels and the data of each sample in the training batch
    in descending order of length of the sequence, and then "packs" the ones shorter than the longest one.
    After padding them, it transforms them into "packed" sequences that allow pytorch to present them in order
    without training the network on the padding.

    :param list_of_samples:  A batch resulting from the Dataset object. List of tuples,
                            where [0] is the data of the sequence and [1] is its label.
    :return: A batch of samples with packed sequences.
    """
    sample_data = [sample[0] for sample in list_of_samples]
    sample_labels = [sample[1] for sample in list_of_samples]
    lengths = [len(word) for word in sample_data]
    lengths = torch.LongTensor(lengths)
    sorted_lengths, indices = torch.sort(lengths, descending=True)
    sample_data = [sample_data[index] for index in indices]
    sample_labels = torch.LongTensor([sample_labels[index] for index in indices])
    sample_data = rnn_utils.pad_sequence(sample_data)
    sample_data = rnn_utils.pack_padded_sequence(sample_data, sorted_lengths, batch_first=False)
    return [sample_data, sample_labels]


# - Classes

class Dataset(data.Dataset):
    """ Dataset object that inherits functionality from Dataset in pytorch.
    For more information, see https://pytorch.org/docs/stable/data.html

    This implementation returns the needed structure pack the sequences using pack_word.
    Loads training data by returning a tuple of [data, label] using the id number of the sample.
    """

    def __init__(self, ids, labels, all_data):
        self.labels = labels
        self.ids = ids
        self.all_data = all_data

    def __len__(self):
        return len(self.ids)

    def __getitem__(self, index):
        id_n = self.ids[index]
        x = self.all_data[int(id_n)]
        y = self.labels[id_n]
        return x, y


class RNNConcept(nn.Module):
    """Class for the RNN. Inherits from pytorch RNN.

    Recurrent neural network used in the project.
    It has one hidden layer, and outputs to a 2-unit layer
    with softmax activation.
    """

    def __init__(self, hidden_dim, vocab_size):
        """Function that initializes the network.

        After creating the layers, it initializes the weights of the layers by using the
        "iRNN" method detailed here: https://arxiv.org/pdf/1504.00941.pdf
        This method initializes the weights of the hidden layer with a normal distribution with
        mean = 0 and standard deviation = 0.001. The recurrent weights are initialized with
         a (scaled) identity matrix. The bias is initialized as 0.

        The hidden layer uses ReLU as the non linearity.

        :param hidden_dim: An integer. The number of units on the hidden layer.
        :param vocab_size: An integer. The number of possible characters,
        i.e. the length of the one-hot encoded characters.
        """
        super(RNNConcept, self).__init__()
        self.hidden_dim = hidden_dim
        self.vocab_size = vocab_size
        self.rnn = nn.RNN(input_size=vocab_size, hidden_size=hidden_dim,
                          nonlinearity="relu", batch_first=True)
        self.classifier = nn.Linear(hidden_dim, 2)
        self.softmax = nn.LogSoftmax(dim=1)

        for name, param in self.rnn.named_parameters():
            if "weight_ih" in name:
                torch.nn.init.normal_(param.data, mean=0, std=0.001)
            elif "weight_hh" in name:
                torch.nn.init.eye_(param.data)
                param.data = param.data * 0.01
            elif "bias" in name:
                param.data.fill_(0)

    def forward(self, word):
        """Defines the forward pass of the network on the data.

        Because it's an RNN, there's an output at each element of the sequence.
        The predicted label is taken as the output of the last element of the sequence.
        :param word: A one-hot encoded representation of a word.
        :return: The softmax activation of the 2 unit output layer.
        """
        rnn_out, rnn_hidden = self.rnn(word)
        rnn_hidden = rnn_hidden.view(rnn_hidden.size()[1], rnn_hidden.size()[2])
        class_space = self.classifier(rnn_hidden)
        class_scores = self.softmax(class_space)
        return class_scores


def get_network_performance(language_data):
    """Main function of the script. Takes a language Dataframe, encodes its words as packed one-hot sequences.
    Then, depending on the value of spurt_model, it either:
        spurt_model = False:
                            Cross-validates performance measures of the network on its word data using 10-Fold CV.
        spurt_model = True:
                            Runs 100 iterations of the network trained on 50 words of the language and tested on
                            the remaining set of words.

    :param language_data:
    :return: a Dataframe with different measures of accuracy of the network on the
            different iterations or folds.
    """

    # First, encode words as sequences of one-hot vectors.
    # Make a giant string
    char_dict = "".join(language_data["phon"].tolist())

    # Make a list of the unique characters
    char_dict = list(set(char_dict))

    # Create the reference dictionary where every character key has an integer as value
    char_dict = dict(zip(char_dict, [i for i in range(0, (len(char_dict)))]))

    # Encode words into sequences of strings using encode_words function and then
    # stack them into a pytorch tensor.
    encoded_words = [encode_word(word, char_dict) for word in language_data['phon'].tolist()]
    encoded_words = [torch.stack([int_to_one_hot(char, len(char_dict)) for char in word]) for word in encoded_words]

    # Encode the semantic category for prediction as 0 and 1 tensors.
    classes = language_data['ontological.category'].tolist()
    class_dict = {'Action': 1, 'Thing': 0}
    classes = torch.tensor([class_dict[pos] for pos in classes], dtype=torch.float)
    classes = torch.unsqueeze(classes, 1)  # Add dimension for pytorch. Number of samples x 1

    # Stores a dictionary where the "id" of a word (string of index in matrix) maps to its class
    all_id = [str(i) for i in range(len(classes))]
    all_classes = dict(zip(all_id, classes))

    # Training and cross-validation parameters.
    # Number of training epochs
    max_epochs = 300

    if spurt_model:
        # Performance is measured on the spurt model
        n_words = len(language_data.index)
        training_set = cv.StratifiedShuffleSplit(n_splits=100, train_size=50, test_size=n_words - 50)
    else:
        # Cross-validation scheme.
        training_set = cv.StratifiedKFold(n_splits=10, shuffle=True)

    training_set_data = training_set.split(language_data, classes)

    # Creates lists to store the performance of the network in each iteration
    iter_accuracy = []
    iter_action_accuracy = []
    iter_thing_accuracy = []
    iter_f1 = []
    iter_matthews = []

    # Loop through the iteration and get performance in each one.
    for train_indices, test_indices in training_set_data:

        print("doing new iteration")
        # - Allocate data
        all_test = [ind for ind in test_indices]
        # For accessing dict of classes
        train_id = [str(ind) for ind in train_indices]
        test_id = [str(ind) for ind in test_indices]
        partition = {'train': train_id, 'test': test_id}

        # - Create datasets and loaders
        training_set = Dataset(ids=partition['train'], labels=all_classes, all_data=encoded_words)
        training_loader = data.DataLoader(training_set, batch_size=32,
                                          shuffle=True, collate_fn=pack_word)

        # - Create network and optimizer
        # Define loss function
        loss = nn.NLLLoss()
        # Initialize RNN
        model = RNNConcept(hidden_dim=10, vocab_size=len(char_dict))
        model = model.to(device)  # If using CUDA, this sends RNN to GPU.
        optimizer = torch.optim.Adam(model.parameters(), lr=0.001, weight_decay=0.01)

        # - Training loop
        for epoch in range(max_epochs):
            if epoch % 50 == 0:
                # Monitor training.
                print(epoch)
            # -- Loop through training mini batches
            for batch, batch_labels in training_loader:
                batch = batch.to(device)
                batch_labels = batch_labels.to(device)

                # Prediction for this training batch
                prediction_scores = model(batch)
                epoch_loss = loss(prediction_scores, batch_labels)
                model.zero_grad()  # Reset gradient after each batch
                epoch_loss.backward()  # Back-propagate gradient
                optimizer.step()

        # - Performance for this iteration
        with torch.no_grad():  # Don't track this gradient
            # Get the words that weren't used in training for this iteration and pack them manually
            test_data = [encoded_words[index] for index in all_test]
            sorted_lengths = torch.LongTensor([len(word) for word in test_data])
            sorted_lengths, indices = torch.sort(sorted_lengths, descending=True)
            test_data = [test_data[index] for index in indices]
            test_data = rnn_utils.pad_sequence(test_data)
            test_data = rnn_utils.pack_padded_sequence(test_data, sorted_lengths, batch_first=False)
            test_data = test_data.to(device)

            # Get predictions of the packed test sequences
            test_scores = model(test_data).cpu()  # Do this part on the CPU
            test_ground = [all_classes[id_n] for id_n in test_id]
            test_ground = [test_ground[index] for index in indices]
            test_prediction = (torch.max(test_scores, 1)[1]).tolist()

            # Calculate accuracy metrics
            test_matthews = metrics.matthews_corrcoef(test_ground, test_prediction)
            test_accuracy = metrics.balanced_accuracy_score(test_ground, test_prediction)
            test_f1 = metrics.f1_score(test_ground, test_prediction)
            # Hand calculate the accuracy for actions and the accuracy for things separately
            tn, fp, fn, tp = metrics.confusion_matrix(test_ground, test_prediction).ravel()
            acc_action = tp / (tp + fn)
            acc_thing = tn / (tn + fp)

            # Append them to the lists that track per-iteration, per-measure performance
            iter_action_accuracy.append(acc_action)
            iter_thing_accuracy.append(acc_thing)
            iter_matthews.append(test_matthews)
            iter_f1.append(test_f1)
            iter_accuracy.append(test_accuracy)

    # Aggregate iteration performance and return dataframe
    all_results = pd.DataFrame({'Matthews': iter_matthews,
                                'Accuracy': iter_accuracy, 'F1': iter_f1,
                                'ActionAccuracy': iter_action_accuracy, 'ThingAccuracy': iter_thing_accuracy})
    print(all_results)  # Print for monitoring
    return all_results


def get_language_performance(all_data, lang_name):
    """Auxiliary function to prepare language data and get cross-validated measures

    The function filters words from other languages and eliminates words from the "Other" category.

    :param all_data: Dataframe with all words for all languages.
    :param lang_name: Name of the language to measure, e.g. "Danish"
    :return: A dataframe with performance measures for this language.
    """
    print(lang_name)
    language_data = all_data[all_data['language'] == lang_name]
    language_data = language_data[language_data['ontological.category'] != 'Other']
    print(language_data.shape)
    all_measures = get_network_performance(language_data)
    return all_measures


def save_repeated_measures(results, lang_name):
    """Function to save the performance of a particular language.

    Main job of the function is to generate the file name for the particular language.

    :param results: A Dataframe of performance measures per iteration.
    :param lang_name: The name of a particular language, i.e. "Danish"
    :return: Doesn't return anything. Saves a CSV version of the results Dataframe.
    """
    if spurt_model:
        filename_performance = "../Results/Spurt/" + f"{language_name}" + "_spurt_performance.csv"
    else:
        filename_performance = "../Results/ten-fold/" + f"{lang_name}" + "_rnn_performance.csv"
    results.to_csv(filename_performance)


# - Script

# Set random seeds for reproducibility
random.seed(1)
np.random.seed(1)
torch.manual_seed(1)

# Load all language data and extract an ordered set of the names
lang_data = pd.read_csv("../Data/Processed/all_phon_adjusted_mi.csv", keep_default_na=False)
all_languages = sorted(set(lang_data["language"]))

# Loop through all language names, get performance of RNN on them and save them as CSV.
for language_name in all_languages:
    language_performance = get_language_performance(lang_data, language_name)
    save_repeated_measures(language_performance, language_name)
