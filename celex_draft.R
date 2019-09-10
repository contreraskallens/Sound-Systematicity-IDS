library(tidyverse)
library(stringdist)
library(wesanderson)
library(BayesFactor)
library(emmeans)

colorPaletteCont <- wes_palette(name = 'Zissou1', type = 'continuous', n = 5)
colorPaletteDis <- c(wes_palette(name = 'Royal2'), wes_palette(name = 'Royal1'))

# Monosyllable WORDS

celex.pron <-
  read_delim(
    'Data/Celex/epw.cd',
    delim = "\\",
    guess_max = 200000,
    quote = "",
    col_names = F
  ) %>%
  select(1:8)
colnames(celex.pron) <-
  c("Word.ID",
    "Word",
    "Raw.Freq",
    "Lemma.ID",
    "Number.Of.Pronunciations",
    "Status.Of.Pronunciation",
    "phon",
    "Syllable.Structure")

# remove frequency 0 words

celex.pron <- celex.pron %>%
  filter(Raw.Freq > 0)

# celex.pron.clean <- celex.pron %>%
#   select(Headword, Raw.Freq, Lemma.ID, phon)
# load syntax data

celex.synt <- read_delim('Data/Celex/esl.cd', delim = "\\", guess_max = 200000, quote = "", col_names = F) %>%
  select(1:4)
colnames(celex.synt) <- c("Lemma.ID", "Lemma", "Raw.Freq", "Class")
celex.synt <- celex.synt %>%
  filter(Raw.Freq > 0)

# join by lemma id

pron.synt <- celex.synt %>%
  select(Lemma.ID, Class) %>%
  right_join(celex.pron) %>%
  mutate(Lemma.ID = factor(Lemma.ID), Class = factor(Class)) %>%
  select(Lemma.ID, Class, Word, Raw.Freq, phon, Syllable.Structure)

# recode class variable

levels(pron.synt$Class) <- c("Noun", "Adjective", "Numeral", "Verb", "Article",
                             "Pronoun", "Adverb", "Preposition", "Conjunction",
                             "Interjection", "Single.Contraction", "Complex.Contraction",
                             "Other", "Other", "Other")
# remove multi words

pron.synt <- pron.synt %>%
  filter(!(str_detect(Word, " ")))

# keep only monosyllables. get syllables by counting the number of "]" on Syllable Structure variable.

pron.synt <- pron.synt %>%
  mutate(Number.Of.Syllables = str_count(Syllable.Structure, "]")) %>%
  filter(Number.Of.Syllables == 1)

# get relative frequency of each class PER PRONUNCIATION, not necessarily Lemma.

pron.class <- pron.synt %>%
  group_by(phon, Class) %>%
  summarize(Comp.Freq = sum(Raw.Freq)) %>%
  spread(Class, Comp.Freq, fill = 0) %>%
  group_by() %>%
  mutate(Total.Freq = rowSums(.[2:14])) %>%
  # calculate relative frequency of each class per lemma
  mutate_if(is.double, function(x){return(x / .$Total.Freq)}) %>%
  select(-Total.Freq) %>%
  arrange(phon)

# take only nouns and verbs

pron.class <- pron.class %>%
  select(phon, Noun, Verb) %>%
  mutate(total = Noun + Verb) %>%
  filter(total > 0.5) %>%
  # difference in relative frequency
  mutate(diff = abs(Noun - Verb)) %>%
  filter(diff > 0.1) %>%
  select(-total, -diff)

# get majority class

pron.class$Class <- colnames(pron.class)[apply(pron.class, 1, which.max)]

# alternative: get only the unambiguous ones
# pron.class$Class <- apply(pron.class, 1, function(this.row){
#   Noun.Freq <- as.numeric(this.row[2])
#   Verb.Freq <- as.numeric(this.row[5])
#   if(is.nan(Noun.Freq) | is.nan(Verb.Freq)){return(NA)}
#   if(is.na(Noun.Freq) | is.na(Verb.Freq)){return(NA)}
#   if(Noun.Freq == 1){
#     return("Noun")
#   }else if(Verb.Freq == 1){
#     return("Verb")
#   } else {return(NA)}
# }) %>%
#   unlist %>%
#   factor()

# clean phon column

pron.class <- pron.class %>%
  mutate(phon = str_remove_all(phon, "\'"))

pron.class <- pron.class

# Monosyllables

pron.class <- pron.class %>%
  rename(englishPOS = Class)
# Save for RNN

pron.class %>% 
  select(phon, englishPOS) %>% 
  add_column(language = "English") %>% 
  write_csv("Data/Processed/celex_mono_words.csv")
# Save for comparison missing one phoneme

pron.class %>% 
  select(phon, englishPOS) %>% 
  add_column(language = "English") %>% 
  mutate(phon = str_sub(phon, "1", "-2")) %>% 
  mutate(new.length = nchar(phon)) %>% 
  filter(new.length > 0) %>% 
  select(-new.length) %>% 
  write_csv("Data/Processed/celex_mono_words_no_ending.csv")

getDistanceMatrix <- function(languageDF){
  words <- languageDF$phon
  distanceMatrix <- stringdistmatrix(words, words)
  colnames(distanceMatrix) <- words
  row.names(distanceMatrix) <- words
  return(distanceMatrix)
}
getDistanceMatrix.norm <- function(languageDF){
  words <- languageDF$phon
  distanceMatrix <- stringdistmatrix(words, words)
  colnames(distanceMatrix) <- words
  row.names(distanceMatrix) <- words
  lengths <- map_dbl(words, nchar)
  max.lengths <- outer(lengths, lengths, FUN = "pmax")
  distanceMatrix <- distanceMatrix / max.lengths
  return(distanceMatrix)
}

distances <- pron.class %>%
  getDistanceMatrix.norm()

getMeanDistances <- function(languageDF, distanceMatrix){
  verbMask <- languageDF$englishPOS == "Verb"
  nounMask <- languageDF$englishPOS == "Noun"
  meanDistanceMatrix <- data_frame(word = languageDF$phon) %>%
    mutate(meanVerb = rowMeans(distanceMatrix[, verbMask]),
           meanNoun = rowMeans(distanceMatrix[, nounMask]),
           typicality = meanVerb - meanNoun,
           class = languageDF$englishPOS)
  return(meanDistanceMatrix)
}

meanDistances <- getMeanDistances(pron.class, distances)

meanDistances %>%
  mutate(class = factor(class, levels = c("Verb", "Noun"))) %>%
  ggplot(aes(x = meanVerb, y = meanNoun, fill = stat(nlevel))) +
  stat_density2d(geom = "polygon") +
  scale_fill_gradientn(name = "Density", colors = colorPaletteCont) +
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = wes_palettes$Moonrise1[4], size = 1) +
  facet_wrap(vars(class), nrow = 1) +
  labs(x = expression(hat(ND[V])), y = expression(hat(ND[N]))) +
  cowplot::theme_cowplot() +
  cowplot::panel_border()

meanDistances %>%
  ggplot(aes(x = meanVerb, y = meanNoun, color = class)) +
  geom_point() +
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = wes_palettes$Moonrise1[4], size = 1) +
  labs(x = "Mean Distance to Verbs", y = "Mean Distance to Nouns")


meanDistances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  ylab("Typicality") +
  xlab("") +
  cowplot::theme_cowplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "none") +
  scale_fill_manual(values = c(colorPaletteDis[7], colorPaletteDis[5]))

# NHST

meanDistances %>% group_by(class) %>% summarize(median(typicality))

w.test <- coin::wilcox_test(data = mutate(meanDistances, class = factor(class)), typicality ~ class)

# Effect size
Z.value <- w.test %>%
  coin::statistic("standardized")
r.value <- Z.value / sqrt(nrow(meanDistances))
effsize::cohen.d(data = meanDistances, typicality ~ class)

# Nearest neighbor

getNearestNeighbors <- function(aLanguage, randomize = FALSE, distanceMatrix = NULL){
  # Finds the closest phonological neighbor for each word in a specificed
  # language. Phonological neighbors are determined by using Optimal String Alignment distance,
  # i.e. the product of getDistanceMatrix() unless different distance matrix is provided.
  # Returns a tibble with the neighbor and its category of the neighbor.
  # Args:
  #   aLanguage: A string specifying the name of the language to be analyzed
  #   e.g. "English".
  #   data: a data frame as subset of allPhon or allPhonCtrl.
  #   randomize: if TRUE, labels are shuffled because calculating the closest neighbors
  #   distanceMatrix: pre calculated distance matrix to use to speed up calculation.

  if(randomize == TRUE){
    aLanguage$englishPOS <- sample(aLanguage$englishPOS, replace = FALSE)
  }
  distanceMatrix <- distanceMatrix * -1
  diag(distanceMatrix) <- -100
  nearestNeighbors <- max.col(distanceMatrix, ties.method = "random")
  neighbors <- aLanguage$phon[nearestNeighbors]
  neighbors.category <- aLanguage$englishPOS[nearestNeighbors]
  aLanguage <- aLanguage %>%
    as_tibble() %>%
    mutate(neighbor = neighbors,
           neighbor.category = neighbors.category) %>%
    return()
}

neighbor.mc <- map_dfr(1:1000, function(iter){
  print(iter)
  getNearestNeighbors(pron.class, TRUE, distances) %>%
    mutate(correct = neighbor.category == englishPOS) %>%
    group_by(englishPOS) %>%
    summarize(proportion.correct = sum(correct) / n()) %>%
    mutate(iteration = iter) %>%
    return()
  })

neighbor.real <- map_dfr(1:100, function(x){
  getNearestNeighbors(pron.class, FALSE, distances) %>%
    mutate(correct = neighbor.category == englishPOS) %>%
    group_by(englishPOS) %>%
    summarize(proportion.correct = sum(correct) / n()) %>%
    return()
  })

neighbor.real <- neighbor.real %>%
  group_by(englishPOS) %>%
  summarize(Mean = mean(proportion.correct), SD = sd(proportion.correct))

#test

neighbor.mc %>%
  group_by(englishPOS) %>%
  summarize(max(proportion.correct), mean(proportion.correct), sd(proportion.correct))

neighbor.real %>%
  select(-SD) %>%
  right_join(neighbor.mc) %>%
  rename(baseline = proportion.correct) %>%
  mutate(is.higher = baseline >= Mean) %>%
  group_by(englishPOS, is.higher) %>%
  tally()

neighbor.plot <- neighbor.mc %>%
  group_by(englishPOS) %>%
  summarize(baseline = mean(proportion.correct), upper = baseline + (1 * sd(proportion.correct)), lower = baseline - (1 * sd(proportion.correct))) %>%
  right_join(neighbor.real)

neighbor.plot <- neighbor.plot %>%
  mutate(englishPOS = factor(englishPOS, levels = c("Verb", "Noun"))) %>%
  ggplot(aes(x = englishPOS, y = Mean)) +
  geom_col(aes(fill = englishPOS), color = "black") +
  geom_col(fill = "yellow", mapping = aes(y = baseline), color = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper ), width = .4) +
  labs(x = "Class", y = "Proportion of same-class nearest neighbor") +
  scale_fill_manual(values = c(colorPaletteDis[5], colorPaletteDis[7])) +
  theme_minimal() +
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        axis.title.x = element_blank())



# RNN

# kfold

rnn.performance <- read_csv('Results/RNN/celex_rnn_performance.csv') %>%
  select(-X1) %>%
  gather("measure", "value", Matthews:F1)

rnn.performance %>%
  filter(measure == "Matthews") %>%
  .$value %>%
  median()
rnn.performance %>%
  filter(measure == "Matthews") %>%
  .$value %>%
  IQR()
rnn.performance %>%
  filter(measure == "Matthews") %>%
  .$value %>%
  wilcox.test(mu = 0.1, alternative = "greater")

rnn.plot <- rnn.performance %>%
  filter(measure == "Matthews") %>%
  ggplot(aes(y = value, x = measure)) +
  stat_summary(fun.y = median, geom = "bar", fill = wes_palettes$Darjeeling1[2], color = "black") +
  stat_summary(fun.data = median_hilow, fun.args = (confint = .5), geom = "errorbar", width = 0.5) +
  labs(y = "Matthews Correlation Coefficient") +
  cowplot::theme_cowplot() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
  lims(y = c(0, 0.7))

# spurt

spurt.performance <- read_csv('Results/Spurt/celex_spurt_performance.csv') %>%
  select(-X1) %>%
  gather("measure", "value", Matthews:F1)

spurt.performance %>%
  filter(measure == "Matthews") %>%
  .$value %>%
  median()

spurt.performance %>%
  filter(measure == "Matthews") %>%
  .$value %>%
  IQR
spurt.performance %>%
  filter(measure == "Matthews") %>%
  .$value %>%
  wilcox.test(mu = 0.1, alternative = "greater")

spurt.plot <- spurt.performance %>%
  filter(measure == "Matthews") %>%
  ggplot(aes(y = value,  x = measure)) +
  stat_summary(fun.y = median, geom = "bar", fill = wes_palettes$Darjeeling1[1], color = "black") +
  stat_summary(fun.data = median_hilow, fun.args = (confint = .5), geom = "errorbar", width = 0.2) +
  labs(y = "Matthews Correlation Coefficient") +
  cowplot::theme_cowplot() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  lims(y = c(0, 0.7)) +
  theme(axis.title.x = element_blank())


cowplot::plot_grid(rnn.plot, spurt.plot, labels= "AUTO")

# Mono and Di syllable LEMMAS

celex.pron <-
  read_delim(
    'Data/Celex/epl.cd',
    delim = "\\",
    guess_max = 200000,
    quote = "",
    col_names = F
  ) %>%
  select(1:7)
colnames(celex.pron) <-
  c("Lemma.ID",
    "Lemma",
    "Raw.Freq",
    "Number.Of.Pronunciations",
    "Status.Of.Pronunciation",
    "phon",
    "Syllable.Structure")

# remove frequency 0 words

celex.pron <- celex.pron %>%
  filter(Raw.Freq > 0)

# celex.pron.clean <- celex.pron %>%
#   select(Headword, Raw.Freq, Lemma.ID, phon)
# load syntax data

celex.synt <- read_delim('Data/Celex/esl.cd', delim = "\\", guess_max = 200000, quote = "", col_names = F) %>%
  select(1:4)
colnames(celex.synt) <- c("Lemma.ID", "Lemma", "Raw.Freq", "Class")
celex.synt <- celex.synt %>%
  filter(Raw.Freq > 0)

# join by lemma id

pron.synt <- celex.synt %>%
  select(Lemma.ID, Class) %>%
  right_join(celex.pron) %>%
  mutate(Lemma.ID = factor(Lemma.ID), Class = factor(Class)) %>%
  select(Lemma.ID, Class, Lemma, Raw.Freq, phon, Syllable.Structure)

# recode class variable

levels(pron.synt$Class) <- c("Noun", "Adjective", "Numeral", "Verb", "Article",
                             "Pronoun", "Adverb", "Preposition", "Conjunction",
                             "Interjection", "Single.Contraction", "Complex.Contraction",
                             "Other", "Other", "Other")
# remove multi words

pron.synt <- pron.synt %>%
  filter(!(str_detect(Lemma, " ")))

# keep mono and di syllables. get syllables by counting the number of "]" on Syllable Structure variable.

pron.synt <- pron.synt %>%
  mutate(Number.Of.Syllables = str_count(Syllable.Structure, "]")) %>%
  filter(Number.Of.Syllables <= 2)

# get relative frequency of each class PER PRONUNCIATION, not necessarily Lemma.

pron.class <- pron.synt %>%
  group_by(phon, Class) %>%
  summarize(Comp.Freq = sum(Raw.Freq)) %>%
  spread(Class, Comp.Freq, fill = 0) %>%
  group_by() %>%
  mutate(Total.Freq = rowSums(.[2:14])) %>%
  # calculate relative frequency of each class per lemma
  mutate_if(is.double, function(x){return(x / .$Total.Freq)}) %>%
  select(-Total.Freq) %>%
  arrange(phon)

# take only nouns and verbs

pron.class <- pron.class %>%
  select(phon, Noun, Verb) %>%
  mutate(total = Noun + Verb) %>%
  filter(total > 0.5) %>%
  # difference in relative frequency
  mutate(diff = abs(Noun - Verb)) %>%
  filter(diff > 0.1) %>%
  select(-total, -diff)

# get majority class

pron.class$Class <- colnames(pron.class)[apply(pron.class, 1, which.max)]

# clean phon column

pron.class <- pron.class %>%
  mutate(phon = str_remove_all(phon, "\'"))
pron.class <- pron.class %>%
  rename(englishPOS = Class)

# Save for RNN

pron.class %>% 
  select(phon, englishPOS) %>% 
  add_column(language = "English") %>% 
  write_csv("Data/Processed/celex_di_lemmas.csv")

distances <- pron.class %>%
  getDistanceMatrix.norm()


meanDistances <- getMeanDistances(pron.class, distances)

meanDistances %>%
  mutate(class = factor(class, levels = c("Verb", "Noun"))) %>%
  ggplot(aes(x = meanVerb, y = meanNoun, fill = stat(nlevel))) +
  stat_density2d(geom = "polygon") +
  scale_fill_gradientn(name = "Density", colors = colorPaletteCont) +
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = wes_palettes$Moonrise1[4], size = 1) +
  facet_wrap(vars(class), nrow = 1) +
  labs(x = expression(hat(ND[V])), y = expression(hat(ND[N]))) +
  cowplot::theme_cowplot() +
  cowplot::panel_border()

meanDistances %>%
  ggplot(aes(x = meanVerb, y = meanNoun, color = class)) +
  geom_point() +
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = wes_palettes$Moonrise1[4], size = 1) +
  labs(x = "Mean Distance to Verbs", y = "Mean Distance to Nouns")


meanDistances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  ylab("Typicality") +
  xlab("") +
  cowplot::theme_cowplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "none") +
  scale_fill_manual(values = c(colorPaletteDis[7], colorPaletteDis[5]))

# NHST

meanDistances %>% group_by(class) %>% summarize(median(typicality))

w.test <- coin::wilcox_test(data = mutate(meanDistances, class = factor(class)), typicality ~ class)

# Effect size
Z.value <- w.test %>%
  coin::statistic("standardized")
r.value <- Z.value / sqrt(nrow(meanDistances))
effsize::cohen.d(data = meanDistances, typicality ~ class)

# Nearest neighbor

getNearestNeighbors <- function(aLanguage, randomize = FALSE, distanceMatrix = NULL){
  # Finds the closest phonological neighbor for each word in a specificed
  # language. Phonological neighbors are determined by using Optimal String Alignment distance,
  # i.e. the product of getDistanceMatrix() unless different distance matrix is provided.
  # Returns a tibble with the neighbor and its category of the neighbor.
  # Args:
  #   aLanguage: A string specifying the name of the language to be analyzed
  #   e.g. "English".
  #   data: a data frame as subset of allPhon or allPhonCtrl.
  #   randomize: if TRUE, labels are shuffled because calculating the closest neighbors
  #   distanceMatrix: pre calculated distance matrix to use to speed up calculation.
  
  if(randomize == TRUE){
    aLanguage$englishPOS <- sample(aLanguage$englishPOS, replace = FALSE)
  }
  distanceMatrix <- distanceMatrix * -1
  diag(distanceMatrix) <- -100
  nearestNeighbors <- max.col(distanceMatrix, ties.method = "random")
  neighbors <- aLanguage$phon[nearestNeighbors]
  neighbors.category <- aLanguage$englishPOS[nearestNeighbors]
  aLanguage <- aLanguage %>%
    as_tibble() %>%
    mutate(neighbor = neighbors,
           neighbor.category = neighbors.category) %>%
    return()
}

neighbor.mc <- map_dfr(1:1000, function(iter){
  print(iter)
  getNearestNeighbors(pron.class, TRUE, distances) %>%
    mutate(correct = neighbor.category == englishPOS) %>%
    group_by(englishPOS) %>%
    summarize(proportion.correct = sum(correct) / n()) %>%
    mutate(iteration = iter) %>%
    return()
})

neighbor.real <- map_dfr(1:100, function(x){
  getNearestNeighbors(pron.class, FALSE, distances) %>%
    mutate(correct = neighbor.category == englishPOS) %>%
    group_by(englishPOS) %>%
    summarize(proportion.correct = sum(correct) / n()) %>%
    return()
})

neighbor.real <- neighbor.real %>%
  group_by(englishPOS) %>%
  summarize(Mean = mean(proportion.correct), SD = sd(proportion.correct))

#test

neighbor.mc %>%
  group_by(englishPOS) %>%
  summarize(max(proportion.correct), mean(proportion.correct), sd(proportion.correct))

neighbor.real %>%
  select(-SD) %>%
  right_join(neighbor.mc) %>%
  rename(baseline = proportion.correct) %>%
  mutate(is.higher = baseline >= Mean) %>%
  group_by(englishPOS, is.higher) %>%
  tally()

neighbor.plot <- neighbor.mc %>%
  group_by(englishPOS) %>%
  summarize(baseline = mean(proportion.correct), upper = baseline + (1 * sd(proportion.correct)), lower = baseline - (1 * sd(proportion.correct))) %>%
  right_join(neighbor.real)

neighbor.plot <- neighbor.plot %>%
  mutate(englishPOS = factor(englishPOS, levels = c("Verb", "Noun"))) %>%
  ggplot(aes(x = englishPOS, y = Mean)) +
  geom_col(aes(fill = englishPOS), color = "black") +
  geom_col(fill = "yellow", mapping = aes(y = baseline), color = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper ), width = .4) +
  labs(x = "Class", y = "Proportion of same-class nearest neighbor") +
  scale_fill_manual(values = c(colorPaletteDis[5], colorPaletteDis[7])) +
  theme_minimal() +
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        axis.title.x = element_blank())

