library(tidyverse)
library(stringdist)
library(wesanderson)
library(BayesFactor)
library(emmeans)

colorPaletteCont <- wes_palette(name = 'Zissou1', type = 'continuous', n = 5)
colorPaletteDis <- c(wes_palette(name = 'Royal2'), wes_palette(name = 'Royal1'))

celex.pron <-
  read_delim(
    'Data/Celex/epl.cd',
    delim = "\\",
    guess_max = 200000,
    quote = "",
    col_names = F
  ) %>%
  select(1:8)
colnames(celex.pron) <-
  c(
    "Lemma.ID",
    "Lemma",
    "Raw.Freq",
    "Number.Of.Pronunciations",
    "Status.Of.Pronunciation",
    "phon",
    "Syllable.Structure",
    "CELEX.Syllables"
  )

# remove frequency 0 lemmas

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
  # difference in relative frequency frequency
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

celex.mono <- pron.class

# Monosyllables

celex.mono <- celex.mono %>%
  rename(englishPOS = Class)

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

distances <- celex.mono %>%
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

meanDistances <- getMeanDistances(celex.mono, distances)

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
  getNearestNeighbors(celex.mono, TRUE, distances) %>%
    mutate(correct = neighbor.category == englishPOS) %>%
    group_by(englishPOS) %>%
    summarize(proportion.correct = sum(correct) / n()) %>%
    mutate(iteration = iter) %>%
    return()
  })

neighbor.real <- map_dfr(1:100, function(x){
  getNearestNeighbors(celex.mono, FALSE, distances) %>%
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


ids.english <- read_csv('Data/Processed/allPhonFormsConcepticon.csv') %>%
  filter(language == "English") %>%
  select(Form, phon, ontologicalCategory)
ids.english <- ids.english %>%
  mutate(is.in.celex = Form %in% celex.comparison.ids$Headword) %>%
  rename(Headword = Form)
ids.english.short <- ids.english %>%
  filter(is.in.celex == TRUE)
ids.english %>%
  summarize(sum(is.in.celex))
ids.english %>%
  group_by(ontologicalCategory) %>%
  summarize(sum(is.in.celex))
ids.english %>%
  left_join(select(celex.comparison.ids, Class, Headword)) %>%
  mutate(same.class = ifelse((ontologicalCategory == "Action" & Class == "Verb") | (ontologicalCategory == "Thing" & Class == "Noun"), TRUE, FALSE)) %>%
  group_by(ontologicalCategory) %>%
  summarize(sum(same.class, na.rm = TRUE))

ids.english %>%
  mutate(length = nchar(phon)) %>%
  summarize(mean(length))
celex.mono %>%
  mutate(phon = str_remove_all(phon, "-")) %>%
  mutate(length = nchar(phon)) %>%
  summarize(mean(length))
ids.english %>%
  mutate(length = nchar(phon)) %>%
  group_by(ontologicalCategory) %>%
  summarize(mean(length))
celex.mono %>%
  mutate(phon = str_remove_all(phon, "-")) %>%
  mutate(length = nchar(phon)) %>%
  group_by(englishPOS) %>%
  summarize(mean(length))
ids.english.short %>%
  filter(ontologicalCategory != "Other") %>%
  mutate(length = nchar(phon)) %>%
  summarize(mean(length))
ids.english.short %>%
  filter(ontologicalCategory != "Other") %>%
  mutate(length = nchar(phon)) %>%
  group_by(ontologicalCategory) %>%
  summarize(mean(length))

celex.pron %>%
  select(Headword, Raw.Freq) %>%
  right_join(ids.english) %>%
  group_by(Headword) %>%
  mutate(Raw.Freq = sum(Raw.Freq)) %>%
  distinct() %>%
  group_by() %>%
  filter(ontologicalCategory != "Other") %>%
  mutate(Raw.Freq = replace_na(Raw.Freq, 0)) %>%
  summarize(mean(Raw.Freq, na.rm = TRUE))

celex.comparison.ids %>%
  group_by(celex.phon) %>%
  mutate(Raw.Freq = sum(Raw.Freq)) %>%
  distinct() %>%
  group_by() %>%
  summarize(mean(Raw.Freq, na.rm = TRUE))

celex.comparison.ids %>%
  group_by(celex.phon) %>%
  mutate(Raw.Freq = sum(Raw.Freq)) %>%
  distinct() %>%
  group_by(Class) %>%
  summarize(mean(Raw.Freq, na.rm = TRUE))

# this is for the SHARED words
celex.pron %>%
  select(Headword, phon, Raw.Freq) %>%
  right_join(rename(ids.english, cmu.phon = phon)) %>%
  group_by(Headword) %>%
  mutate(Raw.Freq = sum(Raw.Freq)) %>%
  distinct() %>%
  group_by() %>%
  mutate(Raw.Freq = replace_na(Raw.Freq, 0)) %>%
  filter(is.in.celex == TRUE, ontologicalCategory != "Other") %>%
  group_by(ontologicalCategory) %>%
  summarize(mean(Raw.Freq, na.rm = TRUE))


getMeanDistances.Concepticon <- function(languageDF, distanceMatrix){
  actionMask <- languageDF$ontologicalCategory == "Action"
  thingMask <- languageDF$ontologicalCategory == "Thing"
  meanDistanceMatrix <- data_frame(word = languageDF$phon) %>%
    mutate(meanVerb = rowMeans(distanceMatrix[, actionMask]),
           meanNoun = rowMeans(distanceMatrix[, thingMask]),
           typicality = meanVerb - meanNoun,
           class = languageDF$ontologicalCategory)
  return(meanDistanceMatrix)
}

ids.distances <- getDistanceMatrix(languageDF = ids.english.short)
ids.mean.distances <- getMeanDistances.Concepticon(languageDF = ids.english.short, distanceMatrix = ids.distances)
ids.mean.distances %>%
  filter(class != "Other") %>%
  ggplot(aes(x = meanVerb, y = meanNoun, fill = stat(nlevel))) +
  stat_density2d(geom = "polygon") +
  scale_fill_gradientn(name = "Density", colors = colorPaletteCont) +
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = wes_palettes$Moonrise1[4], size = 1) +
  expand_limits(x = 2.5, y= 2.5) +
  facet_wrap(vars(class), nrow = 2) +
  labs(title = "Density for common words between IDS and CELEX",
       subtitle = "Using CMUDict transcription",
       x = "Mean Distance to Verbs", y = "Mean Distance to Nouns")

ids.mean.distances %>%
  filter(class != "Other") %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  labs(title = "Typicality per class for common words between IDS and CELEX",
       subtitle = "Using CMUDict Transcription",
       x = "",
       y = "Typicality") +
  theme_minimal() +
  theme(legend.position = "none")

ids.short.celexphon <- ids.english.short %>%
  left_join(celex.comparison.ids) %>%
  group_by(phon) %>%
  mutate(Raw.Freq = sum(Raw.Freq)) %>%
  distinct() %>%
  group_by() %>%
  rename(ids.phon = phon, phon = celex.phon)

ids.short.celexphon %>%
  mutate(length = nchar(phon)) %>%
  summarize(mean(length))

ids.short.celexphon %>%
  mutate(length = nchar(phon)) %>%
  filter(ontologicalCategory != "Other") %>%
  group_by(ontologicalCategory) %>%
  summarize(mean(length))

ids.distances.celexphon <- ids.short.celexphon %>%
  getDistanceMatrix

ids.mean.distances.celexphon <- getMeanDistances.Concepticon(languageDF = ids.short.celexphon,
                                                             distanceMatrix = ids.distances.celexphon) %>%
  distinct()

ids.mean.distances.celexphon %>%
  filter(class != "Other") %>%
  ggplot(aes(x = meanVerb, y = meanNoun, fill = stat(nlevel))) +
  stat_density2d(geom = "polygon") +
  scale_fill_gradientn(name = "Density", colors = colorPaletteCont) +
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = wes_palettes$Moonrise1[4], size = 1) +
  facet_wrap(vars(class), nrow = 2) +
  expand_limits(x = 2.5, y = 2.5) +
  labs(title = "Density for common words between IDS and CELEX",
       subtitle = "Using CELEX transcription",
       x = "Mean Distance to Verbs", y = "Mean Distance to Nouns")

ids.mean.distances.celexphon %>%
  filter(class != "Other") %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  labs(title = "Typicality per class for common words between IDS and CELEX",
       subtitle = "Using CELEX Transcription",
       x = "",
       y = "Typicality") +
  theme_minimal() +
  theme(legend.position = "none")

#= trying celex.mono analysis with both transcriptions

celex.cmu <- read_csv('Data/PhonMining/DataForG2P/CMU.in.IPA.txt',
                     col_names = c("Headword", "phon")) %>%
  right_join(celex.comparison.ids) %>%
  filter(!(is.na(phon))) %>%
  group_by(phon) %>%
  mutate(Raw.Freq = sum(Raw.Freq)) %>%
  filter(Raw.Freq > 0) %>%
  group_by() %>%
  distinct() %>%
  rename(englishPOS = Class)

celex.cmu.distances <- getDistanceMatrix(celex.cmu)
celex.cmu.mean.distances <- getMeanDistances(celex.cmu, celex.cmu.distances)
celex.cmu.mean.distances %>%
  filter(class != "Other") %>%
  ggplot(aes(x = meanVerb, y = meanNoun, fill = stat(nlevel))) +
  stat_density2d(geom = "polygon") +
  scale_fill_gradientn(name = "Density", colors = colorPaletteCont) +
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = wes_palettes$Moonrise1[4], size = 1) +
  expand_limits(x = 3.5, y= 3.5) +
  facet_wrap(vars(class), nrow = 2) +
  labs(title = "Density for CELEX monosyllables",
       subtitle = "Using CMUDict transcription",
       x = "Mean Distance to Verbs", y = "Mean Distance to Nouns")

celex.cmu.mean.distances %>%
  filter(class != "Other") %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  labs(title = "Typicality per class for CELEX monosyllables",
       subtitle = "Using CMUDict Transcription",
       x = "",
       y = "Typicality") +
  theme_minimal() +
  theme(legend.position = "none")

celex.disc <- celex.cmu %>%
  rename(cmu.phon = phon, phon = celex.phon)
celex.disc.distances <- getDistanceMatrix(celex.disc)
celex.disc.mean.distances <- getMeanDistances(celex.disc, celex.disc.distances)
celex.disc.mean.distances %>%
  filter(class != "Other") %>%
  ggplot(aes(x = meanVerb, y = meanNoun, fill = stat(nlevel))) +
  stat_density2d(geom = "polygon") +
  scale_fill_gradientn(name = "Density", colors = colorPaletteCont) +
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = wes_palettes$Moonrise1[4], size = 1) +
  expand_limits(x = 3.5, y= 3.5) +
  facet_wrap(vars(class), nrow = 2) +
  labs(title = "Density for CELEX monosyllables",
       subtitle = "Using CELEX transcription",
       x = "Mean Distance to Verbs", y = "Mean Distance to Nouns")
celex.disc.mean.distances %>%
  filter(class != "Other") %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  labs(title = "Typicality per class for CELEX monosyllables",
       subtitle = "Using CELEX Transcription",
       x = "",
       y = "Typicality") +
  theme_minimal() +
  theme(legend.position = "none")

# trying IDS analysis with both transcriptions

ids.celexphon <- celex.pron %>%
  select(Headword, phon, Raw.Freq) %>%
  right_join(rename(ids.english, cmu.phon = phon)) %>%
  group_by(Headword) %>%
  mutate(Raw.Freq = sum(Raw.Freq)) %>%
  mutate(phon = str_remove_all(phon, "[\\'-]")) %>%
  distinct() %>%
  filter(!(is.na(phon)))

ids.celexphon.distances <- getDistanceMatrix(ids.celexphon)
ids.celexphon.mean.distances <- getMeanDistances.Concepticon(ids.celexphon, ids.celexphon.distances)

ids.celexphon.mean.distances %>%
  filter(class != "Other") %>%
  ggplot(aes(x = meanVerb, y = meanNoun, fill = stat(nlevel))) +
  stat_density2d(geom = "polygon") +
  scale_fill_gradientn(name = "Density", colors = colorPaletteCont) +
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = wes_palettes$Moonrise1[4], size = 1) +
  expand_limits(x = 3, y = 3) +
  facet_wrap(vars(class), nrow = 2) +
  labs(title = "Density for IDS words",
       subtitle = "Using CELEX transcription",
       x = "Mean Distance to Verbs", y = "Mean Distance to Nouns")

ids.celexphon.mean.distances %>%
  filter(class != "Other") %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  labs(title = "Typicality per class for IDS words",
       subtitle = "Using CELEX Transcription",
       x = "",
       y = "Typicality") +
  theme_minimal() +
  theme(legend.position = "none")

ids.cmuphon <- ids.celexphon %>%
  rename(celex.phon = phon, phon = cmu.phon)

ids.cmuphon.distances <- getDistanceMatrix(ids.cmuphon)
ids.cmuphon.mean.distances <- getMeanDistances.Concepticon(ids.cmuphon, ids.cmuphon.distances)

ids.cmuphon.mean.distances %>%
  filter(class != "Other") %>%
  ggplot(aes(x = meanVerb, y = meanNoun, fill = stat(nlevel))) +
  stat_density2d(geom = "polygon") +
  scale_fill_gradientn(name = "Density", colors = colorPaletteCont) +
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = wes_palettes$Moonrise1[4], size = 1) +
  expand_limits(x = 3, y = 3) +
  facet_wrap(vars(class), nrow = 2) +
  labs(title = "Density for IDS words",
       subtitle = "Using CMUDict transcription",
       x = "Mean Distance to Verbs", y = "Mean Distance to Nouns")

ids.celexphon.mean.distances %>%
  filter(class != "Other") %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  labs(title = "Typicality per class for IDS words",
       subtitle = "Using CMUDict Transcription",
       x = "",
       y = "Typicality") +
  theme_minimal() +
  theme(legend.position = "none")


ids.length <- ids.cmuphon.mean.distances %>%
  mutate(length = as.factor(nchar(word)))
levels(ids.length$length) <- c("<= 3", "<= 3", "4", "5", "6", "7", "> 7", "> 7", "> 7", "> 7")

ids.length %>%
  filter(class != "Other") %>%
  ggplot(aes(x = meanVerb, y = meanNoun, fill = stat(nlevel))) +
  stat_density2d(geom = "polygon") +
  scale_fill_gradientn(name = "Density", colors = colorPaletteCont) +
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = wes_palettes$Moonrise1[4], size = 1) +
  facet_wrap(c("length", "class"), scales = "free", shrink = TRUE, ncol = 2)

ids.length %>%
  filter(class != "Other") %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap("length") +
  geom_hline(yintercept = 0)

ids.english <- ids.english %>%
  mutate(length = nchar(phon))

ids.english.3 <- ids.english %>%
  filter(length <= 3 & ontologicalCategory != "Other")
ids.english.3.distances <- ids.english.3 %>%
  getMeanDistances.Concepticon(languageDF = ., distanceMatrix = getDistanceMatrix.norm(.))

ids.violin.3 <- ids.english.3.distances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0)

ids.english.4 <- ids.english %>%
  filter(length <= 4 & ontologicalCategory != "Other")
ids.english.4.distances <- ids.english.4 %>%
  getMeanDistances.Concepticon(languageDF = ., distanceMatrix = getDistanceMatrix.norm(.))

ids.violin.4 <- ids.english.4.distances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0)

ids.english.5 <- ids.english %>%
  filter(length <= 5 & ontologicalCategory != "Other")
ids.english.5.distances <- ids.english.5 %>%
  getMeanDistances.Concepticon(languageDF = ., distanceMatrix = getDistanceMatrix.norm(.))

ids.violin.5 <- ids.english.5.distances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0)


ids.english.6 <- ids.english %>%
  filter(length <= 6 & ontologicalCategory != "Other")
ids.english.6.distances <- ids.english.6 %>%
  getMeanDistances.Concepticon(languageDF = ., distanceMatrix = getDistanceMatrix.norm(.))

ids.violin.6 <- ids.english.6.distances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0)
ids.english.7 <- ids.english %>%
  filter(length <= 7 & ontologicalCategory != "Other")
ids.english.7.distances <- ids.english.7 %>%
  getMeanDistances.Concepticon(languageDF = ., distanceMatrix = getDistanceMatrix.norm(.))

ids.violin.7 <- ids.english.7.distances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0)


ids.english.all <- ids.english %>%
  filter(ontologicalCategory != "Other")
ids.english.all.distances <- ids.english.all %>%
getMeanDistances.Concepticon(languageDF = ., distanceMatrix = getDistanceMatrix.norm(.))

ids.violin.all <- ids.english.all.distances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0)

cowplot.title.violin <- ggplot() +
  labs(title = "Distributions of typicality by class", subtitle = "With varying length of word considered. Using NORMALIZED edit distance.")


violin.grid <- cowplot::plot_grid(ids.violin.3, ids.violin.4, ids.violin.5, ids.violin.6, ids.violin.7, ids.violin.all, ncol = 2,
                   labels = c("length <= 3", "length <= 4", "length <= 5", "length <= 6", "length <= 7", "all words"))
cowplot::plot_grid(cowplot.title.violin, violin.grid, ncol = 1, rel_heights = c(0.1, 1))


ids.density.3 <- ids.english.3.distances %>%
  ggplot(aes(x = meanVerb, y = meanNoun, fill = stat(nlevel))) +
  stat_density2d(geom = "polygon") +
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = wes_palettes$Moonrise1[4], size = 1) +
  scale_fill_gradientn(name = "Density", colors = colorPaletteCont) +
  facet_wrap(vars(class)) +
  theme(legend.position = "none")
ids.density.4 <- ids.english.4.distances %>%
  ggplot(aes(x = meanVerb, y = meanNoun, fill = stat(nlevel))) +
  stat_density2d(geom = "polygon") +
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = wes_palettes$Moonrise1[4], size = 1) +
  scale_fill_gradientn(name = "Density", colors = colorPaletteCont) +
  facet_wrap(vars(class)) +
  theme(legend.position = "none")
ids.density.5 <- ids.english.5.distances %>%
  ggplot(aes(x = meanVerb, y = meanNoun, fill = stat(nlevel))) +
  stat_density2d(geom = "polygon") +
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = wes_palettes$Moonrise1[4], size = 1) +
  scale_fill_gradientn(name = "Density", colors = colorPaletteCont) +
  facet_wrap(vars(class)) +
  theme(legend.position = "none")
ids.density.6 <- ids.english.6.distances %>%
  ggplot(aes(x = meanVerb, y = meanNoun, fill = stat(nlevel))) +
  stat_density2d(geom = "polygon") +
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = wes_palettes$Moonrise1[4], size = 1) +
  scale_fill_gradientn(name = "Density", colors = colorPaletteCont) +
  facet_wrap(vars(class)) +
  theme(legend.position = "none")
ids.density.7 <- ids.english.7.distances %>%
  ggplot(aes(x = meanVerb, y = meanNoun, fill = stat(nlevel))) +
  stat_density2d(geom = "polygon") +
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = wes_palettes$Moonrise1[4], size = 1) +
  scale_fill_gradientn(name = "Density", colors = colorPaletteCont) +
  facet_wrap(vars(class)) +
  theme(legend.position = "none")
ids.density.all <- ids.english.all.distances %>%
  ggplot(aes(x = meanVerb, y = meanNoun, fill = stat(nlevel))) +
  stat_density2d(geom = "polygon") +
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = wes_palettes$Moonrise1[4], size = 1) +
  scale_fill_gradientn(name = "Density", colors = colorPaletteCont) +
  facet_wrap(vars(class)) +
  theme(legend.position = "none")

cowplot.title.density <- ggplot() +
  labs(title = "Density of mean distances by class", subtitle = "With varying length of word considered. Using NORMALIZED edit distance.")
density.grid <- cowplot::plot_grid(ids.density.3, ids.density.4, ids.density.5, ids.density.6, ids.density.7, ids.density.all, ncol = 2,
                                  labels = c("length <= 3", "length <= 4", "length <= 5", "length <= 6", "length <= 7", "all words"))
cowplot::plot_grid(cowplot.title.density, density.grid, ncol = 1, rel_heights = c(0.1, 1))

danish.words <- read_csv('Data/Processed/allPhonFormsConcepticon.csv') %>%
  filter(language == "Danish") %>%
  select(Form, phon, ontologicalCategory) %>%
  mutate(length = nchar(phon))

danish.words.3 <- danish.words %>%
  filter(length <= 3 & ontologicalCategory != "Other")
danish.words.3.distances <- danish.words.3 %>%
  getMeanDistances.Concepticon(languageDF = ., distanceMatrix = getDistanceMatrix.norm(.))

danish.violin.3 <- danish.words.3.distances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0)

danish.words.4 <- danish.words %>%
  filter(length <= 4 & ontologicalCategory != "Other")
danish.words.4.distances <- danish.words.4 %>%
  getMeanDistances.Concepticon(languageDF = ., distanceMatrix = getDistanceMatrix.norm(.))

danish.violin.4 <- danish.words.4.distances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0)

danish.words.5 <- danish.words %>%
  filter(length <= 5 & ontologicalCategory != "Other")
danish.words.5.distances <- danish.words.5 %>%
  getMeanDistances.Concepticon(languageDF = ., distanceMatrix = getDistanceMatrix.norm(.))

danish.violin.5 <- danish.words.5.distances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0)


danish.words.6 <- danish.words %>%
  filter(length <= 6 & ontologicalCategory != "Other")
danish.words.6.distances <- danish.words.6 %>%
  getMeanDistances.Concepticon(languageDF = ., distanceMatrix = getDistanceMatrix.norm(.))

danish.violin.6 <- danish.words.6.distances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0)
danish.words.7 <- danish.words %>%
  filter(length <= 7 & ontologicalCategory != "Other")
danish.words.7.distances <- danish.words.7 %>%
  getMeanDistances.Concepticon(languageDF = ., distanceMatrix = getDistanceMatrix.norm(.))

danish.violin.7 <- danish.words.7.distances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0)


danish.words.all <- danish.words %>%
  filter(ontologicalCategory != "Other")
danish.words.all.distances <- danish.words.all %>%
  getMeanDistances.Concepticon(languageDF = ., distanceMatrix = getDistanceMatrix.norm(.))

danish.violin.all <- danish.words.all.distances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0)

cowplot.title.violin <- ggplot() +
  labs(title = "Distributions of typicality by class for DANISH", subtitle = "With varying length of word considered. Using NORMALIZED edit distance.")

violin.grid <- cowplot::plot_grid(danish.violin.3, danish.violin.4, danish.violin.5, danish.violin.6, danish.violin.7, danish.violin.all, ncol = 2,
                                  labels = c("length <= 3", "length <= 4", "length <= 5", "length <= 6", "length <= 7", "all words"))
cowplot::plot_grid(cowplot.title.violin, violin.grid, ncol = 1, rel_heights = c(0.1, 1))

spanish.words <- read_csv('Data/Processed/allPhonFormsConcepticon.csv') %>%
  filter(language == "Spanish") %>%
  select(Form, phon, ontologicalCategory) %>%
  mutate(length = nchar(phon))

spanish.words.3 <- spanish.words %>%
  filter(length <= 3 & ontologicalCategory != "Other")
spanish.words.3.distances <- spanish.words.3 %>%
  getMeanDistances.Concepticon(languageDF = ., distanceMatrix = getDistanceMatrix.norm(.))

spanish.violin.3 <- spanish.words.3.distances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0)

spanish.words.4 <- spanish.words %>%
  filter(length <= 4 & ontologicalCategory != "Other")
spanish.words.4.distances <- spanish.words.4 %>%
  getMeanDistances.Concepticon(languageDF = ., distanceMatrix = getDistanceMatrix.norm(.))

spanish.violin.4 <- spanish.words.4.distances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0)

spanish.words.5 <- spanish.words %>%
  filter(length <= 5 & ontologicalCategory != "Other")
spanish.words.5.distances <- spanish.words.5 %>%
  getMeanDistances.Concepticon(languageDF = ., distanceMatrix = getDistanceMatrix.norm(.))

spanish.violin.5 <- spanish.words.5.distances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0)


spanish.words.6 <- spanish.words %>%
  filter(length <= 6 & ontologicalCategory != "Other")
spanish.words.6.distances <- spanish.words.6 %>%
  getMeanDistances.Concepticon(languageDF = ., distanceMatrix = getDistanceMatrix.norm(.))

spanish.violin.6 <- spanish.words.6.distances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0)
spanish.words.7 <- spanish.words %>%
  filter(length <= 7 & ontologicalCategory != "Other")
spanish.words.7.distances <- spanish.words.7 %>%
  getMeanDistances.Concepticon(languageDF = ., distanceMatrix = getDistanceMatrix.norm(.))

spanish.violin.7 <- spanish.words.7.distances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0)


spanish.words.all <- spanish.words %>%
  filter(ontologicalCategory != "Other")
spanish.words.all.distances <- spanish.words.all %>%
  getMeanDistances.Concepticon(languageDF = ., distanceMatrix = getDistanceMatrix.norm(.))

spanish.violin.all <- spanish.words.all.distances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0)

cowplot.title.violin <- ggplot() +
  labs(title = "Distributions of typicality by class for SPANISH", subtitle = "With varying length of word considered. Using NORMALIZED edit distance.")

violin.grid <- cowplot::plot_grid(spanish.violin.3, spanish.violin.4, spanish.violin.5, spanish.violin.6, spanish.violin.7, spanish.violin.all, ncol = 2,
                                  labels = c("length <= 3", "length <= 4", "length <= 5", "length <= 6", "length <= 7", "all words"))
cowplot::plot_grid(cowplot.title.violin, violin.grid, ncol = 1, rel_heights = c(0.1, 1))

avar.words <- read_csv('Data/Processed/allPhonFormsConcepticon.csv') %>%
  filter(language == "Avar") %>%
  select(Form, phon, ontologicalCategory) %>%
  mutate(length = nchar(phon))

avar.words.4 <- avar.words %>%
  filter(length <= 4 & ontologicalCategory != "Other")
avar.words.4.distances <- avar.words.4 %>%
  getMeanDistances.Concepticon(languageDF = ., distanceMatrix = getDistanceMatrix.norm(.))

avar.violin.4 <- avar.words.4.distances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0)

avar.words.5 <- avar.words %>%
  filter(length <= 5 & ontologicalCategory != "Other")
avar.words.5.distances <- avar.words.5 %>%
  getMeanDistances.Concepticon(languageDF = ., distanceMatrix = getDistanceMatrix.norm(.))

avar.violin.5 <- avar.words.5.distances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0)


avar.words.6 <- avar.words %>%
  filter(length <= 6 & ontologicalCategory != "Other")
avar.words.6.distances <- avar.words.6 %>%
  getMeanDistances.Concepticon(languageDF = ., distanceMatrix = getDistanceMatrix.norm(.))

avar.violin.6 <- avar.words.6.distances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0)
avar.words.7 <- avar.words %>%
  filter(length <= 7 & ontologicalCategory != "Other")
avar.words.7.distances <- avar.words.7 %>%
  getMeanDistances.Concepticon(languageDF = ., distanceMatrix = getDistanceMatrix.norm(.))

avar.violin.7 <- avar.words.7.distances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0)


avar.words.all <- avar.words %>%
  filter(ontologicalCategory != "Other")
avar.words.all.distances <- avar.words.all %>%
  getMeanDistances.Concepticon(languageDF = ., distanceMatrix = getDistanceMatrix.norm(.))

avar.violin.all <- avar.words.all.distances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0)

cowplot.title.violin <- ggplot() +
  labs(title = "Distributions of typicality by class for AVAR", subtitle = "With varying length of word considered. Using NORMALIZED edit distance.")

violin.grid <- cowplot::plot_grid(avar.violin.4, avar.violin.5, avar.violin.6, avar.violin.7, avar.violin.all, ncol = 2,
                                  labels = c("length <= 4", "length <= 5", "length <= 6", "length <= 7", "all words"))
cowplot::plot_grid(cowplot.title.violin, violin.grid, ncol = 1, rel_heights = c(0.1, 1))

hawaiian.words <- read_csv('Data/Processed/allPhonFormsConcepticon.csv') %>%
  filter(language == "Hawaiian") %>%
  select(Form, phon, ontologicalCategory) %>%
  mutate(length = nchar(phon))

hawaiian.words.4 <- hawaiian.words %>%
  filter(length <= 4 & ontologicalCategory != "Other")
hawaiian.words.4.distances <- hawaiian.words.4 %>%
  getMeanDistances.Concepticon(languageDF = ., distanceMatrix = getDistanceMatrix.norm(.))

hawaiian.violin.4 <- hawaiian.words.4.distances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0)

hawaiian.words.5 <- hawaiian.words %>%
  filter(length <= 5 & ontologicalCategory != "Other")
hawaiian.words.5.distances <- hawaiian.words.5 %>%
  getMeanDistances.Concepticon(languageDF = ., distanceMatrix = getDistanceMatrix.norm(.))

hawaiian.violin.5 <- hawaiian.words.5.distances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0)


hawaiian.words.6 <- hawaiian.words %>%
  filter(length <= 6 & ontologicalCategory != "Other")
hawaiian.words.6.distances <- hawaiian.words.6 %>%
  getMeanDistances.Concepticon(languageDF = ., distanceMatrix = getDistanceMatrix.norm(.))

hawaiian.violin.6 <- hawaiian.words.6.distances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0)
hawaiian.words.7 <- hawaiian.words %>%
  filter(length <= 7 & ontologicalCategory != "Other")
hawaiian.words.7.distances <- hawaiian.words.7 %>%
  getMeanDistances.Concepticon(languageDF = ., distanceMatrix = getDistanceMatrix.norm(.))

hawaiian.violin.7 <- hawaiian.words.7.distances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0)
hawaiian.words.8 <- hawaiian.words %>%
  filter(length <= 8 & ontologicalCategory != "Other")
hawaiian.words.8.distances <- hawaiian.words.8 %>%
  getMeanDistances.Concepticon(languageDF = ., distanceMatrix = getDistanceMatrix.norm(.))

hawaiian.violin.8 <- hawaiian.words.8.distances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0)


hawaiian.words.all <- hawaiian.words %>%
  filter(ontologicalCategory != "Other")
hawaiian.words.all.distances <- hawaiian.words.all %>%
  getMeanDistances.Concepticon(languageDF = ., distanceMatrix = getDistanceMatrix.norm(.))

hawaiian.violin.all <- hawaiian.words.all.distances %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0)

cowplot.title.violin <- ggplot() +
  labs(title = "Distributions of typicality by class for HAWAIIAN", subtitle = "With varying length of word considered. Using NORMALIZED edit distance.")

violin.grid <- cowplot::plot_grid(hawaiian.violin.4, hawaiian.violin.5, hawaiian.violin.6, hawaiian.violin.7, hawaiian.violin.8, hawaiian.violin.all, ncol = 2,
                                  labels = c("length <= 4", "length <= 5", "length <= 6", "length <= 7", "length <= 8", "all words"))
cowplot::plot_grid(cowplot.title.violin, violin.grid, ncol = 1, rel_heights = c(0.1, 1))