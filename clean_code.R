# Packages ----------------------------------------------------------------

# library(checkpoint)
# checkpoint("2019-03-25")
require(tidyverse)
require(stringdist)
require(emmeans)
require(caret)
require(wesanderson)
require(ggridges)
require(parallelDist)
require(ggthemes)
require(ggpubr)
require(future)
require(furrr)

future::plan(multicore)
cores <- 5
options(future.globals.maxSize = +Inf, mc.cores = cores)

colorPaletteDis <- c(wes_palette(name = 'Royal2'), wes_palette(name = 'Royal1'))
colorPaletteCont <- wes_palette(name = 'Zissou1', type = 'continuous', n = 5)


# Load data -----------------------------------------------------

allPhon <- read_csv('Data/Processed/allPhonFormsConcepticon.csv')
phonLanguages <- read_csv('Data/Processed/allPhonLanguagesConcepticon.csv')

included.languages <- read_delim('included_languages.txt', delim = "\n",
                                col_names = c("language"), comment = "#")

allPhon <- allPhon %>%
  filter(language %in% included.languages$language)

phonLanguages <- phonLanguages %>%
  filter(Name %in% included.languages$language)

# allPhonControl <- allPhon %>%
  # mutate(phon = str_sub(phon, 1, -3)) %>%
  # filter(nchar(phon) > 2) %>%
  # group_by(language) %>%
  # mutate(nwords = n()) %>%
  # filter(nwords > 100) %>%
  # select(-nwords) %>%
  # group_by()

# allPhon <- allPhonControl

# Functions ---------------------------------------------------------------

getLanguage <- function(aLanguage, data = allPhon){
  # Extracts the dataframe of a specific language and subsets variables
  # Args:
  #   aLanguage: String with a language name, e.g. "English"
  #   data: complete data with all languages. By default, it looks for an object called "allPhon"
  # Returns: Tibble with only rows corresponding to specified
  # language and columns for Phon form, Meaning and Class.
  data %>%
    dplyr::select(phon, englishName, language, ontologicalCategory) %>%
    filter(language == aLanguage) %>%
    dplyr::select(-language) %>%
    return()
}

getDistanceMatrix <- function(languageDF){
  # Constructs square distance matrix for the words in a specific language using
  # OSA distance. Designed to be used on products of getLanguage().
  # Args:
  #   languageDF: a dataframe or tibble with a column called "phon" that stores phonetic forms.
  # Returns:
  #   An n x n matrix where n is the number of phon forms included in language
  #   DF, and each cell contains the string distance between the word in the row
  #   and the word in the column.
  distanceMatrix <- stringdistmatrix(languageDF$phon, languageDF$phon)
  colnames(distanceMatrix) <- languageDF$phon
  return(distanceMatrix)
}

getDistanceMatrix.norm <- function(languageDF){
  # Constructs square distance matrix for the words in a specific language using
  # OSA distance. After that, it normalizes the distances by dividing them by the
  # length of the longest word. Designed to be used on products of getLanguage().
  # Args:
  #   languageDF: a dataframe or tibble with a column called "phon" that stores phonetic forms.
  # Returns:
  #   An n x n matrix where n is the number of phon forms included in language
  #   DF, and each cell contains the string distance between the word in the row
  #   and the word in the column.
  words <- languageDF$phon
  distanceMatrix <- stringdistmatrix(words, words)
  colnames(distanceMatrix) <- words
  row.names(distanceMatrix) <- words
  lengths <- map_dbl(words, nchar)
  # max.lengths <- outer(lengths, lengths, FUN = "pmax")
  sum.lengths <- outer(lengths, lengths, FUN = "+")
  numerator <- 2 * distanceMatrix
  denominator <- sum.lengths + distanceMatrix
  distanceMatrix <- numerator / denominator
  # distanceMatrix <- distanceMatrix / max.lengths
  return(distanceMatrix)
}

getMeanDistances <- function(languageDF, distanceMatrix){
  # Gets the mean distance of each word to the different syntactic classes in
  # language DF.
  # Args:
  #   languageDF: a dataframe or tibble with a column called englishPOS that
  #   stores the classes.
  #   distanceMatrix: a square distance matrix specifying the string distance of
  #   the phon forms in languageDF. Designed to work with the product of
  #   getDistanceMatrix().
  # Returns:
  #   A tibble with n rows and 7 columns, where each row is a phon form in
  #   languageDF.
  #     word: contains the phon form of the word.
  #     meanVerb, meanNoun, meanOther: mean distance of the word with each of
  #     the classes in englishPOS
  #     tipicality: measures the relative distance to verbs and nouns with
  #     meanDistanceToVerbs - meanDistanceToNouns.
  #     class: contains the class of the word in the row.
  #     englishName is the english meaning of the word as per IDS.

  action.mask <- languageDF$ontologicalCategory == "Action"
  thing.mask <- languageDF$ontologicalCategory == "Thing"
  diag(distanceMatrix) <- NA
  meanDistanceMatrix <- data_frame(word = languageDF$phon) %>%
    mutate(mean.action = rowMeans(distanceMatrix[, action.mask], na.rm = TRUE),
           mean.thing = rowMeans(distanceMatrix[, thing.mask], na.rm = TRUE),
           typicality = mean.action - mean.thing,
           class = languageDF$ontologicalCategory,
           englishName = languageDF$englishName)
  return(meanDistanceMatrix)
}

getSymMatrix <- function(languageDF){
  # Constructs a phoneme occurrence matrix based on phon forms in a single language.
  # Args:
  #   languageDF: a dataframe or tibble with a column called phon that stores
  #   the phon forms of the language.
  # Returns:
  #   a tibble with n rows and i columns, where n is the number of phon
  # forms in languageDF, and i is the number of unique phonemes occurring all
  # phon forms. Each cell of each row contains a count of the number of times
  # the unique phoneme appears in that phon form.
  uniqueSymbols <-languageDF$phon %>%
    paste(collapse = "") %>%
    strsplit(x = ., split = "")
  uniqueSymbols <- unique(uniqueSymbols[[1]])
  symMatrix <- sapply(X = languageDF$phon, FUN = function(x){
    resultVec <- rep(x = 0, length(uniqueSymbols))
    names(resultVec) <- uniqueSymbols
    wordVec <- strsplit(x, "")[[1]]
    wordVec <- match(wordVec, uniqueSymbols)
    wordVec <- table(wordVec)
    indices <- as.numeric(names(wordVec))
    resultVec[indices] <- wordVec
    return(resultVec)
  }) %>% t()
  colnames(symMatrix) <- make.names(colnames(symMatrix), unique = TRUE)
  return(symMatrix)
}

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
    aLanguage$ontologicalCategory <- sample(aLanguage$ontologicalCategory, replace = FALSE)
  }
  distanceMatrix <- distanceMatrix * -1
  diag(distanceMatrix) <- -100
  nearestNeighbors <- max.col(distanceMatrix, ties.method = "random")
  neighbors <- aLanguage$phon[nearestNeighbors]
  neighbors.category <- aLanguage$ontologicalCategory[nearestNeighbors]
  langName <- unique(aLanguage$language)
  aLanguage <- aLanguage %>%
    as_tibble() %>%
    mutate(neighbor = neighbors,
           neighbor.category = neighbors.category,
           language = langName) %>%
    return()
}

# Wrangling Data ------------------------------------------------------------------

# Get mean class distances for each language and bind it in one tibble

allDistances <- map_dfr(.x = unique(allPhon$language),
                                      .f = function(languageName) {
                                        language <- getLanguage(aLanguage = languageName)
                                        distances <- getDistanceMatrix(language)
                                        meanDistances <-
                                          getMeanDistances(languageDF = language, distanceMatrix = distances) %>%
                                          mutate(language = languageName)
                                        return(meanDistances)
                                        }
                                      )

# Get normalized levenshtein

allDistances.norm <- map_dfr(.x = unique(allPhon$language),
                        .f = function(languageName) {
                          language <- getLanguage(aLanguage = languageName)
                          distances <- getDistanceMatrix.norm(language)
                          meanDistances <-
                            getMeanDistances(languageDF = language, distanceMatrix = distances) %>%
                            mutate(language = languageName)
                          return(meanDistances)
                        }
)



# Get tipicality stats for each class in each language

language.typicality.category <- allDistances %>%
  group_by(language, class) %>%
  summarise(mean.typicality = mean(typicality), sd.typicality = sd(typicality))

# Closest phonological neighbors

allPhonList <- allPhon %>%
  split(.$language)
allPhonList <- allPhonList[sort(names(allPhonList))]
allDistanceMatrices <- map(unique(allPhon$language),
                           function(x){getDistanceMatrix.norm(getLanguage(x))})
names(allDistanceMatrices) <- unique(allPhon$language)
allDistanceMatrices <- allDistanceMatrices[sort(names(allDistanceMatrices))]

# repeated.neighbor <- map2_dfr(.x = allPhonList, .y = allDistanceMatrices, .f = function(x, y){
#   print(unique(x$language))
#       purrr::map_dfr(1:100, function(z){
#         allNeighbors <- getNearestNeighbors(aLanguage = x,
#                                           distanceMatrix = y,
#                                           randomize = FALSE) %>%
#           mutate(sameNeighbor = (ontologicalCategory == neighbor.category)) %>%
#           group_by(language, ontologicalCategory) %>%
#           summarise(proportionOfHits = sum(sameNeighbor) / n()) %>%
#           mutate(permutation = z)
#         return(allNeighbors)
#       }) %>%
#       return()
#   })
# repeated.neighbor %>% write_rds("repeated_neighbor.Rds")


repeated.neighbor <- read_rds("repeated_neighbor.Rds")

neighbor.stats <- repeated.neighbor %>%
  group_by(language, ontologicalCategory) %>%
  do(enframe(Hmisc::smean.cl.normal(.$proportionOfHits, conf.int = 0.999))) %>%
  spread(name, value)

rm(repeated.neighbor)

# neighbor.mc <- furrr::future_map2_dfr(.x = allPhonList, .y = allDistanceMatrices, .progress = TRUE, .f = function(language, distanceMatrix){
#   gc()
#   purrr::map_dfr(1:1000, function(x){
#       allNeighbors <- getNearestNeighbors(aLanguage = language,
#                                         distanceMatrix = distanceMatrix,
#                                         randomize = TRUE) %>%
#         mutate(sameNeighbor = (ontologicalCategory == neighbor.category)) %>%
#         group_by(language, ontologicalCategory) %>%
#         summarise(proportionOfHits = sum(sameNeighbor) / n()) %>%
#         mutate(permutation = x)
#       return(allNeighbors)
#     }) %>%
#     # group_by(language, ontologicalCategory, permutation) %>%
#     # summarize(Mean = mean(proportionOfHits), Upper = Mean + sd(proportionOfHits), Lower = Mean - sd(proportionOfHits)) %>%
#     return()
# })
# neighbor.mc %>%
#   write_rds("neighbor_mc.Rds")

neighbor.mc <- read_rds("neighbor_mc.Rds")

neighbor.mc <- neighbor.mc %>%
  rename(random = proportionOfHits)
neighbor.test <- repeated.neighbor %>%
  group_by(language, ontologicalCategory) %>%
  summarize(proportionOfHits = mean(proportionOfHits)) %>%
  left_join(neighbor.mc) %>%
  mutate(is.higher = random >= proportionOfHits)
neighbor.test <- neighbor.test %>%
  group_by(language, ontologicalCategory) %>%
  summarize(p = sum(is.higher) / 1000)
neighbor.test %>%
  mutate(p = cut(p, breaks = c(0, 0.05, 1), include.lowest = TRUE, right = FALSE)) %>%
  group_by(p, ontologicalCategory) %>%
  tally() %>%
  mutate(n = n/ 222)

neighbor.test %>%
  filter(language %in% c("Mansi", "Romani", "Thai (Korat variety)"))

# Load NN results --------------------------------------------------------

rnn.performance <- list()
rnn.predictions <- list()

for(file in list.files('Results/RNN/', recursive = T, full.names = T)){
  language <- str_extract(file, "(?<=Results/RNN//).+(?=_rnn_)")
  print(language)
  if(str_detect(file, "predictions")){
    rnn.predictions[[language]] <- read_csv(file, col_names = c(1:10, "word"))
    rnn.predictions[[language]]$language <- language
  } else{
    rnn.performance[[language]] <- read_csv(file)
    rnn.performance[[language]]$language <- language
  }
}

sigs <- lapply(rnn.performance, function(stats){
  # return(t.test(x = stats$Matthews, mu = 0, alternative = "greater"))
  return(wilcox.test(x = stats$Matthews, mu = 0, alternative = "greater"))
  # if(sum(stats$Matthews) == 0){return(NA)}
  # return(shapiro.test(stats$Matthews))
})

sigs$Mansi
sigs$Romani
sigs$`Thai (Korat variety)`

sigs <- lapply(sigs, function(stats){
  if(is.nan(stats[["p.value"]]))
  {return(FALSE)}
  if(stats[["p.value"]] >= 0.001){return(FALSE)} else{return(TRUE)}
})

sigs <- bind_rows(sigs) %>%
  gather() %>%
  rename(language = key, significant = value) %>%
  filter(language %in% included.languages$language)

rnn.stats <- rnn.performance %>%
  bind_rows() %>%
  filter(language %in% included.languages$language) %>%
  mutate(language = factor(language, levels = sorted.langs$language)) %>%
  select(-X1) %>%
  group_by(language) %>%
  summarise(Median = median(Matthews), sd = sd(Matthews))

rnn.stats %>%
  filter(language %in% c("Mansi", "Romani", "Thai (Korat variety)"))

median(rnn.stats$Median)
sd(rnn.stats$Median)
rnn.stats %>%
  left_join(sigs) %>%
  group_by(significant) %>%
  tally() %>%
  mutate(n / 222)


# Plotting ----------------------------------------------------------------


# RNN Performance
#
# words.per.language <- allPhon %>%
#   filter(ontologicalCategory != "Other") %>%
#   group_by(language) %>%
#   tally()
#
# words.per.class <- allPhon %>%
#   filter(ontologicalCategory != "Other") %>%
#   group_by(language, ontologicalCategory) %>%
#   tally() %>%
#   spread(ontologicalCategory, n)
#
# median.length <- allPhon %>%
#   filter(ontologicalCategory != "Other") %>%
#   group_by(language) %>%
#   summarize(lang.median.length = median(nchar(phon)))

# rnn.stats <- rnn.stats %>%
#   left_join(select(phonLanguages, language = Name, familyName)) %>%
#   left_join(words.per.language) %>%
#   left_join(median.length) %>%
#   left_join(words.per.class) %>%
#   rename(n.words = n) %>%
#   group_by(familyName) %>%
#   mutate(number.in.family = n()) %>%
#   mutate(family.label = ifelse(number.in.family > 1, familyName, "Other"),
#          language = factor(language, levels = sorted.langs$language))

rnn.stats %>%
  left_join(sigs) %>%
  mutate(language = factor(language, levels = sorted.langs$language)) %>%
  mutate(lower = Median - sd, upper = Median + sd, upper = ifelse(upper > 1, 1, upper)) %>%
  mutate(significant = ifelse(significant, "*", " ")) %>%
  ggplot(aes(x = language, y = Median, label = significant)) +
  geom_col(fill = wes_palettes$Darjeeling1[2], color = "black") +
  scale_y_continuous(limits = c(-0.15, 1), name = "Matthews Correlation Coefficient") +
  scale_x_discrete(name = "Language", labels = labels.for.plot) +
  geom_text(size = 7) +
  cowplot::theme_cowplot()


plot.1 <- rnn.stats %>%
  mutate(lower = mean - sd, upper = mean + sd, upper = ifelse(upper > 1, 1, upper)) %>%
  left_join(sigs) %>%
  mutate(language = factor(language, levels = order.for.rnn)) %>%
  plotly::plot_ly(type = "bar", data = ., x = ~language, y = ~mean, color = ~family.label, colors = "Accent", hoverinfo = "text",
                  text = ~paste("Language: ", language, ";\nFamily: ", familyName,
                                ";\nMCC: ",  round(mean, 2), ";\nSignificant?: ", significant,
                                ";\nNumber Of Words: ", n.words, ";\nN. Actions: ", Action, ";\nN. Things: ", Thing,
                                ";\nMedian Length: ", lang.median.length, sep = "")) %>%
  plotly::layout(title = "Performance of RNN as measured by Matthew's Correlation Coefficient")

htmlwidgets::saveWidget(plot.1, "rnn_plot.html", selfcontained = TRUE)

# Faceted histogram of tipicality per class

allDistances %>%
  ggplot(aes(x = typicality, fill = class)) +
  geom_vline(xintercept = seq(-3, 3, 1), linetype = 'dashed',
             color = wes_palettes$Moonrise1[4], size = 1, alpha = 0.5) +
  geom_histogram(binwidth = .05) +
  facet_wrap(vars(class), nrow = 3, scales = "free_y") +
  geom_vline(xintercept = 0, size = 2, linetype = "solid") +
  labs(x = "Typicality", y = "Count", title = "Histogram of typicality per class") +
  scale_fill_manual(values = c(colorPaletteDis[7], colorPaletteDis[3], colorPaletteDis[5]))

allDistances.norm %>%
  ggplot(aes(x = typicality, fill = class)) +
  # geom_vline(xintercept = seq(-3, 3, 1), linetype = 'dashed',
             # color = wes_palettes$Moonrise1[4], size = 1, alpha = 0.5) +
  geom_histogram(binwidth = .05) +
  facet_wrap(vars(class), nrow = 3, scales = "free_y") +
  geom_vline(xintercept = 0, size = 2, linetype = "solid") +
  labs(x = "Typicality", y = "Count", title = "Histogram of typicality per class") +
  scale_fill_manual(values = c(colorPaletteDis[7], colorPaletteDis[3], colorPaletteDis[5]))

# Violin of typicality per class with boxplot

allDistances %>%
  mutate(class = factor(class, levels =  c("Other", "Thing", "Action"))) %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  scale_fill_manual(values = c(colorPaletteDis[7], colorPaletteDis[3], colorPaletteDis[5])) +
  labs(title = "Typicality per class across languages",
       subtitle = "Typicality = Mean distance to actions - mean distance to things",
       x = "",
       y = "Typicality") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "none")

allDistances.norm %>%
  mutate(class = factor(class, levels =  c("Other", "Thing", "Action"))) %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  scale_fill_manual(values = c(colorPaletteDis[7], colorPaletteDis[3], colorPaletteDis[5])) +
  # labs(title = "Typicality per class across languages",
  #     subtitle = "Typicality = Mean distance to actions - mean distance to things",
  #     x = "",
  #     y = "Typicality") +
  ylab("Typicality") +
  xlab("") +
  cowplot::theme_cowplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "none")

# Kruskal-wallis
allDistances.norm %>%
  mutate(class = factor(class)) %>%
  coin::kruskal_test(data = ., typicality ~ class)

# pairwise wilcox
pairwise.wilcox.test(x = allDistances.norm$typicality, g = factor(allDistances.norm$class), p.adjust.method = "bonferroni")
allDistances.norm %>%
  filter(class != "Other") %>%
  mutate(class = factor(class)) %>%
  coin::wilcox_test(data = ., typicality ~ class)
# effect size
Z.action.thing <- allDistances.norm %>%
  filter(class != "Other") %>%
  mutate(class = factor(class)) %>%
  coin::wilcox_test(data = ., typicality ~ class) %>%
  coin::statistic(type = "standardized")
  Z.action.thing / sqrt(nrow(filter(allDistances.norm, class != "Other")))
effsize::cohen.d(data = filter(allDistances.norm, class != "Other"), typicality ~ class)


allDistances.norm %>%
  mutate(class = factor(class, levels =  c("Other", "Thing", "Action")),
         length.group = cut_number(length.norm, 5)) %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  scale_fill_manual(values = c(colorPaletteDis[7], colorPaletteDis[3], colorPaletteDis[5])) +
  labs(title = "Typicality per class across languages",
       subtitle = "Typicality = Mean distance to actions - mean distance to things",
       x = "",
       y = "Typicality") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "none") +
  facet_wrap(vars(length.group))


# Sorted scatter of typicality per class

data.for.scatter <- allDistances.norm %>%
  filter(class != "Other") %>%
  group_by(language, class) %>%
  summarize(Mean = mean(typicality), Standard.Deviation = sd(typicality)) %>%
  mutate(Lower = Mean - Standard.Deviation, Upper = Mean + Standard.Deviation)

# sort it for cleaner viz

sorted.langs <- data.for.scatter %>% # this one includes the difference in means
  select(-Lower, -Upper, -Standard.Deviation) %>%
  spread(class, Mean) %>%
  mutate(difference = abs(Action - Thing)) %>%
  arrange(desc(abs(difference)))

median(sorted.langs$difference)
sd(sorted.langs$difference)

# mann whitney tests of chosen languages

allDistances.norm %>%
  filter(language == "Mansi" & class != "Other") %>%
  mutate(class = factor(class)) %>%
  coin::wilcox_test(data = ., typicality ~ class)
allDistances.norm %>%
  filter(language == "Mansi" & class != "Other") %>%
  mutate(class = factor(class)) %>%
  effsize::cohen.d(data = ., typicality ~ class)
allDistances.norm %>%
  filter(language == "Romani" & class != "Other") %>%
  mutate(class = factor(class)) %>%
  coin::wilcox_test(data = ., typicality ~ class)
allDistances.norm %>%
  filter(language == "Romani" & class != "Other") %>%
  mutate(class = factor(class)) %>%
  effsize::cohen.d(data = ., typicality ~ class)
allDistances.norm %>%
  filter(language == "Thai (Korat variety)" & class != "Other") %>%
  mutate(class = factor(class)) %>%
  coin::wilcox_test(data = ., typicality ~ class)
allDistances.norm %>%
  filter(language == "Thai (Korat variety)" & class != "Other") %>%
  mutate(class = factor(class)) %>%
  effsize::cohen.d(data = ., typicality ~ class)


allDistances.norm %>%
  filter(language %in% c("Mansi", "Romani", "Thai (Korat variety)")) %>%
  filter(class != "Other") %>%
  mutate(class = factor(class, levels = c("Action", "Thing", "Other"))) %>%
  ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
  stat_density2d(geom = 'polygon') +
  scale_fill_gradientn(colors = colorPaletteCont, name = "Density") +
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = wes_palettes$Moonrise1[4], size = 1) +
  labs(x = expression(hat(Nd[A])), y = expression(hat(Nd[T]))) +
  cowplot::theme_cowplot() +
  facet_wrap(vars(class, language)) +
  panel_border()
data.for.scatter <- data.for.scatter %>%
  filter(class != "Other") %>%
  group_by() %>%
  mutate(language = factor(language, levels = sorted.langs$language), class = factor(class),
         include = ifelse(language %in% c("Mansi", "Thai (Korat variety)", "Romani"), language, NA))

# levels(langForPlotScatter$language) <- as.character(sortedLangs$language)

# keep labels for max, min and median

label.text <- map_chr(sorted.langs$language, function(x){
  if(x %in% c("Mansi", "Thai (Korat variety)", "Romani")){
    return(x)
  } else{return("")}
})


ggplot(data = data.for.scatter, aes(y = Mean, x = language, color = class, shape = class,
                                      ymin = Lower, ymax = Upper, group = class)) +
  labs(x = 'Language',
       y = 'Mean Typicality') +
  geom_segment(aes(x = include, xend = include, y = -Inf, yend = Mean), size = .5,
               color = colorPaletteDis[6]) +
  # geom_hline(yintercept = seq(-2.5, 2.5, .5), linetype = 'dashed', alpha = 0.5) +
  # geom_ribbon(aes(fill = class), color = NA, alpha = .85) +
  geom_point(aes(fill = class), color = 'black', size = 2.5) +
  geom_hline(linetype = 'solid', size = 1, yintercept = 0) +
  # scale_y_continuous(breaks = seq(-3.0, 3, 1)) +
  scale_color_manual(name = "Category", values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[1])) +
  scale_fill_manual(name = "Category", values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[1])) +
  scale_shape_manual(name = "Category", values = c(23, 24)) +
  scale_x_discrete(name = "", labels = label.text) +
  cowplot::theme_cowplot() +
  cowplot::background_grid()

# 2D Density of all words (can also comment / uncomment for hex)

allDistances.norm %>%
  filter(class != "Other") %>%
  mutate(class = factor(class, levels = c("Action", "Thing", "Other"))) %>%
  ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
  stat_density2d(geom = 'polygon') +
  scale_fill_gradientn(colors = colorPaletteCont, name = "Density") +
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = wes_palettes$Moonrise1[4], size = 1) +
  labs(x = expression(hat(Nd[A])), y = expression(hat(Nd[T]))) +
  cowplot::theme_cowplot() +
  facet_wrap(vars(class)) +
  panel_border()

allDistances.norm %>%
  filter(class != "Other") %>%
  mutate(class = factor(class, levels = c("Action", "Thing", "Other")),
         length.group = cut_number(length.norm, 5)) %>%
  ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
  stat_density2d(geom = 'polygon') +
  scale_fill_gradientn(colors = colorPaletteCont, name = "Count") +
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = wes_palettes$Moonrise1[4], size = 1) +
  facet_wrap(vars(length.group, class))


# Neighbors

## Actions

action.mc <- neighbor.mc %>%
  filter(ontologicalCategory == "Action") %>%
  select(language, ontologicalCategory, random = proportionOfHits, permutation)

action.plot <- neighbor.stats %>%
  filter(ontologicalCategory == "Action") %>%
  left_join(action.mc) %>%
  group_by() %>%
  mutate(language = factor(language, levels = sorted.langs$language, labels = sorted.langs$language),
         include = ifelse(language %in% c("Mansi", "Thai (Korat variety)", "Romani"), as.character(language), NA))

action.plot <- action.plot %>%
  group_by(language, ontologicalCategory) %>%
  summarize(Mean = mean(Mean), Lower = mean(Lower), Upper = mean(Upper),
            random = mean(random) + sd(random)) %>%
  group_by() %>%
  mutate(label = ifelse(as.character(language) %in% c("Mansi", "Thai (Korat variety)", "Romani"), as.character(language), ""))

action.plot %>%
  ggplot(aes(x = language, y = Mean, ymin = Lower, ymax = Upper)) +
  geom_bar(stat = "identity", width = 1,
           fill = colorPaletteDis[5],
           size = .25, color = "black") +
  geom_bar(mapping = aes(y = random), fill = "yellow", stat = "identity", width = 1,
           alpha = .8, color = "black") +
  cowplot::theme_cowplot() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(name = "", labels = action.plot$label)

## Things

things.mc <- neighbor.mc %>%
  filter(ontologicalCategory == "Thing") %>%
  select(language, ontologicalCategory, random = proportionOfHits, permutation)

things.plot <- neighbor.stats %>%
  filter(ontologicalCategory == "Thing") %>%
  left_join(things.mc) %>%
  group_by() %>%
  mutate(language = factor(language, levels = sorted.langs$language, labels = sorted.langs$language),
         include = ifelse(language %in% c("Mansi", "Thai (Korat variety)", "Romani"), as.character(language), NA))

things.plot <- things.plot %>%
  group_by(language, ontologicalCategory) %>%
  summarize(Mean = mean(Mean), Lower = mean(Lower), Upper = mean(Upper),
            random = mean(random) + sd(random)) %>%
  group_by() %>%
  mutate(label = ifelse(as.character(language) %in% c("Mansi", "Thai (Korat variety)", "Romani"), as.character(language), ""))

things.plot %>%
  ggplot(aes(x = language, y = Mean, ymin = Lower, ymax = Upper)) +
  geom_bar(stat = "identity", width = 1,
           fill = colorPaletteDis[7],
           size = .25, color = "black") +
  geom_bar(mapping = aes(y = random), fill = "yellow", stat = "identity", width = 1,
           alpha = .8, color = "black") +
  cowplot::theme_cowplot() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(name = "", labels = action.plot$label)

neighbor.mc.plot <- neighbor.mc %>%
  filter(ontologicalCategory != "Other") %>%
  group_by(language, ontologicalCategory) %>%
  summarize(random = mean(proportionOfHits) + sd(proportionOfHits))

neighbor.plot <- neighbor.stats %>%
  group_by() %>%
  filter(ontologicalCategory != "Other") %>%
  left_join(neighbor.mc.plot) %>%
  mutate(language = factor(language, levels = sorted.langs$language, labels = sorted.langs$language))

labels.for.plot <- map_chr(levels(neighbor.plot$language), function(language){
  if(language %in% c("Mansi", "Thai (Korat variety)", "Romani")){return(language)} else{return("")}
})

neighbor.plot %>%
  ggplot(aes(x = language, y = Mean, ymin = Lower, ymax = Upper, fill = ontologicalCategory)) +
  geom_bar(stat = "identity", width = 1,
           size = .25, color = "black") +
  geom_bar(mapping = aes(y = random), fill = "yellow", stat = "identity", width = 1,
           alpha = .8, color = "black") +
  # geom_point(shape = 18, size = 2) +
  scale_x_discrete(name = "", labels = labels.for.plot) +
  facet_wrap(vars(ontologicalCategory), ncol = 1) +
  scale_y_continuous(expand = c(0, 0), name = "Proportion of same neighbor") +
  scale_fill_manual(values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[1])) +
  cowplot::theme_cowplot() +
  cowplot::panel_border() +
  theme(legend.position = "none")

# 10 languages

smaller.mc <- neighbor.mc %>%
  filter(language %in% c("Mansi", "Romani", "Thai (Korat variety)")) %>%
  group_by(language, ontologicalCategory) %>%
  summarize(random = mean(proportionOfHits) + sd(proportionOfHits))

smaller.neighbor <- neighbor.stats %>%
  filter(language %in% c("Mansi", "Romani", "Thai (Korat variety)"))

smaller.neighbor %>%
  # filter(ontologicalCategory != "Other") %>%
  left_join(smaller.mc) %>%
  ggplot(aes(fill = ontologicalCategory, x = language, y = Mean)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  geom_errorbar(mapping = aes(ymin = Lower, ymax = Upper),
                position = position_dodge(width = .9), width = 0.3,
                size = 1) +
  geom_bar(aes(y = random, group = ontologicalCategory),
           fill = "yellow", stat = "identity", position = "dodge", alpha = 0.85, color = "black") +
  scale_y_continuous(name = "Proportion of same neighbor", expand = c(0, 0)) +
  scale_fill_manual(name = "Category", values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[1], wes_palettes$Darjeeling1[5])) +
  cowplot::theme_cowplot() +
  theme(axis.title.x = element_blank())


# World map with languages
languagesX <- phonLanguages$Longitude
languagesY <- phonLanguages$Latitude
ggplot() +
  borders("world", colour=wes_palettes$Moonrise1[4], fill=wes_palettes$Darjeeling1[2]) + # create a layer of borders
  geom_jitter(aes(x = languagesX, y = languagesY), fill = wes_palettes$Darjeeling1[1],
             color = wes_palettes$Darjeeling2[5], size = 3, shape = 25) +
  theme_void() # +
  # labs(title = "Map of languages in database")


# TODO: get stuff from report to this script
# TODO: translate all results to concepticon
# TODO: write Readme
# TODO: Reproduce phonmining directory (ubuntu espeak)

# USE TYP DATABASE
allDistances <- allDistances %>%
  mutate(length = nchar(word))
# model.typicality <- lm(data = allDistances, typicality ~ class + language + length + I(length^2) + class * language + language * length)
# model.typicality2 <- lm(data = allDistances, typicality ~ class + language + length + class * language + language * length)
#
# all.languages.typicality <- model.typicality %>% emmeans("language", "class") %>% contrast(by = "language", method = list("Action - Thing" = c(1, 0 , -1))) %>% summary %>% rename(typ.contrast = estimate) %>% select(-contrast, -SE, -df, -t.ratio)
# all.languages.typicality <- allDistances %>% group_by(language) %>% tally %>% right_join(all.languages.typicality)
# all.languages.typicality %>% mutate(significant = ifelse(p.value < 0.001, TRUE, FALSE)) %>% ggplot(data = ., aes(x = n, y = typ.contrast)) + geom_point(aes(color = significant)) + geom_smooth(method = "lm", formula = y ~ x + I(log(x))) + labs(x = "number of words", y = "typicality action - typicality thing")

network.diagnostics <- allPhon %>% mutate(length = nchar(phon)) %>% group_by(language) %>%
  summarise(n = n(), length = mean(length)) %>%
  left_join(significance.performance) %>%
  mutate(significant = ifelse(p.value > 0.01, TRUE, FALSE),
         language = factor(language)) %>%
  rename(mean = emmean, lower =  lower.CL, upper = upper.CL)

all.languages.typicality <- allDistances %>%
  group_by(language, class) %>%
  summarize(typicality = mean(typicality)) %>%
  filter(class != "Other") %>%
  spread(class, typicality) %>%
  mutate(contrast = Action - Thing)

# network.diagnostics %>%
network.diagnostics %>%
  # filter(language != "Telugu") %>%
  ggplot(data = ., aes(x = n, y = mean, ymin = lower, ymax = upper)) +
  geom_point(aes(color = significant)) +
  geom_errorbar(aes(color = significant)) +
  geom_smooth(method = "lm", formula = y ~ x + I(log(x))) +
  labs(x = "Number of words", y = "Matthews CC")

all.diagnostics <- network.diagnostics %>%
  left_join(all.languages.typicality) %>%
  mutate(langauge = factor(language))

ggplot(data = all.diagnostics, aes(x = contrast, y = mean, ymin = lower, ymax = upper)) +
  geom_point(aes(color = significant)) +
  geom_smooth(method = "loess") +
  labs(x = "Difference in mean typicality (Action - Thing)", y = "Matthews CC")


wals <- read_csv('Data/WALS/walslanguage.csv')
autotyp <- read_csv('Data/AUTOTYP/autotyp-data-master/data/Clause_word_order.csv') %>%
  select(LID, WordOrderAPVBasicLex)
autotyp.reference <- read_csv('Data/AUTOTYP/autotyp-data-master/data/Register.csv') %>%
  select(LID, Glottocode) %>%
  left_join(autotyp)

per_predictors <- phonLanguages %>% select(language = Name, Glottocode) %>%
  right_join(network.diagnostics)

per_predictors <- autotyp.reference %>%
  right_join(per_predictors)


per_predictors %>% drop_na %>% BayesFactor::generalTestBF(data = ., mean ~ WordOrderAPVBasicLex)



# exp

# Checking differences among dialects ---------
#
# allPhon %>% filter(str_detect(language, "dialect")) %>% .$language %>% unique() %>% sort
#
# allDistances.norm %>%
#   filter(str_detect(language, "Aghul"), class != "Other") %>%
#   ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
#   stat_density_2d(geom = "polygon") +
#   facet_wrap(vars(language, class), ncol = 2) +
#   theme_minimal() +
#   lims(x = c(0.5,1), y = c(0.5,1)) +
#   geom_abline(intercept = c(0,0)) +
#   scale_fill_gradientn(colors = colorPaletteCont, name = "Density")
#
# allDistances.norm %>%
#   filter(str_detect(language, "Andi"), class != "Other") %>%
#   ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
#   stat_density_2d(geom = "polygon") +
#   facet_wrap(vars(language, class), ncol = 2) +
#   theme_minimal() +
#   lims(x = c(0.5,1), y = c(0.5,1)) +
#   geom_abline(intercept = c(0,0)) +
#   scale_fill_gradientn(colors = colorPaletteCont, name = "Density")
#
# allDistances.norm %>%
#   filter(str_detect(language, "Avar"), class != "Other") %>%
#   ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
#   stat_density_2d(geom = "polygon") +
#   facet_wrap(vars(language, class), ncol = 2) +
#   theme_minimal() +
#   lims(x = c(0.5,1), y = c(0.5,1)) +
#   geom_abline(intercept = c(0,0)) +
#   scale_fill_gradientn(colors = colorPaletteCont, name = "Density")
#
# allDistances.norm %>%
#   filter(str_detect(language, "Azerbaijani"), class != "Other") %>%
#   ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
#   stat_density_2d(geom = "polygon") +
#   facet_wrap(vars(language, class), ncol = 2) +
#   theme_minimal() +
#   lims(x = c(0.5,1), y = c(0.5,1)) +
#   geom_abline(intercept = c(0,0)) +
#   scale_fill_gradientn(colors = colorPaletteCont, name = "Density")
#
# allDistances.norm %>%
#   filter(str_detect(language, "Bezhta"), class != "Other") %>%
#   ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
#   stat_density_2d(geom = "polygon") +
#   facet_wrap(vars(language, class), ncol = 2) +
#   theme_minimal() +
#   lims(x = c(0.5,1), y = c(0.5,1)) +
#   geom_abline(intercept = c(0,0)) +
#   scale_fill_gradientn(colors = colorPaletteCont, name = "Density")
#
# allDistances.norm %>%
#   filter(str_detect(language, "Botlikh"), class != "Other") %>%
#   ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
#   stat_density_2d(geom = "polygon") +
#   facet_wrap(vars(language, class), ncol = 2) +
#   theme_minimal() +
#   lims(x = c(0.5,1), y = c(0.5,1)) +
#   geom_abline(intercept = c(0,0)) +
#   scale_fill_gradientn(colors = colorPaletteCont, name = "Density")
#
# allDistances.norm %>%
#   filter(str_detect(language, "Chamalal"), class != "Other") %>%
#   ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
#   stat_density_2d(geom = "polygon") +
#   facet_wrap(vars(language, class), ncol = 2) +
#   theme_minimal() +
#   lims(x = c(0.5,1), y = c(0.5,1)) +
#   geom_abline(intercept = c(0,0)) +
#   scale_fill_gradientn(colors = colorPaletteCont, name = "Density")
#
# allDistances.norm %>%
#   filter(str_detect(language, "Chechen"), class != "Other") %>%
#   ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
#   stat_density_2d(geom = "polygon") +
#   facet_wrap(vars(language, class), ncol = 2) +
#   theme_minimal() +
#   lims(x = c(0.5,1), y = c(0.5,1)) +
#   geom_abline(intercept = c(0,0)) +
#   scale_fill_gradientn(colors = colorPaletteCont, name = "Density")
#
# allDistances.norm %>%
#   filter(str_detect(language, "Dargwa"), class != "Other") %>%
#   ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
#   stat_density_2d(geom = "polygon") +
#   facet_wrap(vars(language, class), ncol = 2) +
#   theme_minimal() +
#   lims(x = c(0.5,1), y = c(0.5,1)) +
#   geom_abline(intercept = c(0,0)) +
#   scale_fill_gradientn(colors = colorPaletteCont, name = "Density")
#
# allDistances.norm %>%
#   filter(str_detect(language, "Karata"), class != "Other") %>%
#   ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
#   stat_density_2d(geom = "polygon") +
#   facet_wrap(vars(language, class), ncol = 2) +
#   theme_minimal() +
#   lims(x = c(0.5,1), y = c(0.5,1)) +
#   geom_abline(intercept = c(0,0)) +
#   scale_fill_gradientn(colors = colorPaletteCont, name = "Density")
#
# allDistances.norm %>%
#   filter(str_detect(language, "Khwarshi"), class != "Other") %>%
#   ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
#   stat_density_2d(geom = "polygon") +
#   facet_wrap(vars(language, class), ncol = 2) +
#   theme_minimal() +
#   lims(x = c(0.5,1), y = c(0.5,1)) +
#   geom_abline(intercept = c(0,0)) +
#   scale_fill_gradientn(colors = colorPaletteCont, name = "Density")
#
# allDistances.norm %>%
#   filter(str_detect(language, "Kumyk"), class != "Other") %>%
#   ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
#   stat_density_2d(geom = "polygon") +
#   facet_wrap(vars(language, class), ncol = 2) +
#   theme_minimal() +
#   lims(x = c(0.5,1), y = c(0.5,1)) +
#   geom_abline(intercept = c(0,0)) +
#   scale_fill_gradientn(colors = colorPaletteCont, name = "Density")
#
# allDistances.norm %>%
#   filter(str_detect(language, "Lak"), class != "Other") %>%
#   filter(language == "Lakkia") %>%
#   ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
#   stat_density_2d(geom = "polygon") +
#   facet_wrap(vars(language, class), ncol = 2) +
#   theme_minimal() +
#   lims(x = c(0.5,1), y = c(0.5,1)) +
#   geom_abline(intercept = c(0,0)) +
#   scale_fill_gradientn(colors = colorPaletteCont, name = "Density")
#
# allDistances.norm %>%
#   filter(str_detect(language, "Lezgi"), class != "Other") %>%
#   ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
#   stat_density_2d(geom = "polygon") +
#   facet_wrap(vars(language, class), ncol = 2) +
#   theme_minimal() +
#   lims(x = c(0.5,1), y = c(0.5,1)) +
#   geom_abline(intercept = c(0,0)) +
#   scale_fill_gradientn(colors = colorPaletteCont, name = "Density")
#
# allDistances.norm %>%
#   filter(str_detect(language, "Rutul"), class != "Other") %>%
#   ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
#   stat_density_2d(geom = "polygon") +
#   facet_wrap(vars(language, class), ncol = 2) +
#   theme_minimal() +
#   lims(x = c(0.5,1), y = c(0.5,1)) +
#   geom_abline(intercept = c(0,0)) +
#   scale_fill_gradientn(colors = colorPaletteCont, name = "Density")
#
# allDistances.norm %>%
#   filter(str_detect(language, "Sanapan"), class != "Other") %>%
#   ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
#   stat_density_2d(geom = "polygon") +
#   facet_wrap(vars(language, class), ncol = 2) +
#   theme_minimal() +
#   lims(x = c(0.5,1), y = c(0.5,1)) +
#   geom_abline(intercept = c(0,0)) +
#   scale_fill_gradientn(colors = colorPaletteCont, name = "Density")
#
# allDistances.norm %>%
#   filter(str_detect(language, "Shan"), class != "Other") %>%
#   ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
#   stat_density_2d(geom = "polygon") +
#   facet_wrap(vars(language, class), ncol = 2) +
#   theme_minimal() +
#   lims(x = c(0.5,1), y = c(0.5,1)) +
#   geom_abline(intercept = c(0,0)) +
#   scale_fill_gradientn(colors = colorPaletteCont, name = "Density")
#
# allDistances.norm %>%
#   filter(str_detect(language, "Tabasar"), class != "Other") %>%
#   ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
#   stat_density_2d(geom = "polygon") +
#   facet_wrap(vars(language, class), ncol = 2) +
#   theme_minimal() +
#   lims(x = c(0.5,1), y = c(0.5,1)) +
#   geom_abline(intercept = c(0,0)) +
#   scale_fill_gradientn(colors = colorPaletteCont, name = "Density")
#
# allDistances.norm %>%
#   filter(str_detect(language, "Tsez"), class != "Other") %>%
#   ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
#   stat_density_2d(geom = "polygon") +
#   facet_wrap(vars(language, class), ncol = 2) +
#   theme_minimal() +
#   lims(x = c(0.5,1), y = c(0.5,1)) +
#   geom_abline(intercept = c(0,0)) +
#   scale_fill_gradientn(colors = colorPaletteCont, name = "Density")
#
# allPhon %>% filter(str_detect(language, "variety")) %>% .$language %>% unique() %>% sort
#
# allDistances.norm %>%
#   filter(str_detect(language, "Archi"), class != "Other") %>%
#   ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
#   stat_density_2d(geom = "polygon") +
#   facet_wrap(vars(language, class), ncol = 2) +
#   theme_minimal() +
#   lims(x = c(0.5,1), y = c(0.5,1)) +
#   geom_abline(intercept = c(0,0)) +
#   scale_fill_gradientn(colors = colorPaletteCont, name = "Density")
#
# allDistances.norm %>%
#   filter(str_detect(language, "Armenian"), class != "Other") %>%
#   ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
#   stat_density_2d(geom = "polygon") +
#   facet_wrap(vars(language, class), ncol = 2) +
#   theme_minimal() +
#   lims(x = c(0.5,1), y = c(0.5,1)) +
#   geom_abline(intercept = c(0,0)) +
#   scale_fill_gradientn(colors = colorPaletteCont, name = "Density")
#
# allDistances.norm %>%
#   filter(str_detect(language, "Thai"), class != "Other") %>%
#   ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
#   stat_density_2d(geom = "polygon") +
#   facet_wrap(vars(language, class), ncol = 2) +
#   theme_minimal() +
#   lims(x = c(0.5,1), y = c(0.5,1)) +
#   geom_abline(intercept = c(0,0)) +
#   scale_fill_gradientn(colors = colorPaletteCont, name = "Density")
#
# allDistances.norm %>%
#   filter(str_detect(language, "Bulang"), class != "Other") %>%
#   ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
#   stat_density_2d(geom = "polygon") +
#   facet_wrap(vars(language, class), ncol = 2) +
#   theme_minimal() +
#   lims(x = c(0.5,1), y = c(0.5,1)) +
#   geom_abline(intercept = c(0,0)) +
#   scale_fill_gradientn(colors = colorPaletteCont, name = "Density")
