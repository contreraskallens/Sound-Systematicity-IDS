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
# require(future)
# require(furrr)
#
# future::plan(multicore)
# cores <- 5
# options(future.globals.maxSize = +Inf, mc.cores = cores)

colorPaletteDis <- c(wes_palette(name = 'Royal2'), wes_palette(name = 'Royal1'))
colorPaletteCont <- wes_palette(name = 'Zissou1', type = 'continuous', n = 5)


# Load data -----------------------------------------------------

allPhon <- read_csv('Data/Processed/allPhonFormsConcepticon2.csv')
phonLanguages <- read_csv('Data/Processed/allPhonLanguagesConcepticon.csv')
x <- read_csv('Data/Processed/allPhonLanguages.csv')
phonLanguages <- x %>% select(Name, `Morph+AC0-marker+AC0-Action`) %>% right_join(phonLanguages)
included.languages <- read_delim('included_languages.txt', delim = "\n",
                                col_names = c("language"), comment = "#")

allPhon <- allPhon %>%
  filter(language %in% included.languages$language)

phonLanguages <- phonLanguages %>%
  filter(Name %in% included.languages$language)


markers <- read_csv('newphonlang.csv')
# TODO: DO THIS MORE GRACEFULLY

phonLanguages$marker <- markers$`Morph+AC0-marker+AC0-Action`

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

allDistances.norm %>%
  group_by(class) %>%
  summarize(median.typicality = median(typicality))


language.typicality.category <- allDistances.norm %>%
  group_by(language, class) %>%
  summarise(mean.typicality = mean(typicality), sd.typicality = sd(typicality))

# marker.group <- phonLanguages %>%
#   filter(marker == TRUE) %>%
#   .$Name
# no.marker.group <- phonLanguages %>%
#   filter(marker == FALSE) %>%
#   .$Name

# Kruskal-wallis
allDistances.norm %>%
  mutate(class = factor(class)) %>%
  coin::kruskal_test(data = ., typicality ~ class)

# pairwise wilcox action / thing


allDistances.norm %>%
  filter(class != "Other") %>%
  mutate(class = factor(class)) %>%
  coin::wilcox_test(data = ., typicality ~ class)
allDistances.norm %>%
  filter(language %in% marker.group) %>%
  filter(class != "Other") %>%
  mutate(class = factor(class)) %>%
  coin::wilcox_test(data = ., typicality ~ class)
allDistances.norm %>%
  filter(language %in% no.marker.group) %>%
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
effsize::cohen.d(data = filter(allDistances.norm, class != "Other", language %in% marker.group), typicality ~ class)

effsize::cohen.d(data = filter(allDistances.norm, class != "Other", language %in% no.marker.group), typicality ~ class)

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
  filter(language == "English" & class != "Other") %>%
  mutate(class = factor(class)) %>%
  coin::wilcox_test(data = ., typicality ~ class)
allDistances.norm %>%
  filter(language == "English" & class != "Other") %>%
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


# Plotting ----------------------------------------------------------------

# World map with languages
languagesX <- phonLanguages$Longitude
languagesY <- phonLanguages$Latitude
ggplot() +
  borders("world", colour=wes_palettes$Moonrise1[4], fill=wes_palettes$Darjeeling1[2]) + # create a layer of borders
  geom_jitter(aes(x = languagesX, y = languagesY), fill = wes_palettes$Darjeeling1[1],
              color = wes_palettes$Darjeeling2[5], size = 3, shape = 25) +

  cowplot::theme_map()

# 2D Density of all words

allDistances.norm %>%
  filter(class != "Other") %>%
  mutate(class = factor(class, levels = c("Action", "Thing", "Other"))) %>%
  ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
  stat_density2d(geom = 'polygon') +
  scale_fill_gradientn(colors = colorPaletteCont, name = "Density") +
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = wes_palettes$Moonrise1[4], size = 1) +
  labs(x = expression(hat(ND[A])), y = expression(hat(ND[T]))) +
  cowplot::theme_cowplot() +
  facet_wrap(vars(class)) +
  cowplot::panel_border()

allDistances.norm %>%
  filter(language %in% marker.group) %>%
  filter(class != "Other") %>%
  mutate(class = factor(class, levels = c("Action", "Thing", "Other"))) %>%
  ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
  stat_density2d(geom = 'polygon') +
  scale_fill_gradientn(colors = colorPaletteCont, name = "Density") +
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = wes_palettes$Moonrise1[4], size = 1) +
  labs(x = expression(hat(ND[A])), y = expression(hat(ND[T]))) +
  cowplot::theme_cowplot() +
  facet_wrap(vars(class)) +
  cowplot::panel_border() +
  labs(title = "MARKER GROUP")
allDistances.norm %>%
  filter(language %in% no.marker.group) %>%
  filter(class != "Other") %>%
  mutate(class = factor(class, levels = c("Action", "Thing", "Other"))) %>%
  ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
  stat_density2d(geom = 'polygon') +
  scale_fill_gradientn(colors = colorPaletteCont, name = "Density") +
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = wes_palettes$Moonrise1[4], size = 1) +
  labs(x = expression(hat(ND[A])), y = expression(hat(ND[T]))) +
  cowplot::theme_cowplot() +
  facet_wrap(vars(class)) +
  cowplot::panel_border() +
  labs(title = "NO MARKER GROUP")

# Faceted histogram of typicality per class

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

allDistances.norm %>%
  mutate(class = factor(class, levels =  c("Other", "Thing", "Action"))) %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  scale_fill_manual(name = "Category", values = c(wes_palettes$Darjeeling1[1], wes_palettes$Darjeeling1[5], wes_palettes$Darjeeling1[2])) +
  ylab("Typicality") +
  xlab("") +
  cowplot::theme_cowplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "none")

allDistances.norm %>%
  filter(language %in% marker.group) %>%
  mutate(class = factor(class, levels =  c("Other", "Thing", "Action"))) %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  scale_fill_manual(name = "Category", values = c(wes_palettes$Darjeeling1[1], wes_palettes$Darjeeling1[5], wes_palettes$Darjeeling1[2])) +
  ylab("Typicality") +
  xlab("") +
  cowplot::theme_cowplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "none") +
  labs(title = "MARKER GROUP")
allDistances.norm %>%
  filter(language %in% no.marker.group) %>%
  mutate(class = factor(class, levels =  c("Other", "Thing", "Action"))) %>%
  ggplot(aes(x = class, y = typicality, fill = class)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  scale_fill_manual(name = "Category", values = c(wes_palettes$Darjeeling1[1], wes_palettes$Darjeeling1[5], wes_palettes$Darjeeling1[2])) +
  ylab("Typicality") +
  xlab("") +
  cowplot::theme_cowplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "none") +
  labs(title = "NO MARKER GROUP")



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

allDistances.norm %>%
  filter(language %in% c("Mansi", "English", "Romani", "Thai (Korat variety)")) %>%
  filter(class != "Other") %>%
  mutate(class = factor(class, levels = c("Action", "Thing", "Other")),
         language = factor(language, levels = c("Mansi", "Romani", "English", "Thai (Korat variety)"))) %>%
  ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
  stat_density2d(geom = 'polygon') +
  scale_fill_gradientn(colors = colorPaletteCont, name = "Density") +
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = wes_palettes$Moonrise1[4], size = 1) +
  labs(x = expression(hat(ND[A])), y = expression(hat(ND[T]))) +
  cowplot::theme_cowplot() +
  facet_wrap(vars(class, language), nrow = 2) +
  cowplot::panel_border()

data.for.scatter <- data.for.scatter %>%
  filter(class != "Other") %>%
  group_by() %>%
  mutate(language = factor(language, levels = sorted.langs$language), class = factor(class),
         include = ifelse(language %in% c("Mansi", "English", "Thai (Korat variety)", "Romani"), language, NA))

# keep labels for max, min and median and English

label.text <- map_chr(sorted.langs$language, function(x){
  if(x %in% c("Mansi", "English", "Thai (Korat variety)", "Romani")){
    return(x)
  } else{return("")}
})


ggplot(data = data.for.scatter, aes(y = Mean, x = language, color = class, shape = class,
                                      ymin = Lower, ymax = Upper, group = class)) +
  labs(x = 'Language',
       y = 'Mean Typicality') +
  geom_segment(aes(x = include, xend = include, y = -Inf, yend = Mean), size = .5,
               color = colorPaletteDis[6]) +
  geom_point(aes(fill = class), color = 'black', size = 2.5) +
  geom_hline(linetype = 'solid', size = 1, yintercept = 0) +
  scale_color_manual(name = "Category", values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[1])) +
  scale_fill_manual(name = "Category", values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[1])) +
  scale_shape_manual(name = "Category", values = c(23, 24)) +
  scale_x_discrete(name = "", labels = label.text) +
  cowplot::theme_cowplot() +
  cowplot::background_grid()

ggplot(data = filter(data.for.scatter, language %in% marker.group), aes(y = Mean, x = language, color = class, shape = class,
                                    ymin = Lower, ymax = Upper, group = class)) +
  labs(x = 'Language',
       y = 'Mean Typicality') +
  geom_point(aes(fill = class), color = 'black', size = 2.5) +
  geom_hline(linetype = 'solid', size = 1, yintercept = 0) +
  scale_color_manual(name = "Category", values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[1])) +
  scale_fill_manual(name = "Category", values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[1])) +
  scale_shape_manual(name = "Category", values = c(23, 24)) +
  scale_x_discrete(name = "") +
  cowplot::theme_cowplot() +
  cowplot::background_grid() +
  labs(title = "MARKER GROUP")+
  theme(axis.text.x = element_blank())
ggplot(data = filter(data.for.scatter, language %in% no.marker.group), aes(y = Mean, x = language, color = class, shape = class,
                                                                        ymin = Lower, ymax = Upper, group = class)) +
  labs(x = 'Language',
       y = 'Mean Typicality') +
  geom_point(aes(fill = class), color = 'black', size = 2.5) +
  geom_hline(linetype = 'solid', size = 1, yintercept = 0) +
  scale_color_manual(name = "Category", values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[1])) +
  scale_fill_manual(name = "Category", values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[1])) +
  scale_shape_manual(name = "Category", values = c(23, 24)) +
  scale_x_discrete(name = "") +
  cowplot::theme_cowplot() +
  cowplot::background_grid() +
  labs(title = "NO MARKER GROUP")+
  theme(axis.text.x = element_blank())

# Closest phonological neighbors ---------------

allPhonList <- allPhon %>%
  split(.$language)
allPhonList <- allPhonList[sort(names(allPhonList))]

allDistanceMatrices <- purrr::map(allPhonList, function(x){getDistanceMatrix.norm(x)})

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
  filter(language %in% marker.group) %>%
  mutate(p = cut(p, breaks = c(0, 0.05, 1), include.lowest = TRUE, right = FALSE)) %>%
  group_by(p, ontologicalCategory) %>%
  tally() %>%
  mutate(n = n/ length(marker.group))
neighbor.test %>%
  filter(language %in% no.marker.group) %>%
  mutate(p = cut(p, breaks = c(0, 0.05, 1), include.lowest = TRUE, right = FALSE)) %>%
  group_by(p, ontologicalCategory) %>%
  tally() %>%
  mutate(n = n/ length(no.marker.group))



neighbor.test %>%
  filter(language %in% c("Mansi", "English", "Romani", "Thai (Korat variety)"))


## Actions

action.mc <- neighbor.mc %>%
  filter(ontologicalCategory == "Action") %>%
  select(language, ontologicalCategory, random, permutation)

action.plot <- neighbor.stats %>%
  filter(ontologicalCategory == "Action") %>%
  left_join(action.mc) %>%
  group_by() %>%
  mutate(language = factor(language, levels = sorted.langs$language, labels = sorted.langs$language),
         include = ifelse(language %in% c("Mansi", "English", "Thai (Korat variety)", "Romani"), as.character(language), NA))

action.plot <- action.plot %>%
  group_by(language, ontologicalCategory) %>%
  summarize(Mean = mean(Mean), Lower = mean(Lower), Upper = mean(Upper),
            random = mean(random) + sd(random)) %>%
  group_by() %>%
  mutate(label = ifelse(as.character(language) %in% c("Mansi", "English", "Thai (Korat variety)", "Romani"), as.character(language), ""))

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
  select(language, ontologicalCategory,random, permutation)

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
  mutate(label = ifelse(as.character(language) %in% c("Mansi", "Thai (Korat variety)", "English", "Romani"), as.character(language), ""))

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
  summarize(random = mean(random) + sd(random))

neighbor.plot <- neighbor.stats %>%
  group_by() %>%
  filter(ontologicalCategory != "Other") %>%
  left_join(neighbor.mc.plot) %>%
  mutate(language = factor(language, levels = sorted.langs$language, labels = sorted.langs$language))

labels.for.plot <- map_chr(levels(neighbor.plot$language), function(language){
  if(language %in% c("Mansi", "English", "Thai (Korat variety)", "Romani")){return(language)} else{return("")}
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
neighbor.plot %>%
  filter(language %in% marker.group) %>%
  ggplot(aes(x = language, y = Mean, ymin = Lower, ymax = Upper, fill = ontologicalCategory)) +
  geom_bar(stat = "identity", width = 1,
           size = .25, color = "black") +
  geom_bar(mapping = aes(y = random), fill = "yellow", stat = "identity", width = 1,
           alpha = .8, color = "black") +
  # geom_point(shape = 18, size = 2) +
  scale_x_discrete(name = "") +
  facet_wrap(vars(ontologicalCategory), ncol = 1) +
  scale_y_continuous(expand = c(0, 0), name = "Proportion of same neighbor") +
  scale_fill_manual(values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[1])) +
  cowplot::theme_cowplot() +
  cowplot::panel_border() +
  theme(legend.position = "none", axis.text.x = element_blank()) +
  labs(title = "MARKER GROUP")
neighbor.plot %>%
  filter(language %in% no.marker.group) %>%
  ggplot(aes(x = language, y = Mean, ymin = Lower, ymax = Upper, fill = ontologicalCategory)) +
  geom_bar(stat = "identity", width = 1,
           size = .25, color = "black") +
  geom_bar(mapping = aes(y = random), fill = "yellow", stat = "identity", width = 1,
           alpha = .8, color = "black") +
  # geom_point(shape = 18, size = 2) +
  scale_x_discrete(name = "") +
  facet_wrap(vars(ontologicalCategory), ncol = 1) +
  scale_y_continuous(expand = c(0, 0), name = "Proportion of same neighbor") +
  scale_fill_manual(values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[1])) +
  cowplot::theme_cowplot() +
  cowplot::panel_border() +
  theme(legend.position = "none", axis.text.x = element_blank()) +
  labs(title = "NO MARKER GROUP")



# 4 languages

smaller.mc <- neighbor.mc %>%
  filter(language %in% c("Mansi", "Romani", "English", "Thai (Korat variety)")) %>%
  group_by(language, ontologicalCategory) %>%
  summarize(random = mean(random) + sd(random))

smaller.neighbor <- neighbor.stats %>%
  filter(language %in% c("Mansi", "Romani", "English", "Thai (Korat variety)"))

smaller.neighbor %>%
  left_join(smaller.mc) %>%
  mutate(language = factor(language, levels = c("Mansi", "Romani", "English", "Thai (Korat variety)"))) %>%
  ggplot(aes(fill = ontologicalCategory, x = language, y = Mean)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  geom_errorbar(mapping = aes(ymin = Lower, ymax = Upper),
                position = position_dodge(width = .9), width = 0.3,
                size = 1) +
  geom_bar(aes(y = random, group = ontologicalCategory),
           fill = "yellow", stat = "identity", position = "dodge", alpha = 0.85, color = "black") +
  scale_y_continuous(name = "Proportion of same neighbor", expand = c(0, 0)) +
  scale_fill_manual(name = "Category", values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[5], wes_palettes$Darjeeling1[1])) +
  cowplot::theme_cowplot() +
  theme(axis.title.x = element_blank())

# RNN -------


# Load NN results --------------------------------------------------------

rnn.performance <- list()
rnn.predictions <- list()

for(file in list.files('Results/RNN/', recursive = T, full.names = T)){
  language <- str_extract(file, "(?<=Results/RNN//).+(?=_rnn_)")
  if(language == "celex"){next()}
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
  return(wilcox.test(x = stats$Matthews, mu = 0.1, alternative = "greater"))
})

sigs$Mansi
sigs$Romani
sigs$English
sigs$`Thai (Korat variety)`

sigs <- lapply(sigs, function(stats){
  if(is.nan(stats[["p.value"]]))
  {return(FALSE)}
  if(stats[["p.value"]] >= 0.01){return(FALSE)} else{return(TRUE)}
})

sigs <- bind_rows(sigs) %>%
  gather() %>%
  rename(language = key, significant = value) %>%
  filter(language %in% included.languages$language)

sigs

rnn.stats <- rnn.performance %>%
  bind_rows() %>%
  filter(language %in% included.languages$language) %>%
  mutate(language = factor(language, levels = sorted.langs$language)) %>%
  select(-X1) %>%
  group_by(language) %>%
  summarise(Median = median(Matthews), sd = sd(Matthews))

rnn.stats %>%
  filter(language %in% c("Mansi", "Romani", "English", "Thai (Korat variety)"))

median(rnn.stats$Median)
sd(rnn.stats$Median)
rnn.stats %>%
  left_join(sigs) %>%
  group_by(significant) %>%
  tally() %>%
  mutate(n / 222)
rnn.stats %>%
  filter(language %in% marker.group) %>%
  left_join(sigs) %>%
  group_by(significant) %>%
  tally() %>%
  mutate(n / length(marker.group))
rnn.stats %>%
  filter(language %in% no.marker.group) %>%
  left_join(sigs) %>%
  group_by(significant) %>%
  tally() %>%
  mutate(n / length(no.marker.group))

# RNN Performance

labels.for.plot <- purrr::map(sorted.langs$language, function(language){
  if(language %in% c("Mansi", "English", "Thai (Korat variety)", "Romani")){
    return(language)
  } else{
    return("")
  }
})

k.fold.plot <- rnn.stats %>%
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
  mutate(lower = Median - sd, upper = Median + sd, upper = ifelse(upper > 1, 1, upper)) %>%
  left_join(sigs) %>%
  left_join(rename(phonLanguages, language = Name)) %>%
  mutate(language = factor(language, levels = sorted.langs$language)) %>%
  plotly::plot_ly(type = "bar", data = ., x = ~language, y = ~Median, color = ~familyName, colors = "Accent", hoverinfo = "text",
                  text = ~paste("Language: ", language, ";\nFamily: ", familyName,
                                ";\nMCC: ",  round(Median, 2), ";\nSignificant?: ", significant, sep = "")) %>%
  plotly::layout(title = "Performance of RNN as measured by Matthew's Correlation Coefficient")

htmlwidgets::saveWidget(plot.1, "rnn_plot.html", selfcontained = TRUE)

# Load Spurt results --------------------------------------------------------

spurt.performance <- list()
spurt.predictions <- list()

for(file in list.files('Results/Spurt/', recursive = T, full.names = T, pattern = "performance")){
  language <- str_extract(file, "(?<=Results/Spurt//).+(?=_spurt_)")
  if(str_detect(file, "predictions")){
    spurt.predictions[[language]] <- read_csv(file, col_names = c(1:10, "word"))
    spurt.predictions[[language]]$language <- language
  } else{
    spurt.performance[[language]] <- read_csv(file)
    spurt.performance[[language]]$language <- language
  }
}

sigs.spurt <- lapply(spurt.performance, function(stats){
  print(stats)
  return(wilcox.test(x = stats$Matthews, mu = 0.1, alternative = "greater"))
})

sigs.spurt$Mansi
sigs.spurt$Romani
sigs.spurt$English
sigs.spurt$`Thai (Korat variety)`

sigs.spurt <- lapply(sigs.spurt, function(stats){
  if(is.nan(stats[["p.value"]]))
  {return(FALSE)}
  if(stats[["p.value"]] >= 0.01){return(FALSE)} else{return(TRUE)}
})

sigs.spurt <- sigs.spurt %>%
  bind_rows %>%
  gather("language", "significant")

spurt.stats <- spurt.performance %>%
  bind_rows() %>%
  select(-X1) %>%
  group_by(language) %>%
  summarise(Median = median(Matthews), sd = sd(Matthews)) %>%
  left_join(sigs.spurt) %>%
  mutate(language = factor(language, levels = sorted.langs$language))

spurt.stats %>%
  filter(language %in% marker.group) %>%
  left_join(sigs) %>%
  group_by(significant) %>%
  tally() %>%
  mutate(n / length(marker.group))
spurt.stats %>%
  filter(language %in% no.marker.group) %>%
  left_join(sigs) %>%
  group_by(significant) %>%
  tally() %>%
  mutate(n / length(no.marker.group))

labels.for.plot <- purrr::map(sorted.langs$language, function(language){
  if(language %in% c("Mansi", "English", "Thai (Korat variety)", "Romani")){
    return(language)
  } else{
      return("")
    }
})

k.fold.plot <- rnn.stats %>%
  filter(language %in% marker.group) %>%
  left_join(sigs) %>%
  mutate(language = factor(language, levels = sorted.langs$language)) %>%
  mutate(lower = Median - sd, upper = Median + sd, upper = ifelse(upper > 1, 1, upper)) %>%
  mutate(significant = ifelse(significant, "*", " ")) %>%
  ggplot(aes(x = language, y = Median, label = significant)) +
  geom_col(fill = wes_palettes$Darjeeling1[2], color = "black") +
  scale_y_continuous(limits = c(-0.15, 1), name = "Matthews Correlation Coefficient") +
  # scale_x_discrete(name = "Language", labels = labels.for.plot) +
  geom_text(size = 7) +
  cowplot::theme_cowplot() +
  theme(axis.text.x = element_blank()) +
  labs(title = "MARKER GROUP - K-Fold")

spurt.plot <- spurt.stats %>%
  filter(language %in% marker.group) %>%
  filter(language %in% included.languages$language) %>%
  mutate(lower = Median - sd, upper = Median + sd, upper = ifelse(upper > 1, 1, upper)) %>%
  mutate(significant = ifelse(significant, "*", " ")) %>%
  ggplot(aes(x = language, y = Median, label= significant)) +
  geom_col(fill = wes_palettes$Darjeeling1[1], color = "black") +
  scale_y_continuous(limits = c(-0.15, 1), name = "Matthews Correlation Coefficient") +
  # scale_x_discrete(name = "Language", labels = labels.for.plot) +
  geom_text(size = 7) +
  cowplot::theme_cowplot() +
  theme(axis.text.x = element_blank()) +
  labs(title = "MARKER GROUP - Spurt")

cowplot::plot_grid(k.fold.plot, spurt.plot, nrow = 2, labels = "AUTO")


spurt.stats %>%
  mutate(significant = ifelse(significant, "*", "")) %>%
  ggplot(aes(x = language, y = Median, label = significant)) +
  geom_col(fill = wes_palettes$Darjeeling1[2], color = "black") +
  scale_y_continuous(limits = c(-0.15, 1), name = "Matthews Correlation Coefficient") +
  scale_x_discrete(name = "Language", labels = labels.for.plot) +
  geom_text(size = 7) +
  cowplot::theme_cowplot()


spurt.performance %>%
  bind_rows() %>%
  filter(language %in% c("Mansi", "Romani", "English", "Thai (Korat variety)")) %>%
  group_by(language) %>%
  summarize(median(Matthews))

median(spurt.stats$Median)
sd(spurt.stats$Median)

# RNN Performance

spurt.performance %>%
  bind_rows %>%
  ggplot(aes(x = language, y = Matthews)) +
  geom_violin(aes(fill = language)) +
  geom_jitter(width = 0.2)+
  geom_hline(yintercept = 0) +
  labs(title = "Simulation of vocabulary spurt for chosen languages",
       subtitle = "100 iterations of training with 50 words and prediction of the rest") +
  cowplot::theme_cowplot() +
  theme(legend.position = "none") +
  geom_col(fill = wes_palettes$Darjeeling1[2], color = "black") +
  scale_y_continuous(limits = c(-0.15, 1), name = "Matthews Correlation Coefficient") +
  scale_x_discrete(name = "Language", labels = label.text) +
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



spurt.performance <- list()

for(file in list.files('Results/AoA/', recursive = T, full.names = T)){
  language <- str_extract(file, "(?<=Results/AoA//).+(?=_spurt_)")
  if(str_detect(file, "predictions")){
    spurt.predictions[[language]] <- read_csv(file, col_names = c(1:10, "word"))
    spurt.predictions[[language]]$language <- language
  } else{
    spurt.performance[[language]] <- read_csv(file)
    spurt.performance[[language]]$language <- language
  }
}

sigs.spurt <- lapply(spurt.performance, function(stats){
  return(wilcox.test(x = stats$Matthews, mu = 0, alternative = "greater"))
})

sigs.spurt <- lapply(sigs.spurt, function(stats){
  if(is.nan(stats[["p.value"]]))
  {return(FALSE)}
  if(stats[["p.value"]] >= 0.01){return(FALSE)} else{return(TRUE)}
})

sigs.spurt <- sigs.spurt %>%
  bind_rows %>%
  gather("language", "significant")

spurt.stats <- spurt.performance %>%
  bind_rows() %>%
  select(-X1) %>%
  group_by(language) %>%
  summarise(Median = median(Matthews), sd = sd(Matthews)) %>%
  left_join(sigs.spurt)

spurt.plot <- spurt.stats %>%
  mutate(lower = Median - sd, upper = Median + sd, upper = ifelse(upper > 1, 1, upper)) %>%
  mutate(significant = ifelse(significant, "*", " ")) %>%
  ggplot(aes(x = language, y = Median, label= significant)) +
  geom_col(fill = wes_palettes$Darjeeling1[1], color = "black") +
  scale_y_continuous(limits = c(-0.15, 1), name = "Matthews Correlation Coefficient") +
  geom_text(size = 7) +
  cowplot::theme_cowplot()




# TODO: get stuff from report to this script
# TODO: translate all results to concepticon
# TODO: write Readme
# TODO: Reproduce phonmining directory (ubuntu espeak)

# k-means algorithm

plot.clusters <- function(things, actions){
  # get as indices
  action.distances <- spanish.distance[actions,actions] %>%
    rowMeans() %>%
    enframe(name = "phon", value = "mean.distance.actions") %>%
    add_column(mean.distance.things = rowMeans(spanish.distance[actions, things]),
               predicted = "Action") %>%
    left_join(spanish.frame)
  thing.distances <- spanish.distance[things,actions] %>%
    rowMeans() %>%
    enframe(name = "phon", value = "mean.distance.actions") %>%
    add_column(mean.distance.things = rowMeans(spanish.distance[things, things]),
               predicted = "Thing") %>%
    left_join(spanish.frame)
  plot <- bind_rows(action.distances, thing.distances) %>%
    ggplot(aes(x = mean.distance.things, y = mean.distance.actions, color = ontologicalCategory)) +
    geom_point(aes(color = ontologicalCategory)) +
    geom_abline(intercept = c(0,0), linetype = 'dashed', color = wes_palettes$Moonrise1[4], size = 1) +
    cowplot::theme_cowplot() +
    scale_color_manual(name = "Category", values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[1])) +
    labs(x = "Mean distance to thing cluster", y = "Mean distance to action cluster", title = "Nearest centroid spurt for Romanian")
  return(plot)
}

spanish.distance <- getDistanceMatrix.norm(filter(getLanguage("Romani"), ontologicalCategory != "Other"))
spanish.frame <- getLanguage("Romani") %>%
  filter(ontologicalCategory != "Other")


scores <- list()
mcc <- c()

for(n in 1:100){
  print(n)
  train.things <- which(spanish.frame$ontologicalCategory == "Thing", arr.ind = TRUE) %>%
    sample(25)
  train.actions <- which(spanish.frame$ontologicalCategory == "Action", arr.ind = TRUE) %>%
    sample(25)
  tests <- setdiff(1:nrow(spanish.frame), c(train.things, train.actions))
  train.matrix.things <- spanish.distance[train.things,]
  train.matrix.actions <- spanish.distance[train.actions,]
  action.cluster <- c()
  things.cluster <- c()

  for(test.point in sample(tests)){
    test.distance.things <- train.matrix.things[,test.point] %>%
      mean()
    test.distance.actions <- train.matrix.actions[,test.point] %>%
      mean()
    if(test.distance.things < test.distance.actions){
      prediction <- "Thing"
      train.matrix.things <- rbind(train.matrix.things, spanish.distance[test.point,])
      things.cluster <- c(things.cluster, test.point)
      }else{
      prediction <- "Action"
      train.matrix.actions <- rbind(train.matrix.actions, spanish.distance[test.point,])
      action.cluster <- c(action.cluster, test.point)
      }
  }
  action.cluster <- c(action.cluster, train.actions)
  things.cluster <- c(things.cluster, train.things)

  action.distances <- spanish.distance[action.cluster,action.cluster] %>%
    rowMeans() %>%
    enframe(name = "phon", value = "mean.distance.actions") %>%
    add_column(mean.distance.things = rowMeans(spanish.distance[action.cluster, things.cluster]),
               predicted = "Action")
  thing.distances <- spanish.distance[things.cluster, action.cluster] %>%
    rowMeans() %>%
    enframe(name = "phon", value = "mean.distance.actions") %>%
    add_column(mean.distance.things = rowMeans(spanish.distance[things.cluster, things.cluster]),
               predicted = "Thing")

  spanish.predictions <- bind_rows(action.distances, thing.distances) %>%
    arrange(phon)

  predictions <- spanish.predictions %>% left_join(spanish.frame) %>%
    mutate_at(c("predicted", "ontologicalCategory"), recode, Thing = 0, Action = 1)


  scores[[n]] <- select(spanish.predictions, phon, mean.distance.actions, mean.distance.things)
  mcc[[n]] <- mltools::mcc(preds = predictions$predicted, actuals = predictions$ontologicalCategory)
}

scores %>%
  bind_cols() %>%
  left_join(spanish.frame) %>%
  add_column(mean.things = rowMeans(select(., contains("things"))),
                                                mean.actions = rowMeans(select(., contains("actions")))) %>%
  ggplot(aes(x = mean.things, y = mean.actions, color = ontologicalCategory)) +
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = wes_palettes$Moonrise1[4], size = 1) +
  cowplot::theme_cowplot() +
  scale_color_manual(name = "Category", values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[1])) +
  geom_point() +
  facet_wrap(vars(ontologicalCategory), scales = "fixed") +
  labs(x = "Mean distance to thing cluster", y = "Mean distance to action cluster",
       title = "Aggregation of 100 iterations of dynamic centroids for Romani",
      caption = paste0("Mean MCC = ", round(mean(mcc), 3)))











# spanish.predictions <- spanish.predictions %>% bind_rows

prediction.stats <- spanish.predictions %>%
  mutate(is.correct = ontologicalCategory == predicted) %>%
  group_by(ontologicalCategory) %>%
  summarize(proportion.correct = sum(is.correct) / n())

original.words <- spanish.frame[c(train.things, train.actions), "phon"]
origina.things <- train.things %>%
  spanish.frame[., "phon"]
original.actions <- train.actions %>%
  spanish.frame[., "phon"]

spanish.predictions <- spanish.predictions %>%
  mutate(original = ifelse(phon %in% original.words$phon, TRUE, FALSE))

ggplot(spanish.predictions, aes(x = mean.distance.things, y = mean.distance.actions, shape = original, size = original)) +
  geom_point(aes(color = ontologicalCategory)) +
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = wes_palettes$Moonrise1[4], size = 1) +
  annotate(geom = "Text", x = min(spanish.predictions$mean.distance.things) + 0.025, y = max(spanish.predictions$mean.distance.actions),
           label = paste("Things Correct:", round(prediction.stats[2,2], digits = 2)),
           size = 5) +
  annotate(geom = "Text", x = max(spanish.predictions$mean.distance.things) - 0.025, y = min(spanish.predictions$mean.distance.actions),
         label = paste("Actions Correct:", round(prediction.stats[1,2], digits = 2)),
         size = 5) +
  cowplot::theme_cowplot() +
  scale_color_manual(name = "Category", values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[1])) +
  labs(x = "Mean distance to thing cluster", y = "Mean distance to action cluster", title = "Nearest centroid spurt for Romanian")

# TODO: animation!
# TODO: MCC!



# flexclust::kcca()




spanish.k <- cluster::pam(x = spanish.distance, diss = TRUE,
                          k = 2, do.swap = FALSE, medoids = c(train.things, train.actions), keep.diss = TRUE)
cluster::clusplot(spanish.k)
spanish.k$clustering


# extremes of the word

allPhon.extremes <- allPhon %>%
  mutate(ending = str_sub(phon, -1), start = str_sub(phon, 1, 1))

all.extremes <- map(sort(unique(allPhon.extremes$language)), function(x){
  lang <- allPhon.extremes %>%
    filter(language == x) %>%
    select(ontologicalCategory, ending, start) %>%
    mutate_at(c("ending", "start"), as.factor)
  dummies <- caret::dummyVars(data = lang, ~ ending + start)
  lang <- lang %>%
    bind_cols(as_tibble(predict(object = dummies, newdata = lang))) %>%
    select(ontologicalCategory, contains("ending"), contains("start"), -ending, -start) %>%
    mutate(ontologicalCategory = factor(ontologicalCategory))
  return(lang)
})
names(all.extremes) <- sort(unique(allPhon.extremes$language))

action.props <- map_dfr(names(all.extremes), function(lang){
  lang.name <- lang
  lang <- all.extremes[[lang]]
  tally <- lang %>%
    group_by(ontologicalCategory) %>%
    tally()
  props.per.cat <- lang %>%
    group_by(ontologicalCategory) %>%
    summarize_all(sum) %>%
    left_join(tally) %>%
    mutate_at(vars(-ontologicalCategory, -n), funs(. / n))
  action.prop <- props.per.cat %>%
    select(-n) %>%
    select(ontologicalCategory, apply(., 1, which.max)) %>%
    filter(ontologicalCategory == "Action") %>%
    select(ontologicalCategory, 2)
  colnames(action.prop) <- c("ontologicalCategory", "prop")
  action.prop <- action.prop %>%
    add_column(language = lang.name)
  return(action.prop)
})

marker.group <- action.props %>%
  filter(prop > 0.5) %>%
  .$language
no.marker.group <- action.props %>%
  filter(prop <= 0.5) %>%
  .$language

spanish.example <- allPhon.extremes %>%
  filter(language == "Basque")

lda.f1 <- list()
f1 <- function(data, lev = NULL, model = NULL) {
  f1_val <- F1_Score(y_pred = data$pred, y_true = data$obs, positive = lev[1])
  c(F1 = f1_val)
}

library(MLmetrics)

for(x in unique(allPhon.extremes$language)){
  lang.frame <- all.extremes[[x]]
  print(x)
  if(x %in% names(lda.f1)){next}
  lang.frame <- allPhon.extremes %>%
    filter(language == x)
  lang.frame <- filter(lang.frame, ontologicalCategory != "Other")
  # onehot.encoding <- lang.frame %>%
  #   mutate_at(c("ending", "start"), as.factor) %>%
  #   caret::dummyVars(data = ., ~ ending + start)
  lang.frame <- lang.frame %>%
    # bind_cols(as_tibble(predict(object = onehot.encoding, newdata = lang.frame)))  %>%
    select(ontologicalCategory, contains("ending"), contains("start")) %>%
    mutate(ontologicalCategory = factor(ontologicalCategory))
  lda.model <- caret::train(data = lang.frame, ontologicalCategory ~ ., method = "lda", metric = "F1",
                            trControl = trainControl(classProbs = TRUE, summaryFunction = f1, method = "repeatedcv", number = 3, repeats = 100))
  lda.f1[[x]] <- lda.model$results["F1"] %>% as_tibble %>% mutate(language = x)
  # lda.model.preds <- predict(lda.model, newdata = lang.frame)
  # predictions <- recode(lda.model.preds, Action = 1, Thing = 0)
  # ground <- recode(lang.frame$ontologicalCategory, Action = 1, Thing = 0)
  # mcc <- mltools::mcc(predictions, ground)
  # lda.mcc[[x]] <- tibble(language = x, Matthews = mcc)
}

lda.f1 <- bind_rows(lda.f1) %>%
  mutate(language = unique(allPhon.extremes$language))


spanish.example <- filter(spanish.example, ontologicalCategory != "Other")
onehot.encoding <- spanish.example %>%
  mutate_at(c("ending", "start"), as.factor) %>%
  caret::dummyVars(data = ., ~ ending + start)
spanish.example <- spanish.example %>%
  bind_cols(as_tibble(predict(object = onehot.encoding, newdata = spanish.example)))  %>%
  select(ontologicalCategory, contains("ending"), contains("start"), -ending, -start) %>%
  mutate(ontologicalCategory = factor(ontologicalCategory))

lda.model <- caret::train(data = spanish.example, ontologicalCategory ~ ., method = "lda", trControl = trainControl(
  method = "repeatedcv",
  number = 3,
  repeats = 100
))
lda.model.preds <- predict(lda.model, newdata = spanish.example)
predictions <- recode(lda.model.preds, Action = 1, Thing = 0)
ground <- recode(spanish.example$ontologicalCategory, Action = 1, Thing = 0)
mcc <- mltools::mcc(predictions, ground)


spanish.example



spurt.splits <- caret::createDataPartition(spanish.example$ontologicalCategory, times = 100, p = 0.66)

x <- map(spurt.splits, function(trainpartition){
  train <- spanish.example[trainpartition,]
  onehot.encoding <- train %>%
    mutate_at(c("ending", "start"), as.factor) %>%
    caret::dummyVars(data = ., ~ ending + start)
  train <- train %>%
    bind_cols(as_tibble(predict(object = onehot.encoding, newdata = train))) %>%
    select(ontologicalCategory, contains("ending"), contains("start"), -ending, -start) %>%
    mutate(ontologicalCategory = factor(ontologicalCategory))
  lda.model <- caret::train(data = train, ontologicalCategory ~ ., method = "nb")
  print("e")
  out.of.split <- setdiff(1:nrow(spanish.example), trainpartition)
  test <- spanish.example[out.of.split,]
  onehot.encoding <- test %>%
    mutate_at(c("ending", "start"), as.factor) %>%
    caret::dummyVars(data = ., ~ ending + start)
  print("yo")
  test <- test %>%
    bind_cols(as_tibble(predict(object = onehot.encoding, newdata = test))) %>%
    select(ontologicalCategory, contains("ending"), contains("start"), -ending, -start) %>%
    mutate(ontologicalCategory = factor(ontologicalCategory))
  test <- test[, colnames(train)]
  prediction <- predict(lda.model, test)
  mcc <- prediction %>%
    recode(Thing = 0, Action = 1) %>% mltools::mcc(preds = ., actual = recode(spanish.example$ontologicalCategory[out.of.split], Action = 1, Thing = 0))
  print(mcc)
  return(mcc)
})


rnn.f1 %>% filter(language %in% names(all.extremes)) %>% left_join(lda.f1) %>% left_join(rename(phonLanguages, language = Name)) %>% mutate(F1 = ifelse(is.na(F1), 0, F1)) %>% mutate(diff = rnn.F1 - F1) %>% plotly::plot_ly(
  type = "bar",
  data = .,
  x = ~ language,
  y = ~ diff,
  hoverinfo = "text",
  text = ~ paste(
    "Language =",
    language,
    ";\nLDA F1 = ",
    round(F1, 2),
    ";\nRNN F1 = ",
    round(rnn.F1, 2)
  )
) %>%  plotly::layout(title = "Difference between RNN performance and LDA with edge phonemes performance") %>% htmlwidgets::saveWidget("rnn_lda_difference_plot.html", selfcontained = TRUE)


# THIS BREAKS -> they share variables and it cant do it.
