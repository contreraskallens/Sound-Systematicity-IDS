# Packages and preliminaries ----------------------------------------------------------------

library(tidyverse)
library(stringdist)
library(effsize)
library(cowplot)
library(coin)
library(Hmisc)
library(rsq)
set.seed(1)


palette_a_t <- c("#d06858", "#7e9ec4", "#add4db")
palette_other <-  c("#e0dccb", "#c49464", "#add4db")
palette_line <- c("#28313d")
palette_world <-  c("#62c08f")
palette_con <-  viridis::plasma(10)

# Functions ---------------------------------------------------------------

get.language <- function(a.language, data = all.phon){
  # Extracts the dataframe of a specific language and subsets variables
  # Args:
  #   a.language: String with a language name, e.g. "English"
  #   data: complete data with all languages. By default, it looks for an object called "all.phon"
  # Returns: Tibble with only rows corresponding to specified
  # language and columns for Phon form, Meaning and Class.
  data %>%
    dplyr::select(form = Form, phon, english.name, 
                  language, ontological.category) %>%
    filter(language == a.language) %>%
    dplyr::select(-language) %>%
    return()
}

get.distance.matrix <- function(language.df){
  # Constructs square distance matrix for the words in a specific language using
  # OSA distance. After that, it normalizes the distances by dividing them by the
  # length of the longest word. Designed to be used on products of get.language().
  # Args:
  #   language.df: a dataframe or tibble with a column called "phon" that stores phonetic forms.
  # Returns:
  #   An n x n matrix where n is the number of phon forms included in language
  #   DF, and each cell contains the string distance between the word in the row
  #   and the word in the column.
  language.df <- language.df %>% 
    filter(ontological.category != "Other") %>% 
    droplevels()
  words <- language.df$phon
  distance.matrix <- stringdistmatrix(words, words)
  colnames(distance.matrix) <- words
  row.names(distance.matrix) <- words
  lengths <- map_dbl(words, nchar)
  sum.lengths <- outer(lengths, lengths, FUN = "+")
  numerator <- 2 * distance.matrix
  denominator <- sum.lengths + distance.matrix
  distance.matrix <- numerator / denominator
  return(distance.matrix)
}

get.mean.distances <- function(language.df, distance.matrix){
  # Gets the mean distance of each word to the different semantic classes in
  # language.df.
  # Args:
  #   language.df: a dataframe or tibble with a column called ontological.category that
  #   stores the classes.
  #   distance.matrix: a square distance matrix specifying the string distance of
  #   the phon forms in languageDF. Designed to work with the product of
  #   get.distance.matrix().
  # Returns:
  #   A tibble with n rows and 7 columns, where each row is a phon form in
  #   languageDF.
  #     word: contains the phon form of the word.
  #     mean.action, mean.thing, mean.other: mean distance of the word with each of
  #     the classes in ontological.category
  #     tipicality: measures the relative distance to verbs and nouns with
  #     mean.distance.action - mean.distance.thing.
  #     class: contains the class of the word in the row.
  #     english.name is the english meaning of the word as per IDS.
  action.mask <- language.df$ontological.category == "Action"
  thing.mask <- language.df$ontological.category == "Thing"
  diag(distance.matrix) <- NA
  mean.distance.matrix <- tibble(word = language.df$phon) %>%
    mutate(mean.action = rowMeans(distance.matrix[, action.mask], na.rm = TRUE),
           mean.thing = rowMeans(distance.matrix[, thing.mask], na.rm = TRUE),
           typicality = mean.action - mean.thing,
           class = language.df$ontological.category,
           english.name = language.df$english.name,
           form = language.df$form)
  return(mean.distance.matrix)
}

get.typicality.stats <- function(this.language, distances.df){
  # Gets test statistics and effect size for a specified language. Runs a Wilcoxon test and gets the E^2 and cohen's d.
  # Args:
  #   this.language: a language name as a string.
  #   distances.df: the dataframe with all distances (e.g. all.distances.adjusted)
  # Returns:
  #   A tibble with 1 rows and 6 columns, where each column is a different statistic.
  #     language: the language name, provided as this.language.
  #     Action: median typicality of Action words
  #     Thing: median typicality of Thing words
  #     difference: absolute difference in mean typicality between Action and Thing.
  #     p.value: the p value of a Wilcoxon test that assesses the effect of class in typicality.
  #     Z: the Z value of the Wilcoxon test.
  #     eta.squared: the effect size of class with typicality as a predicted variable.
  #     d: the cohen's d of the distribution of typicality for each class.
  lang.df <- distances.df %>%
    filter(class != "Other", language == this.language) %>%
    mutate(class = factor(class))
  lang.wilcox <- coin::wilcox_test(data = lang.df, typicality ~ class)
  p.value <- coin::pvalue(lang.wilcox)
  Z.value <- coin::statistic(lang.wilcox, type = "standardized")
  eta.squared <- (Z.value ^ 2) / (nrow(lang.df))
  d.value <- effsize::cohen.d(data = lang.df, typicality ~ class)
  results <- lang.df %>% 
    group_by(class) %>% 
    dplyr::summarize(median.typicality = median(typicality)) %>% 
    spread(class, median.typicality) %>% 
    mutate(language = this.language,
           difference = abs(Action - Thing),
           p.value = p.value[1], 
           Z = Z.value[[1]], 
           eta.squared = eta.squared[[1]], 
           d = d.value$estimate) %>% 
    select(language, everything())
  return(results)
}

get.nearest.neighbors <- function(a.language, randomize = FALSE, distance.matrix = NULL){
  # Finds the closest phonological neighbor for each word in a specificed
  # language. Phonological neighbors are determined by using Optimal String Alignment distance,
  # i.e. the product of getDistanceMatrix() unless different distance matrix is provided.
  # Returns a tibble with the neighbor and its category of the neighbor.
  # Args:
  #   a.language: A string specifying the name of the language to be analyzed
  #   e.g. "English".
  #   data: a data frame as subset of allPhon or allPhonCtrl.
  #   randomize: if TRUE, labels are shuffled because calculating the closest neighbors
  #   distance.matrix: pre calculated distance matrix to use to speed up calculation.
  a.language <- a.language %>% 
    filter(ontological.category != "Other") %>% 
    droplevels()
  if(randomize == TRUE){
    a.language$ontological.category <- sample(a.language$ontological.category, replace = FALSE)
  }
  distance.matrix <- distance.matrix * -1
  diag(distance.matrix) <- -100
  nearest.neighbors <- max.col(distance.matrix, ties.method = "random")
  neighbors <- a.language$phon[nearest.neighbors]
  neighbors.category <- a.language$ontological.category[nearest.neighbors]
  lang.name <- unique(a.language$language)
  a.language <- a.language %>%
    as_tibble() %>%
    mutate(neighbor = neighbors,
           neighbor.category = neighbors.category,
           language = lang.name) %>%
    return()
}

rnn.means <- function(data, indices){
  # Ad-hoc function to load data from the RNN runs.
  these.data <- data[indices,] %>% 
    select(-X1, -language)
  return(colMeans(these.data))
}

bootstrapped.cis <- function(rnn.language){
  # This function takes the result of a specific language and bootstraps
  # confidence intervals (R = 10000) of each of the performance measures.
  # Mashco Piro and Waorani throw a small error because they have 100% thing accuracy all the time.
  # Returns a tibble with the bootstrapped confidence intervals and means of all performance measures.
  # Args:
  #   rnn.language: dataframe with all runs (e.g. all folds) where each run is a row and each column is a performance measure.
  # Returns:
  #   A tibble with 1 rows and 3 columns per measure that include mean, lower and upper CI.
  language <- unique(rnn.language$language)
  print(language)
  mean.boots <- boot::boot(data = rnn.language, statistic = rnn.means, R = 1000)
  original.means <- mean.boots$t0[c("Matthews", "F1", "ActionAccuracy","ThingAccuracy")] %>% 
    t() %>% 
    as_tibble()
  matthews <- boot::boot.ci(boot.out = mean.boots, conf = 0.99, type = "perc",
                            index = c(1)) %>% 
    .$percent %>% 
    .[4:5] %>% 
    set_names(c("Matthews.Lower", "Matthews.Upper")) %>%
    t() %>% 
    as_tibble()
  f1 <- boot::boot.ci(boot.out = mean.boots, conf = 0.99, type = "perc",
                      index = c(3)) %>% 
    .$percent %>% 
    .[4:5] %>% 
    set_names(c("F1.Lower", "F1.Upper")) %>%
    t() %>% 
    as_tibble()
  action <- boot::boot.ci(boot.out = mean.boots, conf = 0.99, type = "perc",
                          index = c(4)) %>% 
    .$percent %>% 
    .[4:5] %>% 
    set_names(c("Action.Lower", "Action.Upper")) %>%
    t() %>% 
    as_tibble()
  thing <- try({boot::boot.ci(boot.out = mean.boots, conf = 0.99, type = "perc",
                              index = c(5)) %>% 
      .$percent %>% 
      .[4:5] %>% 
      set_names(c("Thing.Lower", "Thing.Upper")) %>%
      t() %>% 
      as_tibble()})
  
  # this catches if all are 0 or all are 1
  if("try-error" %in% class(thing)){thing <- tibble(Thing.Lower = original.means$ThingAccuracy[1],
                                                    Thing.Upper = original.means$ThingAccuracy[1])}
  return(bind_cols(original.means, matthews, f1, action, thing, Language = language))
}

clean.phon <- function(df){
  df <- filter(df, !is.na(df$phon),
               str_detect(phon, "\\?", negate = TRUE)) %>%  # Delete forms that include question marks 
    ungroup()
  
  # First, delete diacritic markers that are superscript letters.
  # df <- mutate(df, phon = str_remove_all(phon, "\\p{Lm}"))
  df <- mutate(df, phon = stringi::stri_trans_nfc(phon))
  df <- mutate(df, phon = str_squish(str_remove_all(phon, "~.*"))) # extract the first form of multi form entries separated by ~
  df <- mutate(df, phon = str_squish(str_remove_all(phon, ",.*"))) # extract the first form of multi form entries separated by ,
  df <- mutate(df, phon = str_squish(str_remove_all(phon, "\\|.*"))) # extract the first form of multi form entries separated by |
  df <- mutate(df, phon = str_squish(str_remove_all(phon, "/.*"))) # extract the first form of multi form entries separated by /
  df <- mutate(df, phon = str_squish(str_remove_all(phon, "\\\\"))) # Remove all backslashes (typos, only one word)
  df <- mutate(df, phon = str_squish(str_remove_all(phon, "-"))) # Remove all dashes. They're usually used as ligatures.
  # Remove all stress marks and most symbols u02BC is a Modifier Letter Apostrophe. Retain : for long vowels
  # (brackets, « and parentheses  
  df <- mutate(df, phon = str_squish(str_remove_all(phon, "[\u0022\u02BC\u02CB#–\\ʼ'’\\*“‰‘]"))) 
  # Delete empty parentheses
  df <- mutate(df, phon = str_squish(str_remove_all(phon, "\\(\\)")))
  # Delete addenda at the beginning and at the end of the strings
  df <- mutate(df, phon = str_squish(str_remove_all(phon, "^\\(.+\\)(?=.+)")))
  df <- mutate(df, phon = str_squish(str_remove_all(phon, "(?<=.{1})\\(.+\\)$")))
  # Delete parenthesis that enclose the whole string
  df <- mutate(df, phon = str_squish(str_remove_all(phon, "^\\(")))
  df <- mutate(df, phon = str_squish(str_remove_all(phon, "\\)$")))
  # Now delete everything remaining that's in a parenthesis
  df <- mutate(df, phon = str_squish(str_remove_all(phon, "\\(.+\\)")))
  # There's a handful of words where parentheses aren't closed.
  df <- mutate(df, phon = str_squish(str_remove_all(phon, ".+\\)(?=.+)")))
  df <- mutate(df, phon = str_squish(str_remove_all(phon, " \\(.*")))
  df <- mutate(df, phon = str_squish(str_remove_all(phon, "\\.*"))) # Remove dots
  
  # In most languages, the stem is in the square brackets, so cannot afford to delete the things inside.
  df <- mutate(df, phon = str_squish(str_remove_all(phon, "[\\[\\]]")))
  # Delete diacritic markers that couldn't be normalized into the characaters
  df <- mutate(df, phon = str_squish(str_remove_all(phon, "[\\p{Mn}]")))
  # Delete words with 2 or more spaces
  df <- filter(df, (str_count(phon, " ") < 2))
  
  # Delete reduplicated words by extracting the "stem". This probably overcorrects.
  df <- mutate(df, phon = ifelse(str_detect(phon, "^(.{3,})[:space:]*\\1$"),
                                 str_extract(phon, "^(.{3,})(?=[:space:]*\\1)"),
                                 phon))
  df <- mutate(df, phon = str_squish(str_remove_all(phon, " "))) # Delete remaining spaces
  
  
  
  return(df)
}

replace.vowel <- function(string, vowel, tone, placeholder){
  if(str_detect(string, paste0(vowel, "[[a-z][\\p{Sm}]]*\\", tone), negate = TRUE)){
    return(string)
  }
  new.string <- str_replace_all(string = string,
                                pattern = vowel,
                                replacement = placeholder)
  return(new.string)
}
