# Packages and preliminaries ----------------------------------------------------------------
# install.packages('tidyverse', 'stringdist', 'effsize', 'cowplot', 'coin', 'Hmisc', 'rsq', 'MetBrewer')
library(tidyverse)
library(stringdist)
library(effsize)
library(cowplot)
library(coin)
library(Hmisc)
library(boot)
library(infotheo)
set.seed(1)


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
  df <- mutate(df, phon = str_remove_all(phon, "\\p{Lm}"))
  df <- mutate(df, phon = stringi::stri_trans_nfc(phon))
  df <- mutate(df, phon = str_squish(str_remove_all(phon, "~.*"))) # extract the first form of multi form entries separated by ~
  df <- mutate(df, phon = str_squish(str_remove_all(phon, ",.*"))) # extract the first form of multi form entries separated by ,
  df <- mutate(df, phon = str_squish(str_remove_all(phon, "\\|.*"))) # extract the first form of multi form entries separated by |
  df <- mutate(df, phon = str_squish(str_remove_all(phon, "/.*"))) # extract the first form of multi form entries separated by /
  df <- mutate(df, phon = str_squish(str_remove_all(phon, "\\\\"))) # Remove all backslashes (typos, only one word)
  df <- mutate(df, phon = str_squish(str_remove_all(phon, "-"))) # Remove all dashes. They're usually used as ligatures.
  # Remove all stress marks and most symbols u02BC is a Modifier Letter Apostrophe. Retain : for long vowels
  # (brackets, « and parentheses  
  df <- mutate(df, phon = str_squish(str_remove_all(phon, "[\u0323\u0022\u02BC\u02CB#–\\ʼ'’\\*“‰‘]"))) 
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


# Functions for morphological cleaning
get.ngram.endings <- function(language.df, level, variable){
  all.levels <- c(-1:level)
  names(all.levels) <- all.levels
  all.ngram.endings <- map_dfc(all.levels, function(n){
    str_sub(language.df[[variable]], n)
  })
  ngram.df <- language.df %>% 
    dplyr::select(variable, ontological.category, Form) %>% 
    bind_cols(all.ngram.endings)
  return(ngram.df)
}
get.successors <- function(ngram.df, ngram){
  level <- -(nchar(ngram))
  this.ngram.rows <- ngram.df[[as.character(level)]] == ngram
  this.df <- ngram.df[this.ngram.rows,]
  next.level <- as.character(level - 1)
  successors <- str_sub(this.df[[next.level]], 1, 1)
  return(successors)
}

get.random.frequency <- function(ngram, char.table){
  ngram.components <- str_split(ngram, "") %>% 
    unlist()
  if(length(ngram.components) == 1){
    return(char.table[ngram])
  } else{
    return(prod(char.table[ngram.components]))
  }
}

get.ngram.stats <- function(ngram.df, ngram, char.table, ngram.freq){
  level <- -(nchar(ngram))
  this.entropy <- infotheo::entropy(get.successors(ngram.df, ngram))
  ngram.frequency <- table(ngram.df[[as.character(level)]])
  length.frequency <- ngram.frequency[ngram]
  norm.frequency <- scale(ngram.frequency)[ngram, 1]
  if(is.nan(norm.frequency)){
    norm.frequency <- 1 # This is in case there's only one ending, thus scaled frequency is NAN because of 0 SD
  }
  random.frequency <- get.random.frequency(ngram, char.table)
  results <- list("ngram" = ngram, 
                  "entropy" = this.entropy, 
                  "norm.frequency" = norm.frequency, 
                  "length.frequency" = length.frequency,
                  "higher.than.random" = ngram.freq / random.frequency)
  return(results)
}

get.word.stats <- function(word, ngram.results){
  word.ngrams <- map_chr(-1:(-(nchar(word) - 1)), function(n){
    return(str_sub(word, n))
  })
  names(word.ngrams) <- word.ngrams
  word.entropies <- map_dbl(word.ngrams, function(ngram){
    return(ngram.results[[ngram]][["entropy"]])
  })
  word.norm.frequencies <- map_dbl(word.ngrams, function(ngram){
    return(ngram.results[[ngram]][["norm.frequency"]])
  })
  word.overrep <- map_dbl(word.ngrams, function(ngram){
    return(ngram.results[[ngram]][["higher.than.random"]])
  })
  word.length.freq <- map_dbl(word.ngrams, function(ngram){
    return(ngram.results[[ngram]][["length.frequency"]])
  })
  return(list("entropies" = word.entropies, 
              "length.frequencies" = word.length.freq,
              "norm.frequencies" = word.norm.frequencies, 
              "overrep" = word.overrep))
}

evaluate.word <- function(word, ngram.results){
  word.stats <- get.word.stats(word, ngram.results)
  is.candidate <- word.stats$length.frequencies > 1 # Use only segments with more than 1 length frequency
  if(sum(is.candidate) == 0){ # If none of the ngrams appear in any other word, no marker.
    return("#")
  }
  word.entropies <- word.stats$entropies[is.candidate]
  word.entropies <- word.entropies[!is.nan(word.entropies)]
  word.frequencies <- word.stats$norm.frequencies
  word.overrep <- word.stats$overrep
  end.entropy <- ngram.results[["#"]][["entropy"]]
  
  tested.ngrams <- names(word.entropies)[1:(length(word.entropies))]
  peaks <- c()  
  if(length(tested.ngrams) > 1){
    for(x in 1:length(tested.ngrams)){
      this.entropy <- word.entropies[x]
      if(x == 1){
        prev.entropy <- end.entropy
      } else {
        prev.entropy <- word.entropies[x - 1]
      }
      if(this.entropy >= prev.entropy){
        peaks <- c(peaks, tested.ngrams[x])
      }
    }
    possible.markers <- unique(peaks) 
  } else {
    possible.markers <- tested.ngrams
  }
  #If there's no peaks left after that, return the end marker
  if(length(possible.markers) == 0){
    return("#")
  } 
  # Check whether frequency is higher than average OR higher than chance.
  frequency.test <- map_lgl(possible.markers, function(ngram){
    if((word.frequencies[ngram] > 0) | (word.overrep[ngram] > 1)){
      return(TRUE)
    } else{
      return(FALSE)
    }
  })
  # If none pass, return end marker
  if(sum(frequency.test) == 0){ 
    return("#")
  }
  
  possible.markers <- possible.markers[frequency.test]
  # Get entropy of each candidate along with entropy of end mark
  candidate.entropies <- c(end.entropy, word.entropies[possible.markers]) 
  names(candidate.entropies) <- c("#", possible.markers)
  max.entropy <- names(candidate.entropies)[which.max(candidate.entropies)] 
  # Return the name of the candidate with the most entropy
  return(max.entropy)
}

get.morph.markers <- function(language.df, variable){
  
  all.ngrams <- get.ngram.endings(language.df, -(max(nchar(language.df[[variable]]))), variable)
  # Get data for frequency
  all.ngram.segments <- all.ngrams %>% 
    dplyr::select(-variable, -ontological.category, -Form) %>% 
    unlist()
  character.frequency <- paste0(all.ngram.segments, collapse = "") %>% 
    str_split("") %>% 
    table()
  print(character.frequency)
  character.frequency <- character.frequency / sum(character.frequency)
  ngram.frequency <- table(all.ngram.segments)
  ngram.frequency <- ngram.frequency / sum(ngram.frequency)
  
  # Get stats for each unique ngram
  unique.ngrams <- unique(all.ngram.segments)
  names(unique.ngrams) <- unique.ngrams
  
  all.ngram.stats <- map(unique.ngrams, function(ngram){
    ngram.stats <- get.ngram.stats(all.ngrams, ngram, character.frequency, ngram.frequency[ngram])
    return(ngram.stats)
  })
  all.ngram.stats[["#"]] <- list("ngram" = "#", "entropy" = infotheo::entropy(all.ngrams$`-1`), 
                                 "norm.frequency" = 0, "length.frequency" = 0, "higher.than.random" = 0)
  
  all.stats.df <- bind_rows(all.ngram.stats) %>% 
    add_column(ontological.category = language.df$ontological.category[1])
  
  
  words.and.markers <- language.df %>% 
    rowwise() %>% 
    mutate(marker = evaluate.word((!!as.symbol(variable)), ngram.results = all.ngram.stats))
  
  results <- list("marker.census" = all.stats.df, "marked.words" = words.and.markers)
  return(results)
}

clean.language <- function(language, all.data, variable){
  this.language <- language
  lang.df <- all.data %>% 
    filter(language == this.language)
  categories <- as.character(unique(lang.df$ontological.category))
  names(categories) <- categories
  all.marker.df <- map(categories, function(category){
    print(category)
    this.df <- lang.df %>% 
      filter(ontological.category == category) %>% 
      droplevels()
    these.markers <- get.morph.markers(this.df, variable)
    return(these.markers)    
  })
  marker.census <- bind_rows(all.marker.df$Thing$marker.census, all.marker.df$Action$marker.census, all.marker.df$Other$marker.census)
  all.markers <- bind_rows(all.marker.df$Thing$marked.words, all.marker.df$Action$marked.words, all.marker.df$Other$marked.words)
  
  all.markers <- all.markers %>% 
    mutate(marker.position = nchar((!!as.symbol(variable))) - nchar(marker),
           clean.phon = ifelse(marker == "#", (!!as.symbol(variable)), str_sub((!!as.symbol(variable)), 1, marker.position))) %>% 
    dplyr::select(-marker.position)
  results <- list("census" = marker.census, "clean.df" = all.markers)
}