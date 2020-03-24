# Packages and preliminaries ----------------------------------------------------------------

library(tidyverse)
library(stringdist)
library(caret)
library(wesanderson)
library(effsize)
library(cowplot)
library(coin)
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



# Load data -----------------------------------------------------

# All_phon contains info on individual words.
# Phon_languages contains info on each language.

all.phon <- read_csv('../Data/Processed/all_phon.csv')
all.phon.adjusted <- read_csv("../Data/Processed/all_phon_adjusted.csv")
phon.languages <- read_csv('../Data/Processed/all_language_info.csv')

# Basic descriptive statistics of the features of wordlists ------

## All numbers of items
all.phon %>%
  group_by(language) %>%
  tally() %>%
  rename(number.of.words = n) %>%
  arrange(desc(number.of.words))

## Mean and SD number of words
all.phon %>%
  group_by(language) %>%
  tally() %>%
  summarize(mean.number.words = mean(n),
            sd.number.words = sd(n))

## Distribution of number of words
all.phon %>%
  group_by(language) %>%
  tally() %>%
  rename(number.of.words = n) %>%
  ggplot(aes(x = number.of.words)) +
  geom_histogram(fill = palette_other[2], binwidth = 50, color = palette_line) +
  cowplot::theme_cowplot() +
  labs(x = "Number of Words", y = "Count", title = "Number of Words per Language")

## Number of families
phon.languages %>%
  group_by(family) %>%
  tally() %>%
  rename(number.of.languages = n) %>%
  arrange(desc(number.of.languages))

## Category tally
all.phon %>%
  group_by(ontological.category) %>%
  tally() %>%
  mutate(proportion = n / sum(.$n)) %>%
  select(-n)

## Within-language proportions of categories
all.phon %>%
  group_by(language, ontological.category) %>%
  tally() %>%
  group_by(language) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n)

## Plot the proportions of each category in the languages as boxplots
all.phon %>%
  group_by(language, ontological.category) %>%
  tally() %>%
  group_by(language) %>%
  mutate(
    proportion = n / sum(n),
    ontological.category = factor(ontological.category, levels = c("Thing", "Action", "Other"))
  ) %>%
  ggplot(aes(x = ontological.category, y = proportion, fill = ontological.category)) +
  geom_boxplot(width = 0.3, color = palette_line) +
  scale_fill_manual(name = "Category", values = palette_a_t) + 
  cowplot::theme_cowplot() +
  labs(y = "Proportion", x = "Ontological Category",
       title = "Within language proportion of words of each category")

# Same, but with adjusted wordlists

## All numbers of items
all.phon %>%
  group_by(language) %>%
  tally() %>%
  rename(number.of.words = n) %>%
  arrange(desc(number.of.words))

## Mean and SD number of words
all.phon %>%
  group_by(language) %>%
  tally() %>%
  summarize(mean.number.words = mean(n),
            sd.number.words = sd(n))

## Distribution of number of words
all.phon.adjusted %>%
  group_by(language) %>%
  tally() %>%
  rename(number.of.words = n) %>%
  ggplot(aes(x = number.of.words)) +
  geom_histogram(fill = palette_other[2], binwidth = 50, color = palette_line) +
  cowplot::theme_cowplot() +
  labs(x = "Number of Words", y = "Count", title = "Number of Words per Language")

## Category tally
all.phon.adjusted %>%
  group_by(ontological.category) %>%
  tally() %>%
  mutate(proportion = n / sum(.$n)) %>%
  select(-n)

## Within-language proportions of categories
all.phon.adjusted %>%
  group_by(language, ontological.category) %>%
  tally() %>%
  group_by(language) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n)

# Plot the proportions of each category in the languages as boxplots
all.phon.adjusted %>%
  group_by(language, ontological.category) %>%
  tally() %>%
  group_by(language) %>%
  mutate(
    proportion = n / sum(n),
    ontological.category = factor(ontological.category, levels = c("Thing", "Action", "Other"))
    ) %>%
  ggplot(aes(x = ontological.category, y = proportion, fill = ontological.category)) +
  geom_boxplot(width = 0.3, color = palette_line) +
  scale_fill_manual(name = "Category", values = palette_a_t) + 
  cowplot::theme_cowplot() +
  labs(y = "Proportion", x = "Ontological Category",
       title = "Within language proportion of words of each category")

# Wrangling Data ------------------------------------------------------------------

# Get normalized mean class distances for each language and bind it in one tibble

all.distances <- map_dfr(.x = unique(all.phon$language),
                                      .f = function(language.name) {
                                        language <- get.language(a.language = language.name)
                                        distances <- get.distance.matrix(language)
                                        mean.distances <- get.mean.distances(language.df = language, 
                                                                             distance.matrix = distances) %>%
                                          mutate(language = language.name)
                                        return(mean.distances)
                                        }
                                      )

all.distances.adjusted <- map_dfr(.x = unique(all.phon.adjusted$language),
                                  .f = function(language.name) {
                                    language <- get.language(a.language = language.name, 
                                                             data = all.phon.adjusted)
                                    distances <- get.distance.matrix(language)
                                    mean.distances <- get.mean.distances(language.df = language, 
                                                         distance.matrix = distances) %>%
                                      mutate(language = language.name)
                                    return(mean.distances)
                                  }
)

# Typicality tests and visualization ------

# Get tipicality stats for each class in each language
all.distances %>%
  group_by(class) %>%
  summarize(median.typicality = median(typicality))
all.distances.adjusted %>%
  group_by(class) %>%
  summarize(median.typicality = median(typicality))

# Kruskal-wallis of all languages of typicality differences between Action,
# Thing and Other
three.way.test <- all.distances %>%
  mutate(class = factor(class)) %>%
  coin::kruskal_test(data = ., typicality ~ class)
three.way.test.adjusted <- all.distances.adjusted %>%
 mutate(class = factor(class)) %>%
 coin::kruskal_test(data = ., typicality ~ class)

# Get the effect size of k-w test according to
# http://tss.awf.poznan.pl/files/3_Trends_Vol21_2014__no1_20.pdf
chi.square.original <- three.way.test@statistic@teststatistic
eta.squared.original <- (chi.square.original - 4) / (nrow(all.distances) - 3)

chi.square.adjusted <- three.way.test.adjusted@statistic@teststatistic
eta.squared.adjusted <- (chi.square.adjusted - 4) / (nrow(all.distances.adjusted) - 3)

# Pairwise wilcox test of the difference in typicality between Action and Things
wilcox.all <- all.distances %>%
  filter(class != "Other") %>%
  mutate(class = factor(class)) %>%
  coin::wilcox_test(data = ., typicality ~ class)
wilcox.all.adjusted <- all.distances.adjusted %>%
  filter(class != "Other") %>%
  mutate(class = factor(class)) %>%
  coin::wilcox_test(data = ., typicality ~ class)

# Effect size of action/thing comparisons
# Equivalent of R ^ 2, see
# https://www.researchgate.net/profile/Catherine_Fritz2/51554230_Effect_Size_Estimates_Current_Use_Calculations_and_Interpretation/links/5844494108ae2d217566ce33.pdf,
# p. 12
Z.action.thing <- wilcox.all %>% 
  coin::statistic(type = "standardized")
eta.squared.action.thing.original <- (Z.action.thing ^ 2) / (nrow(filter(all.distances, class != "Other"))) 
Z.action.thing.adjusted <- wilcox.all.adjusted %>% 
  coin::statistic(type = "standardized")
eta.squared.action.thing.adjusted <- (Z.action.thing.adjusted ^ 2) / (nrow(filter(all.distances.adjusted, class != "Other")))

# Also get Cohen's d of the two distributions
d.action.thing.original <- effsize::cohen.d(data = filter(all.distances, class != "Other"), typicality ~ class)
d.action.thing.adjusted <-effsize::cohen.d(data = filter(all.distances.adjusted, class != "Other"), typicality ~ class)

# Plotting languages and typicality ----------------------------------------------------------------

# World map with languages

## Get coordinates from phon.languages. They were obtained through WALS.
languages.x <- phon.languages$longitude
languages.y <- phon.languages$latitude

ggplot() +
  borders("world", colour =  palette_world[1], fill = palette_world[1], size = .2) + # create a layer of borders
  geom_jitter(aes(x = languages.x, y = languages.y), 
              fill = palette_other[2], size = .5, shape = 16, color = palette_line) +
  cowplot::theme_map()

ggsave("../Figures/world_map.eps", width = 8.7, height = 7, units = "cm")
ggsave("../Figures/world_map.png", width = 8.7, height = 7, units = "cm", dpi = 300)

# 2D Density of all words
density.original <- all.distances %>%
  filter(class != "Other") %>%
  mutate(class = factor(class, levels = c("Action", "Thing", "Other"))) %>%
  ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
  stat_density2d(geom = 'polygon') +
  viridis::scale_fill_viridis(name = "Density", option = "plasma") + 
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = "black") +
  labs(x = "Mean Distance to Actions", y = "Mean Distance to Things") +
  cowplot::theme_cowplot() +
  facet_wrap(vars(class), ncol = 2) +
  cowplot::panel_border() +
  theme(axis.title.x = element_text(size = 8),
  axis.title.y = element_text(size = 8),
  axis.text.x = element_text(size = 6),
  axis.text.y = element_text(size = 6),
  legend.title = element_text(size = 8),
  legend.text = element_text(size = 6),
  strip.text = element_text(size = 8),
  legend.position = "none")
density.original
ggsave("../Figures/density_original.pdf", width = 17, height = 9, units = "cm", dpi = 900)
ggsave("../Figures/density_original.png", width = 17, height = 9, units = "cm", dpi = 900)

density.adjusted <- all.distances.adjusted %>%
  filter(class != "Other") %>%
  mutate(class = factor(class, levels = c("Action", "Thing"))) %>%
  ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
  stat_density2d(geom = 'polygon') +
  viridis::scale_fill_viridis(name = "Density", option = "plasma") + 
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = "black", size = .5) +
  labs(x = "Mean Distance to Actions", y = "Mean Distance to Things") +
  cowplot::theme_cowplot() +
  facet_wrap(vars(class), ncol = 2) +
  cowplot::panel_border() +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        strip.text = element_text(size = 8))
density.adjusted
ggsave("../Figures/density_adjusted.pdf", width = 17, height = 9, units = "cm", dpi = 900)
ggsave("../Figures/density_adjusted.png", width = 17, height = 9, units = "cm", dpi = 900)

cowplot::plot_grid(density.original, density.adjusted, labels = c("A", "B"), nrow = 1, rel_widths = c(1, 1.1))
ggsave("../Figures/density_panel.pdf", width = 17, height = 9, units = "cm", dpi = 900)
ggsave("../Figures/density_panel.png", width = 17, height = 9, units = "cm", dpi = 900)

# Sorted scatter of typicality per class
data.for.scatter <- all.distances %>%
  filter(class != "Other") %>%
  group_by(language, class) %>%
  summarize(Mean = mean(typicality))
data.for.scatter.adjusted <- all.distances.adjusted %>%
  filter(class != "Other") %>%
  group_by(language, class) %>%
  summarize(Mean = mean(typicality))
all.scatters <- rename(data.for.scatter, original = Mean) %>% 
  left_join(rename(data.for.scatter.adjusted, adjusted = Mean))

# Sort by the absolute value of the difference in typicality between Action and Thing
sorted.langs <- data.for.scatter %>% 
  spread(class, Mean) %>%
  mutate(difference = abs(Action - Thing)) %>%
  arrange(desc(abs(difference)))
sorted.langs.adjusted <- data.for.scatter.adjusted %>% # this one includes the difference in means
  spread(class, Mean) %>%
  mutate(difference = abs(Action - Thing)) %>%
  arrange(desc(abs(difference)))

# Save reference languages: Maximum, median and minimum difference in typicality and English
# Note that min difference is the same in both word lists.
max.difference.original <-
  sorted.langs$language[which.max(sorted.langs$difference)]
min.difference <-
  sorted.langs$language[which.min(sorted.langs$difference)]
mid.difference.original <-
  sorted.langs$language[sorted.langs$difference == quantile(sorted.langs$difference, .5, type = 1)]
english.difference <- "English"
max.difference.adjusted <-
  sorted.langs.adjusted$language[which.max(sorted.langs.adjusted$difference)]
mid.difference.adjusted <-
  sorted.langs.adjusted$language[sorted.langs.adjusted$difference == quantile(sorted.langs.adjusted$difference, .5, type = 1)]

test.languages <- c(max.difference.original,
    min.difference,
    mid.difference.original,
    max.difference.adjusted,
    mid.difference.adjusted,
    "English")

# Filter out "Other" and add field that determines if language is reference or not.
plot.data <- data.for.scatter %>%
  filter(class != "Other") %>%
  group_by() %>%
  mutate(language = factor(language, levels = sorted.langs$language), class = factor(class),
         include = ifelse(language %in% test.languages, language, NA))
plot.data.adjusted <- data.for.scatter.adjusted %>%
  filter(class != "Other") %>%
  group_by() %>%
  mutate(language = factor(language, levels = sorted.langs.adjusted$language), class = factor(class),
         include = ifelse(language %in% test.languages, language, NA))

# Keep labels only for reference languages
label.text <- map_chr(sorted.langs$language, function(x){
  if(x %in% test.languages){
    return(x)
  } else{return("")}
})
label.text.adjusted <- map_chr(sorted.langs.adjusted$language, function(x){
  if(x %in% test.languages){
    return(x)
  } else{return("")}
})

# Manual fix for labels in plot
label.text[which(label.text == "Shipibo-Conibo")] <- "Shipibo\nConibo"
label.text[which(label.text == "Waorani")] <- "      Waorani"

# Plot languages as scatter in y axis of typicality.
scatter.original <- ggplot(data = plot.data, aes(y = Mean, x = language, color = class, shape = class,
                                    group = class)) +
  labs(x = 'Language',
       y = 'Mean Typicality') +
  geom_point(aes(fill = class), color = palette_line, size = .75) +
  geom_vline(aes(xintercept = include), size = 0.2, color = palette_line) + 
  geom_hline(linetype = 'solid', yintercept = 0, color = palette_line) +
  scale_fill_manual(name = "Category", values = c(palette_a_t[1], palette_a_t[2])) + 
  scale_shape_manual(name = "Category", values = c(23, 24)) +
  scale_x_discrete(name = "", labels = label.text) +
  cowplot::theme_cowplot() + 
  expand_limits(x = -1) +
  expand_limits(x = 228) + 
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 6),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        strip.text = element_text(size = 8),
        axis.ticks.x = element_blank())
scatter.original
ggsave("../Figures/scatter_original.pdf", width = 17, height = 7, units = "cm", dpi = 900)
ggsave("../Figures/scatter_original.png", width = 17, height = 7, units = "cm", dpi = 900)

scatter.adjusted <- ggplot(data = plot.data.adjusted, aes(y = Mean, x = language, color = class, shape = class,
                                    group = class)) +
  labs(x = 'Language',
       y = 'Mean Typicality') +
  geom_point(aes(fill = class), color = palette_line, size = .75) +
  geom_vline(aes(xintercept = include), size = .2, color = palette_line) + 
  geom_hline(linetype = 'solid', yintercept = 0, color = palette_line) +
  scale_fill_manual(name = "Category", values = c(palette_a_t[1], palette_a_t[2])) + 
  scale_shape_manual(name = "Category", values = c(23, 24)) +
  scale_x_discrete(name = "", labels = label.text.adjusted) +
  cowplot::theme_cowplot() + 
  expand_limits(x = -1) +
  expand_limits(x = 228) + 
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 6),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        strip.text = element_text(size = 8),
  axis.ticks.x = element_blank())
scatter.adjusted
ggsave("../Figures/scatter_adjusted.pdf", width = 17, height = 7, units = "cm", dpi = 900)
ggsave("../Figures/scatter_adjusted.png", width = 17, height = 7, units = "cm", dpi = 900)

cowplot::plot_grid(scatter.original, scatter.adjusted, nrow = 2, labels = c("A", "B"))
ggsave("../Figures/scatter_panel.pdf", width = 17, height = 8, units = "cm", dpi = 900)
ggsave("../Figures/scatter_panel.png", width = 17, height = 8, units = "cm", dpi = 900)

# Stats for the reference languages ------

# Typicality per category for the reference languages

get.typicality.stats <- function(this.language, distances.df){
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
    summarize(mean.typicality = mean(typicality)) %>% 
    spread(class, mean.typicality) %>% 
    mutate(language = this.language,
           p.value = p.value[1], 
           Z = Z.value[[1]], 
           eta.squared = eta.squared[[1]], 
           d = d.value$estimate) %>% 
    select(language, everything())
  return(results)
}

# Get typicality stats for reference languages in both original and adjusted wordlists
purrr::map2_dfr(test.languages, list(all.distances), get.typicality.stats)
purrr::map2_dfr(test.languages, list(all.distances.adjusted), get.typicality.stats)

# Closest phonological neighbors ---------------

# Generate a count for each category and each language of the number of words
# where the closest neighbor in the Levenshtein phonological space (as
# represented in their distance matrix generated with get.distance.matrix) is a
# word of the same class. Because ties are broken at random, run it 100 times and take the mean.

# This process takes long, so results have been preallocated. If you wish to rerun them, uncomment the following lines.
# all.phon.list <- all.phon %>%
#   split(.$language)
# all.phon.list <- all.phon.list[sort(names(all.phon.list))]
# all.distance.matrices <- purrr::map(all.phon.list, function(x){get.distance.matrix(x)})

# For each language, generate a distance matrix and, for each word, choose the word with the lowest distance.
# Record whether the class of both words is the same.

# repeated.neighbor <- map2_dfr(.x = all.phon.list, .y = all.distance.matrices, .f = function(x, y){
#   print(unique(x$language))
#       purrr::map_dfr(1:100, function(z){
#         all.neighbors <- get.nearest.neighbors(a.language = x,
#                                           distance.matrix = y,
#                                           randomize = FALSE) %>%
#           mutate(same.neighbor = (ontological.category == neighbor.category)) %>%
#           group_by(language, ontological.category) %>%
#           summarise(proportion.of.hits = sum(same.neighbor) / n()) %>%
#           mutate(permutation = z)
#         return(all.neighbors)
#       }) %>%
#       return()
#   })

# all.phon.list.adjusted <- all.phon.adjusted %>%
#   split(.$language)
# all.phon.list.adjusted <- all.phon.list.adjusted[sort(names(all.phon.list.adjusted))]
# all.distance.matrices.adjusted <- purrr::map(all.phon.list.adjusted, function(x){get.distance.matrix(x)})
# repeated.neighbor.adjusted <- map2_dfr(.x = all.phon.list.adjusted, .y = all.distance.matrices.adjusted, .f = function(x, y){
#   print(unique(x$language))
#   purrr::map_dfr(1:100, function(z){
#     all.neighbors <- get.nearest.neighbors(a.language = x,
#                                            distance.matrix = y,
#                                            randomize = FALSE) %>%
#       mutate(same.neighbor = (ontological.category == neighbor.category)) %>%
#       group_by(language, ontological.category) %>%
#       summarise(proportion.of.hits = sum(same.neighbor) / n()) %>%
#       mutate(permutation = z)
#     return(all.neighbors)
#   }) %>%
#     return()
# })

# repeated.neighbor %>% write_rds("../Data/Processed/neighbor_original.Rds")
# repeated.neighbor.adjusted %>% write_rds("../Data/Processed/neighbor_adjusted.Rds")

# Load data generated from previous lines
repeated.neighbor <- read_rds("../Data/Processed/neighbor_original.Rds")
repeated.neighbor.adjusted <- read_rds("../Data/Processed/neighbor_adjusted.Rds")

# For hypothesis testing, generate a permutation-based null distribution. This
# involves running, for each language in both original and adjusted datasets,
# The same algorithm as before after shuffling the category labels of the words.
# Again, because of the time it takes to run this, the results are saved and
# reloaded later in the script. Also, this was run in a parallel multi-core
# scheme using the package FURRR.

# Load multicore mc
# require(future)
# require(furrr)
# future::plan(multiprocess)

# Change this to a reasonable number considering your machine. Number of cores - 2 seems reasonable.
# cores <- 6
# options(future.globals.maxSize = +Inf, mc.cores = cores)

# neighbor.mc <- furrr::future_map2_dfr(.progress = TRUE, .x = all.phon.list, .y = all.distance.matrices,
#                         .f = function(language, distance.matrix){
#                           purrr::map_dfr(1:1000, function(x){
#                             all.neighbors <- get.nearest.neighbors(a.language = language,
#                                                                    distance.matrix = distance.matrix,
#                                                                    randomize = TRUE) %>%
#                               mutate(same.neighbor = (ontological.category == neighbor.category)) %>%
#                               group_by(language, ontological.category) %>%
#                               summarise(proportion.of.hits = sum(same.neighbor) / n()) %>%
#                               mutate(permutation = x)
#                             return(all.neighbors)
#                             }) %>%
#                             group_by(language, ontological.category, permutation) %>%
#                             summarize(Mean = mean(proportion.of.hits),
#                                     Standard.Dev = sd(proportion.of.hits)
#                                     Upper = (Mean + sd(proportion.of.hits)),
#                                     Lower = (Mean - sd(proportion.of.hits))) %>%
#                             return()
# })

# neighbor.mc.adjusted <- furrr::future_map2_dfr(.progress = TRUE, 
#                                                .x = all.phon.list.adjusted, 
#                                                .y = all.distance.matrices.adjusted,
#                                                .f = function(language, distance.matrix){
#                                         purrr::map_dfr(1:1000, function(x){
#                                           all.neighbors <- get.nearest.neighbors(a.language = language,
#                                                                                  distance.matrix = distance.matrix,
#                                                                                  randomize = TRUE) %>%
#                                             mutate(same.neighbor = (ontological.category == neighbor.category)) %>%
#                                             group_by(language, ontological.category) %>%
#                                             summarise(proportion.of.hits = sum(same.neighbor) / n()) %>%
#                                             mutate(permutation = x)
#                                           return(all.neighbors)
#                                         }) %>%
#                                           group_by(language, ontological.category, permutation) %>%
#                                           summarize(Mean = mean(proportion.of.hits),
#                                                    Standard.Dev = sd(proportion.of.hits)
#                                                    Upper = (Mean + sd(proportion.of.hits)),
#                                                     Lower = (Mean - sd(proportion.of.hits))) %>%
#                                           return()
#                                       })
# neighbor.mc %>%
# write_rds("../Data/Processed/neighbor_mc_original.Rds")
# neighbor.mc.adjusted %>%
# write_rds("../Data/Processed/neighbor_mc_adjusted.Rds")

neighbor.mc <- read_rds("../Data/Processed/neighbor_mc_original.Rds")
neighbor.mc.adjusted <- read_rds("../Data/Processed/neighbor_mc_adjusted.Rds")

# Generate 99% normal confidence intervals for the proportion of words with same category nearest neighbor
neighbor.stats <- repeated.neighbor %>%
  group_by(language, ontological.category) %>%
  do(enframe(Hmisc::smean.cl.normal(.$proportion.of.hits, conf.int = 0.99))) %>%
  spread(name, value)
neighbor.stats.adjusted <- repeated.neighbor.adjusted %>%
  group_by(language, ontological.category) %>%
  do(enframe(Hmisc::smean.cl.normal(.$proportion.of.hits, conf.int = 0.99))) %>%
  spread(name, value)


# Use the number of words with same-class neighbors in the shuffle test as "random" baseline.
neighbor.mc <- neighbor.mc %>%
  rename(random = proportion.of.hits)
neighbor.mc.adjusted <- neighbor.mc.adjusted %>%
  rename(random = proportion.of.hits)

# Join the actual and the permuted neighbors per language, per category. Then,
# count what proportion of the 1000 permutations the actual neighbors have a
# lower or equal proportion of same-category neighbors as the null permutation.
# Use the LOWER boundary of the .99 confidence interval as the "actual" number
# to be compared with the permuted numbers.
neighbor.test <- neighbor.stats %>% 
  left_join(neighbor.mc) %>%
  mutate(is.higher = random >= Lower)
neighbor.test.adjusted <- neighbor.stats.adjusted %>% 
  left_join(neighbor.mc.adjusted) %>%
  mutate(is.higher = random >= Lower)
neighbor.test <- neighbor.test %>%
  group_by(language, ontological.category) %>%
  summarize(p = sum(is.higher) / 1000)
neighbor.test.adjusted <- neighbor.test.adjusted %>%
  group_by(language, ontological.category) %>%
  summarize(p = sum(is.higher) / 1000)

# Check statistics for reference languages. For "baseline" performance for each
# language, take 1 SD above the mean of the shuffles.
neighbor.test %>% 
  filter(language %in% test.languages)
neighbor.test.adjusted %>% 
  filter(language %in% test.languages)
neighbor.stats %>% 
  filter(language %in% test.languages)
neighbor.stats.adjusted %>% 
  filter(language %in% test.languages)
neighbor.mc %>% 
  filter(language %in% test.languages) %>%
  group_by(language, ontological.category) %>%
  summarize(random = mean(random) + sd(random))
neighbor.mc.adjusted %>% 
  filter(language %in% test.languages) %>%
  group_by(language, ontological.category) %>%
  summarize(random = mean(random) + sd(random))

# Check proportion of languages that have p < 0.01
neighbor.test %>%
  mutate(p = cut(p, breaks = c(0, 0.01, 1), include.lowest = TRUE, right = FALSE)) %>%
  group_by(p, ontological.category) %>%
  tally() %>%
  mutate(n = n / nrow(phon.languages))
neighbor.test.adjusted %>%
  mutate(p = cut(p, breaks = c(0, 0.01, 1), include.lowest = TRUE, right = FALSE)) %>%
  group_by(p, ontological.category) %>%
  tally() %>%
  mutate(n = n / nrow(phon.languages))

# Plot with bars for each category and each language.
# The height of each bar is the lower boundary of the 99% CI of the actual data
# minus 1 SD above the mean of the random permutations.

neighbor.mc.plot <- neighbor.mc %>%
  filter(ontological.category != "Other") %>%
  group_by(language, ontological.category) %>%
  summarize(random = mean(random) + sd(random))
neighbor.mc.plot.adjusted <- neighbor.mc.adjusted %>%
  filter(ontological.category != "Other") %>%
  group_by(language, ontological.category) %>%
  summarize(random = mean(random) + sd(random))

neighbor.plot <- neighbor.stats %>%
  group_by() %>%
  filter(ontological.category != "Other") %>%
  left_join(neighbor.mc.plot) %>%
  mutate(language = factor(language, levels = sorted.langs$language, labels = sorted.langs$language))
neighbor.plot.adjusted <- neighbor.stats.adjusted %>%
  group_by() %>%
  filter(ontological.category != "Other") %>%
  left_join(neighbor.mc.plot.adjusted) %>%
  mutate(language = factor(language, levels = sorted.langs.adjusted$language, labels = sorted.langs.adjusted$language))

neighbor.original <- neighbor.plot %>%
  mutate(height = Lower - random) %>% 
  ggplot(aes(x = language, y = height, ymin = Lower, ymax = Upper, fill = ontological.category)) +
  geom_bar(stat = "identity", width = 1,
           size = .25, color = palette_line) +
  scale_x_discrete(name = "", labels = function(x){ifelse(x %in% test.languages, str_wrap(as.character(x), 10), "")}) +
  facet_wrap(vars(ontological.category), ncol = 1) +
  scale_y_continuous(expand = c(0, 0), name = "Proportion of same neighbor - MC") +
  scale_fill_manual(values = c(palette_a_t[1], palette_a_t[2])) +
  cowplot::theme_cowplot() +
  cowplot::panel_border() +
  theme(legend.position = "none") + 
  expand_limits(x = -1) +
  expand_limits(x = 229) + 
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        strip.text = element_text(size = 8),
        axis.ticks.x = element_blank())

neighbor.original
ggsave("../Figures/neighbor_original_diff.pdf", width = 17, height = 7, units = "cm", dpi = 900)
ggsave("../Figures/neighbor_original_diff.png", width = 17, height = 7, units = "cm", dpi = 900)

neighbor.adjusted <- neighbor.plot.adjusted %>%
  mutate(height = Lower - random) %>% 
  ggplot(aes(x = language, y = height, ymin = Lower, ymax = Upper, fill = ontological.category)) +
  geom_bar(stat = "identity", width = 1,
           size = .25, color = palette_line) +
  scale_x_discrete(name = "", labels = function(x){ifelse(x %in% test.languages, str_wrap(as.character(x), 10), "")}) +
  facet_wrap(vars(ontological.category), ncol = 1) +
  scale_y_continuous(expand = c(0, 0), name = "Proportion of same neighbor - MC") +
  scale_fill_manual(values = c(palette_a_t[1], palette_a_t[2])) +
  cowplot::theme_cowplot() +
  cowplot::panel_border() +
  theme(legend.position = "none") + 
  expand_limits(x = -1) +
  expand_limits(x = 229) + 
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        strip.text = element_text(size = 8),
        axis.ticks.x = element_blank())
neighbor.adjusted

ggsave("../Figures/neighbor_adjusted_diff.pdf", width = 17, height = 7, units = "cm", dpi = 900)
ggsave("../Figures/neighbor_adjusted_diff.png", width = 17, height = 7, units = "cm", dpi = 900)

cowplot::plot_grid(neighbor.original, neighbor.adjusted, nrow = 1, labels = c("A", "B"))

ggsave("../Figures/neighbor_panel_diff.pdf", width = 17, height = 7, units = "cm", dpi = 900)
ggsave("../Figures/neighbor_panel_diff.png", width = 17, height = 7, units = "cm", dpi = 900)


# RNN K-Fold -------

# Load NN results

## Ad-hoc function to load results from the python script
rnn.means <- function(data, indices){
  these.data <- data[indices,] %>% 
    select(-X1, -language)
  return(colMeans(these.data))
}


rnn.performance <- list()
for(file in list.files('../Results/ten-fold-original//', recursive = T, full.names = T)){
  language <- str_extract(file, "(?<=Results/ten-fold-original//).+(?=_rnn_)")
  print(language)
  rnn.performance[[language]] <- read_csv(file)
  rnn.performance[[language]]$language <- language
}

rnn.performance.adjusted <- list()
for(file in list.files('../Results/ten-fold/', recursive = T, full.names = T)){
  language <- str_extract(file, "(?<=Results/ten-fold/).+(?=_rnn_)")
  print(language)
  rnn.performance.adjusted[[language]] <- read_csv(file)
  rnn.performance.adjusted[[language]]$language <- language
}

## This function takes the result of a specific language and bootstraps
## confidence intervals (R = 10000) of each of the performance measures.
# Mashco Piro and Waorani throw a small error because they have 100% thing accuracy all the time.
bootstrapped.cis <- function(rnn.language){
  language <- unique(rnn.language$language)
  print(language)
  mean.boots <- boot::boot(data = rnn.language, statistic = rnn.means, R = 10000)
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

# Because the bootstrapping takes long, the results are preallocated as an RDS and loaded further down.
# rnn.stats <- purrr::map_dfr(rnn.performance, bootstrapped.cis)
# rnn.stats.adjusted <- purrr::map_dfr(rnn.performance.adjusted, bootstrapped.cis) 
# rnn.stats %>%
#   write_rds("../Data/Processed/boot_ci_fold_original.rds")
# rnn.stats.adjusted %>%
#   write_rds("../Data/Processed/boot_ci_fold_adjusted.rds")

rnn.stats <- read_rds("../Data/Processed/boot_ci_fold_original.rds")
rnn.stats.adjusted <- read_rds("../Data/Processed/boot_ci_fold_adjusted.rds")

# Mark each result as whether the bootstrapped CI for the Matthews Correlation
# Coefficient include a baseline of 0.1 or not. 

## Descriptive stats
rnn.stats %>%
  select(Language, Matthews) %>% 
  summarize(Mean.Matthews = mean(Matthews), SD.Matthews = sd(Matthews))
rnn.stats.adjusted %>%
  select(Language, Matthews) %>% 
  summarize(Mean.Matthews = mean(Matthews), SD.Matthews = sd(Matthews))

## Count the number of languages whose bootstrapped 99% ci include 0.1
rnn.stats %>% 
  select(Language, contains("Matthews")) %>% 
  mutate(includes.baseline = ifelse(Matthews.Lower < 0.1, TRUE, FALSE)) %>% 
  group_by(includes.baseline) %>% 
  tally() %>% 
  mutate(percentage = (n / sum(n)) * 100)
rnn.stats.adjusted %>% 
  select(Language, contains("Matthews")) %>% 
  mutate(includes.baseline = ifelse(Matthews.Lower < 0.1, TRUE, FALSE)) %>% 
  group_by(includes.baseline) %>% 
  tally() %>% 
  mutate(percentage = (n / sum(n)) * 100)

## Check reference languages
rnn.stats %>% 
  select(Language, contains("Matthews")) %>% 
  mutate(includes.baseline = ifelse(Matthews.Lower < 0.1, TRUE, FALSE)) %>% 
  filter(Language %in% test.languages)
rnn.stats.adjusted %>% 
  select(Language, contains("Matthews")) %>% 
  mutate(includes.baseline = ifelse(Matthews.Lower < 0.1, TRUE, FALSE)) %>% 
  filter(Language %in% test.languages)

# Plot all languages, their CI and the baseline

matthews.plot <- rnn.stats %>% 
  arrange(desc(Matthews)) %>% 
  mutate(Language = factor(Language, levels = .$Language),
         include = ifelse(Language %in% test.languages, Language, NA),
         includes.baseline = ifelse(Matthews.Lower < 0.1, TRUE, FALSE))
rnn.original <- ggplot(data = matthews.plot, aes(x = Language, ymin = Matthews.Lower, ymax = Matthews.Upper,
             y = Matthews, fill = includes.baseline)) + 
  geom_vline(aes(xintercept = include), linetype = "dotted", size = .5, color = palette_line) +
  geom_linerange(size = 0.5, color = palette_line) +
  geom_point(shape = 22, size = .75, color = palette_line) + 
  geom_hline(yintercept = 0, size = 1) + 
  geom_hline(yintercept = 0.1, linetype = "dashed", size = 1, color = palette_line) +
  scale_x_discrete(name = "Language", labels = function(x){ifelse(x %in% test.languages,
                                                                  str_wrap(as.character(x), 10), "")}) +
  expand_limits(x = -1) +
  expand_limits(x = 229) + 
  cowplot::theme_cowplot()  + 
  scale_fill_manual(name = "Includes baseline", values = c(palette_other[3], palette_other[2])) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        strip.text = element_text(size = 8),
        axis.ticks.x = element_blank(),
        legend.position = "none")
rnn.original
ggsave("../Figures/rnn_original.pdf", width = 17, height = 7, units = "cm", dpi = 900)
ggsave("../Figures/rnn_original.png", width = 17, height = 7, units = "cm", dpi = 900)


matthews.plot.adjusted <- rnn.stats.adjusted %>% 
  arrange(desc(Matthews)) %>% 
  mutate(Language = factor(Language, levels = .$Language),
         includes.baseline = ifelse(Matthews.Lower < 0.1, "Includes 0.1", "Doesn't Include 0.1"),
         lang.label = ifelse(Language %in% test.languages, 
                             as.character(Language), ""),
         include = ifelse(lang.label == "", NA, Language))


rnn.adjusted <- ggplot(data = matthews.plot.adjusted, aes(x = Language, ymin = Matthews.Lower, ymax = Matthews.Upper,
                                                 y = Matthews, fill = includes.baseline)) + 
  geom_vline(aes(xintercept = include), linetype = "dotted", size = .5, color = palette_line) +
  geom_linerange(size = 0.5, color = palette_line) +
  geom_point(shape = 22, size = .75, color = palette_line) + 
  geom_hline(yintercept = 0, size = 1) + 
  geom_hline(yintercept = 0.1, linetype = "dashed", size = 1, color = palette_line) +
  scale_x_discrete(name = "Language", labels = function(x){ifelse(x %in% test.languages,
                                                                  str_wrap(as.character(x), 10), "")}) +
  expand_limits(x = -1) +
  expand_limits(x = 229) + 
  cowplot::theme_cowplot()  + 
  scale_fill_manual(name = "Includes baseline", values = c(palette_other[3], palette_other[2])) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        strip.text = element_text(size = 8),
        axis.ticks.x = element_blank(),
        legend.position = "none")
rnn.adjusted

ggsave("../Figures/rnn_adjusted.pdf", width = 17, height = 7, units = "cm", dpi = 900)
ggsave("../Figures/rnn_adjusted.png", width = 17, height = 7, units = "cm", dpi = 900)

cowplot::plot_grid(rnn.original, rnn.adjusted, labels = c("A", "B"), nrow = 2)
ggsave("../Figures/rnn_panel.pdf", width = 17, height = 8, units = "cm", dpi = 900)
ggsave("../Figures/rnn_panel.png", width = 17, height = 8, units = "cm", dpi = 900)


# RNN Spurt --------------------------------------------------------

# Load data
spurt.performance <- list()
for(file in list.files('../Results/Spurt//', recursive = T, full.names = T)){
  language <- str_extract(file, "(?<=Results/Spurt//).+(?=_spurt_)")
  print(language)
  spurt.performance[[language]] <- read_csv(file)
  spurt.performance[[language]]$language <- language
}
spurt.performance.adjusted <- list()
for(file in list.files('../Results/Spurt-Adjusted//', recursive = T, full.names = T)){
  language <- str_extract(file, "(?<=Results/Spurt-Adjusted//).+(?=_spurt_)")
  print(language)
  spurt.performance.adjusted[[language]] <- read_csv(file)
  spurt.performance.adjusted[[language]]$language <- language
}

# Get the same bootstrapped confidence intervals for these performance metrics.
# The data is also loaded from an RDS because of how long it takes.

# spurt.stats <- purrr::map_dfr(spurt.performance, bootstrapped.cis)
# spurt.stats.adjusted <- purrr::map_dfr(spurt.performance.adjusted, bootstrapped.cis)

# spurt.stats %>%
#   write_rds("../Data/Processed/boot_ci_spurt_original.rds")
# spurt.stats.adjusted %>%
#   write_rds("../Data/Processed/boot_ci_spurt_adjusted.rds")

spurt.stats <- read_rds("../Data/Processed/boot_ci_spurt_original.rds")
spurt.stats.adjusted <- read_rds("../Data/Processed/boot_ci_spurt_adjusted.rds")

## Descriptive stats
spurt.stats %>%
  select(Language, Matthews) %>% 
  summarize(Mean.Matthews = mean(Matthews), SD.Matthews = sd(Matthews))
spurt.stats.adjusted %>%
  select(Language, Matthews) %>% 
  summarize(Mean.Matthews = mean(Matthews), SD.Matthews = sd(Matthews))

## Count the number of languages whose bootstrapped 99% ci include 0.1
spurt.stats %>% 
  select(Language, contains("Matthews")) %>% 
  mutate(includes.baseline = ifelse(Matthews.Lower < 0.1, TRUE, FALSE)) %>% 
  group_by(includes.baseline) %>% 
  tally() %>% 
  mutate(percentage = (n / sum(n)) * 100)
spurt.stats.adjusted %>% 
  select(Language, contains("Matthews")) %>% 
  mutate(includes.baseline = ifelse(Matthews.Lower < 0.1, TRUE, FALSE)) %>% 
  group_by(includes.baseline) %>% 
  tally() %>% 
  mutate(percentage = (n / sum(n)) * 100)

## Check reference languages
spurt.stats %>% 
  select(Language, contains("Matthews")) %>% 
  mutate(includes.baseline = ifelse(Matthews.Lower < 0.1, TRUE, FALSE)) %>% 
  filter(Language %in% test.languages)
spurt.stats.adjusted %>% 
  select(Language, contains("Matthews")) %>% 
  mutate(includes.baseline = ifelse(Matthews.Lower < 0.1, TRUE, FALSE)) %>% 
  filter(Language %in% test.languages)

# Plot all languages, their CI and the baseline
spurt.plot <- spurt.stats %>% 
  arrange(desc(Matthews)) %>% 
  mutate(Language = factor(Language, levels = .$Language),
         include = ifelse(Language %in% test.languages, Language, NA),
         includes.baseline = ifelse(Matthews.Lower < 0.1, TRUE, FALSE))
spurt.original <- ggplot(data = spurt.plot, aes(x = Language, ymin = Matthews.Lower, ymax = Matthews.Upper,
                                                   y = Matthews, fill = includes.baseline)) + 
  geom_vline(aes(xintercept = include), linetype = "dotted", size = .5, color = palette_line) +
  geom_linerange(size = 0.5, color = palette_line) +
  geom_point(shape = 22, size = .75, color = palette_line) + 
  geom_hline(yintercept = 0, size = 1) + 
  geom_hline(yintercept = 0.1, linetype = "dashed", size = .5, color = palette_line) +
  scale_x_discrete(name = "Language", labels = function(x){ifelse(x %in% test.languages,
                                                                  str_wrap(as.character(x), 10), "")}) +
  expand_limits(x = -1) +
  expand_limits(x = 229) + 
  cowplot::theme_cowplot()  + 
  scale_fill_manual(name = "Includes baseline", values = c(palette_other[3], palette_other[2])) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        strip.text = element_text(size = 8),
        axis.ticks.x = element_blank(),
        legend.position = "none")
spurt.original
ggsave("../Figures/spurt_original.pdf", width = 17, height = 7, units = "cm", dpi = 900)
ggsave("../Figures/spurt_original.png", width = 17, height = 7, units = "cm", dpi = 900)


spurt.plot.adjusted <- spurt.stats.adjusted %>% 
  arrange(desc(Matthews)) %>% 
  mutate(Language = factor(Language, levels = .$Language),
         includes.baseline = ifelse(Matthews.Lower < 0.1, "Includes 0.1", "Doesn't Include 0.1"),
         lang.label = ifelse(Language %in% test.languages, 
                             as.character(Language), ""),
         include = ifelse(lang.label == "", NA, Language))


spurt.adjusted <- ggplot(data = spurt.plot.adjusted, aes(x = Language, ymin = Matthews.Lower, ymax = Matthews.Upper,
                                                   y = Matthews, fill = includes.baseline)) + 
  geom_vline(aes(xintercept = include), linetype = "dotted", size = .5, color = palette_line) +
  geom_linerange(size = 0.5, color = palette_line) +
  geom_point(shape = 22, size = .75, color = palette_line) + 
  geom_hline(yintercept = 0, size = 1) + 
  geom_hline(yintercept = 0.1, linetype = "dashed", size = .5, color = palette_line) +
  scale_x_discrete(name = "Language", labels = function(x){ifelse(x %in% test.languages,
                                                                  str_wrap(as.character(x), 10), "")}) +
  expand_limits(x = -1) +
  expand_limits(x = 229) + 
  cowplot::theme_cowplot()  + 
  scale_fill_manual(name = "Includes baseline", values = c(palette_other[3], palette_other[2])) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        strip.text = element_text(size = 8),
        axis.ticks.x = element_blank(),
        legend.position = "none")
spurt.adjusted
ggsave("../Figures/spurt_adjusted.pdf", width = 17, height = 7, units = "cm", dpi = 900)
ggsave("../Figures/spurt_adjusted.png", width = 17, height = 7, units = "cm", dpi = 900)


cowplot::plot_grid(spurt.original, spurt.adjusted, labels = c("A", "B"), nrow = 2)
ggsave("../Figures/spurt_panel.pdf", width = 17, height = 8, units = "cm", dpi = 900)
ggsave("../Figures/spurt_panel.png", width = 17, height = 8, units = "cm", dpi = 900)

