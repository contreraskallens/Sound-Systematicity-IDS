source('functions.R')
options(dplyr.summarise.inform = FALSE)

# Load data -----------------------------------------------------

# all_phon contains info on individual words.
# all_phon_adjusted has the morphology-adjusted wordlists.
# Phon_languages contains info on each language.
# no_marker_group has a list of the languages that weren't adjusted.

all.phon <- read_csv('../Data/Processed/all_phon.csv')
all.phon.adjusted <- read_csv("../Data/Processed/all_phon_adjusted.csv")
phon.languages <- read_csv('../Data/Processed/all_language_info.csv')
no.marker.group <- read_rds("../Data/r_objects/no_marker_group.rds")
language.groups <- read_csv("../Data/Processed/language_groups.csv") %>% 
  select(language = Name, geo.cluster, family) %>% 
  mutate(geo.cluster = factor(geo.cluster))

wals <- read_csv('../Data/Processed/WALS_Codes.csv') %>% 
  select(Name, wals_code = `WALS code`, ID) %>% 
  mutate(ID = as.factor(ID))
wals_info <- read_csv("../Data/Raw/WALS/walslanguage.csv")


# Wrangling Data ------------------------------------------------------------------

# Get normalized mean class distances for each language and bind it in one tibble for both datasets
# Uncomment to rerun

# all.distances <- map_dfr(.x = unique(all.phon$language),
#                          .f = function(language.name) {
#                            language <- get.language(a.language = language.name)
#                            distances <- get.distance.matrix(language)
#                            mean.distances <- get.mean.distances(language.df = language, 
#                                                                 distance.matrix = distances) %>%
#                              mutate(language = language.name)
#                            return(mean.distances)
#                          }
# )
# 

# all.distances.adjusted <- map_dfr(.x = unique(all.phon.adjusted$language),
#                                   .f = function(language.name) {
#                                     print(language.name)
#                                     language <- get.language(a.language = language.name,
#                                                              data = all.phon.adjusted)
#                                     distances <- get.distance.matrix(language)
#                                     mean.distances <- get.mean.distances(language.df = language,
#                                                                          distance.matrix = distances) %>%
#                                       mutate(language = language.name)
#                                     return(mean.distances)
#                                   }
# )


# Save distance objects
# write_rds(all.distances, "../Data/r_objects/all_distances.RDS")
# write_rds(all.distances.adjusted, "../Data/r_objects/all_distances_adjusted.RDS")

# Load distances objects instead of rerunning everytime. Uncomment previous lines to rerun.

all.distances <- read_rds("../Data/r_objects/all_distances.RDS")
all.distances.adjusted <- read_rds("../Data/r_objects/all_distances_adjusted.RDS")


# Sorted scatter of typicality per class ----

data.for.scatter <- all.distances %>%
  filter(class != "Other") %>%
  group_by(language, class) %>%
  dplyr::summarize(Median = median(typicality))
data.for.scatter.adjusted <- all.distances.adjusted %>%
  filter(class != "Other") %>%
  group_by(language, class) %>%
  dplyr::summarize(Median = median(typicality))
all.scatters <- rename(data.for.scatter, original = Median) %>% 
  left_join(rename(data.for.scatter.adjusted, adjusted = Median))

# Sort by the absolute value of the difference in typicality between Action and Thing
sorted.langs <- data.for.scatter %>% 
  spread(class, Median) %>%
  mutate(difference = abs(Action - Thing)) %>%
  arrange(desc(abs(difference)))
sorted.langs.adjusted <- data.for.scatter.adjusted %>% # this one includes the difference in means
  spread(class, Median) %>%
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

test.languages <- c(max.difference.original, min.difference, mid.difference.original, max.difference.adjusted,
                    mid.difference.adjusted, "English")

# Filter out "Other" and add field that determines if language is reference or not.
plot.data <- data.for.scatter %>%
  filter(class != "Other") %>%
  group_by() %>%
  mutate(language = factor(language, levels = sorted.langs$language), class = factor(class),
         include = ifelse(language %in% test.languages, language, NA)) %>% 
  rename(Category = class)
plot.data.adjusted <- data.for.scatter.adjusted %>%
  filter(class != "Other") %>%
  group_by() %>%
  mutate(language = factor(language, levels = sorted.langs.adjusted$language), class = factor(class),
         include = ifelse(language %in% test.languages, language, NA)) %>% 
  rename(Category = class)

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
label.text[which(label.text == "Waorani")] <- "      Waorani"
label.text[which(label.text == "Thai (Korat variety)")] <- "Thai\n(Korat)"
label.text.adjusted[which(label.text.adjusted == "Thai (Korat variety)")] <- "Thai\n(Korat)"


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
#
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
#   purrr::map_dfr(1:10, function(z){
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

# repeated.neighbor %>% write_rds("../Data/r_objects/neighbor_original.Rds")
# repeated.neighbor.adjusted %>% write_rds("../Data/r_objects/neighbor_adjusted.Rds")
# 
# Load data generated from previous lines
repeated.neighbor <- read_rds("../Data/r_objects/neighbor_original.Rds") %>% 
  filter(language != "Chechen (Akkin dialect)")
repeated.neighbor.adjusted <- read_rds("../Data/r_objects/neighbor_adjusted_mi.Rds")

# For hypothesis testing, generate a permutation-based null distribution. This
# involves running, for each language in both original and adjusted datasets,
# The same algorithm as before after shuffling the category labels of the words.
# Again, because of the time it takes to run this, the results are saved and
# reloaded later in the script. Also, this was run in a parallel multi-core
# scheme using the package FURRR.

# Load multicore mc
# library(future)
# library(furrr)
# future::plan(multisession)

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
#                                     Standard.Dev = sd(proportion.of.hits),
#                                     Upper = (Mean + sd(proportion.of.hits)),
#                                     Lower = (Mean - sd(proportion.of.hits))) %>%
#                             return()
# })

# neighbor.mc.adjusted <- furrr::future_map2_dfr(.progress = TRUE,
#                                                .x = all.phon.list.adjusted,
#                                                .y = all.distance.matrices.adjusted,
#                                                .options = furrr_options(seed = 123),
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
#                                           summarise(Mean = mean(proportion.of.hits),
#                                                    Standard.Dev = sd(proportion.of.hits),
#                                                    Upper = (Mean + sd(proportion.of.hits)),
#                                                     Lower = (Mean - sd(proportion.of.hits))) %>%
#                                           return()
#                                       })
# neighbor.mc %>%
# write_rds("../Data/r_objects/neighbor_mc_original.Rds")
# neighbor.mc.adjusted %>%
#   write_rds("../Data/r_objects/neighbor_mc_adjusted.Rds")

neighbor.mc <- read_rds("../Data/r_objects/neighbor_mc_original.Rds") %>%
  filter(language != "Chechen (Akkin dialect)")
neighbor.mc.adjusted <- read_rds("../Data/r_objects/neighbor_mc_adjusted.Rds")


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


rnn.stats <- map_dfr(rnn.performance.adjusted, function(x){
  median.auc <- median(x$AUC)
  iqr.auc <- IQR(x$AUC)
  median.mcc <- median(x$Matthews)
  iqr.mcc <- IQR(x$Matthews)
  language <- unique(x$language)
  results <- tibble(median.auc = median.auc, iqr.auc = iqr.auc, median.mcc = median.mcc, iqr.mcc = iqr.mcc, language = language)
})

auc.wilcox <- map_dfr(rnn.performance.adjusted, function(x){
  stat.test <- wilcox.test(x$AUC, mu = 0.5, alternative = "greater", conf.int = TRUE, conf.level = 0.95, exact = FALSE,
              digits.rank = 7)
  tibble(p.value = stat.test$p.value, conf = stat.test$conf.int[1], statistic = stat.test$statistic, language = unique(x$language),
         measure = "AUC")
})
mcc.wilcox <- map_dfr(rnn.performance.adjusted, function(x){
  stat.test <- wilcox.test(x$Matthews, mu = 0.2, alternative = "greater", conf.int = TRUE, conf.level = 0.95, exact = FALSE,
              digits.rank = 7)
  tibble(p.value = stat.test$p.value, conf = stat.test$conf.int[1], statistic = stat.test$statistic, language = unique(x$language),
         measure = "Matthews")
})

rnn.stats.adjusted <- bind_rows(auc.wilcox, mcc.wilcox) %>% 
  left_join(rnn.stats)

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
for(file in list.files('../Results/Spurt//', recursive = T, full.names = T)){
  language <- str_extract(file, "(?<=Results/Spurt//).+(?=_spurt_)")
  print(language)
  spurt.performance.adjusted[[language]] <- read_csv(file)
  spurt.performance.adjusted[[language]]$language <- language
}

spurt.stats <- map_dfr(spurt.performance.adjusted, function(x){
  median.auc <- median(x$AUC)
  iqr.auc <- IQR(x$AUC)
  median.mcc <- median(x$Matthews)
  iqr.mcc <- IQR(x$Matthews)
  language <- unique(x$language)
  results <- tibble(median.auc = median.auc, iqr.auc = iqr.auc, median.mcc = median.mcc, iqr.mcc = iqr.mcc, language = language)
})

spurt.auc.wilcox <- map_dfr(spurt.performance.adjusted, function(x){
  stat.test <- wilcox.test(x$AUC, mu = 0.5, alternative = "greater", conf.int = TRUE, conf.level = 0.95, exact = FALSE,
                           digits.rank = 7)
  tibble(p.value = stat.test$p.value, conf = stat.test$conf.int[1], statistic = stat.test$statistic, language = unique(x$language),
         measure = "AUC")
})
spurt.mcc.wilcox <- map_dfr(spurt.performance.adjusted, function(x){
  stat.test <- wilcox.test(x$Matthews, mu = 0.1, alternative = "greater", conf.int = TRUE, conf.level = 0.95, exact = FALSE,
                           digits.rank = 7)
  tibble(p.value = stat.test$p.value, conf = stat.test$conf.int[1], statistic = stat.test$statistic, language = unique(x$language),
         measure = "Matthews")
})

spurt.stats.adjusted <- bind_rows(spurt.auc.wilcox, spurt.mcc.wilcox) %>% 
  left_join(spurt.stats)
