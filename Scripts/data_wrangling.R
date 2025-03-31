source('functions.r')
# Load multicore mc only if you plan to rerun the nearest-neighbor analyses.
# library(future)
# library(furrr)

options(dplyr.summarise.inform = FALSE)

# Load data -----------------------------------------------------

# all_phon_adjusted has the morphology-adjusted wordlists.
# Phon_languages contains info on each language.

all.phon.adjusted <- read_csv("../data/Processed/all_phon_adjusted.csv", col_types = cols())
all.phon.adjusted <- filter(all.phon.adjusted, language != 'Puinave')
phon.languages <- read_csv('../data/Processed/all_language_info.csv', col_types = cols()) %>% 
  filter(Name %in% unique(all.phon.adjusted$language))


wals <- read_csv('../data/Processed/WALS_Codes.csv', col_types = cols()) %>% 
  select(Name, wals_code, ID) %>% 
  mutate(ID = as.factor(ID))
wals_info <- read_csv("../data/Raw/WALS/walslanguage.csv", col_types = cols())


# Wrangling Data ------------------------------------------------------------------

# Get normalized mean class distances for each language and bind it in one tibble for both datasets
# Takes a bit, so pre-saved and loaded by default.

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
# write_rds(all.distances.adjusted, "../data/Processed/r_objects/all_distances_adjusted.RDS", compress = 'xz')

# Load distances objects instead of rerunning everytime. Uncomment previous lines to rerun.
all.distances.adjusted <- read_rds("../data/Processed/r_objects/all_distances_adjusted.rds")


# Sorted scatter of typicality per class ----

data.for.scatter.adjusted <- all.distances.adjusted %>%
  group_by(language, class) %>%
  dplyr::summarize(Median = median(typicality))

# Sort by the absolute value of the difference in typicality between Action and Thing
sorted.langs.adjusted <- data.for.scatter.adjusted %>%
  spread(class, Median) %>%
  mutate(difference = abs(Action - Thing)) %>%
  arrange(desc(abs(difference)))

# Save reference languages: Maximum, median and minimum difference in typicality and English

english.difference <- "English"
max.difference.adjusted <-
  sorted.langs.adjusted$language[which.max(sorted.langs.adjusted$difference)]
mid.difference.adjusted <-
  sorted.langs.adjusted$language[sorted.langs.adjusted$difference == quantile(sorted.langs.adjusted$difference, .5, type = 1)]
min.difference.adjusted <-
  sorted.langs.adjusted$language[which.min(sorted.langs.adjusted$difference)]
test.languages <- c(max.difference.adjusted, mid.difference.adjusted, min.difference.adjusted, "English")

# Filter out "Other" and add field that determines if language is reference or not.
plot.data.adjusted <- data.for.scatter.adjusted %>%
  filter(class != "Other") %>%
  group_by() %>%
  mutate(language = factor(language, levels = sorted.langs.adjusted$language), class = factor(class),
         include = ifelse(language %in% test.languages, language, NA)) %>% 
  rename(Category = class)

# Keep labels only for reference languages
label.text.adjusted <- map_chr(sorted.langs.adjusted$language, function(x){
  if(x %in% test.languages){
    return(x)
  } else{return("")}
})

# Manual fix for labels in plot. Uncomment as needed.
label.text.adjusted[which(label.text.adjusted == "Hlai (Baoting variety)")] <- "Hlai\n(Baoting variety)"
label.text.adjusted[which(label.text.adjusted == "Khwarshi (Inkhokvari dialect)")] <- "Khwarshi \n(Inkhokvari dialect)"
label.text.adjusted[which(label.text.adjusted == "Breton")] <- "\nBreton"

# Closest phonological neighbors ---------------

# Generate a count for each category and each language of the number of words
# where the closest neighbor in the Levenshtein phonological space (as
# represented in their distance matrix generated with get.distance.matrix) is a
# word of the same class. Because ties are broken at random, run it 10 times and take the mean.

# Takes a bit, so pre-saved and loaded by default.

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
#     }) %>%
#     return()
#   })
# repeated.neighbor.adjusted %>% write_rds("../data/Processed/r_objects/neighbor_adjusted.Rds")

repeated.neighbor.adjusted <- read_rds("../data/Processed/r_objects/neighbor_adjusted.Rds")

# For hypothesis testing, generate a permutation-based null distribution. This
# involves running, for each language in both original and adjusted datasets,
# The same algorithm as before after shuffling the category labels of the words.
# Again, because of the time it takes to run this, the results are saved and
# reloaded later in the script. Also, this was run in a parallel multi-core
# scheme using the package FURRR.


#  Change this to a reasonable number considering your machine. Number of cores - 2 seems reasonable.
# cores <- 8
# options(future.globals.maxSize = +Inf, mc.cores = cores, future.seed = TRUE)
# furrr_options(seed = 123)
# plan(multisession, workers = cores)
# neighbor.mc.adjusted <- future_map2_dfr(.progress = TRUE,
#                                                .x = all.phon.list.adjusted,
#                                                .y = all.distance.matrices.adjusted,
#                                                .f = function(language, distance.matrix){
#                                         all.neighbors <- map_dfr(1:1000, function(x){
#                                           options(dplyr.summarise.inform = FALSE)
#                                           all.neighbors <- get.nearest.neighbors(a.language = language,
#                                                                                  distance.matrix = distance.matrix,
#                                                                                  randomize = TRUE) %>%
#                                             mutate(same.neighbor = (ontological.category == neighbor.category)) %>%
#                                             group_by(language, ontological.category) %>%
#                                             summarise(proportion.of.hits = sum(same.neighbor) / n()) %>%
#                                             mutate(permutation = x)
#                                           return(all.neighbors)
#                                         })
#                                         return(all.neighbors)
#                                       })
# 
# 
# random.neigh.stats <- group_by(neighbor.mc.adjusted,
#                                language,
#                                ontological.category) %>%
#   select(-permutation) %>%
#   group_modify(~ enframe(smean.cl.boot(., conf.int = .99, B = 10000, na.rm = TRUE))) %>% 
#   pivot_wider(names_from = name, values_from = value)
# 
# neighbor.mc.adjusted %>%
#   write_rds("../data/Processed/r_objects/neighbor_mc_adjusted.Rds")
# random.neigh.stats %>% 
#   write_rds('../data/Processed/r_objects/random_neigh_stats.Rds')

neighbor.mc.adjusted <- read_rds("../data/Processed/r_objects/neighbor_mc_adjusted.Rds")
random.neigh.stats <- read_rds('../data/Processed/r_objects/random_neigh_stats.Rds')

# RNN K-Fold -------

# Load NN results

rnn.means <- function(data, indices){
  these.data <- data[indices,] %>% 
    select(-X1, -language)
  return(colMeans(these.data))
}


rnn.performance <- list()
for(file in list.files('../data/Processed/RNN/ten-fold//', recursive = T, full.names = T)){
  language <- str_extract(file, "(?<=/ten-fold///).+(?=_rnn_)")
  print(language)
  
  rnn.performance[[language]] <- read_csv(file, col_types = cols(), 
                                          col_names = c("Index",
                                                        "Matthews","AUC",
                                                        "Accuracy", "F1",
                                                        "ActionAccuracy", 
                                                        "ThingAccuracy",
                                                        "language"),
                                          col_select = -1, skip = 1)
}


rnn.stats <- map_dfr(rnn.performance, function(x){
  this.auc <- x$AUC
  this.mcc <- x$Matthews
  mean.auc <- mean(this.auc)
  mean.mcc <- mean(this.mcc)
  se.auc <- sqrt(var(this.auc) / length(this.auc))
  se.mcc <- sqrt(var(this.mcc) / length(this.mcc))
  top.ci.auc <- mean.auc + se.auc * 2.576
  bot.ci.auc <- mean.auc - se.auc * 2.576
  top.ci.mcc <- mean.mcc + se.mcc * 2.576
  bot.ci.mcc <- mean.mcc - se.mcc * 2.576
  language <- unique(x$language)
  top.quant <- quantile(this.auc, 0.75)
  bot.quant <- quantile(this.auc, 0.25)
  iqr.auc <- IQR(this.auc)
  median.auc <- median(this.auc)
  language <- unique(x$language)
  results <- tibble(mean.auc = mean.auc, se.auc = se.auc, mean.mcc = mean.mcc, 
                    se.mcc = se.mcc, language = language,
                    median.auc = median.auc,
                    top.quant = top.quant, bot.quant = bot.quant, iqr.auc = iqr.auc,
                    top.ci = top.ci.auc, bot.ci = bot.ci.auc, top.ci.mcc = top.ci.mcc, bot.ci.mcc = bot.ci.mcc)
})


# RNN Spurt --------------------------------------------------------

# Load data
spurt.performance <- list()
for(file in list.files('../data/Processed/RNN/Spurt//', recursive = T, full.names = T)){
  language <- str_extract(file, "(?<=/Spurt///).+(?=_spurt_)")
  spurt.performance[[language]] <- read_csv(file, col_types = cols(), 
                                            col_names = c("Index",
                                                          "Matthews","AUC",
                                                          "Accuracy", "F1",
                                                          "ActionAccuracy", 
                                                          "ThingAccuracy",
                                                          'language'),
                                            col_select = -1, skip = 1)
  spurt.performance[[language]]$language <- language
}
spurt.stats <- map_dfr(spurt.performance, function(x){
  this.auc <- x$AUC
  this.mcc <- x$Matthews
  mean.auc <- mean(this.auc)
  mean.mcc <- mean(this.mcc)
  se.auc <- sqrt(var(this.auc) / length(this.auc))
  se.mcc <- sqrt(var(this.mcc) / length(this.mcc))
  top.ci.auc <- mean.auc + se.auc * 2.576
  bot.ci.auc <- mean.auc - se.auc * 2.576
  top.ci.mcc <- mean.mcc + se.mcc * 2.576
  bot.ci.mcc <- mean.mcc - se.mcc * 2.576
  language <- unique(x$language)
  top.quant <- quantile(this.auc, 0.75)
  bot.quant <- quantile(this.auc, 0.25)
  iqr.auc <- IQR(this.auc)
  median.auc <- median(this.auc)
  language <- unique(x$language)
  results <- tibble(mean.auc = mean.auc, se.auc = se.auc, mean.mcc = mean.mcc, 
                    se.mcc = se.mcc, language = language,
                    median.auc = median.auc,
                    top.quant = top.quant, bot.quant = bot.quant, iqr.auc = iqr.auc,
                    top.ci = top.ci.auc, bot.ci = bot.ci.auc, top.ci.mcc = top.ci.mcc,
                    bot.ci.mcc = bot.ci.mcc)
})

# Load baselines

baseline_kfold <- read_csv('../data/Processed/RNN/baseline_kfold.csv', 
                           col_types = cols(), 
                           col_names = c("Index",
                                         "Matthews","AUC",
                                         "Accuracy", "F1",
                                         "ActionAccuracy", 
                                         "ThingAccuracy",
                                         'language',
                                         'iteration'),
                           col_select = -1,
                           skip = 1)

baseline_kfold_auc <- baseline_kfold %>% 
  group_by(language) %>% 
  dplyr::reframe(quants = quantile(AUC, probs = c(0.05, 0.95))) %>% 
  add_column(bound = rep(c('Base_Lower', 'Base_Upper'), 200)) %>% 
  pivot_wider(names_from = bound, values_from = quants)

rnn.stats <- left_join(rnn.stats, baseline_kfold_auc)

baseline_spurt <- read_csv('../data/Processed/RNN/baseline_spurt.csv', 
                           col_types = cols(), 
                           col_names = c("Index",
                                         "Matthews","AUC",
                                         "Accuracy", "F1",
                                         "ActionAccuracy", 
                                         "ThingAccuracy",
                                         'language',
                                         'iteration'),
                           col_select = -1,
                           skip = 1)

baseline_spurt_auc <- baseline_spurt %>% 
  group_by(language) %>% 
  dplyr::reframe(quants = quantile(AUC, probs = c(0.05, 0.95))) %>% 
  add_column(bound = rep(c('Base_Lower', 'Base_Upper'), 200)) %>% 
  pivot_wider(names_from = bound, values_from = quants)

spurt.stats <- left_join(spurt.stats, baseline_spurt_auc) #%>% 
