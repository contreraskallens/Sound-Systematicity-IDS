source('functions.r')
options(dplyr.summarise.inform = FALSE)

# Load data -----------------------------------------------------

# all_phon contains info on individual words.
# all_phon_adjusted has the morphology-adjusted wordlists.
# Phon_languages contains info on each language.

# all.phon <- read_csv('../Data/Processed/all_phon.csv', col_types = cols())
all.phon.adjusted <- read_csv("../Data/Processed/all_phon_adjusted.csv", col_types = cols())
phon.languages <- read_csv('../Data/Processed/all_language_info.csv', col_types = cols())
# language.groups <- read_csv("../Data/Processed/language_groups.csv") %>% 
  # select(language = Name, geo.cluster, family) %>% 
  # mutate(geo.cluster = factor(geo.cluster))

wals <- read_csv('../Data/Processed/WALS_Codes.csv', col_types = cols()) %>% 
  select(Name, wals_code, ID) %>% 
  mutate(ID = as.factor(ID))
wals_info <- read_csv("../Data/Raw/WALS/walslanguage.csv", col_types = cols())


# Wrangling Data ------------------------------------------------------------------

# Get normalized mean class distances for each language and bind it in one tibble for both datasets
# all.phon.adjusted <- filter(all.phon.adjusted, language != 'Puinave')
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
# 
# 
# # Save distance objects
# write_rds(all.distances.adjusted, "../Data/r_objects/all_distances_adjusted.RDS", compress = 'xz')

# Load distances objects instead of rerunning everytime. Uncomment previous lines to rerun.

all.distances.adjusted <- read_rds("../Data/r_objects/all_distances_adjusted.RDS")


# Sorted scatter of typicality per class ----

data.for.scatter.adjusted <- all.distances.adjusted %>%
  group_by(language, class) %>%
  dplyr::summarize(Median = median(typicality))

# Sort by the absolute value of the difference in typicality between Action and Thing
sorted.langs.adjusted <- data.for.scatter.adjusted %>% # this one includes the difference in means
  spread(class, Median) %>%
  mutate(difference = abs(Action - Thing)) %>%
  arrange(desc(abs(difference)))

# Save reference languages: Maximum, median and minimum difference in typicality and English
# Note that min difference is the same in both word lists.

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

# # Manual fix for labels in plot
# label.text[which(label.text == "Waorani")] <- "      Waorani"
# label.text[which(label.text == "Thai (Korat variety)")] <- "Thai\n(Korat)"
label.text.adjusted[which(label.text.adjusted == "Hlai (Baoting variety)")] <- "Hlai\n(Baoting variety)"
label.text.adjusted[which(label.text.adjusted == "Khwarshi (Inkhokvari dialect)")] <- "Khwarshi \n(Inkhokvari dialect)"
label.text.adjusted[which(label.text.adjusted == "Breton")] <- "\nBreton"

# Closest phonological neighbors ---------------

# Generate a count for each category and each language of the number of words
# where the closest neighbor in the Levenshtein phonological space (as
# represented in their distance matrix generated with get.distance.matrix) is a
# word of the same class. Because ties are broken at random, run it 10 times and take the mean.

# This process takes long, so results have been preallocated. If you wish to rerun them, uncomment the following lines.

all.phon.list.adjusted <- all.phon.adjusted %>%
  split(.$language)
all.phon.list.adjusted <- all.phon.list.adjusted[sort(names(all.phon.list.adjusted))]
all.distance.matrices.adjusted <- purrr::map(all.phon.list.adjusted, function(x){get.distance.matrix(x)})
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
# 
# repeated.neighbor.adjusted %>% write_rds("../Data/r_objects/neighbor_adjusted.Rds")

# Load data generated from previous lines
repeated.neighbor.adjusted <- read_rds("../Data/r_objects/neighbor_adjusted.Rds")

# For hypothesis testing, generate a permutation-based null distribution. This
# involves running, for each language in both original and adjusted datasets,
# The same algorithm as before after shuffling the category labels of the words.
# Again, because of the time it takes to run this, the results are saved and
# reloaded later in the script. Also, this was run in a parallel multi-core
# scheme using the package FURRR.

# Load multicore mc
library(future)
library(furrr)
#
# Change this to a reasonable number considering your machine. Number of cores - 2 seems reasonable.
cores <- 6
options(future.globals.maxSize = +Inf, mc.cores = cores, future.seed = TRUE)
furrr_options(seed = 123)
plan(multisession, workers = cores)
neighbor.mc.adjusted <- future_map2_dfr(.progress = TRUE,
                                               .x = all.phon.list.adjusted,
                                               .y = all.distance.matrices.adjusted,
                                               .f = function(language, distance.matrix){
                                        all.neighbors <- map_dfr(1:1000, function(x){
                                          options(dplyr.summarise.inform = FALSE)
                                          all.neighbors <- get.nearest.neighbors(a.language = language,
                                                                                 distance.matrix = distance.matrix,
                                                                                 randomize = TRUE) %>%
                                            mutate(same.neighbor = (ontological.category == neighbor.category)) %>%
                                            group_by(language, ontological.category) %>%
                                            summarise(proportion.of.hits = sum(same.neighbor) / n()) %>%
                                            mutate(permutation = x)
                                          return(all.neighbors)
                                        })

                                        random.neigh.stats <- group_by(all.neighbors,
                                                                       language,
                                                                       ontological.category) %>%
                                          select(-permutation) %>%
                                          group_map(~ smean.cl.boot(., conf.int = .99, B = 10000, na.rm = TRUE))  %>%
                                          bind_rows %>%
                                          add_column(ontological.category = c('Action', 'Thing'),
                                                     language = unique(language$language))
                                        return(random.neigh.stats)
                                      })

neighbor.mc.adjusted %>%
  write_rds("../Data/r_objects/neighbor_mc_adjusted.Rds")
# 
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
for(file in list.files('../Results/ten-fold//', recursive = T, full.names = T)){
  language <- str_extract(file, "(?<=Results/ten-fold//).+(?=_rnn_)")
  print(language)
  rnn.performance[[language]] <- read_csv(file, col_types = cols(), 
                                          col_names = c("Index",
                                                        "Matthews","AUC",
                                                        "Accuracy", "F1",
                                                        "ActionAccuracy", 
                                                        "ThingAccuracy" ),
                                          col_select = -1, skip = 1)
  rnn.performance[[language]]$language <- language
}


rnn.stats <- map_dfr(rnn.performance, function(x){
  this.auc <- x$AUC
  this.mcc <- x$Matthews
  mean.auc <- mean(this.auc)
  mean.mcc <- mean(this.mcc)
  se.auc <- sqrt(var(this.auc) / length(this.auc))
  se.mcc <- sqrt(var(this.mcc) / length(this.mcc))
  language <- unique(x$language)
  results <- tibble(mean.auc = mean.auc, se.auc = se.auc, mean.mcc = mean.mcc, se.mcc = se.mcc, language = language)
})


# RNN Spurt --------------------------------------------------------

# Load data
spurt.performance <- list()
for(file in list.files('../Results/Spurt//', recursive = T, full.names = T)){
  language <- str_extract(file, "(?<=Results/Spurt//).+(?=_spurt_)")
  spurt.performance[[language]] <- read_csv(file, col_types = cols(), 
                                            col_names = c("Index",
                                                          "Matthews","AUC",
                                                          "Accuracy", "F1",
                                                          "ActionAccuracy", 
                                                          "ThingAccuracy" ),
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
  language <- unique(x$language)
  results <- tibble(mean.auc = mean.auc, se.auc = se.auc, mean.mcc = mean.mcc, se.mcc = se.mcc, language = language)
})
