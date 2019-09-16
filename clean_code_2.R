# Packages ----------------------------------------------------------------

require(tidyverse)
require(stringdist)
require(caret)
require(wesanderson)

color.palette.dis <- c(wes_palette(name = 'Royal2'), wes_palette(name = 'Royal1'))
color.palette.cont <- wes_palette(name = 'Zissou1', type = 'continuous', n = 5)


# Load data -----------------------------------------------------

all.phon <- read_csv('Data/Processed/all_phon.csv')
phon.languages <- read_csv('Data/Processed/all_language_info.csv')

# allPhon <- allPhonControl

# Functions ---------------------------------------------------------------

get.language <- function(a.language, data = all.phon){
  # Extracts the dataframe of a specific language and subsets variables
  # Args:
  #   aLanguage: String with a language name, e.g. "English"
  #   data: complete data with all languages. By default, it looks for an object called "all.phon"
  # Returns: Tibble with only rows corresponding to specified
  # language and columns for Phon form, Meaning and Class.
  data %>%
    dplyr::select(phon, english.name, language, ontological.category) %>%
    filter(language == a.language) %>%
    dplyr::select(-language) %>%
    return()
}

get.distance.matrix <- function(language.df){
  # Constructs square distance matrix for the words in a specific language using
  # OSA distance. After that, it normalizes the distances by dividing them by the
  # length of the longest word. Designed to be used on products of getLanguage().
  # Args:
  #   languageDF: a dataframe or tibble with a column called "phon" that stores phonetic forms.
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
  # Gets the mean distance of each word to the different syntactic classes in
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
  mean.distance.matrix <- data_frame(word = language.df$phon) %>%
    mutate(mean.action = rowMeans(distance.matrix[, action.mask], na.rm = TRUE),
           mean.thing = rowMeans(distance.matrix[, thing.mask], na.rm = TRUE),
           typicality = mean.action - mean.thing,
           class = language.df$ontological.category,
           englishName = language.df$english.name)
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


# extremes of the word --------

all.phon.adjusted <- all.phon
marker.group <- c()


for(i in 1:6){
  markers <- purrr::map_dfr(sort(unique(all.phon.adjusted$language)), function(lang){
    lang.df <- all.phon.adjusted %>% 
      filter(language == lang)
    map_dfr(c("Action", "Thing", "Other"), function(category){
      cat.df <- lang.df %>% 
        filter(ontological.category == category)
      number <- nrow(cat.df)
      proportions <- cat.df %>% 
        mutate(ending = str_sub(phon, "-1")) %>% 
        .$ending %>% 
        table()
      proportions <- proportions / number
      has.marker <- proportions > 0.33
      if(TRUE %in% has.marker){
        return(tibble(ontological.category = category,
                                             marker = TRUE))} else{
                                               return(tibble(ontological.category = category,
                                                             marker = FALSE))
                                             }
      
    }) %>% 
      add_column(language = lang)
  })
  langs.with.markers <- markers %>% 
    filter(marker == TRUE) %>% 
    .$language
  marker.group <- c(marker.group, langs.with.markers)
  all.phon.adjusted <- map_dfr(1:nrow(markers), function(index){
    lang <- markers$language[index]
    category <- markers$ontological.category[index]
    has.marker <- markers$marker[index]
    if(has.marker){
      all.phon.adjusted %>% 
        filter(lang == language, ontological.category == category) %>% 
        mutate(phon = str_sub(phon, "1", "-2")) %>% 
        return()
    }else{
      all.phon.adjusted %>% 
        filter(lang == language, ontological.category == category) %>% 
        return()
    }
  })
  all.phon.adjusted <- all.phon.adjusted %>% 
    filter(nchar(phon) > 1)
}

marker.group <- unique(marker.group)
no.marker.group <- setdiff(unique(all.phon.adjusted$language), marker.group)

all.distances.adjusted <- map_dfr(.x = unique(all.phon.adjusted$language),
                         .f = function(language.name) {
                           print(language.name)
                           language <- get.language(a.language = language.name, all.phon.adjusted)
                           distances <- get.distance.matrix(language)
                           mean.distances <-
                             get.mean.distances(language.df = language, distance.matrix = distances) %>%
                             mutate(language = language.name)
                           return(mean.distances)
                         }
)

all.distances.adjusted %>%
  mutate(class = factor(class, levels = c("Action", "Thing", "Other"))) %>%
  ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
  stat_density2d(geom = 'polygon') +
  scale_fill_gradientn(colors = color.palette.cont, name = "Density") +
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = wes_palettes$Moonrise1[4], size = 1) +
  labs(x = expression(hat(ND[A])), y = expression(hat(ND[T]))) +
  cowplot::theme_cowplot() +
  facet_wrap(vars(class), ncol = 1) +
  cowplot::panel_border()



# Wrangling Data ------------------------------------------------------------------

# Get normalized mean class distances for each language and bind it in one tibble

all.distances <- map_dfr(.x = unique(all.phon$language),
                                      .f = function(language.name) {
                                        language <- get.language(a.language = language.name)
                                        distances <- get.distance.matrix(language)
                                        mean.distances <-
                                          get.mean.distances(language.df = language, distance.matrix = distances) %>%
                                          mutate(language = language.name)
                                        return(mean.distances)
                                        }
                                      )

# Get tipicality stats for each class in each language

all.distances %>%
  group_by(class) %>%
  summarize(median.typicality = median(typicality))


language.typicality.category <- all.distances %>%
  group_by(language, class) %>%
  summarise(mean.typicality = mean(typicality), sd.typicality = sd(typicality))

# Kruskal-wallis
all.distances %>%
  mutate(class = factor(class)) %>%
  coin::kruskal_test(data = ., typicality ~ class)

# pairwise wilcox action / thing

all.distances %>%
  filter(class != "Other") %>%
  mutate(class = factor(class)) %>%
  coin::wilcox_test(data = ., typicality ~ class)

# effect size
Z.action.thing <- all.distances %>% 
  filter(class != "Other") %>%
  mutate(class = factor(class)) %>%
  coin::wilcox_test(data = ., typicality ~ class) %>%
  coin::statistic(type = "standardized")
Z.action.thing / sqrt(nrow(filter(all.distances, class != "Other")))
effsize::cohen.d(data = filter(all.distances, class != "Other"), typicality ~ class)

# mann whitney tests of chosen languages
## Mansi
all.distances %>%
  filter(language == "Mansi" & class != "Other") %>%
  mutate(class = factor(class)) %>%
  coin::wilcox_test(data = ., typicality ~ class)
all.distances %>%
  filter(language == "Mansi" & class != "Other") %>%
  mutate(class = factor(class)) %>%
  effsize::cohen.d(data = ., typicality ~ class)
## Romani
all.distances %>%
  filter(language == "Romani" & class != "Other") %>%
  mutate(class = factor(class)) %>%
  coin::wilcox_test(data = ., typicality ~ class)
all.distances %>%
  filter(language == "Romani" & class != "Other") %>%
  mutate(class = factor(class)) %>%
  effsize::cohen.d(data = ., typicality ~ class)
## English
all.distances %>%
  filter(language == "English" & class != "Other") %>%
  mutate(class = factor(class)) %>%
  coin::wilcox_test(data = ., typicality ~ class)
all.distances %>%
  filter(language == "English" & class != "Other") %>%
  mutate(class = factor(class)) %>%
  effsize::cohen.d(data = ., typicality ~ class)
## Thai (Korat)
all.distances %>%
  filter(language == "Thai (Korat variety)" & class != "Other") %>%
  mutate(class = factor(class)) %>%
  coin::wilcox_test(data = ., typicality ~ class)
all.distances %>%
  filter(language == "Thai (Korat variety)" & class != "Other") %>%
  mutate(class = factor(class)) %>%
  effsize::cohen.d(data = ., typicality ~ class)

# Plotting ----------------------------------------------------------------

# World map with languages
languagesX <- phon.languages$longitude
languagesY <- phon.languages$latitude
ggplot() +
  borders("world", colour=wes_palettes$Moonrise1[4], fill=wes_palettes$Darjeeling1[2]) + # create a layer of borders
  geom_jitter(aes(x = languagesX, y = languagesY), fill = wes_palettes$Darjeeling1[1],
              color = wes_palettes$Darjeeling2[5], size = 3, shape = 25) +

  cowplot::theme_map()

# 2D Density of all words

all.distances %>%
  filter(class != "Other") %>%
  mutate(class = factor(class, levels = c("Action", "Thing", "Other"))) %>%
  ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
  stat_density2d(geom = 'polygon') +
  scale_fill_gradientn(colors = color.palette.cont, name = "Density") +
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = wes_palettes$Moonrise1[4], size = 1) +
  labs(x = expression(hat(ND[A])), y = expression(hat(ND[T]))) +
  cowplot::theme_cowplot() +
  facet_wrap(vars(class), ncol = 2) +
  cowplot::panel_border()

# Faceted histogram of typicality per class

all.distances %>%
  ggplot(aes(x = typicality, fill = class)) +
  geom_histogram(binwidth = .05) +
  facet_wrap(vars(class), nrow = 3, scales = "free_y") +
  geom_vline(xintercept = 0, size = 2, linetype = "solid") +
  labs(x = "Typicality", y = "Count", title = "Histogram of typicality per class") +
  scale_fill_manual(values = c(color.palette.dis[7], color.palette.dis[3], color.palette.dis[5]))

# Violin of typicality per class with boxplot

all.distances %>%
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

# Sorted scatter of typicality per class

data.for.scatter <- all.distances %>%
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

all.distances %>%
  filter(language %in% c("Mansi", "English", "Romani", "Thai (Korat variety)")) %>%
  filter(class != "Other") %>%
  mutate(class = factor(class, levels = c("Action", "Thing", "Other")),
         language = factor(language, levels = c("Mansi", "Romani", "English", "Thai (Korat variety)"))) %>%
  ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
  stat_density2d(geom = 'polygon') +
  scale_fill_gradientn(colors = color.palette.cont, name = "Density") +
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

data.for.scatter <- data.for.scatter %>% 
  mutate(Marker.Group = ifelse(language %in% no.marker.group, "No marker", "Marker"))
ggplot(data = data.for.scatter, aes(y = Mean, x = language, color = class, shape = class,
                                      ymin = Lower, ymax = Upper, group = class)) +
  labs(x = 'Language',
       y = 'Mean Typicality') +
  # geom_segment(aes(x = marker, xend = marker, y = -Inf, yend = Mean), size = .6,
  # color = color.palette.dis[6]) +
  geom_point(aes(fill = class), color = 'black', size = 3) +
  geom_hline(linetype = 'solid', size = 1, yintercept = 0) +
  scale_color_manual(name = "Category", values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[1])) +
  scale_fill_manual(name = "Category", values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[1])) +
  scale_shape_manual(name = "Category", values = c(23, 24)) +
  scale_x_discrete(name = "", labels = label.text) +
  cowplot::theme_cowplot() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        strip.text.x = element_text(size = 16)) +
  cowplot::background_grid() + 
  facet_wrap(vars(Marker.Group), scales = "free_x")



ggsave("Figures/Evolang/scatter_all.pdf", scale = 2)


ggplot(data = data.for.scatter, mapping = aes(x = language, y = Mean)) +
  geom_col(data = filter(data.for.scatter, class == "Thing"), fill = wes_palettes$Darjeeling1[1]) + 
  geom_col(data = filter(data.for.scatter, class == "Action"), fill = wes_palettes$Darjeeling1[2]) + 
  scale_x_discrete(name = "", labels = label.text)

data.for.scatter %>% # this one includes the difference in means
  select(-Lower, -Upper, -Standard.Deviation) %>%
  spread(class, Mean) %>%
  mutate(difference = Action - Thing) %>%
  arrange(desc(abs(difference)))
data.for.scatter %>% # this one includes the difference in means
  select(-Lower, -Upper, -Standard.Deviation) %>%
  spread(class, Mean) %>%
  mutate(difference = Thing - Action) %>%
  arrange(desc(abs(difference))) %>% 
  mutate(language = factor(language, levels = .$language)) %>% 
  ggplot() + 
    geom_density(aes(x = difference, fill = Marker.Group), alpha = 0.8,
                 size = 0.5) + 
    geom_rug(aes(x = difference, y = 0, color = Marker.Group), 
             position = position_jitter(height = 0, width = 0.005), show.legend = FALSE) + 
    cowplot::theme_cowplot() +
  scale_x_continuous(name = "Difference in mean typicality, Thing - Action") +
  scale_fill_manual(values = c(wes_palettes$Darjeeling1[1], wes_palettes$Darjeeling1[2])) + 
  scale_color_manual(values = c(wes_palettes$Darjeeling1[1], wes_palettes$Darjeeling1[2])) + 
  theme(legend.position = c(0.7, 0.8),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

# Closest phonological neighbors ---------------

all.phon.list <- all.phon %>%
  split(.$language)
all.phon.list <- all.phon.list[sort(names(all.phon.list))]

all.distance.matrices <- purrr::map(all.phon.list, function(x){get.distance.matrix(x)})

# names(all.distance.matrices) <- unique(all.phon$language)

# all.distance.matrices <- all.distance.matrices[sort(names(all.distance.matrices))]

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
# repeated.neighbor %>% write_rds("Data/Processed/repeated_neighbor.Rds")



wilcox.tests <- map(sort(unique(all.phon$language)), function(x){
  lang.distances <- all.distances %>% 
    filter(class != "Other", language == x) %>%
    mutate(class = factor(class))
  return(coin::wilcox_test(data = lang.distances, typicality ~ class))
})

names(wilcox.tests) <- sort(unique(all.phon$language))
significances.marker <- map_dbl(wilcox.tests[marker.group], function(x){
  return(coin::pvalue(x))
})

adjusted <- significances.marker %>% p.adjust(method = "bonferroni", n = 227)

sum(adjusted < 0.001)


cohen.marker <- map_dbl(sort(marker.group), function(x){
  lang.distances <- all.distances %>% 
    filter(class != "Other", language == x) %>%
    mutate(class = factor(class))
  effsize::cohen.d(data = lang.distances, typicality ~ class) %>% 
           .$estimate %>% 
    return()
})

cohen.marker %>% median()


significances.no.marker <- map_dbl(wilcox.tests[no.marker.group], function(x){
  return(coin::pvalue(x))
})

adjusted.no.marker <- significances.no.marker %>% p.adjust(method = "bonferroni", n = 227)

sum(adjusted.no.marker < 0.001)

cohen.no.marker <- map_dbl(sort(no.marker.group), function(x){
  lang.distances <- all.distances %>% 
    filter(class != "Other", language == x) %>%
    mutate(class = factor(class))
  effsize::cohen.d(data = lang.distances, typicality ~ class) %>% 
    .$estimate %>% 
    return()
})

cohen.no.marker %>% median

significances.marker / length(significances.marker)

repeated.neighbor <- read_rds("Data/Processed/repeated_neighbor.Rds")

neighbor.stats <- repeated.neighbor %>%
  group_by(language, ontological.category) %>%
  do(enframe(Hmisc::smean.cl.normal(.$proportion.of.hits, conf.int = 0.999))) %>%
  spread(name, value)

# load multicore mc

# require(future)
# require(furrr)

# future::plan(multicore)
# cores <- 4
# options(future.globals.maxSize = +Inf, mc.cores = cores)

# neighbor.mc <- map2_dfr(.x = all.phon.list, .y = all.distance.matrices, 
                                      # .f = function(language, distance.matrix){
                                        # print(language)
                                        # gc()
                                        # 
                                        # purrr::map_dfr(1:1000, function(x){
                                          # all.neighbors <- get.nearest.neighbors(a.language = language,
                                                                                 # distance.matrix = distance.matrix,
                                                                                 # randomize = TRUE) %>%
                                            # mutate(same.neighbor = (ontological.category == neighbor.category)) %>%
                                            # group_by(language, ontological.category) %>%
                                            # summarise(proportion.of.hits = sum(same.neighbor) / n()) %>%
                                            # mutate(permutation = x)
                                          # print(x)
                                          # return(all.neighbors)
    # }) %>%
    # group_by(language, ontologicalCategory, permutation) %>%
    # summarize(Mean = mean(proportionOfHits), Upper = Mean + sd(proportionOfHits), Lower = Mean - sd(proportionOfHits)) %>%
    # return()
# })

# neighbor.mc %>%
  # write_rds("Data/Processed/neighbor_mc.Rds")

neighbor.mc <- read_rds("Data/Processed/neighbor_mc.Rds")

neighbor.mc <- neighbor.mc %>%
  rename(random = proportion.of.hits)
neighbor.test <- repeated.neighbor %>%
  group_by(language, ontological.category) %>%
  summarize(proportionOfHits = mean(proportion.of.hits)) %>%
  left_join(neighbor.mc) %>%
  mutate(is.higher = random >= proportionOfHits)
neighbor.test <- neighbor.test %>%
  group_by(language, ontological.category) %>%
  summarize(p = sum(is.higher) / 1000)

neighbor.test %>%
  mutate(p = cut(p, breaks = c(0, 0.05, 1), include.lowest = TRUE, right = FALSE)) %>%
  group_by(p, ontological.category) %>%
  tally() %>%
  mutate(n = n/ 222)

neighbor.test %>%
  filter(language %in% c("Mansi", "English", "Romani", "Thai (Korat variety)"))


## Actions

action.mc <- neighbor.mc %>%
  filter(ontological.category == "Action") %>%
  select(language, ontological.category, random, permutation)

action.plot <- neighbor.stats %>%
  filter(ontological.category == "Action") %>%
  left_join(action.mc) %>%
  group_by() %>%
  mutate(language = factor(language, levels = sorted.langs$language, labels = sorted.langs$language),
         include = ifelse(language %in% c("Mansi", "English", "Thai (Korat variety)", "Romani"), as.character(language), NA))

action.plot <- action.plot %>%
  group_by(language, ontological.category) %>%
  summarize(Mean = mean(Mean), Lower = mean(Lower), Upper = mean(Upper),
            random = mean(random) + sd(random)) %>%
  group_by() %>%
  mutate(label = ifelse(as.character(language) %in% c("Mansi", "English", "Thai (Korat variety)", "Romani"), as.character(language), ""))

action.plot %>%
  ggplot(aes(x = language, y = Mean, ymin = Lower, ymax = Upper)) +
  geom_bar(stat = "identity", width = 1,
           fill = color.palette.dis[5],
           size = .25, color = "black") +
  geom_bar(mapping = aes(y = random), fill = "yellow", stat = "identity", width = 1,
           alpha = .8, color = "black") +
  cowplot::theme_cowplot() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(name = "", labels = action.plot$label)

## Things

things.mc <- neighbor.mc %>%
  filter(ontological.category == "Thing") %>%
  select(language, ontological.category,random, permutation)

things.plot <- neighbor.stats %>%
  filter(ontological.category == "Thing") %>%
  left_join(things.mc) %>%
  group_by() %>%
  mutate(language = factor(language, levels = sorted.langs$language, labels = sorted.langs$language),
         include = ifelse(language %in% c("Mansi", "Thai (Korat variety)", "Romani"), as.character(language), NA))

things.plot <- things.plot %>%
  group_by(language, ontological.category) %>%
  summarize(Mean = mean(Mean), Lower = mean(Lower), Upper = mean(Upper),
            random = mean(random) + sd(random)) %>%
  group_by() %>%
  mutate(label = ifelse(as.character(language) %in% c("Mansi", "Thai (Korat variety)", "English", "Romani"), as.character(language), ""))

things.plot %>%
  ggplot(aes(x = language, y = Mean, ymin = Lower, ymax = Upper)) +
  geom_bar(stat = "identity", width = 1,
           fill = color.palette.dis[7],
           size = .25, color = "black") +
  geom_bar(mapping = aes(y = random), fill = "yellow", stat = "identity", width = 1,
           alpha = .8, color = "black") +
  cowplot::theme_cowplot() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(name = "", labels = action.plot$label)

neighbor.mc.plot <- neighbor.mc %>%
  filter(ontological.category != "Other") %>%
  group_by(language, ontological.category) %>%
  summarize(random = mean(random) + sd(random))

neighbor.plot <- neighbor.stats %>%
  group_by() %>%
  filter(ontological.category != "Other") %>%
  left_join(neighbor.mc.plot) %>%
  mutate(language = factor(language, levels = sorted.langs$language, labels = sorted.langs$language))

labels.for.plot <- map_chr(levels(neighbor.plot$language), function(language){
  if(language %in% c("Mansi", "English", "Thai (Korat variety)", "Romani")){return(language)} else{return("")}
})

neighbor.plot %>%
  ggplot(aes(x = language, y = Mean, ymin = Lower, ymax = Upper, fill = ontological.category)) +
  geom_bar(stat = "identity", width = 1,
           size = .25, color = "black") +
  geom_bar(mapping = aes(y = random), fill = "yellow", stat = "identity", width = 1,
           alpha = .8, color = "black") +
  # geom_point(shape = 18, size = 2) +
  scale_x_discrete(name = "", labels = labels.for.plot) +
  facet_wrap(vars(ontological.category), ncol = 1) +
  scale_y_continuous(expand = c(0, 0), name = "Proportion of same neighbor") +
  scale_fill_manual(values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[1])) +
  cowplot::theme_cowplot() +
  cowplot::panel_border() +
  theme(legend.position = "none")

# 4 languages

smaller.mc <- neighbor.mc %>%
  filter(language %in% c("Mansi", "Romani", "English", "Thai (Korat variety)")) %>%
  group_by(language, ontological.category) %>%
  summarize(random = mean(random) + sd(random))

smaller.neighbor <- neighbor.stats %>%
  filter(language %in% c("Mansi", "Romani", "English", "Thai (Korat variety)"))

smaller.neighbor %>%
  left_join(smaller.mc) %>%
  group_by() %>% 
  mutate(language = factor(language, levels = c("Mansi", "Romani", "English", "Thai (Korat variety)"))) %>%
  ggplot(aes(fill = ontological.category, x = language, y = Mean)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  geom_errorbar(mapping = aes(ymin = Lower, ymax = Upper),
                position = position_dodge(width = .9), width = 0.3,
                size = 1) +
  geom_bar(aes(y = random, group = ontological.category),
           fill = "yellow", stat = "identity", position = "dodge", alpha = 0.85, color = "black") +
  scale_y_continuous(name = "Proportion of same neighbor", expand = c(0, 0)) +
  scale_fill_manual(name = "Category", values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[5], wes_palettes$Darjeeling1[1])) +
  cowplot::theme_cowplot() +
  theme(axis.title.x = element_blank())


# Word Edges -----

all.phon.extremes <- all.phon %>%
  mutate(ending = str_sub(phon, -1), start = str_sub(phon, 1, 1))

all.extremes <- map(sort(unique(all.phon.extremes$language)), function(x){
  lang <- all.phon.extremes %>%
    filter(language == x) %>%
    select(ontological.category, ending, start) %>%
    mutate_at(c("ending", "start"), as.factor)
  dummies <- caret::dummyVars(data = lang, ~ ending + start)
  lang <- lang %>%
    bind_cols(as_tibble(predict(object = dummies, newdata = lang))) %>%
    select(ontological.category, contains("ending"), contains("start"), -ending, -start) %>%
    mutate(ontological.category = factor(ontological.category))
  return(lang)
})
names(all.extremes) <- sort(unique(all.phon.extremes$language))

action.props <- map_dfr(names(all.extremes), function(lang){
  lang.name <- lang
  lang <- all.extremes[[lang]]
  tally <- lang %>%
    group_by(ontological.category) %>%
    tally()
  props.per.cat <- lang %>%
    group_by(ontological.category) %>%
    summarize_all(sum) %>%
    left_join(tally) %>%
    mutate_at(vars(-ontological.category, -n), funs(. / n))
  action.prop <- props.per.cat %>%
    select(-n)
  action.prop <- action.prop %>%
    add_column(language = lang.name)
  return(action.prop)
})



# RNN -------

# save modified all.phon for morphological control

all.phon %>% 
  mutate(phon = str_sub(phon, "1", "-2")) %>% 
  write_csv("Data/Processed/all_phon_no_ending.csv")
all.phon %>% 
  mutate(phon = str_sub(phon, "2", "-1")) %>% 
  write_csv("Data/Processed/all_phon_no_start.csv")



# Load NN results --------------------------------------------------------

rnn.performance <- list()

for(file in list.files('Results/RNN/', recursive = T, full.names = T)){
  language <- str_extract(file, "(?<=Results/RNN/).+(?=_rnn_)")
  print(language)
  rnn.performance[[language]] <- read_csv(file)
  rnn.performance[[language]]$language <- language
}

sigs <- lapply(rnn.performance, function(stats){
  return(wilcox.test(x = stats$Matthews, mu = 0, alternative = "greater"))
})

sigs <- lapply(sigs, function(stats){
  if(is.nan(stats[["p.value"]]))
  {return(FALSE)}
  pvalue <- stats[["p.value"]]
  pvalue <- p.adjust(p = pvalue, method = "bonferroni", n = 227)
  if(pvalue >= 0.01){return(FALSE)} else{return(TRUE)}
})

sigs <- bind_rows(sigs) %>%
  gather() %>%
  rename(language = key, significant = value) %>%
  filter(language %in% phon.languages$Name)

sigs

sigs %>% 
  mutate(marker = ifelse(language %in% marker.group, "Marker", "No marker")) %>% 
  group_by(marker) %>% 
  summarize(number.significant = sum(significant))

rnn.stats <- rnn.performance %>%
  bind_rows() %>%
  filter(language %in% phon.languages$Name) %>%
  # mutate(language = factor(language, levels = sorted.langs$language)) %>%
  select(-X1) %>%
  group_by(language) %>%
  summarise(Median = median(Matthews), sd = sd(Matthews))

rnn.stats

rnn.performance %>%
  bind_rows() %>%
  filter(language %in% phon.languages$Name) %>% mutate(marker = ifelse(language %in% marker.group, "Marker", "No Marker")) %>%
  rename(MCC = Matthews) %>%
  ggplot(aes(x = marker, y = MCC, fill = marker)) +
  # geom_violin(alpha = 0.8, lwd = 1.5) +
  geom_boxplot(width = 0.5, lwd = 1) +
  # cowplot::theme_cowplot() +
  # cowplot::background_grid() +
  theme_minimal() + 
  theme(legend.position = "none") + 
  scale_fill_manual(values = c(wes_palettes$Darjeeling1[1], wes_palettes$Darjeeling1[2])) +
  geom_hline(yintercept = 0, lwd = 2, linetype = "dashed") +
  coord_flip()  +
  scale_y_continuous(name = "Matthew's Correlation Coefficient", breaks = c(0.25, 0.5, 0.75, 1)) + 
  theme(axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_blank())




# boxplot in evolang

rnn.stats %>%
  bind_rows() %>%
  filter(language %in% phon.languages$Name) %>% mutate(marker = ifelse(language %in% marker.group, "Marker", "No Marker")) %>%
  rename(MCC = Median) %>%
  ggplot(aes(x = marker, y = MCC, fill = marker)) +
  geom_boxplot(width = 0.5, lwd = 1) +
  # cowplot::theme_cowplot() +
  # cowplot::background_grid() +
  theme_minimal() + 
  theme(legend.position = "none") + 
  scale_fill_manual(values = c(wes_palettes$Darjeeling1[1], wes_palettes$Darjeeling1[2])) +
  geom_hline(yintercept = 0, lwd = 2, linetype = "dashed") +
  scale_y_continuous(name = "Matthew's Correlation Coefficient", breaks = c(0.25, 0.5, 0.75, 1)) + 
  theme(axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_blank(),
        plot.margin = margin(10, 15, 10, 10)) +
  coord_flip()
ggsave("Figures/Evolang/small_boxplot.png")

# bar plot

rnn.stats %>%
  bind_rows() %>%
  filter(language %in% phon.languages$Name) %>% mutate(marker = ifelse(language %in% marker.group, "Marker", "No Marker")) %>%
  arrange(desc(Median)) %>% 
  mutate(language = factor(language, levels = .$language)) %>% 
  rename(MCC = Median) %>%
  ggplot(aes(x = language, y = MCC)) +
  geom_col(fill = wes_palettes$Darjeeling1[2], color = "black") + 
  cowplot::theme_cowplot() +
  cowplot::panel_border() + 
  theme(legend.position = "none") + 
  scale_fill_manual(values = c(wes_palettes$Darjeeling1[1], wes_palettes$Darjeeling1[2])) +
  # geom_hline(yintercept = 0, lwd = 2, linetype = "dashed") +
  scale_y_continuous(name = "Median Matthew's Correlation Coefficient", breaks = c(0.25, 0.5, 0.75, 1)) + 
  labs(x = "Language") + 
  theme(axis.text.x = element_blank(),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 20),
        axis.ticks.x = element_blank()) + 
  facet_wrap(vars(marker), scales = "free_x")

ggsave("Figures/Evolang/All_rnn_median.pdf", scale = 2)

# rnn.stats %>%
#   filter(language %in% c("Mansi", "Romani", "English", "Thai (Korat variety)"))
# 
median(rnn.stats$Median)
sd(rnn.stats$Median)
rnn.stats %>%
  left_join(sigs) %>%
  group_by(significant) %>%
  tally() %>%
  mutate(n / 227)

# RNN without endings ----

rnn.no.ending <- list()

for(file in list.files('Results/No-Ending//', recursive = T, full.names = T)){
  language <- str_extract(file, "(?<=Results/No-Ending//).+(?=_rnn_)")
  print(language)
  rnn.no.ending[[language]] <- read_csv(file)
  rnn.no.ending[[language]]$language <- language
}

sigs.no.ending <- lapply(rnn.no.ending, function(stats){
  return(wilcox.test(x = stats$Matthews, mu = 0, alternative = "greater"))
})

sigs.no.ending <- lapply(sigs.no.ending, function(stats){
  if(is.nan(stats[["p.value"]]))
  {return(FALSE)}
  if(stats[["p.value"]] >= 0.01){return(FALSE)} else{return(TRUE)}
})

sigs.no.ending <- bind_rows(sigs.no.ending) %>%
  gather() %>%
  rename(language = key, significant = value) %>%
  filter(language %in% phon.languages$Name)

sigs.no.ending

rnn.stats.no.ending <- rnn.no.ending %>%
  bind_rows() %>%
  filter(language %in% phon.languages$Name) %>%
  # mutate(language = factor(language, levels = sorted.langs$language)) %>%
  select(-X1) %>%
  group_by(language) %>%
  summarise(Median = median(Matthews), sd = sd(Matthews)) %>% 
  rename(No.Ending = Median)

morph.comparison <- select(rnn.stats, language, Ending = Median, Ending.SD = sd)  %>% 
  left_join(select(rnn.stats.no.ending, language, No.Ending, No.Ending.SD = sd)) %>% 
  arrange(desc(Ending)) %>% 
  mutate(language = factor(language, levels = .$language))

morph.comparison %>% 
  ggplot(aes(x = language, y = Ending, ymin = Ending - Ending.SD, ymax = Ending + Ending.SD)) +
  geom_col()
morph.comparison %>% 
  ggplot(aes(x = language, y = No.Ending, ymin = No.Ending - No.Ending.SD, ymax = No.Ending + Ending.SD)) +
  geom_col()

comparison.plot <- morph.comparison %>% 
  select(-Ending.SD, -No.Ending.SD) %>% 
  gather("Mode", "MCC", Ending:No.Ending) %>% 
  mutate(marker.group = ifelse(language %in% marker.group.3, 
                               "3-or-more-long-marker",
                               ifelse(language %in% marker.group.2, "2-long marker",
                                      ifelse(language %in% marker.group, "1-long marker",
                                             "No marker"))))
ggplot(mapping = aes(x = language, y = MCC)) +
  geom_bar(data = filter(comparison.plot, Mode == "Ending"), stat = "identity",
           mapping = aes(fill = wes_palettes$Darjeeling1[1]), color = "black") +
  geom_bar(data = filter(comparison.plot, Mode == "No.Ending"), stat = "identity",
           mapping = aes(fill = wes_palettes$Darjeeling1[2]), color = "black") + 
  cowplot::theme_cowplot() + 
  scale_fill_manual(name = "Ending?", values = c(wes_palettes$Darjeeling1[2], 
                                                 wes_palettes$Darjeeling1[1]),
                    labels = c("No Ending", "Ending")) +
  theme(axis.text.x = element_blank()) + 
  facet_wrap(vars(marker.group), drop = TRUE, scales = "free_x") + 
  labs(title = "Comparison of RNN before and after removing the last phoneme")

morph.comparison %>% 
  select(-Ending.SD, -No.Ending.SD) %>% 
  gather("Mode", "MCC", Ending:No.Ending) %>% 
  ggplot(aes(x = language, y = MCC)) + 
  geom_col(fill = wes_palettes$Darjeeling1[1]) + 
  facet_wrap(vars(Mode), ncol = 1) + 
  cowplot::theme_cowplot() + 
  theme(axis.text.x = element_blank())
morph.comparison %>% 
  select(-Ending.SD, -No.Ending.SD) %>% 
  mutate(difference.in.median = Ending - No.Ending,
         Now.Random = ifelse(No.Ending <= 0.1, TRUE, FALSE)) %>% 
  ggplot(aes(x = language, y = difference.in.median)) + 
  geom_col(aes(fill = Now.Random)) + 
  cowplot::theme_cowplot() + 
  theme(axis.text.x = element_blank()) 

complete.comparison <- morph.comparison %>% 
  left_join(rename(phon.languages, language = Name)) %>% 
  mutate(marker = ifelse(language %in% marker.group, "Marker", "No Marker"),
         family = factor(family),
         marker = factor(marker),
         difference = Ending - No.Ending,
         Now.Random = ifelse(No.Ending <= 0.1, TRUE, FALSE))


diff.interactive <- plotly::plot_ly(complete.comparison, x = ~language, y = ~difference,
                color = ~marker, text = ~paste0("Language: ", language, 
                                                "\nMarker group: ", marker, 
                                               "\nMedian with ending: ", round(Ending,2),
                                               "\nMedian without ending: ", round(No.Ending, 2),
                                               "\nDifference: ", round(difference,2),
                                               "\nFamily: ", family,
                                               "\nDid removal make it random?: ", Now.Random)) %>% 
  plotly::layout(xaxis = list(title = "", tickangle = -45),
         yaxis = list(title = ""),
         margin = list(b = 80))
htmlwidgets::saveWidget(diff.interactive, "interactive_difference.html", selfcontained = TRUE)

# rnn.stats %>%
#   filter(language %in% marker.group) %>%
#   left_join(sigs) %>%
#   group_by(significant) %>%
#   tally() %>%
#   mutate(n / length(marker.group))
# rnn.stats %>%
#   filter(language %in% no.marker.group) %>%
#   left_join(sigs) %>%
#   group_by(significant) %>%
#   tally() %>%
#   mutate(n / length(no.marker.group))

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

all.phon.extremes <- all.phon %>%
  mutate(ending = str_sub(phon, -1), start = str_sub(phon, 1, 1))

all.extremes <- purrr::map(sort(unique(all.phon.extremes$language)), function(x){
  print(x)
  lang <- all.phon.extremes %>%
    filter(language == x) %>%
    select(ontological.category, ending, start) %>%
    mutate_at(c("ending", "start"), as.factor)
  dummies <- caret::dummyVars(data = lang, ~ ending + start, fullRank = TRUE)
  lang <- lang %>%
    bind_cols(as_tibble(predict(object = dummies, newdata = lang))) %>%
    select(ontological.category, contains("ending"), contains("start"), -ending, -start) %>%
    mutate(ontological.category = factor(ontological.category))
  return(lang)
})


names(all.extremes) <- sort(unique(all.phon.extremes$language))

all.props <- map_dfr(names(all.extremes), function(lang){
  lang.name <- lang
  lang <- all.extremes[[lang]]
  tally <- lang %>%
    group_by(ontological.category) %>%
    tally()
  props.per.cat <- lang %>%
    group_by(ontological.category) %>%
    summarize_all(sum) %>%
    left_join(tally) %>%
    mutate_at(vars(-ontological.category, -n), funs(. / n))
  max.per.cat <- select(props.per.cat, -n, -ontological.category) %>% 
    apply(1, max)
  props.per.cat %>% 
    select(ontological.category) %>% 
    add_column(max.proportions = max.per.cat) %>% 
    filter(ontological.category != "Other") %>% 
    add_column(language = lang.name) %>% 
    return()
})

max.props <- all.props %>% 
  group_by(language) %>% 
  summarize(max.prop = max(max.proportions))
marker.group <- max.props %>% 
  filter(max.prop > 0.5) %>% 
  .$language
no.marker.group <- max.props %>% 
  filter(max.prop <= 0.5) %>% 
  .$language

# get two phoneme markers

marker.phon <- all.phon %>% 
  filter(nchar(phon) > 2) %>% 
  filter(language %in% marker.group)

all.phon.extremes.2 <- marker.phon %>%
  mutate(phon = str_sub(phon, "1", "-2")) %>% 
  mutate(ending = str_sub(phon, -1))

all.extremes.2 <- purrr::map(sort(unique(all.phon.extremes.2$language)), function(x){
  print(x)
  lang <- all.phon.extremes.2 %>%
    filter(language == x) %>%
    select(ontological.category, ending) %>%
    mutate_at(c("ending"), as.factor)
  dummies <- caret::dummyVars(data = lang, ~ ending, fullRank = TRUE)
  lang <- lang %>%
    bind_cols(as_tibble(predict(object = dummies, newdata = lang))) %>%
    select(ontological.category, contains("ending"), -ending) %>%
    mutate(ontological.category = factor(ontological.category))
  return(lang)
})


names(all.extremes.2) <- sort(unique(all.phon.extremes.2$language))

all.props.2 <- map_dfr(names(all.extremes.2), function(lang){
  lang.name <- lang
  lang <- all.extremes.2[[lang]]
  tally <- lang %>%
    group_by(ontological.category) %>%
    tally()
  props.per.cat <- lang %>%
    group_by(ontological.category) %>%
    summarize_all(sum) %>%
    left_join(tally) %>%
    mutate_at(vars(-ontological.category, -n), funs(. / n))
  max.per.cat <- select(props.per.cat, -n, -ontological.category) %>% 
    apply(1, max)
  props.per.cat %>% 
    select(ontological.category) %>% 
    add_column(max.proportions = max.per.cat) %>% 
    filter(ontological.category != "Other") %>% 
    add_column(language = lang.name) %>% 
    return()
})

max.props.2 <- all.props.2 %>% 
  group_by(language) %>% 
  summarize(max.prop = max(max.proportions))
marker.group.2 <- max.props.2 %>% 
  filter(max.prop > 0.5) %>% 
  .$language

# last: do a third marker

marker.phon.2 <- all.phon %>% 
  filter(nchar(phon) > 3) %>% 
  filter(language %in% marker.group.2)

all.phon.extremes.3 <- marker.phon.2 %>%
  mutate(phon = str_sub(phon, "1", "-3")) %>% 
  mutate(ending = str_sub(phon, -1))

all.extremes.3 <- purrr::map(sort(unique(all.phon.extremes.3$language)), function(x){
  print(x)
  lang <- all.phon.extremes.3 %>%
    filter(language == x) %>%
    select(ontological.category, ending) %>%
    mutate_at(c("ending"), as.factor)
  dummies <- caret::dummyVars(data = lang, ~ ending, fullRank = TRUE)
  lang <- lang %>%
    bind_cols(as_tibble(predict(object = dummies, newdata = lang))) %>%
    select(ontological.category, contains("ending"), -ending) %>%
    mutate(ontological.category = factor(ontological.category))
  return(lang)
})


names(all.extremes.3) <- sort(unique(all.phon.extremes.3$language))

all.props.3 <- map_dfr(names(all.extremes.3), function(lang){
  lang.name <- lang
  lang <- all.extremes.3[[lang]]
  tally <- lang %>%
    group_by(ontological.category) %>%
    tally()
  props.per.cat <- lang %>%
    group_by(ontological.category) %>%
    summarize_all(sum) %>%
    left_join(tally) %>%
    mutate_at(vars(-ontological.category, -n), funs(. / n))
  max.per.cat <- select(props.per.cat, -n, -ontological.category) %>% 
    apply(1, max)
  props.per.cat %>% 
    select(ontological.category) %>% 
    add_column(max.proportions = max.per.cat) %>% 
    filter(ontological.category != "Other") %>% 
    add_column(language = lang.name) %>% 
    return()
})

max.props.3 <- all.props.3 %>% 
  group_by(language) %>% 
  summarize(max.prop = max(max.proportions))
marker.group.3 <- max.props.3 %>% 
  filter(max.prop > 0.5) %>% 
  .$language






# scatter by marker group

data.for.scatter.marker <- all.distances %>%
  filter(class != "Other", language %in% marker.group) %>%
  group_by(language, class) %>%
  summarize(Mean = mean(typicality), Standard.Deviation = sd(typicality)) %>%
  mutate(Lower = Mean - Standard.Deviation, Upper = Mean + Standard.Deviation)

# sort it for cleaner viz

sorted.langs.marker <- data.for.scatter.marker %>% # this one includes the difference in means
  select(-Lower, -Upper, -Standard.Deviation) %>%
  spread(class, Mean) %>%
  mutate(difference = abs(Action - Thing)) %>%
  arrange(desc(abs(difference)))

median(sorted.langs.marker$difference)
sd(sorted.langs.marker$difference)

data.for.scatter.marker <- data.for.scatter.marker %>%
  filter(class != "Other") %>%
  group_by() %>%
  mutate(language = factor(language, levels = sorted.langs.marker$language), class = factor(class),
         include = ifelse(language %in% c("Mansi", "Movima", "Andi"), language, NA))

# keep labels for max, min and median and English

label.text.marker <- map_chr(sorted.langs.marker$language, function(x){
  if(x %in% c("Mansi", "Movima", "Andi")){
    return(x)
  } else{return("")}
})


marker.scatter <- ggplot(data = data.for.scatter.marker, aes(y = Mean, x = language, color = class, shape = class,
                                    ymin = Lower, ymax = Upper, group = class)) +
  labs(x = 'Language',
       y = 'Mean Typicality') +
  geom_segment(aes(x = include, xend = include, y = -Inf, yend = Mean), size = .5,
               color = color.palette.dis[6]) +
  geom_point(aes(fill = class), color = 'black', size = 2.5) +
  geom_hline(linetype = 'solid', size = 1, yintercept = 0) +
  scale_color_manual(name = "Category", values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[1])) +
  scale_fill_manual(name = "Category", values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[1])) +
  scale_shape_manual(name = "Category", values = c(23, 24)) +
  scale_x_discrete(name = "", labels = label.text.marker) +
  cowplot::theme_cowplot() +
  cowplot::background_grid() + 
  theme(axis.text.x = element_text(size = 20),
        legend.text = element_text(size = 16),
        legend.title = element_blank(),
        axis.title.y = element_text(size = 20),
        legend.position = c(0.85, 0.9))


# no marker scatter

data.for.scatter.no.marker <- all.distances %>%
  filter(class != "Other", language %in% no.marker.group) %>%
  group_by(language, class) %>%
  summarize(Mean = mean(typicality), Standard.Deviation = sd(typicality)) %>%
  mutate(Lower = Mean - Standard.Deviation, Upper = Mean + Standard.Deviation)

# sort it for cleaner viz

sorted.langs.no.marker <- data.for.scatter.no.marker %>% # this one includes the difference in means
  select(-Lower, -Upper, -Standard.Deviation) %>%
  spread(class, Mean) %>%
  mutate(difference = abs(Action - Thing)) %>%
  arrange(desc(abs(difference)))

median(sorted.langs.no.marker$difference)
sd(sorted.langs.no.marker$difference)

data.for.scatter.no.marker <- data.for.scatter.no.marker %>%
  filter(class != "Other") %>%
  group_by() %>%
  mutate(language = factor(language, levels = sorted.langs.no.marker$language), class = factor(class),
         include = ifelse(language %in% c("Akhvakh (Northern dialect)", "English", "Basque", "Thai (Korat variety)"), language, NA))

# keep labels for max, min and median and English

label.text.no.marker <- map_chr(sorted.langs.no.marker$language, function(x){
  if(x %in% c("Akhvakh (Northern dialect)", "English", "Basque", "Thai (Korat variety)")){
    if(as.character(x) == "Akhvakh (Northern dialect)"){
      return("Akhvakh\n(Northern)")
    }
    if(as.character(x) == "Thai (Korat variety)"){
      return("Thai\n(Korat)")
    }
    else{return(as.character(x))}
  } else{return("")}
})


no.marker.scatter <- ggplot(data = data.for.scatter.no.marker, aes(y = Mean, x = language, color = class, shape = class,
                                                             ymin = Lower, ymax = Upper, group = class)) +
  labs(x = 'Language',
       y = 'Mean Typicality') +
  geom_segment(aes(x = include, xend = include, y = -Inf, yend = Mean), size = .5,
               color = color.palette.dis[6]) +
  geom_point(aes(fill = class), color = 'black', size = 2.5) +
  geom_hline(linetype = 'solid', size = 1, yintercept = 0) +
  scale_color_manual(name = "Category", values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[1])) +
  scale_fill_manual(name = "Category", values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[1])) +
  scale_shape_manual(name = "Category", values = c(23, 24)) +
  scale_x_discrete(name = "", labels = label.text.no.marker) +
  cowplot::theme_cowplot() +
  cowplot::background_grid() + 
  theme(axis.text.x = element_text(size = 20),
        legend.text = element_text(size = 16),
        legend.title = element_blank(),
        axis.title.y = element_text(size = 20),
        legend.position = c(0.85, 0.9),
        plot.margin = margin(5, 30, 5, 15))

cowplot::plot_grid(marker.scatter, no.marker.scatter, align = "h", nrow = 1, ncol = 2, labels = c("A", "B"))







# 
# marker.group <- action.props %>%
#   filter(prop > 0.5) %>%
#   .$language
# no.marker.group <- action.props %>%
#   filter(prop <= 0.5) %>%
#   .$language
# 
# spanish.example <- allPhon.extremes %>%
#   filter(language == "Basque")
# 
# lda.f1 <- list()
# f1 <- function(data, lev = NULL, model = NULL) {
#   f1_val <- F1_Score(y_pred = data$pred, y_true = data$obs, positive = lev[1])
#   c(F1 = f1_val)
# }
# 
# library(MLmetrics)
# 
# for(x in unique(allPhon.extremes$language)){
#   lang.frame <- all.extremes[[x]]
#   print(x)
#   if(x %in% names(lda.f1)){next}
#   lang.frame <- allPhon.extremes %>%
#     filter(language == x)
#   lang.frame <- filter(lang.frame, ontologicalCategory != "Other")
#   # onehot.encoding <- lang.frame %>%
#   #   mutate_at(c("ending", "start"), as.factor) %>%
#   #   caret::dummyVars(data = ., ~ ending + start)
#   lang.frame <- lang.frame %>%
#     # bind_cols(as_tibble(predict(object = onehot.encoding, newdata = lang.frame)))  %>%
#     select(ontologicalCategory, contains("ending"), contains("start")) %>%
#     mutate(ontologicalCategory = factor(ontologicalCategory))
#   lda.model <- caret::train(data = lang.frame, ontologicalCategory ~ ., method = "lda", metric = "F1",
#                             trControl = trainControl(classProbs = TRUE, summaryFunction = f1, method = "repeatedcv", number = 3, repeats = 100))
#   lda.f1[[x]] <- lda.model$results["F1"] %>% as_tibble %>% mutate(language = x)
#   # lda.model.preds <- predict(lda.model, newdata = lang.frame)
#   # predictions <- recode(lda.model.preds, Action = 1, Thing = 0)
#   # ground <- recode(lang.frame$ontologicalCategory, Action = 1, Thing = 0)
#   # mcc <- mltools::mcc(predictions, ground)
#   # lda.mcc[[x]] <- tibble(language = x, Matthews = mcc)
# }
# 
# lda.f1 <- bind_rows(lda.f1) %>%
#   mutate(language = unique(allPhon.extremes$language))
# 
# 
# spanish.example <- filter(spanish.example, ontologicalCategory != "Other")
# onehot.encoding <- spanish.example %>%
#   mutate_at(c("ending", "start"), as.factor) %>%
#   caret::dummyVars(data = ., ~ ending + start)
# spanish.example <- spanish.example %>%
#   bind_cols(as_tibble(predict(object = onehot.encoding, newdata = spanish.example)))  %>%
#   select(ontologicalCategory, contains("ending"), contains("start"), -ending, -start) %>%
#   mutate(ontologicalCategory = factor(ontologicalCategory))
# 
# lda.model <- caret::train(data = spanish.example, ontologicalCategory ~ ., method = "lda", trControl = trainControl(
#   method = "repeatedcv",
#   number = 3,
#   repeats = 100
# ))
# lda.model.preds <- predict(lda.model, newdata = spanish.example)
# predictions <- recode(lda.model.preds, Action = 1, Thing = 0)
# ground <- recode(spanish.example$ontologicalCategory, Action = 1, Thing = 0)
# mcc <- mltools::mcc(predictions, ground)
# 
# 
# spanish.example
# 
# 
# 
# spurt.splits <- caret::createDataPartition(spanish.example$ontologicalCategory, times = 100, p = 0.66)
# 
# x <- map(spurt.splits, function(trainpartition){
#   train <- spanish.example[trainpartition,]
#   onehot.encoding <- train %>%
#     mutate_at(c("ending", "start"), as.factor) %>%
#     caret::dummyVars(data = ., ~ ending + start)
#   train <- train %>%
#     bind_cols(as_tibble(predict(object = onehot.encoding, newdata = train))) %>%
#     select(ontologicalCategory, contains("ending"), contains("start"), -ending, -start) %>%
#     mutate(ontologicalCategory = factor(ontologicalCategory))
#   lda.model <- caret::train(data = train, ontologicalCategory ~ ., method = "nb")
#   print("e")
#   out.of.split <- setdiff(1:nrow(spanish.example), trainpartition)
#   test <- spanish.example[out.of.split,]
#   onehot.encoding <- test %>%
#     mutate_at(c("ending", "start"), as.factor) %>%
#     caret::dummyVars(data = ., ~ ending + start)
#   print("yo")
#   test <- test %>%
#     bind_cols(as_tibble(predict(object = onehot.encoding, newdata = test))) %>%
#     select(ontologicalCategory, contains("ending"), contains("start"), -ending, -start) %>%
#     mutate(ontologicalCategory = factor(ontologicalCategory))
#   test <- test[, colnames(train)]
#   prediction <- predict(lda.model, test)
#   mcc <- prediction %>%
#     recode(Thing = 0, Action = 1) %>% mltools::mcc(preds = ., actual = recode(spanish.example$ontologicalCategory[out.of.split], Action = 1, Thing = 0))
#   print(mcc)
#   return(mcc)
# })
# 
# 
# rnn.f1 %>% filter(language %in% names(all.extremes)) %>% left_join(lda.f1) %>% left_join(rename(phonLanguages, language = Name)) %>% mutate(F1 = ifelse(is.na(F1), 0, F1)) %>% mutate(diff = rnn.F1 - F1) %>% plotly::plot_ly(
#   type = "bar",
#   data = .,
#   x = ~ language,
#   y = ~ diff,
#   hoverinfo = "text",
#   text = ~ paste(
#     "Language =",
#     language,
#     ";\nLDA F1 = ",
#     round(F1, 2),
#     ";\nRNN F1 = ",
#     round(rnn.F1, 2)
#   )
# ) %>%  plotly::layout(title = "Difference between RNN performance and LDA with edge phonemes performance") %>% htmlwidgets::saveWidget("rnn_lda_difference_plot.html", selfcontained = TRUE)
# 
# 
# # THIS BREAKS -> they share variables and it cant do it.
