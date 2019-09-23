# Packages ----------------------------------------------------------------

require(tidyverse)
require(stringdist)
require(caret)
require(wesanderson)

color.palette.dis <- c(wes_palette(name = 'Royal2'), wes_palette(name = 'Royal1'))
color.palette.cont <- wes_palette(name = 'Zissou1', type = 'continuous', n = 5)

set.seed(1)




# Load data -----------------------------------------------------

all.phon <- read_csv('Data/Processed/all_phon.csv')
phon.languages <- read_csv('Data/Processed/all_language_info.csv')

# Functions ---------------------------------------------------------------

get.language <- function(a.language, data = all.phon){
  # Extracts the dataframe of a specific language and subsets variables
  # Args:
  #   aLanguage: String with a language name, e.g. "English"
  #   data: complete data with all languages. By default, it looks for an object called "all.phon"
  # Returns: Tibble with only rows corresponding to specified
  # language and columns for Phon form, Meaning and Class.
  data %>%
    dplyr::select(Form, phon, english.name, 
                  language, ontological.category) %>%
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
           englishName = language.df$english.name,
           Form = language.df$Form)
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
marker.group <- list()

ending.census <- list()

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
  ending.census[[i]] <- add_column(markers, ending.markers = i)
  langs.with.markers <- markers %>% 
    filter(marker == TRUE) %>% 
    .$language %>% 
    unique()
  marker.group[[i]] <- c(langs.with.markers)
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

ending.census <- ending.census %>% 
  bind_rows() %>% 
  filter(marker == TRUE) %>% 
  group_by(language, ontological.category) %>% 
  summarize(ending.markers = max(ending.markers))

start.langs <- list()
start.census <- list()


for(i in 1:4){
  # 3 passes gets rid of all of them
  print(i)
  markers <- purrr::map_dfr(sort(unique(all.phon.adjusted$language)), function(lang){
    lang.df <- all.phon.adjusted %>% 
      filter(language == lang)
    map_dfr(c("Action", "Thing", "Other"), function(category){
      cat.df <- lang.df %>% 
        filter(ontological.category == category)
      number <- nrow(cat.df)
      proportions <- cat.df %>% 
        mutate(start = str_sub(phon, "1", "1")) %>% 
        .$start %>% 
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
  start.census[[i]] <- add_column(markers, start.markers = i)
  
  langs.with.markers <- markers %>% 
    filter(marker == TRUE) %>% 
    .$language %>% 
    unique()
  print(langs.with.markers)
  start.langs[[i]] <- c(langs.with.markers)
  all.phon.adjusted <- map_dfr(1:nrow(markers), function(index){
    lang <- markers$language[index]
    category <- markers$ontological.category[index]
    has.marker <- markers$marker[index]
    if(has.marker){
      all.phon.adjusted %>% 
        filter(lang == language, ontological.category == category) %>% 
        mutate(phon = str_sub(phon, "2", "-1")) %>% 
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

start.census <- start.census %>% 
  bind_rows() %>% 
  filter(marker == TRUE) %>% 
  group_by(language, ontological.category) %>% 
  summarize(start.markers = max(start.markers))


marker.census <- full_join(start.census, ending.census)

all.marker.group <- c(unique(unlist(marker.group)), unique(unlist(start.langs))) %>% 
  unique()
no.marker.group <- setdiff(unique(all.phon.adjusted$language), all.marker.group)

# remove within-class homophones

homophone.census <- all.phon.adjusted %>% 
  group_by(language, ontological.category, phon) %>% 
  tally() %>% 
  filter(n > 1) %>% 
  group_by() %>% 
  group_by(language, ontological.category) %>% 
  tally()

all.phon.adjusted <- all.phon.adjusted %>% 
  distinct(language, ontological.category, phon, .keep_all = TRUE)

cross.homophone.census <- all.phon.adjusted %>% 
  filter(ontological.category != "Other") %>%  # "Other" homophones don't interfere with rnn results
  group_by(language, phon) %>% 
  tally() %>% 
  filter(n > 1) %>% 
  group_by() %>%
  group_by(language) %>%
  tally()

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

all.distances.adjusted <- map_dfr(.x = unique(all.phon.adjusted$language),
                                  .f = function(language.name) {
                                    language <- get.language(a.language = language.name, 
                                                             data = all.phon.adjusted)
                                    distances <- get.distance.matrix(language)
                                    mean.distances <-
                                      get.mean.distances(language.df = language, 
                                                         distance.matrix = distances) %>%
                                      mutate(language = language.name)
                                    return(mean.distances)
                                  }
)


# Get tipicality stats for each class in each language

all.distances %>%
  group_by(class) %>%
  summarize(median.typicality = median(typicality))

# language.typicality.category <- all.distances %>%
#   group_by(language, class) %>%
#   summarise(mean.typicality = mean(typicality), sd.typicality = sd(typicality))
# 


# Kruskal-wallis

three.way.test <- all.distances %>%
  mutate(class = factor(class)) %>%
  coin::kruskal_test(data = ., typicality ~ class)

# get effect size of k-w according to http://tss.awf.poznan.pl/files/3_Trends_Vol21_2014__no1_20.pdf
chi.square.original <- three.way.test@statistic@teststatistic
eta.squared.original <- (chi.square.original - 4) / (nrow(all.distances) - 3)
three.way.test.adjusted <- all.distances.adjusted %>%
  mutate(class = factor(class)) %>%
  coin::kruskal_test(data = ., typicality ~ class)
chi.square.adjusted <- three.way.test.adjusted@statistic@teststatistic
eta.squared.adjusted <- (chi.square.adjusted - 4) / (nrow(all.distances.adjusted) - 3)


# pairwise wilcox action / thing

all.distances %>%
  filter(class != "Other") %>%
  mutate(class = factor(class)) %>%
  coin::wilcox_test(data = ., typicality ~ class)

all.distances.adjusted %>%
  filter(class != "Other") %>%
  mutate(class = factor(class)) %>%
  coin::wilcox_test(data = ., typicality ~ class)


# effect size of action/thing comparisons

Z.action.thing <- all.distances %>% 
  filter(class != "Other") %>%
  mutate(class = factor(class)) %>%
  coin::wilcox_test(data = ., typicality ~ class) %>%
  coin::statistic(type = "standardized")

eta.squared.action.thing.original <- (Z.action.thing ^ 2) / (nrow(filter(all.distances, class != "Other"))) # Equivalent of R ^ 2, see https://www.researchgate.net/profile/Catherine_Fritz2/publication/51554230_Effect_Size_Estimates_Current_Use_Calculations_and_Interpretation/links/5844494108ae2d217566ce33.pdf, p. 12

Z.action.thing.adusted <- all.distances.adjusted %>% 
  filter(class != "Other") %>%
  mutate(class = factor(class)) %>%
  coin::wilcox_test(data = ., typicality ~ class) %>%
  coin::statistic(type = "standardized")

eta.squared.action.thing.adjusted <- (Z.action.thing.adusted ^ 2) / (nrow(filter(all.distances.adjusted, class != "Other")))

d.action.thing.original <- effsize::cohen.d(data = filter(all.distances, class != "Other"), typicality ~ class)
d.action.thing.adjusted <-effsize::cohen.d(data = filter(all.distances.adjusted, class != "Other"), typicality ~ class)

# Plotting ----------------------------------------------------------------

# World map with languages

ggplot() +
  borders("world", colour=wes_palettes$Moonrise1[4], fill=wes_palettes$Darjeeling1[2]) + # create a layer of borders
  geom_jitter(aes(x = languagesX, y = languagesY), fill = wes_palettes$Darjeeling1[1],
              color = wes_palettes$Darjeeling2[5], size = 3, shape = 25) +

  cowplot::theme_map()

# 2D Density of all words

all.distances %>%
  # filter(class != "Other") %>%
  mutate(class = factor(class, levels = c("Action", "Thing", "Other"))) %>%
  ggplot(aes(x = mean.action, y = mean.thing, fill = stat(nlevel))) +
  stat_density2d(geom = 'polygon') +
  scale_fill_gradientn(colors = color.palette.cont, name = "Density") +
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = wes_palettes$Moonrise1[4], size = 1) +
  labs(x = expression(hat(ND[A])), y = expression(hat(ND[T]))) +
  cowplot::theme_cowplot() +
  facet_wrap(vars(class), ncol = 2) +
  cowplot::panel_border()

all.distances.adjusted %>%
  # filter(class != "Other") %>%
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
all.distances.adjusted %>%
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
all.distances.adjusted %>%
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


# all.distances.modes <- rename(select(all.distances, -word), action.original = mean.action,
#                               thing.original = mean.thing,
#                               typicality.original = typicality) %>% 
#   left_join(rename(select(all.distances.adjusted, -word), action.adjusted = mean.action, thing.adjusted = mean.thing, typicality.adjusted = typicality))
# 

# Animation of violin

# violin.animation <- all.distances.modes %>% 
#   select(class, contains("typicality")) %>% 
#   gather("Mode", "Typicality", -class) %>% 
#   mutate(class = factor(class, levels = c("Other", "Thing", "Action")),
#          Mode = factor(Mode, levels = c("typicality.original",
#                                         "typicality.adjusted"),
#                        labels = c("Original", "Adjusted"))) %>% 
#   ggplot(aes(x = class, y = Typicality, fill = class, group = class)) +
#     geom_violin(trim = TRUE) +
#     geom_boxplot(width = 0.1, outlier.alpha = 0, fill = "white", alpha = 0.5) +
#     theme(legend.position = "none") +
#     coord_flip() +
#     scale_fill_manual(name = "Category", values = c(wes_palettes$Darjeeling1[1], wes_palettes$Darjeeling1[5], wes_palettes$Darjeeling1[2])) +
#     ylab("Typicality") +
#     xlab("") +
#     cowplot::theme_cowplot() +
#     geom_hline(yintercept = 0, linetype = "dashed") +
#     theme(legend.position = "none") + 
#   gganimate::transition_states(Mode, transition_length = .5, state_length = 3) + 
#   gganimate::ease_aes('sine-in-out') +
#   labs(title = '{closest_state}')
# 
# gganimate::animate(violin.animation, width = 1000, height = 800)

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

sorted.langs <- data.for.scatter %>% # this one includes the difference in means
  spread(class, Mean) %>%
  mutate(difference = abs(Action - Thing)) %>%
  arrange(desc(abs(difference)))
sorted.langs.adjusted <- data.for.scatter.adjusted %>% # this one includes the difference in means
  spread(class, Mean) %>%
  mutate(difference = abs(Action - Thing)) %>%
  arrange(desc(abs(difference)))

data.for.scatter <- data.for.scatter %>%
  filter(class != "Other") %>%
  group_by() %>%
  mutate(language = factor(language, levels = sorted.langs$language), class = factor(class),
         include = ifelse(language %in% c("Waorani", "Danish","Mansi", "English", "Thai (Korat variety)", "Romani"), language, NA))
data.for.scatter.adjusted <- data.for.scatter.adjusted %>%
  filter(class != "Other") %>%
  group_by() %>%
  mutate(language = factor(language, levels = sorted.langs.adjusted$language), class = factor(class),
         include = ifelse(language %in% c("Waorani", "English", "Thai(Korat variety)", "Danish", "Mansi", "Romani"), language, NA))

# keep labels for max, min and median and English

label.text <- map_chr(sorted.langs$language, function(x){
  if(x %in% c("Waorani", "English", "Thai (Korat variety)", "Danish", "Mansi", "Romani")){
    return(x)
  } else{return("")}
})

label.text.adjusted <- map_chr(sorted.langs.adjusted$language, function(x){
  if(x %in% c("Waorani", "English", "Thai (Korat variety)", "Danish", "Mansi", "Romani")){
    return(x)
  } else{return("")}
})



ggplot(data = data.for.scatter, aes(y = Mean, x = language, color = class, shape = class,
                                    group = class)) +
  labs(x = 'Language',
       y = 'Mean Typicality') +
  geom_point(aes(fill = class), color = 'black', size = 3) +
  geom_vline(aes(xintercept = include)) + 
  geom_hline(linetype = 'solid', size = 1, yintercept = 0) +
  scale_color_manual(name = "Category", values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[1])) +
  scale_fill_manual(name = "Category", values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[1])) +
  scale_shape_manual(name = "Category", values = c(23, 24)) +
  scale_x_discrete(name = "", labels = label.text) +
  cowplot::theme_cowplot() + 
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text.x = element_text(size = 14)) +
  cowplot::background_grid() 

ggplot(data = data.for.scatter.adjusted, aes(y = Mean, x = language, color = class, shape = class,
                                    group = class)) +
  labs(x = 'Language',
       y = 'Mean Typicality') +
  geom_point(aes(fill = class), color = 'black', size = 3) +
  geom_vline(aes(xintercept = include)) + 
  geom_hline(linetype = 'solid', size = 1, yintercept = 0) +
  scale_color_manual(name = "Category", values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[1])) +
  scale_fill_manual(name = "Category", values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[1])) +
  scale_shape_manual(name = "Category", values = c(23, 24)) +
  scale_x_discrete(name = "", labels = label.text.adjusted) +
  cowplot::theme_cowplot() + 
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text.x = element_text(size = 14)) +
  cowplot::background_grid() 


# Animation of scatter

# scatter.anim <- all.scatters %>% 
#   gather("Mode", 
#          "Mean.Typicality", 
#          original:adjusted.homophones) %>% 
#   group_by() %>% 
#   mutate(language = factor(language, levels = sorted.langs$language),
#          Mode = factor(Mode, levels = c("original", "adjusted", "adjusted.homophones"))) %>% 
#   ggplot(aes(x = language, y = Mean.Typicality, group = language,
#              color = class, shape = class)) + 
#   geom_point(aes(fill = class), color = 'black', size = 3) +
#   geom_hline(linetype = 'solid', size = 1, yintercept = 0) +
#   scale_color_manual(name = "Category", values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[1])) +
#   scale_fill_manual(name = "Category", values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[1])) +
#   scale_shape_manual(name = "Category", values = c(23, 24)) + 
#   theme(axis.text.x = element_blank()) + 
#   gganimate::transition_states(Mode, transition_length = 2, state_length = 2) + 
#   gganimate::ease_aes('sine-in-out') + 
#   labs(title = '{closest_state}')
# 
# gganimate::animate(scatter.anim, width = 1500, height = 800)


# Closest phonological neighbors ---------------

# commented out lines of code used to generate saved da
# 
# all.phon.list <- all.phon %>%
#   split(.$language)
# all.phon.list <- all.phon.list[sort(names(all.phon.list))]
# all.distance.matrices <- purrr::map(all.phon.list, function(x){get.distance.matrix(x)})
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
# 
# all.phon.list.adjusted <- all.phon.adjusted %>%
#   split(.$language)
# all.phon.list.adjusted <- all.phon.list.adjusted[sort(names(all.phon.list.adjusted))]
# all.distance.matrices.adjusted <- purrr::map(all.phon.list.adjusted, function(x){get.distance.matrix(x)})
# 
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
# 
# repeated.neighbor.adjusted %>% write_rds("Data/Processed/adjusted_repeated_neighbor.Rds")

repeated.neighbor <- read_rds("Data/Processed/repeated_neighbor.Rds")
repeated.neighbor.adjusted <- read_rds("Data/Processed/adjusted_repeated_neighbor.Rds")

neighbor.stats <- repeated.neighbor %>%
  group_by(language, ontological.category) %>%
  do(enframe(Hmisc::smean.cl.normal(.$proportion.of.hits, conf.int = 0.999))) %>%
  spread(name, value)
neighbor.stats.adjusted <- repeated.neighbor.adjusted %>%
  group_by(language, ontological.category) %>%
  do(enframe(Hmisc::smean.cl.normal(.$proportion.of.hits, conf.int = 0.999))) %>%
  spread(name, value)
# 
# # load multicore mc
# 
# require(future)
# require(furrr)
# 
# future::plan(multiprocess)
# cores <- 6
# options(future.globals.maxSize = +Inf, mc.cores = cores)
# 
# neighbor.mc <- furrr::future_map2_dfr(.progress = TRUE, .x = all.phon.list, .y = all.distance.matrices,
#                         .f = function(language, distance.matrix){
#                           # print(language)
#                           gc()
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
#                             # group_by(language, ontological.category, permutation) %>%
#                             # summarize(Mean = mean(proportion.of.hits),
#                             #           Standard.Dev = sd(proportion.of.hits)
#                             #           Upper = (Mean + sd(proportion.of.hits)),
#                             #           Lower = (Mean - sd(proportion.of.hits))) %>%
#                             return()
# })
# 
# neighbor.mc %>%
# write_rds("Data/Processed/neighbor_mc.Rds")
# 
# neighbor.mc.adjusted <- furrr::future_map2_dfr(.progress = TRUE, 
#                                                .x = all.phon.list.adjusted, 
#                                                .y = all.distance.matrices.adjusted,
#                                                .f = function(language, distance.matrix){
#                                         # print(language)
#                                         gc()
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
#                                           # group_by(language, ontological.category, permutation) %>%
#                                           # summarize(Mean = mean(proportion.of.hits),
#                                           #           Standard.Dev = sd(proportion.of.hits)
#                                           #           Upper = (Mean + sd(proportion.of.hits)),
#                                           #           Lower = (Mean - sd(proportion.of.hits))) %>%
#                                           return()
#                                       })
# 
# neighbor.mc.adjusted %>%
# write_rds("Data/Processed/adjusted_neighbor_mc.Rds")

neighbor.mc <- read_rds("Data/Processed/neighbor_mc.Rds")
neighbor.mc.adjusted <- read_rds("Data/Processed/adjusted_neighbor_mc.Rds")
  

neighbor.mc <- neighbor.mc %>%
  rename(random = proportion.of.hits)
neighbor.mc.adjusted <- neighbor.mc.adjusted %>%
  rename(random = proportion.of.hits)
neighbor.test <- repeated.neighbor %>%
  group_by(language, ontological.category) %>%
  summarize(proportionOfHits = mean(proportion.of.hits)) %>%
  left_join(neighbor.mc) %>%
  mutate(is.higher = random >= proportionOfHits)
neighbor.test <- neighbor.test %>%
  group_by(language, ontological.category) %>%
  summarize(p = sum(is.higher) / 1000)
neighbor.test.adjusted <- repeated.neighbor.adjusted %>%
  group_by(language, ontological.category) %>%
  summarize(proportionOfHits = mean(proportion.of.hits)) %>%
  left_join(neighbor.mc) %>%
  mutate(is.higher = random >= proportionOfHits)
neighbor.test.adjusted <- neighbor.test.adjusted %>%
  group_by(language, ontological.category) %>%
  summarize(p = sum(is.higher) / 1000)

neighbor.test %>%
  mutate(p = cut(p, breaks = c(0, 0.05, 1), include.lowest = TRUE, right = FALSE)) %>%
  group_by(p, ontological.category) %>%
  tally() %>%
  mutate(n = n / nrow(phon.languages))
neighbor.test.adjusted %>%
  mutate(p = cut(p, breaks = c(0, 0.05, 1), include.lowest = TRUE, right = FALSE)) %>%
  group_by(p, ontological.category) %>%
  tally() %>%
  mutate(n = n / nrow(phon.languages))

# plot with bars

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

neighbor.mc.plot.adjusted <- neighbor.mc.adjusted %>%
  filter(ontological.category != "Other") %>%
  group_by(language, ontological.category) %>%
  summarize(random = mean(random) + sd(random))

neighbor.plot.adjusted <- neighbor.stats.adjusted %>%
  group_by() %>%
  filter(ontological.category != "Other") %>%
  left_join(neighbor.mc.plot.adjusted) %>%
  mutate(language = factor(language, levels = sorted.langs.adjusted$language, labels = sorted.langs.adjusted$language))

labels.for.plot <- map_chr(levels(neighbor.plot$language), function(language){
  if(language %in% c("Mansi", 
                     "English", 
                     "Thai (Korat variety)", 
                     "Romani",
                     "Danish",
                     "Waorani")){return(language)} else{return("")}
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

labels.for.plot.adjusted <- map_chr(levels(neighbor.plot.adjusted$language), function(language){
  if(language %in% c("Mansi", 
                     "English", 
                     "Thai (Korat variety)", 
                     "Romani",
                     "Danish",
                     "Waorani")){return(language)} else{return("")}
})

neighbor.plot.adjusted %>%
  ggplot(aes(x = language, y = Mean, ymin = Lower, ymax = Upper, fill = ontological.category)) +
  geom_bar(stat = "identity", width = 1,
           size = .25, color = "black") +
  geom_bar(mapping = aes(y = random), fill = "yellow", stat = "identity", width = 1,
           alpha = .8, color = "black") +
  # geom_point(shape = 18, size = 2) +
  scale_x_discrete(name = "", labels = labels.for.plot.adjusted) +
  facet_wrap(vars(ontological.category), ncol = 1) +
  scale_y_continuous(expand = c(0, 0), name = "Proportion of same neighbor") +
  scale_fill_manual(values = c(wes_palettes$Darjeeling1[2], wes_palettes$Darjeeling1[1])) +
  cowplot::theme_cowplot() +
  cowplot::panel_border() +
  theme(legend.position = "none")

# Hand-picked languages

smaller.mc <- neighbor.mc %>%
  filter(language %in% c("Mansi", 
                         "Romani", 
                         "English", 
                         "Thai (Korat variety)",
                         "Danish",
                         "Waorani")) %>%
  group_by(language, ontological.category) %>%
  summarize(random = mean(random) + sd(random))

smaller.neighbor <- neighbor.stats %>%
  filter(language %in% c("Mansi",
                         "Romani", 
                         "English", 
                         "Thai (Korat variety)",
                         "Danish",
                         "Waorani"))

smaller.neighbor %>%
  left_join(smaller.mc) %>%
  group_by() %>% 
  mutate(language = factor(language, levels = c("Mansi", 
                                                "Romani", 
                                                "English", 
                                                "Thai (Korat variety)",
                                                "Danish",
                                                "Waorani"))) %>%
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


smaller.mc.adjusted <- neighbor.mc.adjusted %>%
  filter(language %in% c("Mansi", 
                         "Romani", 
                         "English", 
                         "Thai (Korat variety)",
                         "Danish",
                         "Waorani")) %>%
  group_by(language, ontological.category) %>%
  summarize(random = mean(random) + sd(random))

smaller.neighbor.adjusted <- neighbor.stats.adjusted %>%
  filter(language %in% c("Mansi",
                         "Romani", 
                         "English", 
                         "Thai (Korat variety)",
                         "Danish",
                         "Waorani"))

smaller.neighbor.adjusted %>%
  left_join(smaller.mc.adjusted) %>%
  group_by() %>% 
  mutate(language = factor(language, levels = c("Mansi", 
                                                "Romani", 
                                                "English", 
                                                "Thai (Korat variety)",
                                                "Danish",
                                                "Waorani"))) %>%
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


# RNN -------

# Load NN results --------------------------------------------------------


rnn.means <- function(data, indices){
  these.data <- data[indices,] %>% 
    select(-X1, -language)
  return(colMeans(these.data))
}

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

rnn.performance <- list()

for(file in list.files('Results/ten-fold-original//', recursive = T, full.names = T)){
  language <- str_extract(file, "(?<=Results/ten-fold-original//).+(?=_rnn_)")
  print(language)
  rnn.performance[[language]] <- read_csv(file)
  rnn.performance[[language]]$language <- language
}

# rnn.stats <- purrr::map_dfr(rnn.performance, bootstrapped.cis)
# 
# rnn.stats %>%
#   write_rds("Data/Processed/boot_ci_original.rds")

rnn.stats <- read_rds("Data/Processed/boot_ci_original.rds")

rnn.performance.adjusted <- list()

for(file in list.files('Results/ten-fold/', recursive = T, full.names = T)){
  language <- str_extract(file, "(?<=Results/ten-fold/).+(?=_rnn_)")
  print(language)
  rnn.performance.adjusted[[language]] <- read_csv(file)
  rnn.performance.adjusted[[language]]$language <- language
}


# rnn.stats.adjusted <- purrr::map_dfr(rnn.performance.adjusted, bootstrapped.cis) 
# rnn.stats.adjusted %>%
#   write_rds("Data/Processed/adjusted_boot_ci_original.rds")

rnn.stats.adjusted <- read_rds("Data/Processed/adjusted_boot_ci_original.rds")

# Mashco Piro and Waorani have a small error because they have 100% thing accuracy all the time.

matthews.plot <- rnn.stats %>% 
  arrange(desc(Matthews)) %>% 
  mutate(Language = factor(Language, levels = .$Language),
         includes.baseline = ifelse(Matthews.Lower < 0.1, "Includes 0.1", "Doesn't Include 0.1"),
         lang.label = ifelse(Language %in% c("Mansi", "English", "Romani", "Thai (Korat variety)", "Danish", "Waorani"), 
                        as.character(Language), ""),
         include = ifelse(lang.label == "", NA, Language))


ggplot(data = matthews.plot, aes(x = Language, ymin = Matthews.Lower, ymax = Matthews.Upper,
             y = Matthews, fill = includes.baseline, label = lang.label)) + 
  geom_vline(aes(xintercept = include)) + 
  geom_crossbar() +
  geom_hline(yintercept = 0, size = 2) + 
  geom_hline(yintercept = 0.1, linetype = "dashed", size = 1.5) + 
  # geom_pointrange(size = .9) +
  scale_x_discrete(name = "Language", labels = matthews.plot$lang.label) + 
  cowplot::theme_cowplot()

matthews.plot.adjusted <- rnn.stats.adjusted %>% 
  arrange(desc(Matthews)) %>% 
  mutate(Language = factor(Language, levels = .$Language),
         includes.baseline = ifelse(Matthews.Lower < 0.1, "Includes 0.1", "Doesn't Include 0.1"),
         lang.label = ifelse(Language %in% c("Mansi", "English", "Romani", "Thai (Korat variety)", "Danish", "Waorani"), 
                             as.character(Language), ""),
         include = ifelse(lang.label == "", NA, Language))


ggplot(data = matthews.plot.adjusted, aes(x = Language, ymin = Matthews.Lower, ymax = Matthews.Upper,
                                 y = Matthews, fill = includes.baseline, label = lang.label)) + 
  geom_vline(aes(xintercept = include)) + 
  geom_crossbar() +
  geom_hline(yintercept = 0, size = 2) + 
  geom_hline(yintercept = 0.1, linetype = "dashed", size = 1.5) + 
  # geom_pointrange(size = .9) +
  scale_x_discrete(name = "Language", labels = matthews.plot.adjusted$lang.label) + 
  cowplot::theme_cowplot()

left_join(select(rnn.stats, Language, Original = Matthews),
          select(rnn.stats.adjusted, Language, Adjusted = Matthews)) %>% 
  filter(Language %in% no.marker.group) %>% 
  mutate(difference = abs(Original - Adjusted)) %>% 
  arrange(desc(difference)) %>% 
  mutate(Language = factor(Language, levels = .$Language)) %>% 
  ggplot() + 
  geom_segment(aes(x = Language, xend = Language, y = Original, yend = Adjusted)) +
  geom_point(aes(x = Language, y = Original), color = "red", shape = 16) + 
  geom_point(aes(x = Language, y = Adjusted), color = "blue", shape = 17) +
  geom_hline(yintercept = 0.1, linetype = "dashed") + 
  geom_hline(yintercept = 0)

# count the number of languages whose bootstrapped 99% ci include 0.1

rnn.stats %>% 
  select(Language, contains("Matthews")) %>% 
  mutate(includes.baseline = ifelse(Matthews.Lower < 0.1, TRUE, FALSE)) %>% 
  group_by(includes.baseline) %>% 
  tally()
rnn.stats.adjusted %>% 
  select(Language, contains("Matthews")) %>% 
  mutate(includes.baseline = ifelse(Matthews.Lower < 0.1, TRUE, FALSE)) %>% 
  group_by(includes.baseline) %>% 
  tally()

# Alternative analysis: one-sided wilcox test
# 

wilcox.tests <- lapply(rnn.performance, function(stats){
  return(wilcox.test(x = stats$Matthews, mu = 0.1, alternative = "greater"))
})
wilcox.tests <- lapply(wilcox.tests, function(stats){
  if(is.nan(stats[["p.value"]]))
  {return(FALSE)}
  pvalue <- stats[["p.value"]]
  # pvalue <- p.adjust(p = pvalue, method = "bonferroni", n = 227) # bonferroni for the repeated 3-fold in evolang
  if(pvalue >= 0.01){return(FALSE)} else{return(TRUE)}
})
wilcox.tests <- bind_rows(wilcox.tests) %>%
  gather() %>%
  rename(language = key, significant = value)
wilcox.tests %>%
  summarize(number.significant = sum(significant))

wilcox.tests.adjusted <- lapply(rnn.performance.adjusted, function(stats){
  return(wilcox.test(x = stats$Matthews, mu = 0.1, alternative = "greater"))
})
wilcox.tests.adjusted <- lapply(wilcox.tests.adjusted, function(stats){
  if(is.nan(stats[["p.value"]]))
  {return(FALSE)}
  pvalue <- stats[["p.value"]]
  # pvalue <- p.adjust(p = pvalue, method = "bonferroni", n = 227) # bonferroni for the repeated 3-fold in evolang
  if(pvalue >= 0.01){return(FALSE)} else{return(TRUE)}
})
wilcox.tests.adjusted <- bind_rows(wilcox.tests.adjusted) %>%
  gather() %>%
  rename(language = key, significant = value)
wilcox.tests.adjusted %>%
  summarize(number.significant = sum(significant))


# bar plot

rnn.stats %>%
  bind_rows() %>%
  arrange(desc(Matthews)) %>% 
  mutate(Language = factor(Language, levels = .$Language)) %>% 
  rename(MCC = Matthews) %>%
  ggplot(aes(x = Language, y = MCC)) +
  geom_col(fill = wes_palettes$Darjeeling1[2], color = "black") + 
  cowplot::theme_cowplot() +
  cowplot::panel_border() + 
  theme(legend.position = "none") + 
  scale_fill_manual(values = c(wes_palettes$Darjeeling1[1], wes_palettes$Darjeeling1[2])) +
  geom_hline(yintercept = 0.1, lwd = 2, linetype = "dashed") +
  scale_y_continuous(name = "Mean Matthew's Correlation Coefficient", breaks = c(0.25, 0.5, 0.75, 1)) + 
  labs(x = "Language") + 
  theme(axis.text.x = element_blank(),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 20),
        axis.ticks.x = element_blank())
rnn.stats.adjusted %>%
  bind_rows() %>%
  arrange(desc(Matthews)) %>% 
  mutate(Language = factor(Language, levels = .$Language)) %>% 
  rename(MCC = Matthews) %>%
  ggplot(aes(x = Language, y = MCC)) +
  geom_col(fill = wes_palettes$Darjeeling1[2], color = "black") + 
  cowplot::theme_cowplot() +
  cowplot::panel_border() + 
  theme(legend.position = "none") + 
  scale_fill_manual(values = c(wes_palettes$Darjeeling1[1], wes_palettes$Darjeeling1[2])) +
  geom_hline(yintercept = 0.1, lwd = 2, linetype = "dashed") +
  scale_y_continuous(name = "Mean Matthew's Correlation Coefficient", breaks = c(0.25, 0.5, 0.75, 1)) + 
  labs(x = "Language") + 
  theme(axis.text.x = element_blank(),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 20),
        axis.ticks.x = element_blank())

full.comparison <- left_join(select(rnn.stats, Language, contains("Matthews")),
                              select(rnn.stats.adjusted, Language, contains("Matthews")),
                        by = c("Language"), suffix = c(".Original", ".Adjusted"))
sorted.rnn <- full.comparison %>% 
  arrange(desc(Matthews.Original))


original.stats <- full.comparison %>% 
  select(Language, contains("Original")) %>% 
  rename(Matthews = Matthews.Original, Upper = Matthews.Upper.Original, Lower = Matthews.Lower.Original) %>% 
  add_column(Mode = "Original") %>% 
  mutate(contains.baseline = ifelse(Lower <= 0.1, TRUE, FALSE),
         Language = factor(Language, levels = sorted.rnn$Language))
adjusted.stats <- full.comparison %>% 
  select(Language, contains("Adjusted")) %>% 
  rename(Matthews = Matthews.Adjusted, Upper = Matthews.Upper.Adjusted, Lower = Matthews.Lower.Adjusted) %>% 
  add_column(Mode = "Adjusted") %>% 
  mutate(contains.baseline = ifelse(Lower <= 0.1, TRUE, FALSE),
         Language = factor(Language, levels = sorted.rnn$Language))

animation <- bind_rows(original.stats, adjusted.stats) %>% 
  mutate(Mode = factor(Mode, levels = c("Original", "Adjusted"))) %>% 
  ggplot(aes(x = Language, y = Matthews, ymin = Lower, ymax = Upper, group = Language, fill = contains.baseline)) +
  geom_crossbar() +
  gganimate::transition_states(Mode, 
                               transition_length = 3, 
                               state_length = 2) + 
  gganimate::ease_aes('sine-in-out') + 
  cowplot::theme_cowplot() + 
  theme(axis.text.x = element_blank()) + 
  geom_hline(yintercept = 0.1, linetype = "dashed", size = 1.5)

gganimate::animate(animation, width = 1500, height = 800)

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

























# #- Evolang significance testing -----
# wilcox.tests <- map(sort(unique(all.phon$language)), function(x){
#   lang.distances <- all.distances %>% 
#     filter(class != "Other", language == x) %>%
#     mutate(class = factor(class))
#   return(coin::wilcox_test(data = lang.distances, typicality ~ class))
# })
# 
# names(wilcox.tests) <- sort(unique(all.phon$language))
# significances.marker <- map_dbl(wilcox.tests[marker.group], function(x){
#   return(coin::pvalue(x))
# })
# 
# adjusted <- significances.marker %>% p.adjust(method = "bonferroni", n = 227)
# 
# sum(adjusted < 0.001)
# 
# 
# cohen.marker <- map_dbl(sort(marker.group), function(x){
#   lang.distances <- all.distances %>% 
#     filter(class != "Other", language == x) %>%
#     mutate(class = factor(class))
#   effsize::cohen.d(data = lang.distances, typicality ~ class) %>% 
#            .$estimate %>% 
#     return()
# })
# 
# cohen.marker %>% median()
# 
# 
# significances.no.marker <- map_dbl(wilcox.tests[no.marker.group], function(x){
#   return(coin::pvalue(x))
# })
# 
# adjusted.no.marker <- significances.no.marker %>% p.adjust(method = "bonferroni", n = 227)
# 
# sum(adjusted.no.marker < 0.001)
# 
# cohen.no.marker <- map_dbl(sort(no.marker.group), function(x){
#   lang.distances <- all.distances %>% 
#     filter(class != "Other", language == x) %>%
#     mutate(class = factor(class))
#   effsize::cohen.d(data = lang.distances, typicality ~ class) %>% 
#     .$estimate %>% 
#     return()
# })
# 
# cohen.no.marker %>% median
# 
# significances.marker / length(significances.marker)


# Word Edges -----

# all.phon.extremes <- all.phon %>%
#   mutate(ending = str_sub(phon, -1), start = str_sub(phon, 1, 1))
# 
# all.extremes <- map(sort(unique(all.phon.extremes$language)), function(x){
#   lang <- all.phon.extremes %>%
#     filter(language == x) %>%
#     select(ontological.category, ending, start) %>%
#     mutate_at(c("ending", "start"), as.factor)
#   dummies <- caret::dummyVars(data = lang, ~ ending + start)
#   lang <- lang %>%
#     bind_cols(as_tibble(predict(object = dummies, newdata = lang))) %>%
#     select(ontological.category, contains("ending"), contains("start"), -ending, -start) %>%
#     mutate(ontological.category = factor(ontological.category))
#   return(lang)
# })
# names(all.extremes) <- sort(unique(all.phon.extremes$language))
# 
# action.props <- map_dfr(names(all.extremes), function(lang){
#   lang.name <- lang
#   lang <- all.extremes[[lang]]
#   tally <- lang %>%
#     group_by(ontological.category) %>%
#     tally()
#   props.per.cat <- lang %>%
#     group_by(ontological.category) %>%
#     summarize_all(sum) %>%
#     left_join(tally) %>%
#     mutate_at(vars(-ontological.category, -n), funs(. / n))
#   action.prop <- props.per.cat %>%
#     select(-n)
#   action.prop <- action.prop %>%
#     add_column(language = lang.name)
#   return(action.prop)
# })
# boxplot in evolang
# 
# rnn.stats %>%
#   bind_rows() %>%
#   filter(language %in% phon.languages$Name) %>% mutate(marker = ifelse(language %in% marker.group, "Marker", "No Marker")) %>%
#   rename(MCC = Median) %>%
#   ggplot(aes(x = marker, y = MCC, fill = marker)) +
#   geom_boxplot(width = 0.5, lwd = 1) +
#   # cowplot::theme_cowplot() +
#   # cowplot::background_grid() +
#   theme_minimal() + 
#   theme(legend.position = "none") + 
#   scale_fill_manual(values = c(wes_palettes$Darjeeling1[1], wes_palettes$Darjeeling1[2])) +
#   geom_hline(yintercept = 0, lwd = 2, linetype = "dashed") +
#   scale_y_continuous(name = "Matthew's Correlation Coefficient", breaks = c(0.25, 0.5, 0.75, 1)) + 
#   theme(axis.text.x = element_text(size = 18),
#         axis.title.x = element_text(size = 20),
#         axis.text.y = element_text(size = 20),
#         axis.title.y = element_blank(),
#         plot.margin = margin(10, 15, 10, 10)) +
#   coord_flip()
# ggsave("Figures/Evolang/small_boxplot.png")
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


