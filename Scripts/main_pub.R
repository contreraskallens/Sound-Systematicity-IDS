source("functions.r")  # Functions and packages
source("data_wrangling.R")  # Distances and typicality data

library(ggpointdensity)
library(tidyverse)
library(rnaturalearth)
library(geosphere)
library(sf)
library(cowplot)
library(MetBrewer)

all.phon.adjusted <- filter(all.phon.adjusted, language != 'Puinave')
all.languages <- read_csv('../data/Processed/all_language_info.csv') %>% 
  filter(Name %in% all.phon.adjusted$language)

palette_a_t <- met.brewer('Hiroshige', n = 2)
names(palette_a_t) <- c('Thing', 'Action')
palette_other <- met.brewer('Hokusai1', n = 4)
palette_line <- c("#28313d")
palette_world <-  met.brewer('Hokusai3', n=6)[c(2,4)]
palette_con <-  viridis::plasma(10)

# Basic descriptive statistics of the features of wordlists ------

# Make it only with languages that have geographical information
reduced <- select(all.languages, Name,longitude, latitude) %>% 
  filter(complete.cases(.))

# Make plot World plot

world <- ne_download(type = 'land', category = 'physical', returnclass = 'sf')
ocean <- ne_download(type = 'ocean', category = 'physical', returnclass = 'sf')
world.map <- ggplot() +  
  geom_sf(data = world, color = palette_line[1], fill = palette_world[1], size = 0.5) +
  geom_sf(data = ocean, color = palette_line[1], fill = palette_world[2], size = 0.5) +
  geom_jitter(data = phon.languages, 
              aes(x = longitude, y = latitude),
              size = 1, shape = 22, fill = palette_other[2]) +
  ylim(c(-55, 70)) + 
  theme_void()
ggsave(plot = world.map, "../results/Figures/Main/world_map.png", width = 18, height = 7, units = "cm", dpi = 300,
       bg = 'white')

# Look at families as a proportion of total families in wals
total.families.wals <- na.omit(wals_info$family) %>% 
  unique() %>% 
  length()
families.in.data <- wals %>% 
  filter(!(is.na(wals_code))) %>% 
  left_join(select(wals_info, wals_code, family))
families.in.data

# Percentage of families in wals that our overlapping datasets have
(length(unique(families.in.data$family)) / total.families.wals) * 100


## Mean and SD number of words
all.phon.adjusted %>%
  group_by(language) %>%
  tally() %>%
  dplyr::summarize(mean.number.words = mean(n),
                   sd.number.words = sd(n),
                   minimum = min(n),
                   maximum = max(n))

## Distribution of number of words
all.phon.adjusted %>%
  group_by(language) %>%
  tally() %>%
  rename(number.of.words = n) %>%
  ggplot(aes(x = number.of.words)) +
  geom_histogram(fill = palette_other[2], binwidth = 50, color = palette_line) +
  cowplot::theme_cowplot() +
  labs(x = "Number of Words", y = "Count", title = "Number of Words per Language", subtitle = "Adjusted")
ggsave("../results/Figures/Other/number_items_adjusted.png", bg = 'white')


# Category tally
all.phon.adjusted %>%
  group_by(ontological.category) %>%
  tally() %>%
  mutate(proportion = n / sum(.$n))

# Plot the proportions of each category in the languages as boxplots
all.phon.adjusted %>%
  group_by(language, ontological.category) %>%
  tally() %>%
  group_by(language) %>%
  mutate(proportion = n / sum(n),
    ontological.category = factor(ontological.category, levels = c("Thing", "Action"))) %>%
  ggplot(aes(x = ontological.category, y = proportion, fill = ontological.category)) +
  geom_boxplot(width = 0.3, color = palette_line) +
  scale_fill_manual(name = "Category", values = palette_a_t) + 
  cowplot::theme_cowplot() +
  labs(y = "Proportion", x = "Ontological Category", title = "Within language proportion of words of each category", 
       subtitle = "Adjusted")
ggsave("../results/Figures/Other/within_language_category_adjusted.png", bg = 'white')

# Typicality tests and visualization ------

# Get typicality stats for each class in each language

all.distances.adjusted %>%
  group_by(class) %>%
  dplyr::summarize(median.typicality = median(typicality),
                   Inter.Quantile.Range = IQR(typicality))
all.distances.adjusted <- mutate(all.distances.adjusted, class = factor(class,
                                                                        levels = c('Thing', 'Action')),
                                 length = nchar(word))



# Pairwise wilcox test of the difference in typicality between Action and Things
wilcox.all.adjusted <- all.distances.adjusted %>%
  mutate(class = factor(class)) %>%
  coin::wilcox_test(data = ., typicality ~ class)

# Effect size of action/thing comparisons
# Equivalent of R ^ 2, see
# https://www.researchgate.net/profile/Catherine_Fritz2/51554230_Effect_Size_Estimates_Current_Use_Calculations_and_Interpretation/links/5844494108ae2d217566ce33.pdf, p. 12
Z.action.thing.adjusted <- wilcox.all.adjusted %>% 
  coin::statistic(type = "standardized")
print(Z.action.thing.adjusted)
eta.squared.action.thing.adjusted <- (Z.action.thing.adjusted ^ 2) / (nrow(filter(all.distances.adjusted, class != "Other")))
print(eta.squared.action.thing.adjusted)
# Also get Cohen's d of the two distributions
d.action.thing.adjusted <-effsize::cohen.d(data = filter(all.distances.adjusted, class != "Other"), typicality ~ class)
print(abs(d.action.thing.adjusted$estimate))

# Plot distances as 2D density

density.adjusted <- all.distances.adjusted %>%
  mutate(class = factor(class, levels = c("Action", "Thing"))) %>%
  ggplot(aes(x = mean.action, y = mean.thing)) +
  stat_pointdensity(aes(col = after_stat(ndensity)), size = 0.5, adjust = 0.3) + 
  scale_color_met_c(palette_name = 'Tam', direction = -1, name = 'Density') +
  geom_abline(intercept = c(0,0), linetype = 'dashed', color = "black") +
  labs(x = "Mean Distance to Actions", y = "Mean Distance to Things") +
  theme_classic() +
  scale_x_continuous(breaks = c(0.5, 0.6, 0.7, 0.8)) +
  scale_y_continuous(breaks = c(0.5, 0.6, 0.7, 0.8)) +
  facet_wrap(vars(class), ncol = 2)
density.adjusted
ggsave("../results/Figures/Main/density_adjusted.png", 
       width = 18, height = 9, units = "cm", dpi = 500, bg = 'white')

# Look at languages on an individual level ----

scatter.adjusted <- ggplot(data = plot.data.adjusted, aes(y = Median, x = language, color = Category, shape = Category,
                                                          group = Category)) +
  labs(x = 'Language', y = 'Median Typicality') +
  geom_point(aes(fill = Category), size = 1) +
  geom_vline(aes(xintercept = include), linewidth = 0.2, color = palette_line) + 
  geom_hline(linetype = 'solid', yintercept = 0, color = palette_line, linewidth = .2) +
  geom_segment(data = filter(pivot_wider(plot.data.adjusted, names_from = Category, values_from = Median), is.na(include)),
               aes(x = language, xend = language, y = Thing, yend = Action), inherit.aes = FALSE,
               linetype = 'dotted', linewidth = 0.25) +
  scale_color_manual(name = "Category", values = palette_a_t) + 
  scale_shape_manual(name = "Category", values = c(18, 17)) +
  scale_x_discrete(name = "", labels = label.text.adjusted) +
  theme_classic() +
  expand_limits(x = -1) +
  expand_limits(x = 201) +
  theme(
    axis.ticks.x = element_blank()) +
  guides(color = guide_legend(override.aes = list(size=4)))

scatter.adjusted
ggsave("../results/Figures/Main/scatter_adjusted.png", width = 18, height = 7, units = "cm", dpi = 500, bg = 'white')


# Get test statistics for test languages.
map2_dfr(test.languages, list(all.distances.adjusted), get.typicality.stats) %>% 
  arrange(desc(difference))

# Get all with significant stats

all.p <- map2_dfr(unique(all.phon.adjusted$language), 
                  list(all.distances.adjusted), 
                  function(x, y){print(x); return(get.typicality.stats(x, y))}) %>% 
  mutate(p.value.adj = p.adjust(p.value, method = "bonferroni")) %>% # Bonferroni adjusted
  mutate(sig = p.value.adj < 0.01)
table(all.p$sig)
mean(all.p$d)
sd(all.p$d)
hist(all.p$eta.squared, breaks = 25)


# Closest phonological neighbors ----

# Data is generated in data_wrangling.R

# Generate 99% normal confidence intervals for the proportion of words with same category nearest neighbor
neighbor.stats.adjusted <- repeated.neighbor.adjusted %>%
  group_by(language, ontological.category) %>%
  do(enframe(Hmisc::smean.cl.normal(.$proportion.of.hits, conf.int = 0.99))) %>%
  spread(name, value)
# Get stats per word type
neighbor.stats.adjusted %>% 
  group_by(ontological.category) %>% 
  dplyr::summarize(all.mean = mean(Mean * 100), sd = sd(Mean * 100))

# Use the number of words with same-class neighbors in the shuffle test as "random" baseline.

# Join the actual and the permuted neighbors per language, per category. Then,
# count what proportion of the 1000 permutations the actual neighbors have a
# lower or equal proportion of same-category neighbors as the null permutation.
# Use the LOWER boundary of the .99 confidence interval as the "actual" number
# to be compared with the permuted numbers.

neighbor.test.adjusted <- neighbor.stats.adjusted %>% 
  left_join(dplyr::select(neighbor.mc.adjusted, language, ontological.category, random = proportion.of.hits)) %>%
  mutate(is.higher = random >= Lower)
neighbor.test.adjusted <- neighbor.test.adjusted %>%
  group_by(language, ontological.category) %>%
  summarise(p = sum(is.higher) / 1000)

# Check statistics for reference languages. For "baseline" performance for each
# language, take upper boundary of bootstrapped MCs

random.neigh.stats %>% 
  rename(random = Upper) %>% 
  select(language, ontological.category, random) %>% 
  right_join(select(neighbor.stats.adjusted, Mean)) %>% 
  left_join(neighbor.test.adjusted) %>% 
  filter(language %in% test.languages) %>% 
  select(language, ontological.category, Mean, random, p) %>% 
  mutate(Mean = Mean * 100, random = random * 100)

# Check proportion of languages that have p < 0.01
neighbor.proportions.adjusted <- neighbor.test.adjusted %>%
  mutate(p = cut(p, breaks = c(0, 0.01, 1), include.lowest = TRUE, right = FALSE)) %>%
  group_by(p, ontological.category) %>%
  tally() %>%
  mutate(n = n / nrow(phon.languages), n = n * 100)
neighbor.proportions.adjusted

# Plot with bars for each category and each language.
# The height of each bar is the lower boundary of the 99% bootstrapped CI of the actual data
# minus upper boundary of 99% bootstrapped CI of random permutations.

neighbor.plot.adjusted <- neighbor.stats.adjusted %>%
  group_by() %>%
  left_join(select(random.neigh.stats, random = Upper)) %>%
  mutate(language = factor(language, 
                           levels = sorted.langs.adjusted$language, 
                            labels = sorted.langs.adjusted$language))

neighbor.adjusted <- neighbor.plot.adjusted %>%
  mutate(height = Lower - random) %>% 
  ggplot(aes(x = language, y = height, ymin = Lower, ymax = Upper, fill = ontological.category)) +
  geom_bar(stat = "identity", width = 1,
           linewidth = .25) +
  scale_x_discrete(name = "", labels = rep("", 226)) +
  facet_wrap(vars(ontological.category), ncol = 2) +
  scale_y_continuous(expand = c(0, 0), name = "Same Neighbor") +
  scale_fill_manual(values = palette_a_t) +
  theme_classic() +
  theme(legend.position = "none") + 
  expand_limits(x = -1) +
  expand_limits(x = 201) + 
  theme(axis.ticks.x = element_blank())
neighbor.adjusted

ggsave("../results/Figures/Main/neighbor_adjusted_diff.png", width = 18, height = 7, units = "cm", dpi = 500)


# RNN K-Fold -------

# Mark each result as whether the bootstrapped CI for the Matthews Correlation
# Coefficient include a baseline of 0.1 or not. 

## Descriptive stats
rnn.stats

rnn.stats <- rnn.stats %>% 
  arrange(desc(mean.auc)) %>% 
  mutate(language = str_remove_all(language, '/'))
rnn.stats$language <- factor(rnn.stats$language, levels = rnn.stats$language)

# kfold ----

rnn.tally <- rnn.stats %>% 
  mutate(significant_permut = ifelse(bot.ci > Base_Upper, TRUE, FALSE))

rnn.tally %>% 
  group_by(significant_permut) %>% 
  tally() %>% 
  mutate(percentage = (n / sum(n)) * 100)

rnn.tally

replacement_names <- c('A', 'B', 'C', 'D')
names(replacement_names) <- test.languages

# Check reference languages
rnn.tally %>% 
  filter(language %in% test.languages) 

rnn.stats %>% 
  arrange(desc(median.auc)) %>% 
  mutate(language = factor(language, levels = .$language),
         significant = ifelse(bot.ci > Base_Upper, TRUE, FALSE),
         lang.label = ifelse(language %in% test.languages, 
                             as.character(language), ""),
         include = ifelse(lang.label == "", NA, language)) %>% 
  ggplot(aes(x = language, y = median.auc, fill = significant)) +
  geom_ribbon(aes(x = 1:length(language), ymin = Base_Lower, ymax = Base_Upper), fill = 'red', inherit.aes = FALSE,
              alpha = 0.5) +
  geom_vline(aes(xintercept = include), linetype = "dotted", size = .5, color = palette_line) +
  geom_linerange(aes(ymin = bot.ci, ymax = top.ci), size = 0.5, color = palette_line) +
  geom_point(shape = 22, size = .75, color = palette_line) + 
  geom_hline(yintercept = 0.5) +
  scale_x_discrete(name = "Language", labels = function(x){
    ifelse(x %in% test.languages, 
           str_replace_all(string = as.character(x), pattern = fixed(replacement_names)), "")
  }) +
  expand_limits(x = 201) +
  theme_classic() +
  scale_fill_manual(name = "Significantly > baseline", values = c(palette_other[1], palette_world)) +
  scale_y_continuous(name = "Learning performance (AUC)", breaks = seq(0, 1, 0.1)) + 
  theme(axis.ticks.x = element_blank(),
        legend.position = "none") 

ggsave("../results/Figures/Main/rnn_kfold.png", width = 18, height = 9, units = "cm", dpi = 500, bg = 'white')

# RNN Spurt --------------------------------------------------------

spurt.stats

spurt.tally <- spurt.stats %>% 
  mutate(significant_permut = ifelse(bot.ci > Base_Upper, TRUE, FALSE)) 

spurt.tally %>% 
  group_by(significant_permut) %>% 
  tally() %>% 
  mutate(percentage = (n / sum(n)) * 100)

mean(rnn.stats$mean.auc)
sd(rnn.stats$mean.auc)

mean(spurt.stats$mean.auc)
sd(spurt.stats$mean.auc)

spurt.stats %>% 
  arrange(desc(median.auc)) %>% 
  mutate(language = factor(language, levels = .$language),
         significant = ifelse(bot.ci > Base_Upper, TRUE, FALSE),
         lang.label = ifelse(language %in% test.languages, 
                             as.character(language), ""),
         include = ifelse(lang.label == "", NA, language)) %>% 
  ggplot(aes(x = language, y = median.auc, fill = significant)) +
  geom_ribbon(aes(x = 1:length(language), ymin = Base_Lower, ymax = Base_Upper), fill = 'red', inherit.aes = FALSE,
              alpha = 0.5) +
  geom_vline(aes(xintercept = include), linetype = "dotted", size = .5, color = palette_line) +
  geom_linerange(aes(ymin = bot.ci, ymax = top.ci), size = 0.5, color = palette_line) +
  geom_point(shape = 22, size = .75, color = palette_line) + 
  geom_hline(yintercept = 0.5) +
  scale_x_discrete(name = "Language", labels = function(x){
    ifelse(x %in% test.languages, 
           str_replace_all(string = as.character(x), pattern = fixed(replacement_names)), "")
  }) +
  expand_limits(x = 201) +
  theme_classic() +
  scale_fill_manual(name = "Significantly > baseline", values = c(palette_other[1], palette_world)) +
  scale_y_continuous(name = "Learning performance (AUC)", breaks = seq(0, 1, 0.1)) + 
  theme(axis.ticks.x = element_blank(),
        legend.position = "none") 

ggsave("../results/Figures/Main/rnn_spurt.png", width = 18, height = 9, units = "cm", dpi = 500, bg = 'white')