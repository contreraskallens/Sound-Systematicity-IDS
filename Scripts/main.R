source("functions.r")  # Functions and packages
source("data_wrangling.R")  # Distances and typicality data


morph.complexity <- read_csv("../Data/Processed/morph_complexity.csv")

# Basic descriptive statistics of the features of wordlists ------


# Look at families as a proportion of total families in wals
total.families.wals <- na.omit(wals_info$family) %>% 
  unique() %>% 
  length()

families.in.data <- wals %>% 
  filter(!(is.na(wals_code))) %>% 
  left_join(select(wals_info, wals_code, family))

# Percentage of families in wals that our overlapping datasets have
(length(unique(families.in.data$family)) / total.families.wals) * 100


## Mean and SD number of words
all.phon.adjusted %>%
  group_by(language) %>%
  tally() %>%
  dplyr::summarize(mean.number.words = mean(n),
                   sd.number.words = sd(n))

## Distribution of number of words
all.phon.adjusted %>%
  group_by(language) %>%
  tally() %>%
  rename(number.of.words = n) %>%
  ggplot(aes(x = number.of.words)) +
  geom_histogram(fill = palette_other[2], binwidth = 50, color = palette_line) +
  cowplot::theme_cowplot() +
  labs(x = "Number of Words", y = "Count", title = "Number of Words per Language", subtitle = "Adjusted")
ggsave("../Figures/Other/number_items_adjusted.png")


## Category tally
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
    ontological.category = factor(ontological.category, levels = c("Thing", "Action", "Other"))) %>%
  ggplot(aes(x = ontological.category, y = proportion, fill = ontological.category)) +
  geom_boxplot(width = 0.3, color = palette_line) +
  scale_fill_manual(name = "Category", values = palette_a_t) + 
  cowplot::theme_cowplot() +
  labs(y = "Proportion", x = "Ontological Category", title = "Within language proportion of words of each category", 
       subtitle = "Adjusted")
ggsave("../Figures/Other/within_language_category_adjusted.png")

# Typicality tests and visualization ------

# Get tipicality stats for each class in each language

all.distances.adjusted %>%
  group_by(class) %>%
  dplyr::summarize(median.typicality = median(typicality),
                   Inter.Quantile.Range = IQR(typicality))

# Kruskal-wallis of all languages of typicality differences between Action,
# Thing and Other

three.way.test.adjusted <- all.distances.adjusted %>%
  mutate(class = factor(class)) %>%
  coin::kruskal_test(data = ., typicality ~ class)

print(three.way.test.adjusted)

# Get the effect size of k-w test according to
# http://tss.awf.poznan.pl/files/3_Trends_Vol21_2014__no1_20.pdf
chi.square.adjusted <- three.way.test.adjusted@statistic@teststatistic
eta.squared.adjusted <- (chi.square.adjusted - 4) / (nrow(all.distances.adjusted) - 3)

# Pairwise wilcox test of the difference in typicality between Action and Things
wilcox.all.adjusted <- all.distances.adjusted %>%
  filter(class != "Other") %>%
  mutate(class = factor(class)) %>%
  coin::wilcox_test(data = ., typicality ~ class)

# Effect size of action/thing comparisons
# Equivalent of R ^ 2, see
# https://www.researchgate.net/profile/Catherine_Fritz2/51554230_Effect_Size_Estimates_Current_Use_Calculations_and_Interpretation/links/5844494108ae2d217566ce33.pdf, p. 12
Z.action.thing.adjusted <- wilcox.all.adjusted %>% 
  coin::statistic(type = "standardized")
eta.squared.action.thing.adjusted <- (Z.action.thing.adjusted ^ 2) / (nrow(filter(all.distances.adjusted, class != "Other")))

# Also get Cohen's d of the two distributions
d.action.thing.adjusted <-effsize::cohen.d(data = filter(all.distances.adjusted, class != "Other"), typicality ~ class)
print(abs(d.action.thing.adjusted$estimate))

# Plot distances as 2D density

density.adjusted <- all.distances.adjusted %>%
  filter(class != "Other") %>%
  mutate(class = factor(class, levels = c("Action", "Thing", "Other"))) %>%
  ggplot(aes(x = mean.action, y = mean.thing)) +
  ggpointdensity::stat_pointdensity(aes(col = stat(ndensity)), size = 0.5, adjust = 0.3, method = "auto") + 
  viridis::scale_color_viridis(name = "Density", option = "plasma") +
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
        strip.text = element_text(size = 8))
density.adjusted
ggsave("../Figures/Main/density_adjusted.png", width = 17, height = 9, units = "cm", dpi = 900)


# Look at languages on an individual level ----

scatter.adjusted <- ggplot(data = plot.data.adjusted, aes(y = Median, x = language, color = Category, shape = Category,
                                                          group = Category)) +
  labs(x = 'Language', y = 'Mean Typicality') +
  geom_point(aes(fill = Category), size = 1) +
  geom_vline(aes(xintercept = include), size = 0.2, color = palette_line) + 
  geom_hline(linetype = 'solid', yintercept = 0, color = palette_line, size = .2) +
  scale_color_manual(name = "Category", values = c(palette_a_t[1], palette_a_t[2])) + 
  scale_shape_manual(name = "Category", values = c(18, 17)) +
  scale_x_discrete(name = "", labels = label.text.adjusted) +
  cowplot::theme_cowplot() + 
  expand_limits(x = -1) +
  expand_limits(x = 228) + 
  theme(axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 6),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    strip.text = element_text(size = 8),
    axis.ticks.x = element_blank()) +
  guides(color = guide_legend(override.aes = list(size=4)))

scatter.adjusted
ggsave("../Figures/Main/scatter_adjusted.png", width = 17, height = 7, units = "cm", dpi = 900)


# include complexity covariate

morph.complexity %>% 
  rename(language = Language) %>% 
  filter(number.not.na > 3) %>% 
  left_join(plot.data.adjusted) %>% 
  lm(data = ., Median ~ Category * complexity) %>% 
  summary()
  
# Get test statistics for test languages.
map2_dfr(test.languages, list(all.distances.adjusted), get.typicality.stats) %>% 
  arrange(desc(difference))

# Get all with significant stats
all.p <- map2_dfr(unique(all.phon.adjusted$language), list(all.distances.adjusted), get.typicality.stats) %>% 
  mutate(p.value.adj = p.adjust(p.value, method = "bonferroni")) %>% # Bonferroni adjusted
  mutate(sig = p.value.adj < 0.01)
table(all.p$sig)
mean(all.p$d)
sd(all.p$d)

morph.complexity %>% 
  rename(language = Language) %>% 
  filter(number.not.na > 3) %>% 
  left_join(all.p) %>% 
  ggplot(aes(x = complexity, y = eta.squared)) + geom_point() + geom_smooth(method = 'lm')


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
neighbor.mc.adjusted <- neighbor.mc.adjusted %>%
  rename(random = Mean)

# Join the actual and the permuted neighbors per language, per category. Then,
# count what proportion of the 1000 permutations the actual neighbors have a
# lower or equal proportion of same-category neighbors as the null permutation.
# Use the LOWER boundary of the .99 confidence interval as the "actual" number
# to be compared with the permuted numbers.
neighbor.test.adjusted <- neighbor.stats.adjusted %>% 
  left_join(dplyr::select(neighbor.mc.adjusted, language, ontological.category, random)) %>%
  mutate(is.higher = random >= Lower)
neighbor.test.adjusted <- neighbor.test.adjusted %>%
  group_by(language, ontological.category) %>%
  summarise(p = sum(is.higher) / 1000)

# Check statistics for reference languages. For "baseline" performance for each
# language, take 1 SD above the mean of the shuffles.
## This is the mean reported

neighbor.mc.adjusted %>% 
  group_by(language, ontological.category) %>%
  dplyr::summarize(random = mean(random) + sd(random)) %>% 
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
# The height of each bar is the lower boundary of the 99% CI of the actual data
# minus 1 SD above the mean of the random permutations.

neighbor.mc.plot.adjusted <- neighbor.mc.adjusted %>%
  filter(ontological.category != "Other") %>%
  group_by(language, ontological.category) %>%
  dplyr::summarize(random = mean(random) + sd(random))

neighbor.plot.adjusted <- neighbor.stats.adjusted %>%
  group_by() %>%
  filter(ontological.category != "Other") %>%
  left_join(neighbor.mc.plot.adjusted) %>%
  mutate(language = factor(language, 
                           levels = sorted.langs.adjusted$language, 
                            labels = sorted.langs.adjusted$language))

morph.complexity %>% 
  rename(language = Language) %>% 
  filter(number.not.na > 3) %>% 
  left_join(neighbor.plot.adjusted) %>% 
  mutate(perf = Lower - random) %>% 
  ggplot(aes(x = complexity, y = perf)) + geom_point() + geom_smooth(method = "lm") + facet_wrap(vars(ontological.category))

neighbor.adjusted <- neighbor.plot.adjusted %>%
  mutate(height = Lower - random) %>% 
  ggplot(aes(x = language, y = height, ymin = Lower, ymax = Upper, fill = ontological.category)) +
  geom_bar(stat = "identity", width = 1,
           size = .25) +
  scale_x_discrete(name = "", labels = rep("", 226)) +
  facet_wrap(vars(ontological.category), ncol = 2) +
  scale_y_continuous(expand = c(0, 0), name = "Same Neighbor") +
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

ggsave("../Figures/Main/neighbor_adjusted_diff.png", width = 17, height = 7, units = "cm", dpi = 900)


# RNN K-Fold -------

# Mark each result as whether the bootstrapped CI for the Matthews Correlation
# Coefficient include a baseline of 0.1 or not. 

## Descriptive stats
rnn.stats.adjusted

rnn.stats.adjusted %>% 
  arrange(desc(median.auc)) %>% 
  filter(measure == "AUC") %>% 
  mutate(language = factor(language, levels = .$language)) %>% 
  ggplot(aes(x = language, y = median.auc)) +
  geom_pointrange(aes(ymin = median.auc - iqr.auc, ymax = median.auc + iqr.auc), size = 0.2) +
  geom_hline(yintercept = 0.5) +
  labs(y = "Mean AUC")

rnn.stats.adjusted %>% 
  arrange(desc(median.auc)) %>% 
  filter(measure == "AUC") %>% 
  mutate(language = factor(language, levels = .$language),
         significant = ifelse(p.value < 0.01, TRUE, FALSE)) %>% 
  ggplot(aes(x = language, y = median.auc, color = significant)) +
  geom_pointrange(aes(ymin = conf, ymax = median.auc), size = 0.2) +
  geom_hline(yintercept = 0.5) +
  labs(y = "Median AUC") +
  theme_minimal()

rnn.stats.adjusted %>% 
  arrange(desc(median.mcc)) %>% 
  filter(measure == "Matthews") %>% 
  mutate(language = factor(language, levels = .$language),
         significant = ifelse(p.value < 0.01, TRUE, FALSE)) %>% 
  ggplot(aes(x = language, y = median.mcc, color = significant)) +
  geom_pointrange(aes(ymin = conf, ymax = median.mcc), size = 0.2) +
  geom_hline(yintercept = 0.2) +
  labs(y = "Median MCC") +
  theme_minimal()

## Count the number of languages with UAC and MCC significantly higher than baseline
rnn.tally.adjusted <- rnn.stats.adjusted %>% 
  mutate(significant = ifelse(p.value < 0.01, TRUE, FALSE)) %>% 
  group_by(measure, significant) %>% 
  tally() %>% 
  mutate(percentage = (n / sum(n)) * 100)
rnn.tally.adjusted

## Check reference languages
rnn.stats.adjusted %>% 
  mutate(significant = ifelse(p.value < 0.01, TRUE, FALSE)) %>% 
  filter(language %in% test.languages) 

rnn.stats.adjusted %>% 
  arrange(desc(median.auc)) %>% 
  filter(measure == "AUC") %>% 
  mutate(language = factor(language, levels = .$language),
         significant = ifelse(p.value < 0.01, TRUE, FALSE),
         lang.label = ifelse(language %in% test.languages, 
                             as.character(language), ""),
         include = ifelse(lang.label == "", NA, language)) %>% 
  ggplot(aes(x = language, y = median.auc, fill = significant)) +
  geom_vline(aes(xintercept = include), linetype = "dotted", size = .5, color = palette_line) +
  geom_linerange(aes(ymin = median.auc - iqr.auc, ymax = median.auc + iqr.auc), size = 0.5, color = palette_line) +
  geom_point(shape = 22, size = .75, color = palette_line) + 
  geom_hline(yintercept = 0.5) +
  scale_x_discrete(name = "Language", labels = function(x){
    ifelse(x %in% test.languages, str_wrap(as.character(x), 10), "")
  }) +
  expand_limits(x = 229) +
  cowplot::theme_cowplot()  + 
  scale_fill_manual(name = "Significantly > 0.5", values = c(palette_other[1], palette_world)) +
  scale_y_continuous(name = "Learning performance (AUC)", breaks = seq(0, 1, 0.1)) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        strip.text = element_text(size = 8),
        axis.ticks.x = element_blank(),
        legend.position = "none") 

ggsave("../Figures/Main/rnn_adjusted_auc.png", width = 17, height = 7, units = "cm", dpi = 900)


rnn.stats.adjusted %>% 
  arrange(desc(median.mcc)) %>% 
  filter(measure == "Matthews") %>% 
  mutate(language = factor(language, levels = .$language),
         significant = ifelse(p.value < 0.01, TRUE, FALSE),
         lang.label = ifelse(language %in% test.languages, 
                             as.character(language), ""),
         include = ifelse(lang.label == "", NA, language)) %>% 
  ggplot(aes(x = language, y = median.mcc, fill = significant)) +
  geom_vline(aes(xintercept = include), linetype = "dotted", size = .5, color = palette_line) +
  geom_linerange(aes(ymin = median.mcc - iqr.mcc, ymax = median.mcc + iqr.mcc), size = 0.5, color = palette_line) +
  geom_point(shape = 22, size = .75, color = palette_line) + 
  geom_hline(yintercept = 0.2) +
  scale_x_discrete(name = "Language", labels = function(x){
    ifelse(x %in% test.languages, str_wrap(as.character(x), 10), "")
  }) +
  expand_limits(x = 229) +
  cowplot::theme_cowplot()  + 
  scale_fill_manual(name = "Significantly > 0.2", values = c(palette_other[1], palette_world)) +
  scale_y_continuous(name = "Learning performance (MCC)", breaks = seq(0, 1, 0.1)) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        strip.text = element_text(size = 8),
        axis.ticks.x = element_blank(),
        legend.position = "none") 

ggsave("../Figures/Main/rnn_adjusted_mcc.png", width = 17, height = 7, units = "cm", dpi = 900)


# RNN Spurt --------------------------------------------------------

spurt.stats.adjusted

spurt.stats.adjusted %>% 
  arrange(desc(median.auc)) %>% 
  filter(measure == "AUC") %>% 
  mutate(language = factor(language, levels = .$language)) %>% 
  ggplot(aes(x = language, y = median.auc)) +
  geom_pointrange(aes(ymin = median.auc - iqr.auc, ymax = median.auc + iqr.auc), size = 0.2) +
  geom_hline(yintercept = 0.5) +
  labs(y = "Mean AUC")

spurt.stats.adjusted %>% 
  arrange(desc(median.auc)) %>% 
  filter(measure == "AUC") %>% 
  mutate(language = factor(language, levels = .$language),
         significant = ifelse(p.value < 0.01, TRUE, FALSE)) %>% 
  ggplot(aes(x = language, y = median.auc, color = significant)) +
  geom_pointrange(aes(ymin = conf, ymax = median.auc), size = 0.2) +
  geom_hline(yintercept = 0.5) +
  labs(y = "Median AUC") +
  theme_minimal()

spurt.stats.adjusted %>% 
  arrange(desc(median.mcc)) %>% 
  filter(measure == "Matthews") %>% 
  mutate(language = factor(language, levels = .$language),
         significant = ifelse(p.value < 0.01, TRUE, FALSE)) %>% 
  ggplot(aes(x = language, y = median.mcc, color = significant)) +
  geom_pointrange(aes(ymin = conf, ymax = median.mcc), size = 0.2) +
  geom_hline(yintercept = 0.1) +
  labs(y = "Median MCC") +
  theme_minimal()

## Count the number of languages with UAC and MCC significantly higher than baseline
spurt.tally.adjusted <- spurt.stats.adjusted %>% 
  mutate(significant = ifelse(p.value < 0.01, TRUE, FALSE)) %>% 
  group_by(measure, significant) %>% 
  tally() %>% 
  mutate(percentage = (n / sum(n)) * 100)
spurt.tally.adjusted

## Check reference languages
spurt.stats.adjusted %>% 
  mutate(significant = ifelse(p.value < 0.01, TRUE, FALSE)) %>% 
  filter(language %in% test.languages) 

spurt.stats.adjusted %>% 
  arrange(desc(median.auc)) %>% 
  filter(measure == "AUC") %>% 
  mutate(language = factor(language, levels = .$language),
         significant = ifelse(p.value < 0.01, TRUE, FALSE),
         lang.label = ifelse(language %in% test.languages, 
                             as.character(language), ""),
         include = ifelse(lang.label == "", NA, language)) %>% 
  ggplot(aes(x = language, y = median.auc, fill = significant)) +
  geom_vline(aes(xintercept = include), linetype = "dotted", size = .5, color = palette_line) +
  geom_linerange(aes(ymin = median.auc - iqr.auc, ymax = median.auc + iqr.auc), size = 0.5, color = palette_line) +
  geom_point(shape = 22, size = .75, color = palette_line) + 
  geom_hline(yintercept = 0.5) +
  scale_x_discrete(name = "Language", labels = function(x){
    ifelse(x %in% test.languages, str_wrap(as.character(x), 10), "")
  }) +
  expand_limits(x = 229) +
  cowplot::theme_cowplot()  + 
  scale_fill_manual(name = "Significantly > 0.5", values = c(palette_other[1], palette_world)) +
  scale_y_continuous(name = "Learning performance (AUC)", breaks = seq(0, 1, 0.1)) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        strip.text = element_text(size = 8),
        axis.ticks.x = element_blank(),
        legend.position = "none") 

ggsave("../Figures/Main/rnn_adjusted_auc.png", width = 17, height = 7, units = "cm", dpi = 900)


spurt.stats.adjusted %>% 
  arrange(desc(median.mcc)) %>% 
  filter(measure == "Matthews") %>% 
  mutate(language = factor(language, levels = .$language),
         significant = ifelse(p.value < 0.01, TRUE, FALSE),
         lang.label = ifelse(language %in% test.languages, 
                             as.character(language), ""),
         include = ifelse(lang.label == "", NA, language)) %>% 
  ggplot(aes(x = language, y = median.mcc, fill = significant)) +
  geom_vline(aes(xintercept = include), linetype = "dotted", size = .5, color = palette_line) +
  geom_linerange(aes(ymin = median.mcc - iqr.mcc, ymax = median.mcc + iqr.mcc), size = 0.5, color = palette_line) +
  geom_point(shape = 22, size = .75, color = palette_line) + 
  geom_hline(yintercept = 0.1) +
  scale_x_discrete(name = "Language", labels = function(x){
    ifelse(x %in% test.languages, str_wrap(as.character(x), 10), "")
  }) +
  expand_limits(x = 229) +
  cowplot::theme_cowplot()  + 
  scale_fill_manual(name = "Significantly > 0.2", values = c(palette_other[1], palette_world)) +
  scale_y_continuous(name = "Learning performance (MCC)", breaks = seq(0, 1, 0.1)) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        strip.text = element_text(size = 8),
        axis.ticks.x = element_blank(),
        legend.position = "none") 

ggsave("../Figures/Main/rnn_adjusted_mcc.png", width = 17, height = 7, units = "cm", dpi = 900)