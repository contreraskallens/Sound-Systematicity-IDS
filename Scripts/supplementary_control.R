source("functions.r")
source("data_wrangling.R")
source("main.R")

# Reduplication ---- 
# Code whether languages have a reduplication in WALS

redup_languages <- language.groups %>% 
  rename(Name = language) %>% 
  left_join(wals) %>% 
  left_join(select(wals_info, -Name, -family, -genus)) %>% 
  select(Name, family, reduplication = `27A Reduplication`) %>% 
  filter(!is.na(reduplication))

# Differences in mean typicality depending on coded reduplication

all.distances.adjusted %>% 
  group_by(language, class) %>% 
  filter(class != "Other") %>% 
  rename(Name = language) %>% 
  dplyr::summarize(mean.typ = mean(typicality)) %>% 
  right_join(redup_languages) %>% 
  ggplot(aes(x = reduplication, y = mean.typ)) +
  geom_boxplot(width = 0.3) +
  facet_wrap(vars(class), ncol = 1, scales = "free_y") +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1.5) +
  labs(y = "Mean Typicality", title = "Mean typicality per class for reduplication mode", subtitle = "Adjusted") +
  theme_cowplot() +
  theme(axis.title.x = element_blank())
ggsave("../Figures/Other/typicality_reduplication.png")


## Code reduplication  as binary
redup_density = all.distances.adjusted %>% 
  group_by(language, class) %>% 
  filter(class != "Other") %>% 
  rename(Name = language) %>% 
  dplyr::summarize(mean.typ = mean(typicality)) %>% 
  left_join(redup_languages) %>% 
  mutate(reduplication = ifelse(reduplication %in% c("1 Productive full and partial reduplication", "2 Full reduplication only"), TRUE, FALSE),
         reduplication = ifelse(is.na(reduplication), FALSE, reduplication))

## Density of typicality per class according to presence or absence of reduplication

redup_density %>% 
  filter(class == "Action") %>% 
  ggplot(aes(x = mean.typ, y = ..density..)) +
  geom_density(fill = palette_a_t[2]) +
  geom_vline(xintercept = filter(redup_density, reduplication == TRUE & class == "Action")$mean.typ,
             linetype = "dashed") +
  labs(title = "Mean typicality for Actions",
       subtitle = "Dashed lines represent languages with known productive reduplication",
       x = "Mean typicality") +
  cowplot::theme_cowplot()
ggsave("../Figures/Other/redup_typicality_actions.png")

redup_density %>% 
  filter(class == "Thing") %>% 
  ggplot(aes(x = mean.typ, y = ..density..)) +
  geom_density(fill = palette_a_t[1]) +
  geom_vline(xintercept = filter(redup_density, reduplication == TRUE & class == "Thing")$mean.typ,
             linetype = "dashed") +
  labs(title = "Mean typicality for Things",
       subtitle = "Dashed lines represent languages with known productive reduplication") +
  cowplot::theme_cowplot()
ggsave("../Figures/Other/redup_typicality_things.png")


# Linear model of the effect of reduplication

## Only with the languages with no missing information
redup.no.na <- lm(data = filter(redup_density, Name %in% redup_languages$Name), mean.typ ~ reduplication + class)
summary(redup.no.na)

## Code missing data as no reduplication
redup.all <- lm(data = redup_density, mean.typ ~ reduplication + class)
summary(redup.all)

rsq.partial(redup.no.na)
rsq.partial(redup.all)

# Redup effect on RNN

## Add redup information to rnn stats
rnn.redup <- rnn.stats.adjusted %>% 
  rename(Name = Language) %>% 
  left_join(wals, by = "Name") %>% 
  left_join(select(wals_info, -Name), by = "wals_code") %>% 
  select(Name, Matthews, reduplication = `27A Reduplication`) %>% 
  mutate(redup = ifelse(reduplication %in% c("1 Productive full and partial reduplication", "2 Full reduplication only"),
                        TRUE, FALSE),
         redup = ifelse(is.na(redup), FALSE, redup))

## Plot distribution of MCC
rnn.redup %>% 
  ggplot(aes(x = Matthews, y = ..density..)) +
  geom_density(fill = "seagreen") +
  geom_vline(xintercept = filter(rnn.redup, redup == TRUE)$Matthews,
             linetype = "dashed") +
  theme_cowplot() +
  labs(title = "Distribution of Mean MCC across languages",
       subtitle = "Dashed lines represent languages with known productive reduplication")
ggsave("../Figures/Other/redup_rnn.png")


## Linear model assessment of effect of redup

## Only non NA languages
rnn.redup %>% 
  filter(Name %in% redup_languages$Name) %>% 
  lm(data = ., Matthews ~ redup) %>% summary
## All languages
rnn.redup %>% 
  lm(data = ., Matthews ~ redup) %>% 
  summary


# Effect of redup on Spurt
## Code redup on spurt
spurt.redup <-spurt.stats.adjusted %>% 
  rename(Name = Language) %>% 
  left_join(wals, by = "Name") %>% 
  left_join(select(wals_info, -Name), by = "wals_code") %>% 
  select(Name, Matthews, reduplication = `27A Reduplication`) %>% 
  mutate(redup = ifelse(reduplication %in% c("1 Productive full and partial reduplication", "2 Full reduplication only"),
                        TRUE, FALSE),
         redup = ifelse(is.na(redup), FALSE, redup))
## Plot distribution of Spurt marking Redup
spurt.redup %>% 
  ggplot(aes(x = Matthews, y = ..density..)) +
  geom_density(fill = "seagreen") +
  geom_vline(xintercept = filter(rnn.redup, redup == TRUE)$Matthews,
             linetype = "dashed") +
  labs(title = "Distribution of Mean MCC across spurt",
       subtitle = "Dashed lines represent languages with known productive reduplication") +
  theme_cowplot()
ggsave("../Figures/Other/redup_spurt.png")

## Assess effect via linear model
spurt.redup %>% 
  filter(Name %in% redup_languages$Name) %>%
  lm(data = ., Matthews ~ redup) %>% 
  summary

spurt.redup %>% 
  lm(data = ., Matthews ~ redup) %>% 
  summary


# Family sampling ----

# Add family and geo region information to distance
all.distances.adjusted <- all.distances.adjusted %>% 
  left_join(language.groups)

# Get one language of each family and take stat measures for aggregate. Do this 1000 times. 

family.stats <- map_dfr(1:1000, function(i){
  if(i %% 50 == 0){print(i)}
  this.sample <- language.groups %>% 
    group_by(family) %>% 
    sample_n(1) %>% 
    .$language
  this.sample.distances <- all.distances.adjusted %>% 
    filter(language %in% this.sample, class != "Other")
  
  this.wilcox <- this.sample.distances %>%
    mutate(class = factor(class)) %>%
    coin::wilcox_test(data = ., typicality ~ class)
  this.z <- coin::statistic(this.wilcox, type = "standardized")
  eta.squared <- (this.z ^ 2) / (nrow(this.sample.distances)) 
  this.sample.d <-  effsize::cohen.d(data = this.sample.distances, typicality ~ class)
  stats <- tibble(eta = eta.squared, d = abs(this.sample.d$estimate))
  return(stats)
})

print("Median of family permutation, eta^2")
median(family.stats$eta)
print("IQR of family permutation, eta ^2")
IQR(family.stats$eta)
print("Actual measure, cohen's eta^2")
eta.squared.action.thing.adjusted

print("Median of family permutation, cohen's d")
median(family.stats$d)
print("IQR of family permutation, cohen's d")
IQR(family.stats$d)
print("Actual measure, cohen's d")
abs(d.action.thing.adjusted$estimate)

# Plot permutations as histograms, mark permutation median (blue) and data measure (red)
family.stats %>% 
  ggplot(aes(x = eta)) + 
  geom_histogram(bins = 50, color = palette_line, fill = palette_other[2]) +
  cowplot::theme_cowplot() + 
  geom_vline(xintercept = eta.squared.action.thing.adjusted, size = 3, color = "red") +
  geom_vline(xintercept = median(family.stats$eta), color = "blue", linetype = "dashed", size = 3) +
  labs(title = "Distribution of eta^2 in family permutations", subtitle = "Red = Actual data, blue = permutation median")
ggsave("../Figures/Other/family_eta.png")

family.stats %>% 
  ggplot(aes(x = d)) + 
  geom_histogram(bins = 50, color = palette_line, fill = palette_other[2]) +
  cowplot::theme_cowplot() + 
  geom_vline(xintercept = abs(d.action.thing.adjusted$estimate), size = 3, color = "red") +
  geom_vline(xintercept = median(family.stats$d), color = "blue", linetype = "dashed", size = 3) +
  labs(title = "Distribution of Cohen's d in family permutations", subtitle = "Red = Actual data, blue = permutation median")
ggsave("../Figures/Other/family_d.png")

# Family permutations for nearest neighbor analysis

neighbor.samples.family <-  map_dfr(1:1000, function(i){
  if(i %% 50 == 0){print(i)}
  this.sample <- language.groups %>% 
    group_by(family) %>% 
    sample_n(1) %>% 
    .$language
  this.rep.neighbor <- repeated.neighbor.adjusted %>% 
    filter(language %in% this.sample) %>% 
    group_by(language, ontological.category) %>%
    do(enframe(Hmisc::smean.cl.normal(.$proportion.of.hits, conf.int = 0.99))) %>%
    spread(name, value)
  this.rep.neighbor %>% 
    group_by(ontological.category) %>% 
    dplyr::summarize(all.mean = mean(Mean * 100), sd = sd(Mean * 100))
  this.mc <- neighbor.mc.adjusted %>%
    filter(language %in% this.sample)
  this.test <- this.rep.neighbor %>% 
    left_join(this.mc) %>%
    mutate(is.higher = random >= Lower)
  this.test <- this.test %>%
    group_by(language, ontological.category) %>%
    dplyr::summarize(p = sum(is.higher) / 1000)
  this.proportion <- this.test %>%
    mutate(p = cut(p, breaks = c(0, 0.01, 1), include.lowest = TRUE, right = FALSE)) %>%
    group_by(p, ontological.category) %>%
    tally() %>%
    mutate(n = n / length(unique(this.sample)))
  return(this.proportion)
})

print("Proportion of languages with p < 0.01 in neighbor test, Actions, permutation median")
median(filter(neighbor.samples.family, ontological.category == "Action",p == "[0,0.01)")$n)
print("Proportion of languages with p < 0.01 in neighbor test, Actions, permutation IQR")
IQR(filter(neighbor.samples.family, ontological.category == "Action",  p == "[0,0.01)")$n)
print("Proportion of languages with p < 0.01 in neighbor test, Actions, actual data")
neighbor.proportions.adjusted$n[1]

neighbor.samples.family %>% 
  filter(ontological.category == "Action", p == "[0,0.01)") %>% 
  ggplot(aes(x = n)) +
  geom_histogram(bins = 8, color = palette_line, fill = palette_other[2]) +
  geom_vline(xintercept = neighbor.proportions.adjusted$n[1], color = "red", size = 3) +
  geom_vline(xintercept = median(filter(neighbor.samples.family,
                                        ontological.category == "Action", 
                                        p == "[0,0.01)")$n),
             color = "blue", size = 3, linetype = "dashed") +
  labs(title = "Proportion of languages with p < 0.01 in neighbor MC test, sampling for family for ACTIONS",
       subtitle = "Red = actual data, blue = permutation median") +
  cowplot::theme_cowplot()
ggsave("../Figures/Other/neighbor_family_actions.png")

print("Proportion of languages with p < 0.01 in neighbor test, Things, permutation median")
median(filter(neighbor.samples.family, ontological.category == "Thing",  p == "[0,0.01)")$n)
print("Proportion of languages with p < 0.01 in neighbor test, Things, permutation IQR")
IQR(filter(neighbor.samples.family, ontological.category == "Thing",  p == "[0,0.01)")$n)
print("Proportion of languages with p < 0.01 in neighbor test, Things, actual")
neighbor.proportions.adjusted$n[3]

neighbor.samples.family %>% 
  filter(ontological.category == "Thing", p == "[0,0.01)") %>% 
  ggplot(aes(x = n)) +
  geom_histogram(bins = 8, color = palette_line, fill = palette_other[2]) +
  geom_vline(xintercept = neighbor.proportions.adjusted$n[3], color = "red", size = 3) +
  geom_vline(xintercept = median(filter(neighbor.samples.family,
                                        ontological.category == "Thing", 
                                        p == "[0,0.01)")$n),
             color = "blue", size = 3, linetype = "dashed") +
  labs(title = "Proportion of languages with p < 0.01 in neighbor MC test, sampling for family for THINGS",
       subtitle = "Red = actual data, blue = permutation median") +
  cowplot::theme_cowplot()
ggsave("../Figures/Other/neighbor_family_things.png")



# 10-fold RNN sampling for family

matthews.sample.family <- map_dfr(1:1000, function(i){
  if(i %% 50 == 0){print(i)}
  this.sample <- language.groups %>% 
    group_by(family) %>% 
    sample_n(1) %>% 
    .$language
  this.matthews <- rnn.stats %>% 
    filter(Language %in% this.sample) 
  this.tally <- this.matthews %>% 
    mutate(includes.baseline = ifelse(Matthews.Lower < 0.1, TRUE, FALSE)) %>% 
    group_by(includes.baseline) %>% 
    tally() %>% 
    mutate(percentage = (n / sum(n)) * 100)
  results <- tibble(Mean.Matthews = mean(this.matthews$Matthews),
                    tally = this.tally$percentage[1])
  return(results)
})

print("Mean MCC across languages, sample median")
median(matthews.sample.family$Mean.Matthews)
print("Mean MCC across languages, sample IQR")
IQR(matthews.sample.family$Mean.Matthews)
print("Mean MCC across languages, actual")
mean(rnn.stats.adjusted$Matthews)

matthews.sample.family %>% 
  ggplot(aes(x = Mean.Matthews)) +
  geom_histogram(bins = 50, color = palette_line, fill = palette_other[2]) +
  geom_vline(xintercept = mean(rnn.stats.adjusted$Matthews), color = "red", size = 3) +
  geom_vline(xintercept = median(matthews.sample.family$Mean.Matthews), color = "blue", size = 3, linetype = "dashed") +
  theme_cowplot() +
  labs(title = "Mean MCC sampling for family",
       subtitle = "Red: sample median, blue: actual measure")
ggsave("../Figures/Other/mean_matthews_family.png")

# Tally
print("Proportion of languages above chance, sample median")
median(matthews.sample.family$tally)
print("Proportion of languages above chance, sample IQR")
IQR(matthews.sample.family$tally)
print("Proportion of languages above chance, actual")
rnn.tally.adjusted$percentage[1]

matthews.sample.family %>% 
  ggplot(aes(x = tally)) +
  geom_histogram(bins = 10, color = palette_line, fill = palette_other[2]) +
  geom_vline(xintercept = rnn.tally.adjusted$percentage[1], color = "red", size = 3) +
  geom_vline(xintercept = median(matthews.sample.family$tally), color = "blue", size = 3, linetype = "dashed") + 
  theme_cowplot() +
  labs(title = "Percentage of languages with MCC above the baseline sampling for family",
       subtitle = "Red: actual data, blue: sample median")
ggsave("../Figures/Other/rnn_tally_family.png")


# Family sampling in Spurt model

spurt.sample.family <- map_dfr(1:1000, function(i){
  if(i %% 50 == 0){print(i)}
  this.sample <- language.groups %>% 
    group_by(family) %>% 
    sample_n(1) %>% 
    .$language
  this.matthews <- spurt.stats %>% 
    filter(Language %in% this.sample) 
  this.tally <- this.matthews %>% 
    mutate(includes.baseline = ifelse(Matthews.Lower < 0.1, TRUE, FALSE)) %>% 
    group_by(includes.baseline) %>% 
    tally() %>% 
    mutate(percentage = (n / sum(n)) * 100)
  results <- tibble(Mean.Matthews = mean(this.matthews$Matthews),
                    tally = this.tally$percentage[1])
  return(results)
})

print("Spurt model mean MCC across languages, sample median")
median(spurt.sample.family$Mean.Matthews)
print("Spurt model mean MCC across languages, sample IQR")
IQR(spurt.sample.family$Mean.Matthews)
print("Spurt model mean MCC across languages, actual")
spurt.stats.adjusted$Matthews %>% mean

spurt.sample.family %>% 
  ggplot(aes(x = Mean.Matthews)) +
  geom_histogram(bins = 50, color = palette_line, fill = palette_other[2]) +
  geom_vline(xintercept = mean(spurt.stats.adjusted$Matthews), size = 2, color = "red") +
  geom_vline(xintercept = median(spurt.sample.family$Mean.Matthews), color = "blue", linetype = "dashed", size = 2) +
  theme_cowplot() +
  labs(title = "Spurt mean MCC sampling for family",
       subtitle = "Red: actual data, blue: sample median")
ggsave("../Figures/Other/spurt_family_matthews.png")

print("Proportion of languages above chance in spurt model, sample median")
median(spurt.sample.family$tally)
print("Proportion of languages above chance in spurt model, sample IQR")
IQR(spurt.sample.family$tally)
print("Proportion of languages above chance in spurt model, actual data")
spurt.tally.adjusted

spurt.sample.family %>% 
  ggplot(aes(x = tally)) +
  geom_histogram(bins = 10, color = palette_line, fill = palette_other[2]) +
  geom_vline(xintercept = spurt.tally.adjusted$percentage[1], color = "red", size = 2) +
  geom_vline(xintercept = median(spurt.sample.family$tally), color = "blue", size = 2, linetype = "dashed") +
  cowplot::theme_cowplot() +
  labs(title = "Spurt percentage of languages with MCC above baseline sampling for family",
       subtitle = "Red: actual data, blue: sample median")
ggsave("../Figures/Other/spurt_tally_family.png")


# Geographical cluster sampling ----

# Sample 1 member for each geographical cluster and take statistical measures
geo.stats <- map_dfr(1:1000, function(i){
  if(i %% 50 == 0){print(i)}
  this.sample <- language.groups %>% 
    filter(!is.na(geo.cluster)) %>% 
    group_by(geo.cluster) %>% 
    sample_n(1) %>% 
    .$language
  this.sample.distances <- all.distances.adjusted %>% 
    filter(language %in% this.sample, class != "Other")
  
  this.wilcox <- this.sample.distances %>%
    mutate(class = factor(class)) %>%
    coin::wilcox_test(data = ., typicality ~ class)
  this.z <- coin::statistic(this.wilcox, type = "standardized")
  eta.squared <- (this.z ^ 2) / (nrow(this.sample.distances)) 
  this.sample.d <-  effsize::cohen.d(data = this.sample.distances, typicality ~ class)
  stats <- tibble(eta = eta.squared, d = abs(this.sample.d$estimate))
  return(stats)
})

print("Median of geo permutation, eta^2")
median(geo.stats$eta)
print("IQR of geo permutation, eta ^2")
IQR(geo.stats$eta)
print("Actual measure, cohen's eta^2")
eta.squared.action.thing.adjusted

print("Median of geo permutation, cohen's d")
median(geo.stats$d)
print("IQR of geo permutation, cohen's d")
IQR(geo.stats$d)
print("Actual measure, cohen's d")
abs(d.action.thing.adjusted$estimate)


# Plot permutations as histograms, mark permutation median (blue) and data measure (red)
geo.stats %>% 
  ggplot(aes(x = eta)) + 
  geom_histogram(bins = 50, color = palette_line, fill = palette_other[2]) +
  cowplot::theme_cowplot() + 
  geom_vline(xintercept = eta.squared.action.thing.adjusted, size = 3, color = "red") +
  geom_vline(xintercept = median(geo.stats$eta), color = "blue", linetype = "dashed", size = 3) +
  labs(title = "Distribution of eta^2 in geo permutations", subtitle = "Red = Actual data, blue = permutation median")
ggsave("../Figures/Other/geo_eta.png")

geo.stats %>% 
  ggplot(aes(x = d)) + 
  geom_histogram(bins = 50, color = palette_line, fill = palette_other[2]) +
  cowplot::theme_cowplot() + 
  geom_vline(xintercept = abs(d.action.thing.adjusted$estimate), size = 3, color = "red") +
  geom_vline(xintercept = median(geo.stats$d), color = "blue", linetype = "dashed", size = 3) +
  labs(title = "Distribution of Cohen's d in geo permutations", subtitle = "Red = Actual data, blue = permutation median")
ggsave("../Figures/Other/geo_d.png")


# Geo neighbor

neighbor.samples.geo <-  map_dfr(1:1000, function(i){
  if(i %% 50 == 0){print(i)}
  this.sample <- language.groups %>% 
    filter(!is.na(geo.cluster)) %>% 
    group_by(geo.cluster) %>% 
    sample_n(1) %>% 
    .$language
  this.rep.neighbor <- repeated.neighbor.adjusted %>% 
    filter(language %in% this.sample) %>% 
    group_by(language, ontological.category) %>%
    do(enframe(Hmisc::smean.cl.normal(.$proportion.of.hits, conf.int = 0.99))) %>%
    spread(name, value)
  this.rep.neighbor %>% 
    group_by(ontological.category) %>% 
    dplyr::summarize(all.mean = mean(Mean * 100), sd = sd(Mean * 100))
  this.mc <- neighbor.mc.adjusted %>%
    filter(language %in% this.sample)
  this.test <- this.rep.neighbor %>% 
    left_join(this.mc) %>%
    mutate(is.higher = random >= Lower)
  this.test <- this.test %>%
    group_by(language, ontological.category) %>%
    dplyr::summarize(p = sum(is.higher) / 1000)
  this.proportion <- this.test %>%
    mutate(p = cut(p, breaks = c(0, 0.01, 1), include.lowest = TRUE, right = FALSE)) %>%
    group_by(p, ontological.category) %>%
    tally() %>%
    mutate(n = n / length(unique(this.sample)))
  return(this.proportion)
})


print("Proportion of languages with p < 0.01 in neighbor test, Actions, permutation median")
median(filter(neighbor.samples.geo, ontological.category == "Action",p == "[0,0.01)")$n)
print("Proportion of languages with p < 0.01 in neighbor test, Actions, permutation IQR")
IQR(filter(neighbor.samples.geo, ontological.category == "Action",  p == "[0,0.01)")$n)
print("Proportion of languages with p < 0.01 in neighbor test, Actions, actual data")
neighbor.proportions.adjusted$n[1]

neighbor.samples.geo %>% 
  filter(ontological.category == "Action", p == "[0,0.01)") %>% 
  ggplot(aes(x = n)) +
  geom_histogram(bins = 8, color = palette_line, fill = palette_other[2]) +
  geom_vline(xintercept = neighbor.proportions.adjusted$n[1], color = "red", size = 3) +
  geom_vline(xintercept = median(filter(neighbor.samples.geo,
                                        ontological.category == "Action", 
                                        p == "[0,0.01)")$n),
             color = "blue", size = 3, linetype = "dashed") +
  labs(title = "Proportion of languages with p < 0.01 in neighbor MC test, sampling for geo for ACTIONS",
       subtitle = "Red = actual data, blue = permutation median") +
  cowplot::theme_cowplot()
ggsave("../Figures/Other/neighbor_geo_actions.png")

print("Proportion of languages with p < 0.01 in neighbor test, Things, permutation median")
median(filter(neighbor.samples.geo, ontological.category == "Thing",  p == "[0,0.01)")$n)
print("Proportion of languages with p < 0.01 in neighbor test, Things, permutation IQR")
IQR(filter(neighbor.samples.geo, ontological.category == "Thing",  p == "[0,0.01)")$n)
print("Proportion of languages with p < 0.01 in neighbor test, Things, actual")
neighbor.proportions.adjusted$n[3]

neighbor.samples.geo %>% 
  filter(ontological.category == "Thing", p == "[0,0.01)") %>% 
  ggplot(aes(x = n)) +
  geom_histogram(bins = 8, color = palette_line, fill = palette_other[2]) +
  geom_vline(xintercept = neighbor.proportions.adjusted$n[3], color = "red", size = 3) +
  geom_vline(xintercept = median(filter(neighbor.samples.geo,
                                        ontological.category == "Thing", 
                                        p == "[0,0.01)")$n),
             color = "blue", size = 3, linetype = "dashed") +
  labs(title = "Proportion of languages with p < 0.01 in neighbor MC test, sampling for geo for THINGS",
       subtitle = "Red = actual data, blue = permutation median") +
  cowplot::theme_cowplot()
ggsave("../Figures/Other/neighbor_geo_things.png")


# 10 fold permutations for geo cluster


matthews.sample.geo <- map_dfr(1:1000, function(i){
  if(i %% 50 == 0){print(i)}
  this.sample <- language.groups %>% 
    filter(!is.na(geo.cluster)) %>% 
    group_by(geo.cluster) %>% 
    sample_n(1) %>% 
    .$language
  this.matthews <- rnn.stats %>% 
    filter(Language %in% this.sample) 
  this.tally <- this.matthews %>% 
    mutate(includes.baseline = ifelse(Matthews.Lower < 0.1, TRUE, FALSE)) %>% 
    group_by(includes.baseline) %>% 
    tally() %>% 
    mutate(percentage = (n / sum(n)) * 100)
  results <- tibble(Mean.Matthews = mean(this.matthews$Matthews),
                    tally = this.tally$percentage[1])
  return(results)
})


print("Mean MCC across languages, sample median")
median(matthews.sample.geo$Mean.Matthews)
print("Mean MCC across languages, sample IQR")
IQR(matthews.sample.geo$Mean.Matthews)
print("Mean MCC across languages, actual")
mean(rnn.stats.adjusted$Matthews)

matthews.sample.geo %>% 
  ggplot(aes(x = Mean.Matthews)) +
  geom_histogram(bins = 50, color = palette_line, fill = palette_other[2]) +
  geom_vline(xintercept = mean(rnn.stats.adjusted$Matthews), color = "red", size = 3) +
  geom_vline(xintercept = median(matthews.sample.geo$Mean.Matthews), color = "blue", size = 3, linetype = "dashed") +
  theme_cowplot() +
  labs(title = "Mean MCC sampling for geo",
       subtitle = "Red: sample median, blue: actual measure")
ggsave("../Figures/Other/mean_matthews_geo.png")

# Tally
print("Proportion of languages above chance, sample median")
median(matthews.sample.geo$tally)
print("Proportion of languages above chance, sample IQR")
IQR(matthews.sample.geo$tally)
print("Proportion of languages above chance, actual")
rnn.tally.adjusted$percentage[1]

matthews.sample.geo %>% 
  ggplot(aes(x = tally)) +
  geom_histogram(bins = 10, color = palette_line, fill = palette_other[2]) +
  geom_vline(xintercept = rnn.tally.adjusted$percentage[1], color = "red", size = 3) +
  geom_vline(xintercept = median(matthews.sample.geo$tally), color = "blue", size = 3, linetype = "dashed") + 
  theme_cowplot() +
  labs(title = "Percentage of languages with MCC above the baseline sampling for geo",
       subtitle = "Red: actual data, blue: sample median")
ggsave("../Figures/Other/rnn_tally_geo.png")

# Spurt model

spurt.sample.geo <- map_dfr(1:1000, function(i){
  if(i %% 50 == 0){print(i)}
  this.sample <- language.groups %>% 
    filter(!(is.na(geo.cluster))) %>% 
    group_by(geo.cluster) %>% 
    sample_n(1) %>% 
    .$language
  this.matthews <- spurt.stats %>% 
    filter(Language %in% this.sample) 
  this.tally <- this.matthews %>% 
    mutate(includes.baseline = ifelse(Matthews.Lower < 0.1, TRUE, FALSE)) %>% 
    group_by(includes.baseline) %>% 
    tally() %>% 
    mutate(percentage = (n / sum(n)) * 100)
  results <- tibble(Mean.Matthews = mean(this.matthews$Matthews),
                    tally = this.tally$percentage[1])
  return(results)
})


print("Spurt model mean MCC across languages, sample median")
median(spurt.sample.geo$Mean.Matthews)
print("Spurt model mean MCC across languages, sample IQR")
IQR(spurt.sample.geo$Mean.Matthews)
print("Spurt model mean MCC across languages, actual")
spurt.stats.adjusted$Matthews %>% mean

spurt.sample.geo %>% 
  ggplot(aes(x = Mean.Matthews)) +
  geom_histogram(bins = 50, color = palette_line, fill = palette_other[2]) +
  geom_vline(xintercept = mean(spurt.stats.adjusted$Matthews), size = 2, color = "red") +
  geom_vline(xintercept = median(spurt.sample.geo$Mean.Matthews), color = "blue", linetype = "dashed", size = 2) +
  theme_cowplot() +
  labs(title = "Spurt mean MCC sampling for geo",
       subtitle = "Red: actual data, blue: sample median")
ggsave("../Figures/Other/spurt_geo_matthews.png")

print("Proportion of languages above chance in spurt model, sample median")
median(spurt.sample.geo$tally)
print("Proportion of languages above chance in spurt model, sample IQR")
IQR(spurt.sample.geo$tally)
print("Proportion of languages above chance in spurt model, actual data")
spurt.tally.adjusted

spurt.sample.geo %>% 
  ggplot(aes(x = tally)) +
  geom_histogram(bins = 10, color = palette_line, fill = palette_other[2]) +
  geom_vline(xintercept = spurt.tally.adjusted$percentage[1], color = "red", size = 2) +
  geom_vline(xintercept = median(spurt.sample.geo$tally), color = "blue", size = 2, linetype = "dashed") +
  cowplot::theme_cowplot() +
  labs(title = "Spurt percentage of languages with MCC above baseline sampling for geo",
       subtitle = "Red: actual data, blue: sample median")
ggsave("../Figures/Other/spurt_tally_geo.png")
