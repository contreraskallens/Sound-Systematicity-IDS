library(tidyverse)

wals.languages <- read_csv("../Data/Processed/wals_info.csv")

morph.features <- c("22A",
  "26A",
  "27A",
  "28A",
  "29A",
  "30A",
  "33A",
  "34A",
  "37A",
  # "38A", # This one has only 0, so ignore
  "49A",
  "51A",
  "57A",
  "59A",
  "65A",
  "66A",
  "67A",
  "69A",
  "70A",
  "73A",
  "74A",
  "75A",
  "78A",
  "94A",
  "101A",
  "102A",
  "111A",
  "112A")

languages <- wals.languages$Name

wals.languages <- map_dfc(morph.features, function(x){
  select(wals.languages, starts_with(x))
}) %>% 
  mutate_all(as.factor)

# Recode 22

levels(wals.languages$`22A Inflectional Synthesis of the Verb`) <- as.character(c(1, 2, 3, 4, 5, 6))

# Recode 26

levels(wals.languages$`26A Prefixing vs. Suffixing in Inflectional Morphology`) <- as.character(c(0, 1, 1, 1, 1, 1))

# Recode 27

levels(wals.languages$`27A Reduplication`) <- c("0", "1")

# Recode 28

# levels(wals.languages$`28A Case Syncretism`) <- c("1", "3", "2", "4")

# Recode 29

# levels(wals.languages$`29A Syncretism in Verbal Person/Number Marking`) <- c("1", "2", "3")

# Recode 30

levels(wals.languages$`30A Number of Genders`) <- as.character(c(1, 2, 3, 4, 5))

# 33A Coding of Nominal Plurality: 9 categories (partial continuous)
#    binarization: plural word, no plural: 0   all others (including clitics!):1

levels(wals.languages$`33A Coding of Nominal Plurality`) <- as.character(c(1, 1, 0, 1, 0))

# 34A Occurrence of Nominal Plurality: 6 categories (continuous)

levels(wals.languages$`34A Occurrence of Nominal Plurality`) <- as.character(c(1, 2, 3, 4, 5, 6))

# 37A Definite Articles: 5 cat. (non-continuous)
#    binarization: definite affix: 1   all others: 0
# HERE: SWEDISH AND DANISH DONT HAVE THE AFFIXES. ROMANIAN controls it with the cleanup. Everything else is noise.

# levels(wals.languages$`37A Definite Articles`) <- as.character(c(0, 0, 1, 0, 0))

# 38A Indefinite Articles: 5 categories (non-continuous)
#    binarization: indefinite affix: 1   all others: 
# No affixes here
# 
# levels(wals.languages$`38A Indefinite Articles`) <- as.character(c(0, 0, 0, 0))

# 49A Number of Cases: 9 categories (continuous, but "9 borderline case marking" removed)
# Estonian and Finnish, nominative. Hunzib has themn in Absolutive.

levels(wals.languages$`49A Number of Cases`) <- as.character(c(1, 2, 3, 4, 5, 6, 7, 8, NA))
  
# 51A Position of Case Affixes: 9 categories
#    binarization: No case affixes or adpositional clitics: 0   all others (including clitics!): 1

levels(wals.languages$`51A Position of Case Affixes`) <- as.character(c(1, 1, 1, 0))

# 57A Position of Pronominal Possessive Affixes: 4 categories (non-continuous)
#    binarization: no affixes: 0   all others: 1
# Words not expected to be in possessives


# levels(wals.languages$`57A Position of Pronominal Possessive Affixes`) <- as.character(c(1, 1, 1, 0))

# 59A Possessive Classification: 4 cat. (continuous)

# levels(wals.languages$`59A Possessive Classification`) <- as.character(c(1, 2, 3, 4))
# Words not expected to be in possessives


# 65A Perfective/Imperfective Aspect
#    binarization: "little or no inflectional morphology": 0   all others: 1
# Words not expected to be in past/perfect
# levels(wals.languages$`65A Perfective/Imperfective Aspect`) <- c("1", "0")


# 66A The Past Tense: 4 continuous (but reordered)
#    reordering: 1: "4 no past tense", 2: "1 Present, no remoteness distinctions", 
#             3: "2 Present, 2-3 remoteness distinctions", 4: "3 Present, 4 or more remoteness distinctions"
# Words not expected to be in past/perfect
# levels(wals.languages$`66A The Past Tense`) <- c("2", "3", "4", "1")

# 67A The Future Tense
#    binarization: existent/non-existent
# Words not expected to be in Future
# levels(wals.languages$`67A The Future Tense`) <- c("1", "0")

# 69A Position of Tense/Aspect Affixes: 5 cat. (non-continuous)
#    binarization: no tense-aspect inflection: 0   all others: 1

levels(wals.languages$`69A Position of Tense-Aspect Affixes`) <- c("1", "1", "1", "0")

# 70A The Morphological Imperative: 5 cat. (partial continuous)
#    recategorization: 1: "5 no second person imperatives", 2: "4 second person number-neutral", 
#                     3: "3 Second plural" and "2 Second singular", 4: "1 Second singular and plural"
# Words not in imperative
# levels(wals.languages$`70A The Morphological Imperative`) <- c("4", "3", "3", "2", "1")

# 73A The Optative
#    binarization: present/absent

levels(wals.languages$`73A The Optative`) <- c("1", "0")

# 74A Situational possibility: 3 cat. (non-continuous)
#    binarization: verbal constructions, other markers (particles, adverbials, nouns): 0   affixes on verbs: 1

levels(wals.languages$`74A Situational Possibility`) <- c("1", "0")

# 75A Epistemic Possibility: 3 cat. (non-continuous)
#    binarization: verbal constructions, other markers (particles, adverbials, nouns): 0   affixes on verbs: 1

levels(wals.languages$`75A Epistemic Possibility`) <- c("0", "1", "0")

# 78A Coding of Evidentiality:6 cat. (non-continuous)
#   binarization: no grammatical evidentials, evidential particles: 0
#                 verbal affix or clitic, part of the tense system: 1
#   excluded: modal morpheme (can be either modal verb or bound morpheme), mixed

levels(wals.languages$`78A Coding of Evidentiality`) <- c("0", "1", "1", "0", NA, NA)

# 94A Subordination: 5 cat. (non-continuous)
#   binarization: subordination suffix: 1
#                 all others: 0
#   removed: 5 Mixed

levels(wals.languages$`94A Order of Adverbial Subordinator and Clause`) <- c("0", "0", "1", NA)

# 101A Expression of Pronominal Subjects: 6 cat. (non-continuous)
#   binarization: obligatory pronouns in subj. position, subj. pronouns in different position, optional pronouns in subj. position: 0
#                 subject affixes on verbs, subject clitics on variable host: 1
#   excluded: mixed	

levels(wals.languages$`101A Expression of Pronominal Subjects`) <- c("0", "1", "1", "0", "0", NA)

# 102A Verbal Person Marking: 5 cat. (partially continuous)
#    recategorization: 1: "No person marking", 
#                      2: "only A argument"/"only P argument"/"A or P argument", 3: "A and P argument" 

levels(wals.languages$`102A Verbal Person Marking`) <- c("1", "2", "2", "3")

# 111A Nonperiphrastic causative constructions: 4 cat. (non-continuous)
#   binarization: Neither morph nor compound, compound but not morph.: 0
#                 morph. but not compound: 1
#   excluded: "Both morph. and compound type"

levels(wals.languages$`111A Nonperiphrastic Causative Constructions`) <- c("1", "0", NA)

# 112A Negative Morphemes: 6 cat. (non-continuous)
#   binarization: negative particle, negative word, negative auxiliary verb: 0
#                 negative affix: 1
#   exclude: double negation, variation between negative word and affix

levels(wals.languages$`112A Negative Morphemes`) <- c("1", "0", "0", "0", NA)

wals.languages <- wals.languages %>% 
  mutate_all(function(x){as.numeric(as.character(x))})
max.values <- map_dbl(1:(ncol(wals.languages)), function(x){return(max(wals.languages[,x], na.rm = TRUE))})
wals.languages <- as.matrix(wals.languages) %>% 
  scale(center = FALSE, scale = max.values) %>% 
  as_tibble()
wals.languages$Language <- languages
lang.complexity <- pivot_longer(wals.languages, -Language) %>% 
  group_by(Language) %>% 
  mutate(num.na = is.na(value)) %>% 
  summarize(number.not.na = n() - sum(num.na), complexity = mean(value, na.rm = TRUE))
lang.complexity %>% 
  write_csv("../Data/Processed/morph_complexity.csv")
