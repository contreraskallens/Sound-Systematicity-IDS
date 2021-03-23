library(tidyverse)
library(rnaturalearth)
library(ggdendro)
library(cluster)
library(geosphere)


# Loading and coding data language data  -----------------------------------------------

# Construct list of languages and language data

# All languages reference to Glottolog was hand fixed for the following languages after it was found they didn't coincide with the codes in glottocode.
# Both Armenians, 
all.languages <- read_csv("../Data/Raw/IDS/languages.csv") %>% 
  mutate(ID = factor(ID))

glottocode <- read_csv("../Data/Raw/IDS/Glottocode.csv") %>% 
  select(Glottocode = id, family_id, parent_id, latitude, longitude, iso639P3code, country_ids)
wals <- read_csv('../Data/Processed/WALS_Codes.csv') %>% 
  select(Name, wals_code = `WALS code`, ID) %>% 
  mutate(ID = as.factor(ID))
wals_info <- read_csv("../Data/Raw/WALS/walslanguage.csv")

# get list of included languages

included.languages <- read_csv("../Data/Processed/included_languages.txt", comment = "#", col_names = "Name", quote = "\"")

# Match WALS and IDS by using the WALS CODE (hand-coded) in wals_codes.csv
all.languages <- all.languages %>% 
  filter(Name %in% included.languages$Name) %>% 
  left_join(wals) %>% 
  select(-Macroarea)
not.on.wals <- all.languages %>% 
  filter(is.na(wals_code))

all.languages <- all.languages %>% 
  filter(!(is.na(wals_code))) %>% 
  # get rid of information that's better on WALS
  select(-Latitude, -Longitude) %>% 
  left_join(select(wals_info, wals_code, latitude, longitude, genus, family))

# Get info of languages not in WALS from Glottolog using Glottocode

not.on.wals <- left_join(not.on.wals, glottocode) %>% 
  select(-Latitude, -Longitude, -iso639P3code, family = family_id, genus = parent_id, - country_ids)
not.on.wals$family[which(is.na(not.on.wals$family))] <- "book1242" # Assign the missing Sanapana to bookkeeping too
not.on.wals$genus[which(is.na(not.on.wals$genus))] <- "book1242" # Assign the missing Sanapana to bookkeeping too


not.on.wals <- not.on.wals %>% 
  mutate(family = recode(family, 
                         afro1255 = "Afro-Asiatic",
                         araw1281 = "Arawakan",
                         aust1305 = "Austro-Asiatic",
                         book1242 = "Bookkeeping",
                         indo1319 = "Indo-European",
                         jodi1234 = "Jodi-Saliban",
                         nakh1245 = "Nakh-Daghestanian",
                         pano1259 = "Panoan",
                         taik1256 = "Tai-Kadai"),

         genus = recode(genus,
                        "boly1240" = "Pakanic",
                        "book1242" = "Bookkeeping",
                        "botl1243" = "Avar-Andic-Tsezic",
                        "bula1260" = "Palaung-Khmuic",
                        "cauc1242" = "Iranian",
                        "chut1252" = "Chutic",
                        "iran1269" = "Iranian",
                        "jodi1234" = "Jodi-Saliban",
                        "khao1243" = "Palaung-Khmuic",
                        "laot1235" = "Kam-Tai",
                        "male1282" = "Chutic",
                        "mang1377" = "Mangic",
                        "maru1251" = "Panoan",
                        "moxo1234" = "Bolivia-Parana",
                        "nort2739" = "Kam-Tai",
                        "nort2744" = "Kadai",
                        "nucl1728" = "Indic",
                        "nyam1284" = "West Chadic",
                        "pare1275" = "Alto Orinoco",
                        "pear1246" = "Pearic", 
                        "pram1235" = "Palaung-Khmuic",
                        "sout2744" = "Kam-Tai",
                        "sout3232" = "Palaung-Khmuic",
                        "viet1250" = "Cuoi",
                        "west2394" = "Pearic")
  )
         

all.languages <- all.languages %>% 
  bind_rows(not.on.wals)

all.languages$family %>% 
  unique() %>% 
  length()


# Save relevant data on new file
all.languages <- all.languages %>% 
  select(ID, Name, iso_code = ISO639P3code, latitude, longitude, family)
all.languages %>% 
  write_csv("../Data/Processed/all_language_info.csv")

# Loading and coding word data  -----------------------------------------------

# Get all word forms from IDS
all.words <- read_csv(file = "../Data/Raw/IDS/forms.csv", col_names = T, locale = locale(encoding = "UTF-8")) %>%
  mutate(Parameter_ID = factor(Parameter_ID),
    transcription = factor(transcription),
    alt_transcription = factor(alt_transcription),
    Language_ID = factor(Language_ID))

# First get language to limit the words to the languages we're including

all.words <- all.words %>% 
  filter(Language_ID %in% all.languages$ID) %>% 
  left_join(select(all.languages, Language_ID = ID, Name)) %>% 
  rename(language = Name) %>% 
  select(-Language_ID, -Segments, -Comment, -Source, -Contribution_ID)

# Link word forms to concepticon to obtain semantic field
ids.to.concepticon <- read_csv("../Data/Raw/IDS/parameters.csv") %>%
  mutate(ID = factor(ID),
         Concepticon_ID = factor(Concepticon_ID)) %>% 
  select(-Description) %>% 
  rename(Parameter_ID = ID, english.name = Name)
all.words <- left_join(all.words, ids.to.concepticon)

# Get semantic field from linkage with concepticon id

concepticon <- read_csv('../Data/Raw/Concepticon/semanticFields.csv') %>% 
  select(Concepticon_ID = id, ontological.category = ontological_category) %>% 
  mutate(Concepticon_ID = factor(Concepticon_ID))

all.words <- left_join(all.words, concepticon) %>% 
  select(-Concepticon_ID, -ID, -Parameter_ID)

# Determine languages and words that will need a manual phon transcription

levels(all.words$transcription) <- c("CyrillTrans", "phon", "LatinTrans", "phon", "phon", "Standard", "StandardOrth", "StandardOrthTone")
# previously: "CyrillTrans"      "IPA"              "LatinTrans"       "Phonemic"         "phonetic"         "Standard"         "StandardOrth"     "StandardOrthTone"
# Recoded IPA, Phonemic, and Phonetic as "phon"

levels(all.words$alt_transcription) <- c("Original (M. R. Key)", "phon", "Phonemic (vars)", "Standard")
# previously: "Original (M. R. Key)" "Phonemic"             "Phonemic (vars)"      "Standard"
# recoded Phonemic as "phon". Phonemic (vars) is only NA (rapa nui).

all.words <- all.words %>% 
  mutate(has.phon = ifelse(transcription == "phon" | alt_transcription == "phon", TRUE, FALSE))
no.phon.languages <- all.words %>% 
  filter(is.na(has.phon) | has.phon == FALSE) %>% 
  .$language %>% 
  unique()
no.phon <- all.words %>% 
  filter(language %in% no.phon.languages)

# Save to work in phon extraction/scraping python notebook. remove multi words.

no.phon <- no.phon %>% 
  filter(!(str_detect(Form, " ")))

no.phon %>% write_csv('../Data/Processed/no_phon.csv')

# Store the phon form as "phon" in the dataframe. 
all.words <- all.words %>% 
  filter(!(language %in% no.phon.languages)) %>% 
  mutate(phon = ifelse(transcription == "phon", Form, alt_form))

# Load words that were "hand" mined (modified no.phon data frame)
no.phon.with.phon <- read_csv('../Data/Processed/no_phon_with_phon.csv') %>% 
  mutate(method = ifelse(language %in% c("Danish", "English", "Ossetic"), "manual", method)) %>% 
  mutate(phon = str_to_lower(phon))

# Add Danish

danish.phon <- read_csv('../Data/Raw/PhonMining/DataForG2P/danishOrthPhon.csv') %>% 
  select(Form = Orth, phon = Phon) %>% 
  add_column(language = "Danish")
danish.frame <- no.phon.with.phon %>% 
  filter(language == "Danish")
danish.frame <- danish.frame %>% 
  select(-phon) %>% 
  left_join(danish.phon)
no.phon.with.phon <- no.phon.with.phon %>% 
  filter(language != "Danish") %>% 
  bind_rows(danish.frame)

# Join all word form data

all.phon <- all.words %>% 
  bind_rows(no.phon.with.phon) %>% 
  select(-transcription, -alt_transcription, -has.phon, -method) %>% 
  mutate(language = factor(language), ontological.category = factor(ontological.category)) %>% 
  filter(!(is.na(phon)))

levels(all.phon$ontological.category) <- c("Action", "Other", "Other", "Thing", "Other")

# Clean phon forms --------------------------------------------------------

while(length(all.phon$phon[str_detect(all.phon$phon, "~")]) > 0){ # clean multiwords separated by ~
  all.phon$phon[str_detect(all.phon$phon, "~")] <- str_extract(all.phon$phon[str_detect(all.phon$phon, "~")], 
                                                               "[[:graph:]]*[[:space:]]*[[:graph:]]+(?=[[:space:]]*[[:punct:]]*~)")} # extract the first form of multi form entries separated by ~

while(length(all.phon$phon[str_detect(all.phon$phon, "^[-/\\.'’ˈ\\*\\+\\s]")]) > 0){
  all.phon$phon[str_detect(all.phon$phon, "^[-/\\.'’ˈ\\*\\+\\s]")] <- str_remove_all(all.phon$phon[str_detect(all.phon$phon, "^[-/\\.'’ˈ\\*\\+\\s]")],
                                                                                   "^[-/\\.'’ˈ\\*\\+\\s]")
} # Remove all initial dashes, slash, dots, asterisks, quotes, plus signs and spaces

while(length(all.phon$phon[str_detect(all.phon$phon, "[-/\\.'’ˈ\\*\\+\\s]$")]) > 0){
  all.phon$phon[str_detect(all.phon$phon, "[-/\\.'’ˈ\\*\\+\\s]$")] <- str_remove_all(all.phon$phon[str_detect(all.phon$phon, "[-/\\.'’ˈ\\*\\+\\s]$")],
                                                                                   "[-/\\.'’ˈ\\*\\+\\s]$")
} # Remove all final dashes, slash, dots, asterisks, quotes, plus signs and spaces

# Parentheses
all.phon$phon[str_detect(all.phon$phon, "^\\(+.+\\)$")] <- str_remove_all(all.phon$phon[str_detect(all.phon$phon,
                                                                                                "^\\(+.+\\)$")],
                                                                        "[\\(\\)]") # When words are surrounded by () with nothing outside, keep the whole word

all.phon$phon[str_detect(all.phon$phon, "\\(+.+\\)")] <- str_remove_all(all.phon$phon[str_detect(all.phon$phon,
                                                                                                "\\(+.+\\)")],
                                                                        "\\(+.+\\)") # Anything else inside a parenthesis is most probably an optional addendum and it can go

# Another pass of initial and final cleaning

while(length(all.phon$phon[str_detect(all.phon$phon, "^[-/\\.'’ˈ\\*\\+\\s]")]) > 0){
  all.phon$phon[str_detect(all.phon$phon, "^[-/\\.'’ˈ\\*\\+\\s]")] <- str_remove_all(all.phon$phon[str_detect(all.phon$phon, "^[-/\\.'’ˈ\\*\\+\\s]")],
                                                                                   "^[-/\\.'’ˈ\\*\\+\\s]")
} # Remove all initial dashes, slash, dots, asterisks, quotes, plus signs and spaces

while(length(all.phon$phon[str_detect(all.phon$phon, "[-/\\.'’ˈ\\*\\+\\s]$")]) > 0){
  all.phon$phon[str_detect(all.phon$phon, "[-/\\.'’ˈ\\*\\+\\s]$")] <- str_remove_all(all.phon$phon[str_detect(all.phon$phon, "[-/\\.'’ˈ\\*\\+\\s]$")],
                                                                                   "[-/\\.'’ˈ\\*\\+\\s]$")
} # Remove all final dashes, slash, dots, asterisks, quotes, plus signs and spaces

# Square brackets

all.phon$phon[str_detect(all.phon$phon, "^\\[+.+\\]$")] <- str_remove_all(all.phon$phon[str_detect(all.phon$phon,
                                                                                                "^\\[+.+\\]$")],
                                                                        "[\\[\\]]") # When words are surrounded by [] with nothing outside, keep the whole word

all.phon$phon[str_detect(all.phon$phon, "\\[.{1,2}\\]")] <- str_remove_all(all.phon$phon[str_detect(all.phon$phon, "\\[.{1,2}\\]")],
                                                                         "[\\[\\]]") # Remove square brackets when addenda inside square brackets are 1 or characters long
# Another pass of initial and final cleaning

while(length(all.phon$phon[str_detect(all.phon$phon, "^[-/\\.'’ˈ\\*\\+\\s]")]) > 0){
  all.phon$phon[str_detect(all.phon$phon, "^[-/\\.'’ˈ\\*\\+\\s]")] <- str_remove_all(all.phon$phon[str_detect(all.phon$phon, "^[-/\\.'’ˈ\\*\\+\\s]")],
                                                                                   "^[-/\\.'’ˈ\\*\\+\\s]")
} # Remove all initial dashes, slash, dots, asterisks, quotes, plus signs and spaces

while(length(all.phon$phon[str_detect(all.phon$phon, "[-/\\.'’ˈ\\*\\+\\s]$")]) > 0){
  all.phon$phon[str_detect(all.phon$phon, "[-/\\.'’ˈ\\*\\+\\s]$")] <- str_remove_all(all.phon$phon[str_detect(all.phon$phon, "[-/\\.'’ˈ\\*\\+\\s]$")],
                                                                                   "[-/\\.'’ˈ\\*\\+\\s]$")
} # Remove all final dashes, slash, dots, asterisks, quotes, plus signs and spaces

# Slash

all.phon$phon[str_detect(all.phon$phon, "/+.*")] <- str_remove_all(all.phon$phon[str_detect(all.phon$phon, "/+.*")], "/+.*")

# Another pass of initial and final cleaning

while(length(all.phon$phon[str_detect(all.phon$phon, "^[-/\\.'’ˈ\\*\\+\\s]")]) > 0){
  all.phon$phon[str_detect(all.phon$phon, "^[-/\\.'’ˈ\\*\\+\\s]")] <- str_remove_all(all.phon$phon[str_detect(all.phon$phon, "^[-/\\.'’ˈ\\*\\+\\s]")],
                                                                                   "^[-/\\.'’ˈ\\*\\+\\s]")
} # Remove all initial dashes, slash, dots, asterisks, quotes, plus signs and spaces

while(length(all.phon$phon[str_detect(all.phon$phon, "[-/\\.'’ˈ\\*\\+\\s]$")]) > 0){
  all.phon$phon[str_detect(all.phon$phon, "[-/\\.'’ˈ\\*\\+\\s]$")] <- str_remove_all(all.phon$phon[str_detect(all.phon$phon, "[-/\\.'’ˈ\\*\\+\\s]$")],
                                                                                   "[-/\\.'’ˈ\\*\\+\\s]$")
} # Remove all final dashes, slash, dots, asterisks, quotes, plus signs and spaces

# There seems to be no consistent use of [] outside of 1 character long, so just remove them and keep the whole word.

all.phon$phon[str_detect(all.phon$phon, "[\\[\\]]")] <- str_remove_all(all.phon$phon[str_detect(all.phon$phon,
                                                                                             "[\\[\\]]")],
                                                                     "[\\[\\]]")

# Now remove every word that still has spaces.
# Have to assume by this point that it means that it's encoded as more than one word.

all.phon <- all.phon %>%
  filter(!(str_detect(phon, "\\s"))) %>%  # delete all spaces.
  mutate(phon = str_remove_all(phon, regex("[\\p{Lm}\\p{N}\\p{S}\\p{P}\\p{M}\\s]"))) %>%
  filter(nchar(phon) > 1) # Only keep strings longer than 1

all.phon <- all.phon %>%
  filter(!(is.na(ontological.category))) %>%  # remove words without concepticon information
  distinct(phon, ontological.category, language, .keep_all = TRUE) %>%
  group_by(language) %>%
  mutate(numberOfWords = n()) %>%
  group_by() %>%
  filter(numberOfWords > 100) %>% # Exclude languages with fewer than 100 phon forms
  dplyr::select(-numberOfWords) %>%
  droplevels()

all.phon %>% write_csv('../Data/Processed/all_phon.csv')


# Create a morphology-adjusted dataset --------

# Create a copy of the original dataset to modify "in place"
all.phon.adjusted <- all.phon



# ----- ALTERNATIVE PROCEDURE
# 
# 
# all.langs.info.suffix <- map_dfr(unique(all.phon$language), function(this.language){
#   print(this.language)
#   lang.df <- all.phon %>% 
#     filter(language == this.language) %>% 
#     mutate(ending = str_sub(phon, "-1"), ending = factor(ending)) %>% 
#     filter(ontological.category != "Other") %>% 
#     droplevels()
#   all.combs <- expand.grid(-1:-7, -1:-7)
#   
#   all.mut.info <- map_dfr(1:nrow(all.combs), function(combination){
#     remove.actions <- all.combs[combination, 1]
#     remove.things <- all.combs[combination, 2]
#     this.attempt <- lang.df %>% 
#       mutate(phon = ifelse(ontological.category == "Action", 
#                            str_sub(phon, "1", as.character(remove.actions)), 
#                            str_sub(phon, "1", as.character(remove.things))
#                            ), 
#              ending = str_sub(phon, "-1"), ending = as.factor(ending)) %>% 
#       filter(nchar(phon) > 2) %>% 
#       droplevels()
#     if(nrow(this.attempt) == 0){return(tibble(actions.removed = NA, things.removed = NA, mut.info = NA, words.remaining = NA))}
#     cat <- this.attempt$ontological.category
#     affix <- this.attempt$ending
#     mutual.information <- mutinformation(affix, cat) / sqrt(entropy(cat) * entropy(affix))
#     tibble(actions.removed = remove.actions, 
#            things.removed = remove.things, 
#            mut.info = mutual.information, 
#            words.remaining = nrow(this.attempt) / nrow(lang.df)) %>% 
#       return()
#   }) %>% 
#     filter(!(is.na(mut.info)))
#   
#   all.mut.info <- all.mut.info %>% 
#     arrange(desc(words.remaining)) %>% 
#     add_column(language = this.language)
# })
# 
# quantiles <- quantile(all.langs.info.suffix$mut.info, probs = seq(0, 1, 0.25))
# all.langs.info.suffix %>% ggplot(aes(x = mut.info)) + geom_density(fill = "blue", alpha = 0.3) + geom_vline(xintercept = quantiles)
# 
# 
# suffix.removal <- all.langs.info.suffix %>% 
#   group_by(language) %>% 
#   filter(mut.info <= quantiles[2]) %>% 
#   top_n(1, words.remaining)
# missing.langs <- setdiff(unique(all.phon$language), suffix.removal$language)
# missing.langs <- all.langs.info.suffix %>% 
#   filter(language %in% missing.langs) %>% 
#   filter(mut.info <= 0.1) %>% 
#   group_by(language) %>% 
#   top_n(1, words.remaining)
# suffix.removal <- bind_rows(suffix.removal, missing.langs)
# 
# all.phon.adjusted <- map_dfr(1:nrow(suffix.removal), function(this.row){
#   this.language <- suffix.removal$language[this.row]
#   action.position <- suffix.removal$actions.removed[this.row]
#   thing.position <- suffix.removal$things.removed[this.row]
#   all.phon %>% 
#     filter(language == this.language) %>%
#     mutate(phon = ifelse(ontological.category == "Action",
#                          str_sub(phon, "1", as.character(action.position)),
#                          str_sub(phon, "1", as.character(thing.position))))
#     
# })
# 
# write_csv(suffix.removal, "suffix_info.csv")
# 
# all.langs.info.preffix <- map_dfr(unique(all.phon$language), function(this.language){
#   print(this.language)
#   lang.df <- all.phon %>% 
#     filter(language == this.language) %>% 
#     mutate(beginning = str_sub(phon, "1", "1"), beginning = factor(beginning)) %>% 
#     filter(ontological.category != "Other") %>% 
#     droplevels()
#   all.combs <- expand.grid(1:7, 1:7)
#   
#   all.mut.info <- map_dfr(1:nrow(all.combs), function(combination){
#     remove.actions <- all.combs[combination, 1]
#     remove.things <- all.combs[combination, 2]
#     this.attempt <- lang.df %>% 
#       mutate(phon = ifelse(ontological.category == "Action", 
#                            str_sub(phon, as.character(remove.actions)), 
#                            str_sub(phon, as.character(remove.things))
#       ), 
#       beginning = str_sub(phon, "1", "1"), beginning = as.factor(beginning)) %>% 
#       filter(nchar(phon) > 2) %>% 
#       droplevels()
#     if(nrow(this.attempt) == 0){return(tibble(actions.removed = NA, things.removed = NA, mut.info = NA, words.remaining = NA))}
#     cat <- this.attempt$ontological.category
#     affix <- this.attempt$beginning
#     mutual.information <- mutinformation(affix, cat) / sqrt(entropy(cat) * entropy(affix))
#     tibble(actions.removed = remove.actions, 
#            things.removed = remove.things, 
#            mut.info = mutual.information, 
#            words.remaining = nrow(this.attempt) / nrow(lang.df)) %>% 
#       return()
#   }) %>% 
#     filter(!(is.na(mut.info)))
#   
#   all.mut.info <- all.mut.info %>% 
#     arrange(desc(words.remaining)) %>% 
#     add_column(language = this.language)
# })
# 
# quantiles <- quantile(all.langs.info.preffix$mut.info, probs = seq(0, 1, 0.25))
# all.langs.info.preffix %>% ggplot(aes(x = mut.info)) + geom_density(fill = "blue", alpha = 0.3) + geom_vline(xintercept = quantiles)
# 
# 
# preffix.removal <- all.langs.info.preffix %>% 
#   group_by(language) %>% 
#   filter(mut.info <= quantiles[2]) %>% 
#   top_n(1, words.remaining)
# missing.langs <- setdiff(unique(all.phon$language), preffix.removal$language)
# missing.langs <- all.langs.info.preffix %>% 
#   filter(language %in% missing.langs) %>% 
#   filter(mut.info <= 0.1) %>% 
#   group_by(language) %>% 
#   top_n(1, words.remaining)
# preffix.removal <- bind_rows(preffix.removal, missing.langs)
# 
# all.phon.adjusted <- map_dfr(1:nrow(preffix.removal), function(this.row){
#   this.language <- preffix.removal$language[this.row]
#   action.position <- preffix.removal$actions.removed[this.row]
#   thing.position <- preffix.removal$things.removed[this.row]
#   all.phon.adjusted %>% 
#     filter(language == this.language) %>%
#     mutate(phon = ifelse(ontological.category == "Action",
#                          str_sub(phon, as.character(action.position)),
#                          str_sub(phon, as.character(thing.position))))
#   
# })
# 
# adjustment.census <- select(suffix.removal, language, suffix.action = actions.removed, suffix.things = things.removed) %>% 
#   left_join(select(preffix.removal, preffix.action = actions.removed, preffix.things = things.removed, language)) %>% 
#   replace_na(replace = list(0, 0, 0, 0, 0)) %>% 
#   mutate_if(is.numeric, function(x){return(abs(x) - 1)})
# 
# all.phon.adjusted <- filter(all.phon.adjusted, nchar(phon) > 2)
# 
# homophone.census <- all.phon.adjusted %>% 
#   group_by(language, ontological.category, phon) %>% 
#   tally() %>% 
#   filter(n > 1) %>% 
#   group_by() %>% 
#   group_by(language, ontological.category) %>% 
#   tally()
# 
# homophone.census <- homophone.census %>% 
#   group_by(language) %>% 
#   dplyr::summarize(Number.Homophones = sum(n))
# all.phon.adjusted <- all.phon.adjusted %>% 
#   distinct(language, ontological.category, phon, .keep_all = TRUE)
# 
# write_csv(all.phon.adjusted, "../Data/Processed/all_phon_adjusted_mi.csv")

# --- Old procedure -------


# Store all languages that had something removed in a list
marker.group <- list()

# Prepare a list with languages that had suffixes removed and how many
ending.census <- list()
lang <- "Spanish"

lang.df <- all.phon.adjusted %>% 
  filter(language == lang,
         ontological.category == "Action") %>% 
  droplevels()


get.ngram.endings <- function(language.df, level){
  ngram.df <- language.df %>% 
    mutate(ending = str_sub(phon, level),
           successor = str_sub(phon, level - 1, level - 1))
  return(ngram.df)
}

get.ngram.entropy <- function(ngram.df, ngram){
  filtered.df <- filter(ngram.df, ending == ngram) 
  ngram.entropy <- infotheo::entropy(filtered.df$successor)
  results <- list("ngram" = ngram, "entropy" = ngram.entropy)
  return(results)
}

unigram.endings <- get.ngram.endings(lang.df, -1)
for(unigram in unique(unigram.endings$ending)){
  unigram.entropy <- get.ngram.entropy(unigram.endings, unigram)
  # check if bigger
  gram.df <- filter(unigram.endings, ending == unigram)
  bigram.endings <- get.ngram.endings(gram.df, -2)
  for(bigram in unique(bigram.endings$ending)){
    bigram.entropy <- get.ngram.entropy(bigram.endings, bigram)
    print(paste("ngrams", unigram.entropy$ngram, bigram.entropy$ngram))
    print(paste("entropies", unigram.entropy$entropy, bigram.entropy$entropy))
    if(bigram.entropy$entropy > unigram.entropy$entropy){print(paste("morph marker might be", bigram))} else{
      print("morph marker is further ahead")
    }
  }
}

for(ngram in unique(lang.df$ending.bi)){
  ngram.df <- filter(lang.df, ending.uni == unigram.test,
         ending.bi == ngram)
  if(nrow(ngram.df) == 0){
    next
  }
  ngram.df <- ngram.df %>% 
    mutate(successors = str_sub(phon, "-3", "-3"))
  print(ngram)
  print(infotheo::entropy(ngram.df$successors))
}
bigram.test <- "ar"
for(ngram in unique(lang.df$ending.tr)){
  ngram.df <- filter(lang.df, ending.uni == unigram.test,
                     ending.bi == bigram.test, 
                     ending.tr == ngram)
  if(nrow(ngram.df) == 0){
    next
  }
  # print(head(ngram.df))
  
  ngram.df <- ngram.df %>%
    mutate(successors = str_sub(phon, "-4", "-4"))
  print(ngram)
  print(infotheo::entropy(ngram.df$successors))
}
trigram.test <- "rse"
lang.df <- lang.df %>% 
  mutate(ending.te = str_sub(phon, "-4"))
for(ngram in unique(lang.df$ending.te)){
  ngram.df <- filter(lang.df, ending.uni == unigram.test,
                     ending.bi == bigram.test, 
                     ending.tr == trigram.test,
                     ending.te == ngram)
  if(nrow(ngram.df) == 0){
    next
  }
  # print(head(ngram.df))
  
  ngram.df <- ngram.df %>%
    mutate(successors = str_sub(phon, "-5", "-5"))
  print(ngram)
  print(infotheo::entropy(ngram.df$successors))
}
lang.df <- lang.df %>% 
  mutate(ending.pe = str_sub(phon, "-5"))

tetragram.test <- "arse"
for(ngram in unique(lang.df$ending.pe)){
  # print(ngram)
  ngram.df <- filter(lang.df, ending.uni == unigram.test,
                     ending.bi == bigram.test, 
                     ending.tr == trigram.test,
                     ending.te == tetragram.test,
                     ending.pe == ngram)
  if(nrow(ngram.df) == 0){
    next
  }
  ngram.df <- ngram.df %>%
    mutate(successors = str_sub(phon, "-6", "-6"))
  print(ngram)
  print(infotheo::entropy(ngram.df$successors))
}



bigram.test <- unique(lang.df$ending.bi)[1]
successor <- lang.df %>% 
  filter(ending.bi == bigram.test) %>% 
  mutate(bigram.successor = str_sub(phon, "-3", "-3")) %>% 
  .$bigram.successor
infotheo::entropy(successor)



test <- lang.df %>% 
  mutate(second.ending = str_sub(phon, "-2", "-2"),
         third.ending = str_sub(phon, "-3", "-3"),
         fourth.ending = str_sub(phon, "-4", "-4"),
         fifth.ending = str_sub(phon, "-5", "-5"))

model <- MASS::lda(data = test, ontological.category ~ fifth.ending)
prediction <- predict(model, dplyr::select(test, -ontological.category))
caret::confusionMatrix(data = prediction$class, reference = test$ontological.category)$overall["Kappa"]




entropy_distribution <- map_dbl(1:1000, function(i){
  if(i %% 50 == 0){print(i)}
  df <- filter(lang.df, ontological.category == "Action") %>% 
    mutate(phon = stringi::stri_rand_shuffle(phon),
           ending = str_sub(phon, "-1", "-1"))
  return(entropy(table(df$ending)))
})


mean(entropy_distribution) - sd(entropy_distribution)
entropy(table(test$ending))
entropy(table(test$second.ending))
entropy(table(test$third.ending))
entropy(table(test$fourth.ending))



infotheo::entropy(table(test$ending))
infotheo::entropy(table(test$second.ending))


contingency.table <- table(lang.df$ending, lang.df$ontological.category)
(contingency.table / rowSums(contingency.table)) %>% 
  round(3)

corrected.lang.df <- lang.df %>% 
  mutate(phon = str_sub(phon, "1", "-2"),
         ending = str_sub(phon, "-1"))

contingency.table <- table(corrected.lang.df$ending, corrected.lang.df$ontological.category)
(contingency.table / rowSums(contingency.table)) %>% 
  round(3)

corrected.lang.df <- lang.df %>% 
  mutate(phon = ifelse(ontological.category == "Action", str_sub(phon, "1", "-2"), phon),
         ending = str_sub(phon, "-1"))



map_dfr(c("Action", "Thing", "Other"), function(category){
  cat.df <- lang.df %>% 
    filter(ontological.category == category)
  # Get the number of words that share the same suffix
  number <- nrow(cat.df)
  proportions <- cat.df %>% 
    mutate(ending = str_sub(phon, "-1")) # %>% 
  # .$ending %>% 
  # table()
  proportions <- proportions / number
  # Check whether any suffix is present in more than a third of the words of that category
  has.marker <- proportions > 0.33
  # If *any* of them do, return a tibble where "marker" is TRUE
  if(TRUE %in% has.marker){
    return(tibble(ontological.category = category, marker = TRUE))
  } 
  else{
    return(tibble(ontological.category = category, marker = FALSE))
  }
  
})
# Loop through languages 6 times. After 6 passes, no suffixes are removed.

for(i in 1:6){
  markers <- purrr::map_dfr(sort(unique(all.phon.adjusted$language)), function(lang){
    # For each language, make a dataframe of each category
    lang.df <- all.phon.adjusted %>% 
      filter(language == lang)
    map_dfr(c("Action", "Thing", "Other"), function(category){
      cat.df <- lang.df %>% 
        filter(ontological.category == category)
      # Get the number of words that share the same suffix
      number <- nrow(cat.df)
      proportions <- cat.df %>% 
        mutate(ending = str_sub(phon, "-1")) # %>% 
        # .$ending %>% 
        # table()
      proportions <- proportions / number
      # Check whether any suffix is present in more than a third of the words of that category
      has.marker <- proportions > 0.33
      # If *any* of them do, return a tibble where "marker" is TRUE
      if(TRUE %in% has.marker){
        return(tibble(ontological.category = category, marker = TRUE))
      } 
      else{
        return(tibble(ontological.category = category, marker = FALSE))
      }
      
    }) %>% 
      add_column(language = lang)
  })
  # Add an element to the census with all the languages that had a marker when number of chars = i.
  ending.census[[i]] <- markers %>% 
    filter(marker == TRUE) %>%
    add_column(ending.markers = i)
  langs.with.markers <- markers %>% 
    filter(marker == TRUE) %>% 
    .$language %>% 
    unique()
  # Add languages with markers to the marker group list
  marker.group[[i]] <- c(langs.with.markers)
  
  # For each language that had a marker when number of chars = i, update that
  # language's entries in all.phon.adjusted with the strings without the last
  # character
  all.phon.adjusted <- map_dfr(1:nrow(markers), function(index){
    lang <- markers$language[index]
    category <- markers$ontological.category[index]
    has.marker <- markers$marker[index]
    if(has.marker){
      all.phon.adjusted %>% 
        filter(lang == language, ontological.category == category) %>% 
        # Note that after i = 1, all loops will consider strings in their
        # ADJUSTED forms, such that the next line will remove each subsequent
        # character instead of just the suffix.
        mutate(phon = str_sub(phon, "1", "-2")) %>% 
        return()
    }else{
      all.phon.adjusted %>% 
        filter(lang == language, ontological.category == category) %>% 
        return()
    }
  })
  # Delete all words that are now only 1 character long.
  all.phon.adjusted <- all.phon.adjusted %>% 
    filter(nchar(phon) > 1)
}

# Generate a "suffix census" dataframe from the list.
ending.census <- ending.census %>% 
  bind_rows() %>% 
  group_by(language, ontological.category) %>% 
  dplyr::summarize(ending.markers = max(ending.markers))


# Same procedure as above, but for preffixes.
start.langs <- list()
start.census <- list()

for(i in 1:3){
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
  start.census[[i]] <- markers %>% 
    filter(marker == TRUE) %>% 
    add_column(start.markers = i)
  
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
  group_by(language, ontological.category) %>% 
  dplyr::summarize(start.markers = max(start.markers))

# Join suffix and affix census into one dataframe.
marker.census <- full_join(start.census, ending.census) %>% 
  # Note that Munduruku and Kanok had only "Other" words adjusted, so not on census
  filter(ontological.category != "Other")
# Generate category-specific census
action.census <- marker.census %>% 
  filter(ontological.category == "Action") %>% 
  select(-ontological.category) %>% 
  rename(Preffixes.Removed.Action = start.markers, Suffixes.Removed.Action = ending.markers)
thing.census <- marker.census %>% 
  filter(ontological.category == "Thing") %>% 
  select(-ontological.category) %>% 
  rename(Preffixes.Removed.Thing = start.markers, Suffixes.Removed.Thing = ending.markers)
marker.census <-full_join(action.census, thing.census) 
all.marker.group <- unique(marker.census$language)
# Make an "empty" census for languages with no markers removed.
no.marker.group <- setdiff(unique(all.phon$language), all.marker.group)

# Save unmodified languages for reference
no.marker.group %>% 
 write_rds("../Data/Processed/no_marker_group.rds")

no.marker.group.table <- tibble(language = no.marker.group, Preffixes.Removed.Action = 0,
                                Suffixes.Removed.Action = 0, Preffixes.Removed.Thing = 0, 
                                Suffixes.Removed.Thing = 0)

# For each language in the database, display how many suffixes and affixes were
# removed for each category, including languages without adjustment.
marker.census.complete <- marker.census %>% 
  bind_rows(no.marker.group.table) %>% 
  group_by() %>% 
  mutate_if(is.numeric, function(x){replace_na(x, 0)}) %>% 
  arrange(language)

# Further adjustment: remove within-class homophones but keep between-class homophones.
# Gets rid of within-class colexification
# Save how many homophones were removed.

homophone.census <- all.phon.adjusted %>% 
  group_by(language, ontological.category, phon) %>% 
  tally() %>% 
  filter(n > 1) %>% 
  group_by() %>% 
  group_by(language, ontological.category) %>% 
  tally()

homophone.census <- homophone.census %>% 
  group_by(language) %>% 
  dplyr::summarize(Number.Homophones = sum(n))
all.phon.adjusted <- all.phon.adjusted %>% 
  distinct(language, ontological.category, phon, .keep_all = TRUE)

# Join marker census with homophone census, and compare original/adjusted number of words
original.number.words <- all.phon %>% group_by(language) %>% dplyr::summarize(original.number.words = n())
adjusted.number.words <- all.phon.adjusted %>% group_by(language) %>% dplyr::summarize(adjusted.number.words = n())

marker.census.complete <- left_join(marker.census.complete, original.number.words) %>% 
  left_join(adjusted.number.words) %>% 
  left_join(select(all.languages, language = Name, Family = family)) %>% 
  left_join(homophone.census) %>% 
  select(language, Family, everything())

# Replace NA with 0 for the numeric columns. NAs result from languages without within-class homophones to remove.
marker.census.complete <- mutate_if(.tbl = marker.census.complete, .predicate = is.numeric, .funs = function(x){replace_na(x, 0)})

write_csv(marker.census.complete, "../Data/Processed/marker_census.csv")
write_csv(all.phon.adjusted, "../Data/Processed/all_phon_adjusted.csv")


# Make geographical clusters and world map of languages ---------------

# Make it only with languages that have geographical information
reduced <- select(all.languages, Name,longitude, latitude) %>% 
  filter(complete.cases(.))

# Make plot World plot
world <- ne_coastline(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf(size = 0.1, alpha = 0.9) +
  coord_sf(ylim = c(-50, 90)) +
  geom_jitter(data = reduced, aes(x = longitude, y = latitude), color = c("#62c08f"),
              size = 1, shape = 16) +
  cowplot::theme_map()
ggsave("../Figures/Main/world_map.png", width = 8.7, height = 7, units = "cm", dpi = 300)


# Make a matrix with only longitude and latitude
geo.matrix <- as.matrix(select(reduced, longitude, latitude))
row.names(geo.matrix) <- reduced$Name

# Use geosphere distance matrix to get a geodesic distance matrix between all the different coordinates
geo.matrix <- distm(geo.matrix)
row.names(geo.matrix) <- reduced$Name
geo.matrix <- as.dist(geo.matrix)

# Hierarchical cluster analysis using Ward's method
cluster.regions <- hclust(geo.matrix, method = "ward.D2")

# Choose number of clusters using Silhouettes
silhouettes <- map_dbl(2:100, function(i){
  silhouette(cutree(cluster.regions, k = i), dist = geo.matrix) %>% as.matrix %>% .[,"sil_width"] %>% mean %>% return
  })
silhouettes <- silhouettes %>% enframe %>% mutate(name = 2:100)
ggplot(silhouettes, aes(x = name, y = value)) + geom_point() + geom_label(aes(label = name))

# Plot shows that a good number of clusters is k = 20.
# Make a color vector of 20 colors
col_vector<-c('#e6194b', '#3cb44b', '#ffe119', '#4363d8','#f58231', 
              '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', 
              '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', 
              '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080')

# Make a dataframe with all langauge information including WALS info, Family and Geo Cluster
language.groups <- reduced %>% 
  add_column(geo.cluster = cutree(cluster.regions, k = 20)) %>% 
  select(Name, geo.cluster) %>% 
  mutate(geo.cluster = factor(geo.cluster)) %>% 
  right_join(all.languages)
language.groups %>% write_csv("../Data/Processed/language_groups.csv")

# Make a world map with clusters surrounded by a convex hull
hull <- language.groups %>% 
  filter(!(is.na(geo.cluster))) %>% 
  group_by(geo.cluster) %>% 
  slice(chull(longitude, latitude))

# Manually modify to avoid weird cluster 12
cluster.12 <- hull %>% 
  filter(geo.cluster == "12")
hull <- hull %>% 
  filter(geo.cluster != "12")
cluster.12.right <- cluster.12 %>% 
  filter(Name != "Tongan") %>% 
  group_by() %>% 
  add_row(latitude = -19, longitude = 179) %>% 
  add_row(latitude = -30, longitude = 179)
cluster.12.left <- cluster.12 %>% 
  filter(Name == "Tongan") %>% 
  group_by() %>% 
  add_row(latitude = -19, longitude = -179) %>% 
  add_row(latitude = -30, longitude = -179)

# Make cluster plot
ggplot(data = world) +
  geom_sf(size = 0.1, alpha = 0.9) +
  coord_sf(ylim = c(-50, 90)) +
  geom_jitter(data = language.groups, aes(x = longitude, y = latitude, color = geo.cluster), 
              size = 1, shape = 16) + 
  geom_polygon(data = hull, aes(x = longitude, y = latitude, fill = geo.cluster), alpha = 0.5) +
  geom_polygon(data = cluster.12.right, fill = col_vector[12], aes(x = longitude, y = latitude), alpha = 0.5) +
  scale_color_manual(values = col_vector) +
  scale_fill_manual(values = col_vector[-12]) +
  geom_polygon(data = cluster.12.left, fill = col_vector[12], aes(x = longitude, y = latitude), alpha = 0.5) +
  cowplot::theme_map() +
  theme(legend.position = "none")

ggsave("../Figures/Supplemental/geo_cluster_map.png", width = 17, height = 9, units = "cm", dpi = 900)
