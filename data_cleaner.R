require(tidyverse)

# Loading and coding data language data  -----------------------------------------------

# Construct list of languages and language data

all.languages <- read_csv("Data/IDS/languages.csv") %>% 
  mutate(ID = factor(ID))
glottocode <- read_csv("Data/IDS/Glottocode.csv") %>% 
  select(id, family_id, parent_id, latitude, longitude, iso639P3code, country_ids)
wals <- read_csv('Data/Processed/wals_ids.csv') %>% 
  select(-Name) %>% 
  rename(Name = IDS_NAME)

# get list of included languages

included.languages <- read_csv("Data/Processed/included_languages.txt", comment = "#", col_names = "Name", quote = "\"")

# Match WALS and IDS by using the IDS name variable (hand-coded) in wals_ids.csv

all.languages <- all.languages %>% 
  # get rid of information that's better on WALS
  select(-Macroarea, -Latitude, -Longitude) %>% 
  filter(Name %in% included.languages$Name) %>% 
  left_join(select(wals, wals_code, iso_code, Name, glottocode, latitude, longitude, genus, family, macroarea, countrycodes)) %>% 
  arrange(Name)
not.on.wals <- which(is.na(all.languages$wals_code)) %>% 
  all.languages[.,] %>% 
  select(-longitude, -latitude)
all.languages <- all.languages[which(!(is.na(all.languages$wals_code))),]
not.on.wals <- not.on.wals %>% 
  bind_cols(glottocode[match(not.on.wals$ISO639P3code, glottocode$iso639P3code),])

not.on.wals <- not.on.wals %>% 
  # genus gets very messy very fast, just hand-recode family
  mutate(family = family_id) %>% 
  mutate(family = recode(family, 
                         aust1305 = "Austro-Asiatic",
                         nakh1245 = "Nakh-Daghestanian",
                         indo1319 = "Indo-European",
                         pano1259 = "Panoan",
                         taik1256 = "Tai-Kadai",
                         otom1299 = "Otomanguean",
                         araw1281 = "Arawakan",
                         afro1255 = "afro1255",
                         book1242 = "Bookkeeping",
                         nubi1251 = "Nubian",
                         jodi1234 = "Jodi-Saliban"),
         iso_code = iso639P3code, countrycodes = country_ids)

all.languages <- all.languages %>% 
  bind_rows(select(not.on.wals, colnames(all.languages)))

# save relevant data on new file

all.languages <- all.languages %>% 
  select(ID, Name, iso_code, latitude, longitude, family, macroarea, countrycodes)
all.languages %>% 
  write_csv("Data/Processed/all_language_info.csv")

# Loading and coding word data  -----------------------------------------------

# Get all word forms from IDS

all.words <-
  read_csv(
    file = "Data/IDS/forms.csv",
    col_names = T,
    locale = locale(encoding = "UTF-8")
  ) %>%
  mutate(
    Parameter_ID = factor(Parameter_ID),
    transcription = factor(transcription),
    alt_transcription = factor(alt_transcription),
    Language_ID = factor(Language_ID)
  )

# first get language to limit the words to the languages we're including

all.words <- all.words %>% 
  filter(Language_ID %in% all.languages$ID) %>% 
  left_join(select(all.languages, Language_ID = ID, Name)) %>% 
  rename(language = Name) %>% 
  select(-Language_ID, -Segments, -Comment, -Source, -Contribution_ID)

# Link word forms to concepticon to obtain semantic field
ids.to.concepticon <- read_csv("Data/IDS/parameters.csv") %>%
  mutate(ID = factor(ID),
         Concepticon_ID = factor(Concepticon_ID)) %>% 
  select(-Description) %>% 
  rename(Parameter_ID = ID, english.name = Name)
all.words <- left_join(all.words, ids.to.concepticon)

# get semantic field from linkage with concepticon id

concepticon <- read_csv('Data/Concepticon/semanticFields.csv') %>% 
  select(Concepticon_ID = id, ontological.category = ontological_category) %>% 
  mutate(Concepticon_ID = factor(Concepticon_ID))

all.words <- left_join(all.words, concepticon) %>% 
  select(-Concepticon_ID, -ID, -Parameter_ID)

# determine languages and words that will need a manual phon transcription

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

# save to work in phon extraction/scraping python notebook. remove multi words.

no.phon <- no.phon %>% 
  filter(!(str_detect(Form, " ")))

no.phon %>% write_csv('Data/Processed/no_phon.csv')

# store the phon form as "phon" in the dataframe. 
# if the main transcription is phon, get that; 
# else, it means that the alt transcription is the phon one, and get that.
all.words <- all.words %>% 
  filter(!(language %in% no.phon.languages)) %>% 
  mutate(phon = ifelse(transcription == "phon", Form, alt_form))

# Load words that were "hand" mined (modified no.phon data frame)

no.phon.with.phon <- read_csv('Data/Processed/no_phon_with_phon.csv') %>% 
  mutate(method = ifelse(language %in% c("Danish", "English", "Ossetic"), "manual", method)) %>% 
  mutate(phon = str_to_lower(phon))

# add Danish

danish.phon <- read_csv('Data/PhonMining/DataForG2P/danishOrthPhon.csv') %>% 
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

all.phon %>% write_csv('Data/Processed/all_phon.csv')

# # Language data for included languages
# phonLanguages <- allLanguages[match(unique(allPhon$language), allLanguages$Name),]
# phonLanguages %>% write_csv('Data/Processed/allPhonLanguagesConcepticon.csv')
