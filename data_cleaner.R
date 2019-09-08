require(tidyverse)

# Loading and coding data language data  -----------------------------------------------

# Construct list of languages and language data

all.languages <- read_csv("Data/IDS/languages.csv")
# 
# all.languages <- read_csv("Data/Processed/languagesbyhand.csv") %>%
#   mutate(ID = factor(ID)) # list of languages and their information
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
  write_csv("all_language_info.csv")

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
# save to work in IPA EXTRACTION python notebook

no.phon %>% write_csv('no_phon.csv')

all.words <- all.words %>% 
  filter(!(language %in% no.phon.languages))

# Load words that were "hand" mined.
noPhonWithPhon <- read_csv('Data/Processed/noPhonWithPhonFinal.csv') %>%
  filter(!is.na(phon)) %>%
  mutate(phon = str_to_lower(phon)) %>%
  # Remove every multi word from those that were transcribed with multilingual g2p
  filter(!(alt_transcription == "multilingualg2p" & str_detect(Form, "\\s+"))) %>%
  dplyr::select(
    Form,
    transcription,
    englishName,
    language,
    englishPOS,
    languageFamily,
    alt_transcription,
    alt_form,
    phon,
    ontologicalCategory
  )

# Join all word form data
allPhon <- allPhon %>%
  bind_rows(noPhonWithPhon) %>%
  mutate(transcription = factor(transcription), englishName = factor(englishName),
         language = factor(language), englishPOS = factor(englishPOS),
         languageFamily = factor(languageFamily), alt_transcription = factor(alt_transcription),
         ontologicalCategory = factor(ontologicalCategory))

# Previously, levels: "a", "n", "noun", "other", "r", "v", "verb"
levels(allPhon$englishPOS) = c("other", "noun", "noun", "other", "other", "verb", "verb")
levels(allPhon$ontologicalCategory) <- c("Action", "Other", "Other", "Thing", "Other")

# Clean phon forms --------------------------------------------------------

while(length(allPhon$phon[str_detect(allPhon$phon, "~")]) > 0){ # clean multiwords separated by ~
  allPhon$phon[str_detect(allPhon$phon, "~")] <- str_extract(allPhon$phon[str_detect(allPhon$phon, "~")], "[[:graph:]]*[[:space:]]*[[:graph:]]+(?=[[:space:]]*[[:punct:]]*~)")} # extract the first form of multi form entries separated by ~

while(length(allPhon$phon[str_detect(allPhon$phon, "^[-/\\.'’ˈ\\*\\+\\s]")]) > 0){
  allPhon$phon[str_detect(allPhon$phon, "^[-/\\.'’ˈ\\*\\+\\s]")] <- str_remove_all(allPhon$phon[str_detect(allPhon$phon, "^[-/\\.'’ˈ\\*\\+\\s]")],
                                                                                   "^[-/\\.'’ˈ\\*\\+\\s]")
} # Remove all initial dashes, slash, dots, asterisks, quotes, plus signs and spaces

while(length(allPhon$phon[str_detect(allPhon$phon, "[-/\\.'’ˈ\\*\\+\\s]$")]) > 0){
  allPhon$phon[str_detect(allPhon$phon, "[-/\\.'’ˈ\\*\\+\\s]$")] <- str_remove_all(allPhon$phon[str_detect(allPhon$phon, "[-/\\.'’ˈ\\*\\+\\s]$")],
                                                                                   "[-/\\.'’ˈ\\*\\+\\s]$")
} # Remove all final dashes, slash, dots, asterisks, quotes, plus signs and spaces

# Parentheses
allPhon$phon[str_detect(allPhon$phon, "^\\(+.+\\)$")] <- str_remove_all(allPhon$phon[str_detect(allPhon$phon,
                                                                                                "^\\(+.+\\)$")],
                                                                        "[\\(\\)]") # When words are surrounded by () with nothing outside, keep the whole word

allPhon$phon[str_detect(allPhon$phon, "\\(+.+\\)")] <- str_remove_all(allPhon$phon[str_detect(allPhon$phon,
                                                                                                "\\(+.+\\)")],
                                                                        "\\(+.+\\)") # Anything else inside a parenthesis is most probably an optional addendum and it can go

# Another pass of initial and final cleaning

while(length(allPhon$phon[str_detect(allPhon$phon, "^[-/\\.'’ˈ\\*\\+\\s]")]) > 0){
  allPhon$phon[str_detect(allPhon$phon, "^[-/\\.'’ˈ\\*\\+\\s]")] <- str_remove_all(allPhon$phon[str_detect(allPhon$phon, "^[-/\\.'’ˈ\\*\\+\\s]")],
                                                                                   "^[-/\\.'’ˈ\\*\\+\\s]")
} # Remove all initial dashes, slash, dots, asterisks, quotes, plus signs and spaces

while(length(allPhon$phon[str_detect(allPhon$phon, "[-/\\.'’ˈ\\*\\+\\s]$")]) > 0){
  allPhon$phon[str_detect(allPhon$phon, "[-/\\.'’ˈ\\*\\+\\s]$")] <- str_remove_all(allPhon$phon[str_detect(allPhon$phon, "[-/\\.'’ˈ\\*\\+\\s]$")],
                                                                                   "[-/\\.'’ˈ\\*\\+\\s]$")
} # Remove all final dashes, slash, dots, asterisks, quotes, plus signs and spaces

# Square brackets

allPhon$phon[str_detect(allPhon$phon, "^\\[+.+\\]$")] <- str_remove_all(allPhon$phon[str_detect(allPhon$phon,
                                                                                                "^\\[+.+\\]$")],
                                                                        "[\\[\\]]") # When words are surrounded by [] with nothing outside, keep the whole word

allPhon$phon[str_detect(allPhon$phon, "\\[.{1,2}\\]")] <- str_remove_all(allPhon$phon[str_detect(allPhon$phon, "\\[.{1,2}\\]")],
                                                                         "[\\[\\]]") # Remove square brackets when addenda inside square brackets are 1 or characters long
# Another pass of initial and final cleaning

while(length(allPhon$phon[str_detect(allPhon$phon, "^[-/\\.'’ˈ\\*\\+\\s]")]) > 0){
  allPhon$phon[str_detect(allPhon$phon, "^[-/\\.'’ˈ\\*\\+\\s]")] <- str_remove_all(allPhon$phon[str_detect(allPhon$phon, "^[-/\\.'’ˈ\\*\\+\\s]")],
                                                                                   "^[-/\\.'’ˈ\\*\\+\\s]")
} # Remove all initial dashes, slash, dots, asterisks, quotes, plus signs and spaces

while(length(allPhon$phon[str_detect(allPhon$phon, "[-/\\.'’ˈ\\*\\+\\s]$")]) > 0){
  allPhon$phon[str_detect(allPhon$phon, "[-/\\.'’ˈ\\*\\+\\s]$")] <- str_remove_all(allPhon$phon[str_detect(allPhon$phon, "[-/\\.'’ˈ\\*\\+\\s]$")],
                                                                                   "[-/\\.'’ˈ\\*\\+\\s]$")
} # Remove all final dashes, slash, dots, asterisks, quotes, plus signs and spaces

# Slash

allPhon$phon[str_detect(allPhon$phon, "/+.*")] <- str_remove_all(allPhon$phon[str_detect(allPhon$phon, "/+.*")], "/+.*")

# Another pass of initial and final cleaning

while(length(allPhon$phon[str_detect(allPhon$phon, "^[-/\\.'’ˈ\\*\\+\\s]")]) > 0){
  allPhon$phon[str_detect(allPhon$phon, "^[-/\\.'’ˈ\\*\\+\\s]")] <- str_remove_all(allPhon$phon[str_detect(allPhon$phon, "^[-/\\.'’ˈ\\*\\+\\s]")],
                                                                                   "^[-/\\.'’ˈ\\*\\+\\s]")
} # Remove all initial dashes, slash, dots, asterisks, quotes, plus signs and spaces

while(length(allPhon$phon[str_detect(allPhon$phon, "[-/\\.'’ˈ\\*\\+\\s]$")]) > 0){
  allPhon$phon[str_detect(allPhon$phon, "[-/\\.'’ˈ\\*\\+\\s]$")] <- str_remove_all(allPhon$phon[str_detect(allPhon$phon, "[-/\\.'’ˈ\\*\\+\\s]$")],
                                                                                   "[-/\\.'’ˈ\\*\\+\\s]$")
} # Remove all final dashes, slash, dots, asterisks, quotes, plus signs and spaces

# There seems to be no consistent use of [], so just remove them and keep the whole word.

allPhon$phon[str_detect(allPhon$phon, "[\\[\\]]")] <- str_remove_all(allPhon$phon[str_detect(allPhon$phon,
                                                                                             "[\\[\\]]")],
                                                                     "[\\[\\]]")

# allPhon$phon <- str_remove_all(allPhon$phon, "\\([mft\\?]\\.*\\)") # Assume: markers for researcher
# allPhon$phon <- str_remove_all(allPhon$phon, "\\(\\s*t\\.*\\)") # Assume: markers for researchers ( t. and k.)
# allPhon$phon <- str_remove_all(allPhon$phon, "\\(v\\.(n\\.)*\\)") # Assume: markers for researchers ( v. and v.n.)
# allPhon$phon <- str_remove_all(allPhon$phon, "\\s+\\(tn\\)") # Assume: markers for researchers (tn) and (nt)
# allPhon$phon <- str_remove_all(allPhon$phon, "\\s+\\(nt\\)") # Assume: markers for researchers (tn), (nt), (kt)
# allPhon$phon <- str_remove_all(allPhon$phon, "\\s+\\(k\\.*t\\.*\\)") # Assume: markers for researchers (tn), (nt), (kt), (k.t.)
# allPhon$phon <- str_remove_all(allPhon$phon, "[\\[+\\]+]") # Assume: everything inside [] is optional addendums to the words. keep them.
# allPhon$phon <- str_remove_all(allPhon$phon, "\\(.*\\s.*\\)") # If there's a space in the parenthesis, delete the whole parenthesis
# allPhon$phon <- str_remove_all(allPhon$phon, "\\s*\\(\\S{5,}\\)\\s*") # If there's 5 or more non-space characters inside the parenthesis, it's probably an additional/optional form
# allPhon$phon <- str_remove_all(allPhon$phon, "\\s*\\(\\d+\\)\\s*")  # If there's just number, get rid of them

# allPhon$phon[str_detect(allPhon$phon, "\\S\\(.+\\)\\S")] <- str_remove_all(allPhon$phon[str_detect(allPhon$phon, "\\S\\(.+\\)\\S")], "[\\(\\)]") # Assume: if characters are in middle of words, they are optional additions and markers (keep)
# allPhon$phon[str_detect(allPhon$phon, "^[\\(].+\\)\\S")] <- str_remove_all(allPhon$phon[str_detect(allPhon$phon, "^[\\(].+\\)\\S")], "[\\(\\)]") # Same as previous line, but for additions at the beginning of word
# allPhon$phon[str_detect(allPhon$phon, "\\S[\\(].+\\)$")] <- str_remove_all(allPhon$phon[str_detect(allPhon$phon, "\\S[\\(].+\\)$")], "[\\(\\)]") # Additions at the end of the word. After previous couple of lines clean those parentheses, what remains are mostly additions and markers.
# allPhon$phon[str_detect(allPhon$phon, "^\\(.+\\)$")]  <- str_remove_all(allPhon$phon[str_detect(allPhon$phon, "^\\(.+\\)$")], "[\\(\\)]") # If it's just a parenthesis, get rid of the parenthesis.
# allPhon$phon[str_detect(allPhon$phon, "\\(.{2,4}\\)\\(.{2,4}\\)")] <- str_remove(allPhon$phon[str_detect(allPhon$phon, "\\(.{2,4}\\)\\(.{2,4}\\)")], "\\(.{2,4}\\)") # Some languages have two (e.g. (-pɨ)(-pi)) markers at the end. keep only the second one.
# allPhon$phon[str_detect(allPhon$phon, "\\(poss\\.*\\)")] <- str_remove(allPhon$phon[str_detect(allPhon$phon, "\\(poss\\.*\\)")], "\\(poss\\.*\\)") # Lengua has (poss) in some words. Delete them.
# allPhon$phon[str_detect(allPhon$phon, "\\(root\\)")] <- str_remove(allPhon$phon[str_detect(allPhon$phon, "\\(root\\)")], "\\(root\\)") # A word has (root) in it. Delete.
# allPhon$phon[str_detect(allPhon$phon, "\\S[\\(].+\\)$")] <- str_remove_all(allPhon$phon[str_detect(allPhon$phon, "\\S[\\(].+\\)$")], "[\\(\\)]") # Additions at the end of the word. After previous couple of lines clean those parentheses, what remains are mostly additions and markers.
# allPhon$phon[str_detect(allPhon$phon, "\\s*\\(-.{1,3}\\)")] <- str_remove_all(allPhon$phon[str_detect(allPhon$phon, "\\s*\\(-.{1,3}\\)")], "\\s*[\\(\\)]") # Assume: captures optional markers like (-tui), (-mi). maximum match of in-parenthesis is 3 + "-" marker.
# allPhon$phon[str_detect(allPhon$phon, "\\(.{1,3}-\\)\\s*")] <- str_remove_all(allPhon$phon[str_detect(allPhon$phon, "\\(.{1,3}-\\)\\s*")], "\\s*[\\(\\)]") # Assume: captures optional markers like (tui-), (mi-). maximum match of in-parenthesis is 3 + "-" marker.

# allPhon$phon <- str_remove_all(allPhon$phon, "\\s\\(\\S+\\)")  # Assume: all that remains are gender markers (e.g. fem., he, she), verbal markers ("se" as reflexive, "q'or", etc). Remove them from beginning and end. captures patterns like "(tip) word" and "word (tip)"
# allPhon$phon <- str_remove_all(allPhon$phon, "\\(\\S+\\)\\s")  # Assume: all that remains are gender markers (e.g. fem., he, she), verbal markers ("se" as reflexive, "q'or", etc). Remove them from beginning and end. captures patterns like "(tip) word" and "word (tip)"


# allPhon$phon[str_detect(allPhon$phon, "^\\S+\\s$")] <- str_remove_all(allPhon$phon[str_detect(allPhon$phon, "^\\S+\\s$")], "\\s") # Remove spaces at the end of single words
# allPhon$phon[str_detect(allPhon$phon, "^\\s\\S+$")] <- str_remove_all(allPhon$phon[str_detect(allPhon$phon, "^\\s\\S+$")], "\\s") # Remove spaces at the beginning of single words

# multilingual g2p adds spaces between each of the phonemes. before removing all words with spaces that remain, have to fix that.

# allPhon$phon[(allPhon$alt_transcription == "multilingualg2p" & !is.na(allPhon$alt_transcription))] <-
  # str_remove_all(allPhon$phon[(allPhon$alt_transcription == "multilingualg2p" & !is.na(allPhon$alt_transcription))], "\\s+") # Remove spaces between phonemes

# Now remove every word that still has spaces.
# Have to assume by this point that it means that it's encoded as more than one word.

allPhon <- allPhon %>%
  filter(!(str_detect(phon, "\\s"))) %>%  # delete all spaces.
  mutate(phon = str_remove_all(phon, regex("[\\p{Lm}\\p{N}\\p{S}\\p{P}\\p{M}\\s]"))) %>%
  filter(nchar(phon) > 1) # Only keep strings longer than 1


# OPTIONAL: Uncomment to keep only top 100 languages in word form count without counting dialects and variations

# includedLanguages <- read_csv('includedlanguages.csv', col_names = c("Name", "Count"))
# topLanguages <- allPhon[allPhon$language %in% includedLanguages$Name,]
# allPhon <- allPhon %>%
#   filter(language %in% c("Cofán", "Guaraní", "Kaingáng", "Nivaclé", "Sanapaná (Enlhet dialect)", "Waurá")) %>%  # Tildes dropped by excel
#   bind_rows(topLanguages) %>%
#   droplevels()


allPhon <- allPhon %>%
  filter(!(is.na(ontologicalCategory))) %>%  # remove words without concepticon information
  distinct(phon, ontologicalCategory, language, .keep_all = TRUE) %>%
  # group_by(language, phon) %>%
  # mutate(instances = n()) %>% # Include only phon forms that are not repeated in each language
  # filter(instances <= 5) %>%
  # dplyr::select(-instances) %>%
  group_by(language) %>%
  mutate(numberOfWords = n()) %>%
  group_by() %>%
  filter(numberOfWords > 100) %>% # Exclude languages with fewer than 100 phon forms
  dplyr::select(-numberOfWords) %>%
  droplevels()

allPhon %>% write_csv('Data/Processed/allPhonFormsConcepticon.csv')

# Language data for included languages
phonLanguages <- allLanguages[match(unique(allPhon$language), allLanguages$Name),]
phonLanguages %>% write_csv('Data/Processed/allPhonLanguagesConcepticon.csv')




# -


boot.mean <- function(data, indices){
  data <- data[indices]
  return(mean(data))
}
