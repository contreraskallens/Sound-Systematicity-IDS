library(checkpoint)
checkpoint("2019-03-25")
require(tidyverse)


# Loading and coding data  -----------------------------------------------

# Construct list of languages and language data
allLanguages <- read_csv("Data/Processed/languagesbyhand.csv") %>%
  mutate(ID = factor(ID)) # list of languages and their information
glottocode <- read_csv("Data/IDS/Glottocode.csv")

# Get the glottocode family IDs for languages in database. This level of
# hierarchical organization of languages is where e.g. "IndoEuropean" is.
allLanguages$familyID <-
  glottocode$family_id[match(allLanguages$Glottocode, glottocode$id)]

# Some languages have "na" as the narrowest "family", but have coarser
# information in other columns. Get the "parent" family as language family
naLanguages <- is.na(allLanguages$familyID)
allLanguages$familyID[naLanguages] <-
  glottocode$parent_id[match(allLanguages$Glottocode[naLanguages], glottocode$id)]

# Finally, isolates just get their individual glottocode
naLanguages <- is.na(allLanguages$familyID)
allLanguages$familyID[naLanguages] <-
  glottocode$id[match(allLanguages$Glottocode[naLanguages], glottocode$id)]

# Now get the name of each family from the family data of glottocode
glottoFamilies <- read_csv("Data/IDS/families.csv")
allLanguages$familyName <-
  glottoFamilies$name[match(allLanguages$familyID, glottoFamilies$id)]

# Get missing coordinates from WALS
WALSdata <- read_csv('Data/WALS/walslanguage.csv')
naGeo <- is.na(allLanguages$Latitude)
allLanguages$Latitude[naGeo] <- WALSdata$latitude[match(allLanguages$ISO639P3code[naGeo],
                                                        WALSdata$iso_code)]
allLanguages$Longitude[naGeo] <- WALSdata$longitude[match(allLanguages$ISO639P3code[naGeo],
                                                          WALSdata$iso_code)]

# Get all word forms from IDS

allWords <-
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

# Link word forms to conecticon data to further link with wordnet
referenceToConcepticon <- read_csv("Data/IDS/parameters.csv") %>%
  mutate(ID = factor(ID),
         Concepticon_ID = factor(Concepticon_ID))
wordNet <-
  read_delim("Data/IDS/wordnet.tsv", delim = "\t") %>%
  mutate(CONCEPTICON_ID = factor(CONCEPTICON_ID))

excluded.languages <- read_delim("excludedlanguages.txt", delim = "\n", col_names = c("Language"), comment = "#")
excluded.languages$Language <- str_remove_all(excluded.languages$Language, "\t")

# Subset word form data. Append a "Wordnet ID" field through concepticon to get PoS tags.
allWords <- allWords %>%
  dplyr::select(Form,
                Language_ID,
                Parameter_ID,
                transcription,
                alt_transcription,
                alt_form) %>%
  mutate(
    englishName = referenceToConcepticon$Name[match(Parameter_ID, referenceToConcepticon$ID)],
    languageFamily = factor(allLanguages$familyName[match(Language_ID, allLanguages$ID)]),
    concepticonID = factor(referenceToConcepticon$Concepticon_ID[match(Parameter_ID, referenceToConcepticon$ID)]),
    englishPOS = factor(wordNet$WORDNET_POS[match(concepticonID, wordNet$CONCEPTICON_ID)]),
    wordNetID = factor(wordNet$OPEN_WORDNET_ID[match(concepticonID, wordNet$CONCEPTICON_ID)]),
    language = factor(allLanguages$Name[match(Language_ID, allLanguages$ID)])
  ) %>%
  filter(!(language %in% excluded.languages$Language)) %>%  # Delete reconstructed languages and languages with no information
  dplyr::select(
    Form,
    transcription,
    englishName,
    language,
    englishPOS,
    languageFamily,
    alt_transcription,
    alt_form,
    concepticonID
  )

# Recode every useful transcription as "Phon").
# Note that the only "alt_transcription" == "Phonemic (vars)" is Rapa Nui, with only NA in their forms.
levels(allWords$transcription) <- c("CyrillTrans", "Phon", "LatinTrans", "Phon", "Phon",
                                    "Standard", "StandardOrth", "StandardOrthTone")

# Subset based on transcription. Retain either "phon" main transcription, or "phonemic" alt_transcription
allPhon <- allWords %>%
  filter(transcription == "Phon" | alt_transcription == "Phonemic") %>%
  # If the transcription was coded as useful, get that; else, get the alt transcription.
  # Add "Original" as the method of obtaining the transcription.
  droplevels() %>%
  mutate(phon = if_else(transcription == "Phon", Form, alt_form), method = "original") %>%
  mutate(phon = str_to_lower(phon)) %>%
  filter(!is.na(phon))

# add concepticon field

concepticon <- read_csv('Data/Concepticon/semanticFields.csv')
allPhon$ontologicalCategory <- concepticon$ontological_category[match(allPhon$concepticonID, concepticon$id)]

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
