require(tidyverse)

# Loading and coding data language data  -----------------------------------------------

# Construct list of languages and language data

all.languages <- read_csv("../Data/IDS/languages.csv") %>% 
  mutate(ID = factor(ID))
glottocode <- read_csv("../Data/IDS/Glottocode.csv") %>% 
  select(id, family_id, parent_id, latitude, longitude, iso639P3code, country_ids)
wals <- read_csv('../Data/Processed/wals_ids.csv') %>% 
  select(-Name) %>% 
  rename(Name = IDS_NAME)

# get list of included languages

included.languages <- read_csv("../Data/Processed/included_languages.txt", comment = "#", col_names = "Name", quote = "\"")

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
  bind_cols(glottocode[match(not.on.wals$Glottocode, glottocode$id),])

not.on.wals <- not.on.wals %>% 
  # genus gets very messy very fast, just hand-recode family
  mutate(family = family_id, iso_code = iso639P3code, countrycodes = country_ids)

all.languages <- all.languages %>% 
  bind_rows(select(not.on.wals, colnames(all.languages))) %>% 
  mutate(family = recode(family, 
                  aust1305 = "Austro-Asiatic",
                  aust1307 = "Austronesian",
                  nakh1245 = "Nakh-Daghestanian",
                  indo1319 = "Indo-European",
                  pano1259 = "Panoan",
                  taik1256 = "Tai-Kadai",
                  otom1299 = "Otomanguean",
                  araw1281 = "Arawakan",
                  afro1255 = "Afro-Asiatic",
                  book1242 = "Bookkeeping",
                  nubi1251 = "Nubian",
                  jodi1234 = "Jodi-Saliban",
                  other = "Creoles and Pidgins")) # other recodes Jamaican Creole
all.languages$family[which(is.na(all.languages$family))] <- "Bookkeeping" # Assign the missing Sanapana to bookkeeping too

# save relevant data on new file

all.languages <- all.languages %>% 
  select(ID, Name, iso_code, latitude, longitude, family, macroarea, countrycodes)
all.languages %>% 
  write_csv("../Data/Processed/all_language_info.csv")

# Loading and coding word data  -----------------------------------------------

# Get all word forms from IDS

all.words <-
  read_csv(
    file = "../Data/IDS/forms.csv",
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
ids.to.concepticon <- read_csv("../Data/IDS/parameters.csv") %>%
  mutate(ID = factor(ID),
         Concepticon_ID = factor(Concepticon_ID)) %>% 
  select(-Description) %>% 
  rename(Parameter_ID = ID, english.name = Name)
all.words <- left_join(all.words, ids.to.concepticon)

# get semantic field from linkage with concepticon id

concepticon <- read_csv('../Data/Concepticon/semanticFields.csv') %>% 
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

no.phon %>% write_csv('../Data/Processed/no_phon.csv')

# store the phon form as "phon" in the dataframe. 
# if the main transcription is phon, get that; 
# else, it means that the alt transcription is the phon one, and get that.
all.words <- all.words %>% 
  filter(!(language %in% no.phon.languages)) %>% 
  mutate(phon = ifelse(transcription == "phon", Form, alt_form))

# Load words that were "hand" mined (modified no.phon data frame)

no.phon.with.phon <- read_csv('../Data/Processed/no_phon_with_phon.csv') %>% 
  mutate(method = ifelse(language %in% c("Danish", "English", "Ossetic"), "manual", method)) %>% 
  mutate(phon = str_to_lower(phon))

# add Danish

danish.phon <- read_csv('../Data/PhonMining/DataForG2P/danishOrthPhon.csv') %>% 
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

# # Language data for included languages
# phonLanguages <- allLanguages[match(unique(allPhon$language), allLanguages$Name),]
# phonLanguages %>% write_csv('../Data/Processed/allPhonLanguagesConcepticon.csv')


# Create a morphology-adjusted dataset --------


# Create a copy of the original dataset to modify "in place"

all.phon.adjusted <- all.phon
# Store all languages that had something removed in a list
marker.group <- list()

# Prepare a list with languages that had suffixes removed and how many
ending.census <- list()

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
        mutate(ending = str_sub(phon, "-1")) %>% 
        .$ending %>% 
        table()
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
  summarize(ending.markers = max(ending.markers))


# Same procedure as above, but for affixes.
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
  summarize(start.markers = max(start.markers))

# Join suffix and affix census into one dataframe.
marker.census <- full_join(start.census, ending.census) %>% 
  # Note that Munduruku and Kanok had only "Other" words adjusted, so not on census
  filter(ontological.category != "Other")
# Generate category-specific census
action.census <- marker.census %>% 
  filter(ontological.category == "Action") %>% 
  select(-ontological.category) %>% 
  rename(Affixes.Removed.Action = start.markers, Suffixes.Removed.Action = ending.markers)
thing.census <- marker.census %>% 
  filter(ontological.category == "Thing") %>% 
  select(-ontological.category) %>% 
  rename(Affixes.Removed.Thing = start.markers, Suffixes.Removed.Thing = ending.markers)
marker.census <-full_join(action.census, thing.census) 
all.marker.group <- unique(marker.census$language)
# Make an "empty" census for languages with no markers removed.
no.marker.group <- setdiff(unique(all.phon$language), all.marker.group)
no.marker.group.table <- tibble(language = no.marker.group, Affixes.Removed.Action = 0,
                                Suffixes.Removed.Action = 0, Affixes.Removed.Thing = 0, 
                                Suffixes.Removed.Thing = 0)

# For each language in the database, display how many suffixes and affixes were
# removed for each category, including languages without adjustment.
marker.census.complete <- marker.census %>% 
  bind_rows(no.marker.group.table) %>% 
  group_by() %>% 
  mutate_if(is.numeric, function(x){replace_na(x, 0)}) %>% 
  arrange(language)

# Further adjustment: remove within-class homophones but keep between-class homophones.
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
  summarize(Number.Homophones = sum(n))
all.phon.adjusted <- all.phon.adjusted %>% 
  distinct(language, ontological.category, phon, .keep_all = TRUE)

# Join marker census with homophone census, and compare original/adjusted number of words

original.number.words <- all.phon %>% group_by(language) %>% summarize(original.number.words = n())
adjusted.number.words <- all.phon.adjusted %>% group_by(language) %>% summarize(adjusted.number.words = n())

marker.census.complete <- left_join(marker.census.complete, original.number.words) %>% 
  left_join(adjusted.number.words) %>% 
  left_join(select(phon.languages, language = Name, Family = family)) %>% 
  left_join(homophone.census) %>% 
  select(language, Family, everything())

# Replace NA with 0 for the numeric columns. NAs result from languages without within-class homophones to remove.
marker.census.complete <- mutate_if(.tbl = marker.census.complete, .predicate = is.numeric, .funs = function(x){replace_na(x, 0)})

write_csv(marker.census.complete, "../Data/Processed/marker_census.csv")
write_csv(all.phon.adjusted, "../Data/Processed/all_phon_adjusted.csv")
