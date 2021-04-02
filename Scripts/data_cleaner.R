library(tidyverse)
library(rnaturalearth)
library(ggdendro)
library(cluster)
library(geosphere)

source("functions.r")

# Loading and coding data language data  -----------------------------------------------

# Construct list of languages and language data

# All languages reference to Glottolog was hand fixed for the following languages after it was found they didn't coincide with the codes in glottocode.
# Both Armenians, 
all.languages <- read_csv("../Data/Raw/IDS/languages.csv") %>% 
  mutate(ID = factor(ID))
glottocode <- read_csv("../Data/Raw/IDS/Glottocode.csv") %>% 
  select(Glottocode = id, family_id, parent_id, latitude, longitude, iso639P3code, country_ids)
wals <- read_csv('../Data/Processed/WALS_Codes.csv') %>% 
  mutate(ID = as.factor(ID))
wals_info <- read_csv("../Data/Raw/WALS/walslanguage.csv")

# Get handcoded list of excluded languages (extinct, explicit dialects that have a main one in the database, reconstructions, no info)
excluded.languages <- read_delim("../Data/Processed/excluded_languages.txt", comment = "#", delim = "\n", col_names = "Language") %>% 
  mutate(Language = str_squish(Language))

# Match WALS and IDS by using the WALS CODE (hand-coded) in wals_codes.csv
all.languages <- all.languages %>% 
  filter(!(Name %in% excluded.languages$Language)) %>%
  left_join(wals) %>% 
  select(-Macroarea)
not.on.wals <- all.languages %>% 
  filter(is.na(wals_code))

# For languages that are on WALS, use that information. If not on WALS, use Glottocode.
all.languages <- all.languages %>% 
  filter(!(is.na(wals_code))) %>% 
  select(-Latitude, -Longitude) %>% 
  left_join(select(wals_info, wals_code, latitude, longitude, genus, family))
not.on.wals <- left_join(not.on.wals, glottocode) %>% 
  select(-Latitude, -Longitude, -iso639P3code, family = family_id, genus = parent_id, - country_ids)
# Assign the missing Sanapana to bookkeeping too
not.on.wals$family[which(is.na(not.on.wals$family))] <- "book1242" 
not.on.wals$genus[which(is.na(not.on.wals$genus))] <- "book1242"

# Hand recode the family and genus codes to their names
not.on.wals <- not.on.wals %>% 
  mutate(family = recode(family, 
                         afro1255 = "Afro-Asiatic",
                         araw1281 = "Arawakan",
                         aust1305 = "Austro-Asiatic",
                         book1242 = "Bookkeeping",
                         hmon1336 = "Hmong-Mien",
                         indo1319 = "Indo-European",
                         jodi1234 = "Jodi-Saliban",
                         nakh1245 = "Nakh-Daghestanian",
                         pano1259 = "Panoan",
                         taik1256 = "Tai-Kadai"),

         genus = recode(genus,
                        "boly1240" = "Pakanic",
                        "boli1261" = "Bolivian Nawa",
                        "book1242" = "Bookkeeping",
                        "botl1243" = "Avar-Andic-Tsezic",
                        "bula1260" = "Palaung-Khmuic",
                        "cauc1242" = "Iranian",
                        "chut1252" = "Chutic",
                        "chut1247" = "Chutic",
                        "east2280" = "Eastern Baltic",
                        "guiy1235" = "Guiyang",
                        "iran1269" = "Iranian",
                        "jodi1234" = "Jodi-Saliban",
                        "kami1255" = "Kamic",
                        "khao1243" = "Palaung-Khmuic",
                        "khas1275" = "Khasi-Pnar",
                        "laot1235" = "Kam-Tai",
                        "male1282" = "Chutic",
                        "mang1377" = "Mangic",
                        "maon1240" = "Maonan-Chadong",
                        "maru1251" = "Panoan",
                        "moxo1234" = "Bolivia-Parana",
                        "mula1252" = "Mulam-Kam",
                        "nort2739" = "Kam-Tai",
                        "nort2744" = "Kadai",
                        "nucl1728" = "Indic",
                        "nyam1284" = "West Chadic",
                        "oldm1247" = "Old-Modern Welsh",
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

# Now get the look at transcriptions and keep the ones that have phonological or phonemic information

# Get all word forms from IDS
all.words <- read_csv(file = "../Data/Raw/IDS/forms.csv", col_names = T, locale = locale(encoding = "UTF-8")) %>%
  mutate(Parameter_ID = factor(Parameter_ID),
         transcription = factor(transcription),
         alt_transcription = factor(alt_transcription),
         Language_ID = factor(Language_ID))

# Filter to only words of the languages we're using
all.words <- all.words %>% 
  filter(Language_ID %in% all.languages$ID) %>% 
  left_join(select(all.languages, Language_ID = ID, Name)) %>% 
  rename(language = Name) %>% 
  select(-Language_ID, -Segments, -Comment, -Source, -Contribution_ID)

# Recode Phonemic, IPA and Phonetic transcriptions as "phon" for use in the study
# These levels previously: "CyrillTrans", "IPA", "LatinTrans", "Phonemic", "phonetic", "Standard", "StandardOrth", "StandardOrthTone"
levels(all.words$transcription) <- c("CyrillTrans", "phon", "LatinTrans", "phon", "phon", "Standard", "StandardOrth", "StandardOrthTone")
# These levels previously: "Original (M. R. Key)", "Phonemic", "Phonemic (vars)", "Standard"
levels(all.words$alt_transcription) <- c("Original (M. R. Key)", "phon", "Phonemic (vars)", "Standard") # Phonemic (vars) is only NA in Rapa Nui

all.words <- mutate(all.words, transcription = as.character(transcription), alt_transcription = as.character(alt_transcription))

# Load list of languages with no phon transcription and manually get them if they do
languages.no.phon <- read_csv("../Data/Processed/languages_no_phon.csv")
languages.no.info <- filter(languages.no.phon, transcription == "no_phon")
all.words <- filter(all.words, !(language %in% languages.no.info$language))
all.languages <- filter(all.languages, !(Name %in% languages.no.info$language))

# Languages that already had phon transcription, but unmarked
unmarked.phon <- filter(languages.no.phon, transcription == "is_phon")
all.words <- mutate(all.words, transcription = ifelse(language %in% unmarked.phon$language, "phon", transcription))

# Get words for espeak-ng and then remove them from this
espeak.languages <- filter(languages.no.phon, transcription == "espeak-ng")

number.of.families <- all.languages$family %>% 
  unique() %>% 
  length()
paste("Number of families:", number.of.families)

# Save relevant data on new file
all.languages <- all.languages %>% 
  select(ID, Name, iso_code = ISO639P3code, latitude, longitude, family)
all.languages %>% 
  write_csv("../Data/Processed/all_language_info.csv")


# Loading and coding word data  -----------------------------------------------

# Link word forms to Concepticon to obtain semantic information
ids.to.concepticon <- read_csv("../Data/Raw/IDS/parameters.csv") %>%
  mutate(ID = factor(ID),
         Concepticon_ID = factor(Concepticon_ID)) %>% 
  select(-Description) %>% 
  rename(Parameter_ID = ID, english.name = Name)
all.words <- left_join(all.words, ids.to.concepticon)

concepticon <- read_csv('../Data/Raw/Concepticon/semanticFields.csv')
concepticon <- concepticon %>% 
  select(Concepticon_ID = id, ontological.category = ontological_category) %>% 
  mutate(Concepticon_ID = factor(Concepticon_ID))
all.words <- left_join(all.words, concepticon) %>% 
  select(-Concepticon_ID, -ID, -Parameter_ID) %>% 
  mutate(ontological.category = as.factor(ontological.category)) %>% 
  filter(ontological.category %in% c("Action/Process", "Person/Thing"))

# Store the phon form as "phon" in the dataframe. 
all.phon <- all.words %>% 
  mutate(phon = ifelse(transcription == "phon", Form, alt_form),
         phon = ifelse(is.na(phon), Form, phon))


# Clean phon forms --------------------------------------------------------
all.phon <- all.phon %>% 
  filter(!is.na(phon),
         str_detect(phon, "\\?", negate = TRUE)) # Get rid of forms that have a question mark on them

all.phon <- clean.phon(all.phon)


# Add espeak

espeak.df <- all.phon %>% 
  filter(language %in% espeak.languages$language) 
for(this.language in espeak.languages$language){
  file.name <- paste0("PhonMining/espeak_lists/", this.language, ".txt")
  this.df <- filter(espeak.df, language == this.language)
  write_lines(this.df$phon, file.name, sep = ", ")
}

espeak.phon.df <- map_dfr(espeak.languages$language, function(this.language){
  lang.df <- filter(all.phon, language == this.language)
  file.name <- paste0("PhonMining/phon_transcriptions/", this.language, "_phon.txt")
  new.phon <- read_lines(file.name) %>% 
    str_squish()
  lang.df$phon <- new.phon
  return(lang.df)
})

espeak.phon.df <- clean.phon(espeak.phon.df)

all.phon <- filter(all.phon, !(language %in% espeak.languages$language))
all.phon <- bind_rows(all.phon, espeak.phon.df)


# Save the espeak language list of words

all.phon <- all.phon %>% 
  mutate(number.of.spaces = str_count(phon, " ")) %>% 
  filter(number.of.spaces < 2)

# Remove dashes. They mostly (always?) denote pronunciation pauses
# Remove  stress markers
# Remove diacritic markers and stress markers
all.phon <- all.phon %>% 
  mutate(phon = str_remove_all(phon, "[-'[\\p{Lm}]`:\\$\\\\]"))

# Tonal languages have tones marked with numbers
all.tone.languages <- filter(all.phon, str_detect(phon, "[:digit:]")) 
all.tone.languages <- unique(all.tone.languages$language)
all.tone.languages <- all.tone.languages[!(all.tone.languages == "Telugu")] # Telugu has 2 words with numbers which are not tones
all.tone.languages <- all.tone.languages[!(all.tone.languages == "Pear")] # Same with Pear

all.tones <- filter(all.phon, language %in% all.tone.languages)

all.phon <- filter(all.phon, !(language %in% all.tone.languages))

# Form inventories of characters to hand construct the vowel inventory of each of them (no in principle way of identifying it a priori)
# for(this.language in all.tone.languages){
#   all.chars <- all.tones %>% 
#     filter(language == this.language)
#   all.chars <- str_split(all.chars$phon, "") %>% 
#     unlist() %>% 
#     unique()
#   file.name <- paste0("VowelInventories/", this.language, ".txt")
#   write_lines(all.chars, file.name)
# }

all.tones <- map_dfr(all.tone.languages, function(this.language){
  print(this.language)
  lang.df <- filter(all.tones, language == this.language)
  lang.words <- lang.df[["phon"]]
  file.name <- paste0("VowelInventories/", this.language, ".txt")
  vowel.inventory <- read_lines(file.name)
  # After looking at database, tones are ALWAYS written after a period, 
  # and ONLY use either numbers or upper case letters
  tone.inventory <- str_extract_all(lang.words, "\\.[A-Z0-9]{1,3}") %>% 
    unlist() %>% 
    unique()
  all.combs <- expand_grid(vowel = vowel.inventory, tone = tone.inventory) %>% 
    mutate(pattern = paste0(vowel, "[[a-z][\\p{Sm}]]*\\", tone))
  exists <- map_lgl(all.combs$pattern, function(this.pattern){
    exists <- str_detect(lang.words, this.pattern)
    exists <- sum(exists)
    return(ifelse(exists > 0, TRUE, FALSE))
  })
  all.combs <- all.combs[exists,] %>% 
    arrange(desc(nchar(tone))) # Arrange combinations into descending order of length for matching tones with more than 1 number later
  tone.placeholders <- stringi::stri_rand_strings(nrow(all.combs), 1, "[\\p{Sm}]")
  all.combs$placeholder <- tone.placeholders
  lang.df$new.phon <- lang.df$phon
  for(i in 1:nrow(all.combs)){
    vowel.tone <- all.combs[i,]
    lang.df <- lang.df %>% 
      rowwise() %>% 
      mutate(new.phon = replace.vowel(string = new.phon, 
                                      vowel = vowel.tone[["vowel"]],
                                      tone = vowel.tone[["tone"]],
                                      placeholder = vowel.tone[["placeholder"]]))
  }
  lang.df <- ungroup(lang.df) %>% 
    mutate(new.phon = str_remove_all(new.phon, "\\.[A-Z0-9]{1,3}")) 
  return(lang.df)
})

all.phon <- all.phon %>% 
  mutate(phon = str_remove_all(phon, regex("[\\p{S}]")))

all.tones <- select(all.tones, -phon, phon = new.phon)
all.tones <- clean.phon(all.tones)

all.phon <- bind_rows(all.tones, all.phon)
# Remove all dots (remaining phon linkings, kept there for tone process)
all.phon <-  mutate(all.phon, phon = str_remove_all(phon, "\\."))

# Now merge collocations
all.phon <- mutate(all.phon, phon = str_remove_all(phon, " "),
                   phon = stringi::stri_trans_nfkc_casefold(phon))




# Only keep strings longer than 2
all.phon <- filter(all.phon, nchar(phon) > 2)

all.phon <- all.phon %>%
  filter(!(is.na(ontological.category))) %>%  # remove words without concepticon information
  group_by(language) %>%
  mutate(numberOfWords = n()) %>%
  group_by() %>%
  filter(numberOfWords > 200) %>% # Exclude languages with fewer than 100 phon forms
  dplyr::select(-numberOfWords) %>%
  droplevels()
all.phon <- all.phon %>% 
  mutate(phon = str_remove_all(phon, "[\\p{Mn}[:punct:]]"),
         phon = str_to_lower(phon))


all.phon %>% write_csv('../Data/Processed/all_phon.csv')


# Create a morphology-adjusted dataset --------

get.ngram.endings <- function(language.df, level){
  all.levels <- c(-1:level)
  names(all.levels) <- all.levels
  all.ngram.endings <- map_dfc(all.levels, function(n){
    str_sub(language.df$phon, n)
  })
  ngram.df <- language.df %>% 
    dplyr::select(phon, ontological.category, Form) %>% 
    bind_cols(all.ngram.endings)
  return(ngram.df)
}
get.successors <- function(ngram.df, ngram){
  level <- -(nchar(ngram))
  this.ngram.rows <- ngram.df[[as.character(level)]] == ngram
  this.df <- ngram.df[this.ngram.rows,]
  next.level <- as.character(level - 1)
  successors <- str_sub(this.df[[next.level]], 1, 1)
  return(successors)
}

get.random.frequency <- function(ngram, char.table){
  ngram.components <- str_split(ngram, "") %>% 
    unlist()
  if(length(ngram.components) == 1){
    return(char.table[ngram])
  } else{
    return(prod(char.table[ngram.components]))
  }
}

get.ngram.stats <- function(ngram.df, ngram, char.table, ngram.freq){
  level <- -(nchar(ngram))
  this.entropy <- infotheo::entropy(get.successors(ngram.df, ngram))
  ngram.frequency <- table(ngram.df[[as.character(level)]])
  length.frequency <- ngram.frequency[ngram]
  norm.frequency <- scale(ngram.frequency)[ngram, 1]
  if(is.nan(norm.frequency)){
    norm.frequency <- 1 # This is in case there's only one ending, thus scaled frequency is NAN because of 0 SD
  }
  random.frequency <- get.random.frequency(ngram, char.table)
  results <- list("ngram" = ngram, 
                  "entropy" = this.entropy, 
                  "norm.frequency" = norm.frequency, 
                  "length.frequency" = length.frequency,
                  "higher.than.random" = ngram.freq / random.frequency)
  return(results)
}

get.word.stats <- function(word, ngram.results){
  word.ngrams <- map_chr(-1:(-(nchar(word) - 1)), function(n){
    return(str_sub(word, n))
  })
  names(word.ngrams) <- word.ngrams
  word.entropies <- map_dbl(word.ngrams, function(ngram){
    return(ngram.results[[ngram]][["entropy"]])
  })
  word.norm.frequencies <- map_dbl(word.ngrams, function(ngram){
    return(ngram.results[[ngram]][["norm.frequency"]])
  })
  word.overrep <- map_dbl(word.ngrams, function(ngram){
    return(ngram.results[[ngram]][["higher.than.random"]])
  })
  word.length.freq <- map_dbl(word.ngrams, function(ngram){
    return(ngram.results[[ngram]][["length.frequency"]])
  })
  return(list("entropies" = word.entropies, 
              "length.frequencies" = word.length.freq,
              "norm.frequencies" = word.norm.frequencies, 
              "overrep" = word.overrep))
}

evaluate.word <- function(word, ngram.results){
  word.stats <- get.word.stats(word, ngram.results)
  is.candidate <- word.stats$length.frequencies > 1 # Use only segments with more than 1 length frequency
  if(sum(is.candidate) == 0){ # If none of the ngrams appear in any other word, no marker.
    return("#")
  }
  word.entropies <- word.stats$entropies[is.candidate]
  word.entropies <- word.entropies[!is.nan(word.entropies)]
  word.frequencies <- word.stats$norm.frequencies
  word.overrep <- word.stats$overrep
  end.entropy <- ngram.results[["#"]][["entropy"]]
  
  tested.ngrams <- names(word.entropies)[1:(length(word.entropies))]
  peaks <- c()  
  if(length(tested.ngrams) > 1){
    for(x in 1:length(tested.ngrams)){
      this.entropy <- word.entropies[x]
      if(x == 1){
        prev.entropy <- end.entropy
      } else {
        prev.entropy <- word.entropies[x - 1]
      }
      if(this.entropy >= prev.entropy){
        peaks <- c(peaks, tested.ngrams[x])
      }
    }
    possible.markers <- unique(peaks) 
  } else {
    possible.markers <- tested.ngrams
  }
  
  if(length(possible.markers) == 0){
    return("#")
  } #If there's no peaks left after that, return the end marker
  frequency.test <- map_lgl(possible.markers, function(ngram){
    if((word.frequencies[ngram] > 0) | (word.overrep[ngram] > 1)){ # Check whether frequency is higher than average OR higher than chance.
      return(TRUE)
    } else{
      return(FALSE)
    }
  })
  if(sum(frequency.test) == 0){ # If none pass, return end marker
    return("#")
  }
  
  possible.markers <- possible.markers[frequency.test]
  candidate.entropies <- c(end.entropy, word.entropies[possible.markers]) # Get entropy of each candidate along with entropy of end mark
  names(candidate.entropies) <- c("#", possible.markers)
  max.entropy <- names(candidate.entropies)[which.max(candidate.entropies)] # Return the name of the candidate with the most entropy
  return(max.entropy)
}

get.morph.markers <- function(language.df){
  all.ngrams <- get.ngram.endings(language.df, -(max(nchar(language.df$phon))))
  
  # Get data for frequency
  all.ngram.segments <- all.ngrams %>% 
    dplyr::select(-phon, -ontological.category, -Form) %>% 
    unlist()
  character.frequency <- paste0(all.ngram.segments, collapse = "") %>% 
    str_split("") %>% 
    table()
  print(character.frequency)
  character.frequency <- character.frequency / sum(character.frequency)
  ngram.frequency <- table(all.ngram.segments)
  ngram.frequency <- ngram.frequency / sum(ngram.frequency)
  
  # Get stats for each unique ngram
  unique.ngrams <- unique(all.ngram.segments)
  names(unique.ngrams) <- unique.ngrams
  
  all.ngram.stats <- map(unique.ngrams, function(ngram){
    ngram.stats <- get.ngram.stats(all.ngrams, ngram, character.frequency, ngram.frequency[ngram])
    return(ngram.stats)
  })
  all.ngram.stats[["#"]] <- list("ngram" = "#", "entropy" = infotheo::entropy(all.ngrams$`-1`), 
                                 "norm.frequency" = 0, "length.frequency" = 0, "higher.than.random" = 0)
  
  all.stats.df <- bind_rows(all.ngram.stats) %>% 
    add_column(ontological.category = language.df$ontological.category[1])
  
  
  words.and.markers <- language.df %>% 
    rowwise() %>% 
    mutate(marker = evaluate.word(phon, ngram.results = all.ngram.stats))
  
  results <- list("marker.census" = all.stats.df, "marked.words" = words.and.markers)
  return(results)
}


clean.language <- function(language, all.data){
  this.language <- language
  lang.df <- all.data %>% 
    filter(language == this.language)
  categories <- as.character(unique(lang.df$ontological.category))
  names(categories) <- categories
  all.marker.df <- map(categories, function(category){
    print(category)
    this.df <- lang.df %>% 
      filter(ontological.category == category) %>% 
      droplevels()
    these.markers <- get.morph.markers(this.df)
    return(these.markers)    
  })
  marker.census <- bind_rows(all.marker.df$Thing$marker.census, all.marker.df$Action$marker.census, all.marker.df$Other$marker.census)
  all.markers <- bind_rows(all.marker.df$Thing$marked.words, all.marker.df$Action$marked.words, all.marker.df$Other$marked.words)
  
  all.markers <- all.markers %>% 
    mutate(marker.position = nchar(phon) - nchar(marker),
           clean.phon = ifelse(marker == "#", phon, str_sub(phon, 1, marker.position))) %>% 
    dplyr::select(-marker.position)
  results <- list("census" = marker.census, "clean.df" = all.markers)
}

all.lang.adjusted <- map(as.character(sort(unique(all.phon$language))), function(language){
  print(language)
  lang.results <- clean.language(language, all.phon)
  lang.results$census$language <- language
  return(lang.results)
})

# names(all.lang.adjusted) <- as.character(sort(unique(all.phon$language)))


# write_rds(all.lang.adjusted, "all_langs_adjusted.rds")
all.lang.adjusted <- read_rds("all_langs_adjusted.rds")

complete.markers <- map_dfr(all.lang.adjusted, function(lang){
  return(lang$clean.df)
}) %>% 
  dplyr::select(-clean.phon, suffix = marker)

# prefix.languages <- all.phon %>% 
#   mutate(phon = stringi::stri_reverse(phon))
# 
# all.lang.adjusted.prefix <- map(as.character(sort(unique(all.phon$language))), function(language){
#   print(language)
#   lang.results <- clean.language(language, prefix.languages)
#   lang.results$census$language <- language
#   return(lang.results)
# })

# all.lang.adjusted.prefix %>% 
#   write_rds("all_lang_adjusted_prefix.rds")
all.lang.adjusted.prefix <- read_rds("all_lang_adjusted_prefix.rds")
 
complete.markers.prefix <- map_dfr(all.lang.adjusted.prefix, function(lang){
  return(lang$clean.df)
}) %>% 
  dplyr::select(-clean.phon, prefix = marker) %>% 
  mutate(phon = stringi::stri_reverse(phon), prefix = stringi::stri_reverse(prefix))

all.phon.adjusted <- left_join(complete.markers, complete.markers.prefix)

suffix.census <- all.phon.adjusted %>% 
  group_by(language, ontological.category, suffix) %>% 
  tally() %>% 
  rename(marker = suffix) %>% 
  add_column(position = "Suffix")
prefix.census <- all.phon.adjusted %>% 
  group_by(language, ontological.category, prefix) %>% 
  tally() %>% 
  rename(marker = prefix) %>% 
  add_column(position = "Prefix")

marker.census <- full_join(suffix.census, prefix.census) %>% 
  arrange(language, ontological.category)
write_csv(marker.census, "marker_census.csv")

all.phon.adjusted <- all.phon.adjusted %>% 
  rowwise() %>% 
  mutate(suffix.position = nchar(phon) - nchar(suffix), prefix.position = nchar(prefix) + 1,
    clean.phon = ifelse(suffix == "#", phon, str_sub(phon, 1, suffix.position)),
    clean.phon = ifelse(prefix == "#", clean.phon, str_sub(clean.phon, prefix.position))) %>% 
  dplyr::select(Form, alt_form, language, english.name, ontological.category, old.phon = phon, phon = clean.phon) %>% 
  filter(nchar(phon) > 2)

# Further adjustment: remove within-class homophones but keep between-class homophones.
# Gets rid of within-class colexification
# Save how many homophones were removed.

homophone.census <- all.phon.adjusted %>% 
  # group_by(language, ontological.category, phon) %>%
  group_by(language, phon) %>% 
  tally() %>% 
  filter(n > 1) %>% 
  group_by() %>% 
  group_by(language) %>% 
  tally()

homophone.census <- homophone.census %>% 
  group_by(language) %>% 
  dplyr::summarize(Number.Homophones = sum(n))
all.phon.adjusted <- all.phon.adjusted %>% 
  distinct(language, phon, .keep_all = TRUE)


# Join marker census with homophone census, and compare original/adjusted number of words
# original.number.words <- all.phon %>% group_by(language) %>% dplyr::summarize(original.number.words = n())
# adjusted.number.words <- all.phon.adjusted %>% group_by(language) %>% dplyr::summarize(adjusted.number.words = n())

# marker.census.complete <- left_join(marker.census.complete, original.number.words) %>% 
#   left_join(adjusted.number.words) %>% 
#   left_join(select(all.languages, language = Name, Family = family)) %>% 
#   left_join(homophone.census) %>% 
#   select(language, Family, everything())

# Replace NA with 0 for the numeric columns. NAs result from languages without within-class homophones to remove.
# marker.census.complete <- mutate_if(.tbl = marker.census.complete, .predicate = is.numeric, .funs = function(x){replace_na(x, 0)})

write_csv(homophone.census, "../Data/Processed/homophonre_census.csv")
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
