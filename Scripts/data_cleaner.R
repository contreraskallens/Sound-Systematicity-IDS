# install.packages(c('tidyverse', 'rnaturalearth', 'ggdendro', 'cluster', 'geosphere', 'stringi', 'rnaturalearthdata))
library(tidyverse)
library(rnaturalearth)
library(ggdendro)
library(cluster)
library(geosphere)
library(stringi)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("functions.r")


# Loading and coding data language data  -----------------------------------------------

# Construct list of languages and language data

all.languages <- read_csv("../data/Raw/IDS/languages.csv") %>% 
  mutate(ID = factor(ID))
glottocode <- read_csv("../data/Raw/IDS/Glottocode.csv") %>% 
  select(Glottocode = id, 
         family_id, 
         parent_id, 
         latitude, 
         longitude, 
         iso639P3code, 
         country_ids)
wals <- read_csv('../data/Processed/WALS_Codes.csv') %>% 
  mutate(ID = as.factor(ID))
wals_info <- read_csv("../data/Raw/WALS/walslanguage.csv")

# Get hand-coded list of excluded languages 
# (extinct, explicit dialects that have a main one in the database, 
# reconstructions, no info)
excluded.languages <- read_delim("../data/Processed/excluded_languages.txt", delim = "\\n", col_names = "Language") %>% 
  mutate(Language = str_squish(Language))

all.languages <- all.languages %>% 
  filter(!(Name %in% excluded.languages$Language))

# Keep transcripts that have phonological or phonemic information ----

# Get all word forms from IDS
all.words <- read_csv(file = "../data/Raw/IDS/forms.csv", col_names = T, locale = locale(encoding = "UTF-8")) %>%
  mutate(Parameter_ID = factor(Parameter_ID),
         transcription = factor(transcription),
         alt_transcription = factor(alt_transcription),
         Language_ID = factor(Language_ID))

# Filter to only words of the languages we're using
all.words <- all.words %>% 
  filter(Language_ID %in% all.languages$ID) %>% 
  left_join(select(all.languages, Language_ID = ID, Name)) %>% 
  rename(language = Name) %>% 
  select(-Language_ID, -Segments, -Comment, -Source, -Contribution_ID) %>% 
  filter(!(language %in% excluded.languages$Language))

# Recode Phonemic and IPA  transcriptions as "phon" for use in the study
# These levels previously: "CyrillTrans", "IPA", "LatinTrans", "Phonemic", "phonetic", "Standard", "StandardOrth", "StandardOrthTone"
og.transcriptions <- all.words$transcription
levels(og.transcriptions)
levels(all.words$transcription) <- c("CyrillTrans", "phon", "LatinTrans", "phon", "phonetic", "Standard", "StandardOrth", "StandardOrthTone")

# These levels previously: "Original (M. R. Key)", "Phonemic", "Phonemic (vars)", "Standard"
og.alt.transcriptions <- all.words$alt_transcription
levels(og.alt.transcriptions)
levels(all.words$alt_transcription) <- c("Original (M. R. Key)", "phon", "Phonemic (vars)", "Standard") # Phonemic (vars) is only NA in Rapa Nui

all.words <- mutate(all.words, transcription = as.character(transcription), 
                    alt_transcription = as.character(alt_transcription),
                    og_transcription = og.transcriptions,
                    og_alt_transcription = og.alt.transcriptions)

# Load list of languages with no phon transcription and manually get them if they do. ------
# Note that Spanish is mislabeled as phonemic when it's orthographic 
# Jinsha Dai and Southern Kam are phonetic, not phonemic

languages.no.phon <- read_csv("../Data/Processed/languages_no_phon.csv") %>% 
  filter(transcription %in% c('espeak-ng', 'is_phon'),
         !(language %in% c('Southern Kam', 'Jinsha Dai')))
unmarked.phon <- filter(languages.no.phon, transcription == "is_phon")
all.words <- mutate(all.words, transcription = ifelse(language %in% unmarked.phon$language, "phon", transcription))

espeak.languages <- filter(languages.no.phon, transcription == "espeak-ng")

espeak.words <- filter(all.words, language %in% espeak.languages$language)

all.words <- all.words %>% 
  filter(transcription == 'phon' | alt_transcription == 'phon')

all.languages <- all.languages %>% 
  filter(Name %in% unique(all.words$language) | Name %in% espeak.languages$language)


# Remove espeak languages from frame to later add them in again

all.words <- all.words %>% 
  filter(!(language %in% espeak.languages$language))

# Store espeak lists
all.espeak.langs <- espeak.languages$language
names(all.espeak.langs) <- all.espeak.langs
for(this.language in all.espeak.langs){
  lang.words <- filter(espeak.words, language == this.language)
  lang.word.list <- lang.words$Form %>% 
    stri_trans_nfc() %>% 
    str_remove_all("(?<=.)\\(.+\\)") %>% 
    str_remove_all("\\(.+\\)(?=.)") %>% 
    str_remove_all("[\\[\\]XY\\-']") %>% 
    str_squish() %>% 
    str_remove_all("^/") %>%  # remove initial separators
    str_remove_all("^~") %>% 
    str_remove_all("/.*") %>% # extract the first form of multi form entries separated by / 
    str_remove_all("(?<=\\?).*") %>% # extract the first form of multi form entries separated by ?
    str_remove_all("~.*") %>% # extract the first form of multi form entries separated by ~
    str_remove_all('ˑ') # Remove separators
  write_lines(lang.word.list, paste0("../data/PhonMining/espeak_lists/", this.language, ".txt"), sep="\n\n")
}

# Read in espeak transcriptions

espeak.phon <- map_dfr(all.espeak.langs, function(this.language){
  print(this.language)
  lang.words <- filter(espeak.words, language == this.language)
  espeak.trans <- read_lines(paste0("../data/PhonMining/phon_transcriptions/", this.language, "_phon.txt")) %>% 
    str_remove_all("[ˈˌ]") %>%  # Remove stress marker
    str_remove_all("[\\p{Mn}]") %>% 
    str_remove_all('\u0361') %>% # Remove ligatures
    str_remove_all('\u032F') %>% 
    str_remove_all('\u032A') %>% 
    stri_trans_nfc()
  lang.words$phon <- espeak.trans
  lang.words$IPA <- espeak.trans
  return(lang.words)
}) %>% 
  filter(str_detect(phon, '\\(en\\)', negate = TRUE),
         (str_count(phon, " ") < 2)) %>% 
  mutate(phon = str_remove_all(phon, ' '),
         IPA = str_remove_all(IPA, ' '))

# Link word forms to Concepticon to obtain semantic information ----
ids.to.concepticon <- read_csv("../data/Raw/IDS/parameters.csv") %>%
  mutate(ID = factor(ID),
         Concepticon_ID = factor(Concepticon_ID)) %>% 
  select(-Description) %>% 
  rename(Parameter_ID = ID, english.name = Name)
all.words <- left_join(all.words, ids.to.concepticon)

concepticon <- read_csv('../data/Raw/Concepticon/semanticFields.csv')
concepticon <- concepticon %>% 
  select(Concepticon_ID = id, ontological.category = ontological_category) %>% 
  mutate(Concepticon_ID = factor(Concepticon_ID))
all.words <- left_join(all.words, concepticon) %>% 
  select(-Concepticon_ID, -ID, -Parameter_ID) %>% 
  mutate(ontological.category = as.factor(ontological.category)) %>% 
  filter(ontological.category %in% c("Action/Process", "Person/Thing")) %>% 
  droplevels()
levels(all.words$ontological.category) <- c("Action", "Thing")


espeak.phon <- left_join(espeak.phon, ids.to.concepticon)
espeak.phon <- left_join(espeak.phon, concepticon) %>% 
  select(-Concepticon_ID, -ID, -Parameter_ID) %>% 
  mutate(ontological.category = as.factor(ontological.category)) %>% 
  filter(ontological.category %in% c("Action/Process", "Person/Thing")) %>% 
  droplevels()
levels(espeak.phon$ontological.category) <- c("Action", "Thing")

# Store the phon form as "phon" in the dataframe. 
all.phon <- all.words %>% 
  mutate(phon = ifelse(transcription == "phon", Form, alt_form)) %>% 
  filter(!(is.na(phon)))

# Get rid of forms that have a question mark on them
all.phon <- all.phon %>% 
  filter(!is.na(phon),
         str_detect(phon, "\\?", negate = TRUE)) 

all.phon <- bind_rows(all.phon, espeak.phon)

all.phon <- clean.phon(all.phon)

# Remove all remaining symbols and punctuation marks
all.phon <-  mutate(all.phon, 
                    phon = str_remove_all(phon, "[\\p{S}\\p{P}]"))

# Only keep strings longer than 2
all.phon <- filter(all.phon, 
                   nchar(phon) > 2)

# Final cleanup ----

# Remove words without concepticon information and 
# exclude languages with fewer than 200 phon forms
all.phon <- all.phon %>%
  filter(!(is.na(ontological.category))) %>%  # 
  group_by(language) %>%
  mutate(numberOfWords = n()) %>%
  group_by() %>%
  filter(numberOfWords > 200) %>%
  dplyr::select(-numberOfWords) %>%
  droplevels()

all.phon <- mutate(all.phon, phon = str_to_lower(phon))
all.phon %>% write_csv('../data/Processed/all_phon.csv')

# Match WALS and IDS by using the WALS CODE (hand-coded) in wals_codes.csv
all.languages <- all.languages %>% 
  left_join(wals) %>% 
  select(-Macroarea)
not.on.wals <- all.languages %>% 
  filter(is.na(wals_code))

# For languages that are on WALS, use that information. 
# If not on WALS, use Glottocode.
all.languages <- all.languages %>% 
  filter(!(is.na(wals_code))) %>% 
  select(-Latitude, -Longitude) %>% 
  left_join(select(wals_info, 
                   wals_code, 
                   latitude, 
                   longitude, 
                   genus, 
                   family))
not.on.wals <- left_join(not.on.wals, glottocode) %>% 
  select(-Latitude, 
         -Longitude, 
         -iso639P3code, 
         family = family_id, 
         genus = parent_id, 
         -country_ids)
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

# Number of languages from WALS

number.of.families <- all.languages$family %>% 
  unique() %>% 
  length()
paste("Number of families present in WALS:", number.of.families)
wals.families <- wals_info$family %>% unique() %>% 
  length()
number.of.families / wals.families
all.languages$family %>% 
  table() %>% 
  sort()
not.on.wals %>% 
  filter(!(family %in% all.languages$family))

all.languages <- all.languages %>% 
  bind_rows(not.on.wals) %>% 
  filter(Name %in% all.phon$language)
# Save relevant data on new file
all.languages <- all.languages %>% 
  select(ID, Name, latitude, longitude, family)
all.languages %>% 
  write_csv("../data/Processed/all_language_info.csv")


# Create a morphology-adjusted dataset --------

# Uncomment following lines to rerun the code obtaining the
# string candidates. Commented because it takes a long time to run and
# it's a deterministic process.

# all.lang.adjusted.phon <- map(as.character(sort(unique(all.phon$language))), function(language){
#   print(language)
#   lang.results <- clean.language(language, all.phon, 'phon')
#   lang.results$census$language <- language
#   return(lang.results)
# })
# 
# names(all.lang.adjusted.phon) <- as.character(sort(unique(all.phon$language)))
# 
# 
# write_rds(all.lang.adjusted.phon, "../data/Processed/r_objects/all_langs_adjusted.rds")
all.lang.adjusted.phon <- read_rds("../data/Processed/r_objects/all_langs_adjusted.rds")

complete.markers <- map_dfr(all.lang.adjusted.phon, function(lang){
  return(lang$clean.df)
}) %>%
  dplyr::select(-clean.phon, suffix = marker)

prefix.languages <- all.phon %>%
  mutate(phon = stringi::stri_reverse(phon))
# 
# all.lang.adjusted.prefix <- map(as.character(sort(unique(all.phon$language))), function(language){
#   print(language)
#   lang.results <- clean.language(language, prefix.languages, variable = 'phon')
#   lang.results$census$language <- language
#   return(lang.results)
# })
# 
# all.lang.adjusted.prefix %>%
#   write_rds("../data/Processed/r_objects/all_lang_adjusted_prefix.rds")
all.lang.adjusted.prefix <- read_rds("../data/Processed/r_objects/all_lang_adjusted_prefix.rds")
 
complete.markers.prefix <- map_dfr(all.lang.adjusted.prefix, function(lang){
  return(lang$clean.df)
}) %>% 
  dplyr::select(-clean.phon, prefix = marker) %>% 
  mutate(phon = stringi::stri_reverse(phon), prefix = stringi::stri_reverse(prefix))

# remove duplicate rows
complete.markers <- distinct(complete.markers)
complete.markers.prefix <- distinct(complete.markers.prefix)

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
write_csv(marker.census, "../data/Processed/marker_census.csv")

# number of words to be removed
all.phon.adjusted %>% 
  rowwise() %>% 
  mutate(suffix.position = nchar(phon) - nchar(suffix), prefix.position = nchar(prefix) + 1,
         clean.phon = ifelse(suffix == "#", phon, str_sub(phon, 1, suffix.position)),
         clean.phon = ifelse(prefix == "#", clean.phon, str_sub(clean.phon, prefix.position))) %>% 
  dplyr::select(Form, alt_form, language, english.name, ontological.category, old.phon = phon, phon = clean.phon)  %>% 
  filter(nchar(phon) <= 2) %>% 
  nrow()

all.phon.adjusted <- all.phon.adjusted %>% 
  rowwise() %>% 
  mutate(suffix.position = nchar(phon) - nchar(suffix), prefix.position = nchar(prefix) + 1,
    clean.phon = ifelse(suffix == "#", phon, str_sub(phon, 1, suffix.position)),
    clean.phon = ifelse(prefix == "#", clean.phon, str_sub(clean.phon, prefix.position))) %>% 
  dplyr::select(Form, alt_form, language, english.name, ontological.category, old.phon = phon, phon = clean.phon)  %>% 
  filter(nchar(phon) > 2)

# Further adjustment: remove within-class homophones but keep between-class homophones.
# Gets rid of within-class colexification
# Save how many homophones were removed.

homophone.census <- all.phon.adjusted %>% 
  group_by(language, ontological.category, phon) %>%
  tally() %>% 
  filter(n > 1) %>% 
  group_by() %>% 
  group_by(language) %>% 
  tally()

homophone.census <- homophone.census %>% 
  group_by(language) %>% 
  dplyr::summarize(Number.Homophones = sum(n))

all.phon.adjusted %>% 
  group_by(language) %>% 
  tally() %>% 
  right_join(homophone.census) %>% 
  mutate(perc = Number.Homophones / n) %>% 
  filter(Number.Homophones == max(Number.Homophones) | Number.Homophones == min(Number.Homophones))

sum(homophone.census$Number.Homophones)


all.phon.adjusted <- all.phon.adjusted %>% 
  group_by(language, ontological.category) %>% 
  mutate(has.homophone = duplicated(phon)) %>% 
  filter(has.homophone == FALSE) # Removes only within category homophones

# Join marker census with homophone census, and compare original/adjusted number of words
original.number.words <- all.phon %>% group_by(language) %>% dplyr::summarize(original.number.words = n())
adjusted.number.words <- all.phon.adjusted %>% group_by(language) %>% dplyr::summarize(adjusted.number.words = n())
original.number.words %>% 
  arrange(desc(original.number.words))
mean(original.number.words$original.number.words)
sd(original.number.words$original.number.words)
original.number.words %>% 
  filter(original.number.words == max(original.number.words) | original.number.words == min(original.number.words))
sum(original.number.words$original.number.words)


# Replace NA with 0 for the numeric columns. NAs result from languages without within-class homophones to remove.

write_csv(homophone.census, "../data/Processed/homophone_census.csv")
write_csv(all.phon.adjusted, "../data/Processed/all_phon_adjusted.csv")


# Make geographical clusters and world map of languages ---------------

# Make a matrix with only longitude and latitude
reduced <- filter(all.languages, !(is.na(longitude)), !(is.na(latitude)))
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
ggplot(silhouettes, aes(x = name, y = value)) + geom_point() + geom_label(aes(label = name), size = 2)

# Plot shows that a good number of clusters is k = 20 (peak of silhouette), along with 5, 10, 13, and 28
# Make a color vector of 20 colors
col_vector<-c('#e6194b', '#3cb44b', '#ffe119', '#4363d8','#f58231', 
              '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', 
              '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', 
              '#aaffc3', '#808000', '#ffd8b1', '#000075', '#000000')

# Make a dataframe with all langauge information including WALS info, Family and Geo Cluster
language.groups <- reduced %>% 
  add_column(geo.cluster = cutree(cluster.regions, k = 20)) %>% 
  select(Name, geo.cluster) %>% 
  mutate(geo.cluster = factor(geo.cluster)) %>% 
  right_join(all.languages)
language.groups %>% write_csv("../data/Processed/language_groups.csv")

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
world <- ne_coastline(scale = "medium", returnclass = "sf")
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

ggsave("../results/Figures/Supplemental/geo_cluster_map.png", width = 17, height = 9, units = "cm", dpi = 900)
