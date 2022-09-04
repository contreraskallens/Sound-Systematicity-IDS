library(car)

wals <- read.csv("/Users/Tanya/Dropbox/MorphCompl/Data/WALS/WALS_final.csv")
# Original chapters:11-38; recoded chapters: 39-66; numchap: 67; meanVal:68
wals.original <- wals[1:38]
wals.recoded.C <- wals[39:68]

#################################################################################################
# Recoding
#################################################################################################

#################################################################################################
# 22A Inflectional Synthesis: 7 categories (continuous)
#
old_ch <- "X22A.Inflectional.Synthesis.of.the.Verb"
new_ch <- "Ch_22A"
levels(wals.original[[old_ch]])
wals.original[[new_ch]] <- wals.original[[old_ch]]
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) unlist(strsplit(x," "))[1])
#head(na.exclude(wals[c(old_ch,new_ch)]),20)
#head(wals.original[c(old_ch,new_ch)])
#head(na.exclude(wals.original[c(old_ch,new_ch)]),20)

#################################################################################################
# 26A Prefixing vs. Suffixing in Inflectional Morphology: 6 categories (non-continuous)
#    binarization: "little or no inflectional morphology": 0   all others: 1
#
old_ch <- "X26A.Prefixing.vs..Suffixing.in.Inflectional.Morphology"
new_ch <- "Ch_26A"
levels(wals.original[[old_ch]])
wals.original[[new_ch]] <- wals.original[[old_ch]]
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) unlist(strsplit(x," "))[1])
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                       FUN =function (x) ifelse(x==1,0,1))
#head(na.exclude(wals[c(old_ch,new_ch)]),20)
#head(wals.original[c(old_ch,new_ch)])
#head(na.exclude(wals.original[c(old_ch,new_ch)]),20)

#################################################################################################
# 27A Reduplication: 3 categories (non-continuous)
#    binarization: "no productive reduplication": 0   all others: 1
#
old_ch <- "X27A.Reduplication"
new_ch <- "Ch_27A"
#levels(wals[[old_ch]])
#head(na.exclude(wals[c(old_ch,new_ch)]),20)
wals.original[[new_ch]] <- wals.original[[old_ch]]
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) unlist(strsplit(x," "))[1])
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                       FUN =function (x) ifelse(x==3,0,1))
#head(wals.original[c(old_ch,new_ch)])
#head(na.exclude(wals.original[c(old_ch,new_ch)]),20)

#################################################################################################
# 28A Case Syncretism: 4 categories (contiuous)
#    recategorization: swap "2 core cases only" and "3 core and non-core cases"
#    so that: no case < syncretism in core and non-core < core only < no syncretism
#
old_ch <- "X28A.Case.Syncretism"
new_ch <- "Ch_28A"
levels(wals.original[[old_ch]])
wals.original[[new_ch]] <- wals.original[[old_ch]]
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) unlist(strsplit(x," "))[1])
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                       FUN =function(x) {x <- recode(x,"2=3; 3=2"); x}) 
#head(na.exclude(wals[c(old_ch,new_ch)]),20)
#head(wals.original[c(old_ch,new_ch)])
#head(na.exclude(wals.original[c(old_ch,new_ch)]),20)

#################################################################################################
# 29A Syncretism in Verbal Person/Number marking: 3 categories (continuous)
#
old_ch <- "X29A.Syncretism.in.Verbal.Person.Number.Marking"
new_ch <- "Ch_29A"
#levels(wals[[old_ch]])
#head(na.exclude(wals[c(old_ch,new_ch)]),20)
wals.original[[new_ch]] <- wals.original[[old_ch]]
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) unlist(strsplit(x," "))[1])
#head(wals.original[c(old_ch,new_ch)])
#head(na.exclude(wals.original[c(old_ch,new_ch)]),20)

#################################################################################################
# 30A Number of Genders: 5 categories (continuous)
#
old_ch <- "X30A.Number.of.Genders"
new_ch <- "Ch_30A"
#levels(wals[[old_ch]])
#head(na.exclude(wals[c(old_ch,new_ch)]),20)
wals.original[[new_ch]] <- wals.original[[old_ch]]
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                       FUN =function (x) unlist(strsplit(x," "))[1])
#head(wals.original[c(old_ch,new_ch)])
#head(na.exclude(wals.original[c(old_ch,new_ch)]),20)

#################################################################################################
# 33A Coding of Nominal Plurality: 9 categories (partial continuous)
#    binarization: plural word, no plural: 0   all others (including clitics!):1
# 
old_ch <- "X33A.Coding.of.Nominal.Plurality"
new_ch <- "Ch_33A"
#levels(wals[[old_ch]])
#head(na.exclude(wals[c(old_ch,new_ch)]),20)
wals.original[[new_ch]] <- wals.original[[old_ch]]
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) unlist(strsplit(x," "))[1])
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) ifelse((x==7)|(x==9),0,1))
#head(wals.original[c(old_ch,new_ch)])
#head(na.exclude(wals.original[c(old_ch,new_ch)]),20)

#################################################################################################
# 34A Occurrence of Nominal Plurality: 6 categories (continuous)
#
old_ch <- "X34A.Occurrence.of.Nominal.Plurality"
new_ch <- "Ch_34A"
#levels(wals.original[[old_ch]])
wals.original[[new_ch]] <- wals.original[[old_ch]]
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) unlist(strsplit(x," "))[1])
#head(na.exclude(wals[c(old_ch,new_ch)]),20)
#head(wals.original[c(old_ch,new_ch)])
#head(na.exclude(wals.original[c(old_ch,new_ch)]),20)

#################################################################################################
# 37A Definite Articles: 5 cat. (non-continuous)
#    binarization: definite affix: 1   all others: 0
#
old_ch <- "X37A.Definite.Articles"
new_ch <- "Ch_37A"
levels(wals.original[[old_ch]])
wals.original[[new_ch]] <- wals.original[[old_ch]]
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) unlist(strsplit(x," "))[1])
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) ifelse(x==3,1,0))
#head(na.exclude(wals[c(old_ch,new_ch)]),20)
#head(wals.original[c(old_ch,new_ch)])
#head(na.exclude(wals.original[c(old_ch,new_ch)]),20)

#################################################################################################
# 38A Indefinite Articles: 5 categories (non-continuous)
#    binarization: indefinite affix: 1   all others: 0
#
old_ch <- "X38A.Indefinite.Articles"
new_ch <- "Ch_38A"
levels(wals.original[[old_ch]])
wals.original[[new_ch]] <- wals.original[[old_ch]]
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) unlist(strsplit(x," "))[1])
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) ifelse(x==3,1,0))
#head(na.exclude(wals[c(old_ch,new_ch)]),40)
#head(wals.original[c(old_ch,new_ch)])
#head(na.exclude(wals.original[c(old_ch,new_ch)]),40)

#################################################################################################
# 49A Number of Cases: 9 categories (continuous, but "9 borderline case marking" removed)
#
old_ch <- "X49A.Number.of.Cases"
new_ch <- "Ch_49A"
#wals[wals[[old_ch]]=="9 Exclusively borderline case-marking",]
levels(wals.original[[old_ch]])
wals.original[[new_ch]] <- wals.original[[old_ch]]
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) unlist(strsplit(x," "))[1])
levels(wals.original[[new_ch]])[levels(wals.original[[new_ch]])==9] <- NA
#head(na.exclude(wals[c(old_ch,new_ch)]),20)
#head(wals.original[c(old_ch,new_ch)])
#head(na.exclude(wals.original[c(old_ch,new_ch)]),20)
#wals.original[wals.original[[old_ch]]=="9 Exclusively borderline case-marking",]

#################################################################################################
# 51A Position of Case Affixes: 9 categories
#    binarization: No case affixes or adpositional clitics: 0   all others (including clitics!): 1
#
old_ch <- "X51A.Position.of.Case.Affixes"
new_ch <- "Ch_51A"
levels(wals.original[[old_ch]])
wals.original[[new_ch]] <- wals.original[[old_ch]]
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) unlist(strsplit(x," "))[1])
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) ifelse(x==9,0,1))
#head(na.exclude(wals[c(old_ch,new_ch)]),20)
#head(wals.original[c(old_ch,new_ch)])
#head(na.exclude(wals.original[c(old_ch,new_ch)]),20)

#################################################################################################
# 57A Position of Pronominal Possessive Affixes: 4 categories (non-continuous)
#    binarization: no affixes: 0   all others: 1
#
old_ch <- "X57A.Position.of.Pronominal.Possessive.Affixes"
new_ch <- "Ch_57A"
levels(wals.original[[old_ch]])
wals.original[[new_ch]] <- wals.original[[old_ch]]
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) unlist(strsplit(x," "))[1])
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) ifelse(x==4,0,1))
#head(na.exclude(wals[c(old_ch,new_ch)]),20)
#head(wals.original[c(old_ch,new_ch)])
#head(na.exclude(wals.original[c(old_ch,new_ch)]),20)

#################################################################################################
# 59A Possessive Classification: 4 cat. (continuous)
#
old_ch <- "X59A.Possessive.Classification"
new_ch <- "Ch_59A"
levels(wals.original[[old_ch]])
wals.original[[new_ch]] <- wals.original[[old_ch]]
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) unlist(strsplit(x," "))[1])
#head(na.exclude(wals[c(old_ch,new_ch)]),20)
#head(wals.original[c(old_ch,new_ch)])
#head(na.exclude(wals.original[c(old_ch,new_ch)]),20)

#################################################################################################
# 65A Perfective/Imperfective Aspect
#    binarization: "little or no inflectional morphology": 0   all others: 1
#
old_ch <- "X65A.Perfective.Imperfective.Aspect"
new_ch <- "Ch_65A"
levels(wals.original[[old_ch]])
wals.original[[new_ch]] <- wals.original[[old_ch]]
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) unlist(strsplit(x," "))[1])
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) ifelse(x==1,1,0))
#head(na.exclude(wals[c(old_ch,new_ch)]),20)
#head(wals.original[c(old_ch,new_ch)])
#head(na.exclude(wals.original[c(old_ch,new_ch)]),20)

#################################################################################################
# 66A The Past Tense: 4 continuous (but reordered)
#    reordering: 1: "4 no past tense", 2: "1 Present, no remoteness distinctions", 
#             3: "2 Present, 2-3 remoteness distinctions", 4: "3 Present, 4 or more remoteness distinctions"
#
old_ch <- "X66A.The.Past.Tense"
new_ch <- "Ch_66A"
levels(wals.original[[old_ch]])
wals.original[[new_ch]] <- wals.original[[old_ch]]
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) unlist(strsplit(x," "))[1])
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function(x) {x <- recode(x,"1=2; 2=3; 3=4; 4=1"); x}) 
#head(na.exclude(wals[c(old_ch,new_ch)]),20)
#head(wals.original[c(old_ch,new_ch)])
#head(na.exclude(wals.original[c(old_ch,new_ch)]),20)

#################################################################################################
# 67A The Future Tense
#    binarization: existent/non-existent
#
old_ch <- "X67A.The.Future.Tense"
new_ch <- "Ch_67A"
levels(wals.original[[old_ch]])
wals.original[[new_ch]] <- wals.original[[old_ch]]
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) unlist(strsplit(x," "))[1])
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) ifelse(x==1,1,0))
#head(na.exclude(wals[c(old_ch,new_ch)]),20)
#head(wals.original[c(old_ch,new_ch)])
#head(na.exclude(wals.original[c(old_ch,new_ch)]),20)

#################################################################################################
# 69A Position of Tense/Aspect Affixes: 5 cat. (non-continuous)
#    binarization: no tense-aspect inflection: 0   all others: 1
#
old_ch <- "X69A.Position.of.Tense.Aspect.Affixes"
new_ch <- "Ch_69A"
levels(wals.original[[old_ch]])
wals.original[[new_ch]] <- wals.original[[old_ch]]
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) unlist(strsplit(x," "))[1])
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) ifelse(x==5,0,1))
#head(na.exclude(wals[c(old_ch,new_ch)]),20)
#head(wals.original[c(old_ch,new_ch)])
#head(na.exclude(wals.original[c(old_ch,new_ch)]),20)

#################################################################################################
# 70A The Morphological Imperative: 5 cat. (partial continuous)
#    recategorization: 1: "5 no second person imperatives", 2: "4 second person number-neutral", 
#                     3: "3 Second plural" and "2 Second singular", 4: "1 Second singular and plural"
#
old_ch <- "X70A.The.Morphological.Imperative"
new_ch <- "Ch_70A"
levels(wals.original[[old_ch]])
wals.original[[new_ch]] <- wals.original[[old_ch]]
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) unlist(strsplit(x," "))[1])
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function(x) {x <- recode(x,"1=4; 2=3; 4=2; 5=1"); x}) 
#head(na.exclude(wals[c(old_ch,new_ch)]),20)
#head(wals.original[c(old_ch,new_ch)])
#head(na.exclude(wals.original[c(old_ch,new_ch)]),20)

#################################################################################################
# 73A The Optative
#    binarization: present/absent
#
old_ch <- "X73A.The.Optative"
new_ch <- "Ch_73A"
levels(wals.original[[old_ch]])
wals.original[[new_ch]] <- wals.original[[old_ch]]
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) unlist(strsplit(x," "))[1])
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) ifelse(x==1,1,0))
#head(na.exclude(wals[c(old_ch,new_ch)]),20)
#head(wals.original[c(old_ch,new_ch)])
#head(na.exclude(wals.original[c(old_ch,new_ch)]),20)

#################################################################################################
# 74A Situational possibility: 3 cat. (non-continuous)
#    binarization: verbal constructions, other markers (particles, adverbials, nouns): 0   affixes on verbs: 1
#
old_ch <- "X74A.Situational.Possibility"
new_ch <- "Ch_74A"
levels(wals.original[[old_ch]])
wals.original[[new_ch]] <- wals.original[[old_ch]]
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) unlist(strsplit(x," "))[1])
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) ifelse(x==1,1,0))
#head(na.exclude(wals[c(old_ch,new_ch)]),20)
#head(wals.original[c(old_ch,new_ch)])
#head(na.exclude(wals.original[c(old_ch,new_ch)]),20)

#################################################################################################
# 75A Epistemic Possibility: 3 cat. (non-continuous)
#    binarization: verbal constructions, other markers (particles, adverbials, nouns): 0   affixes on verbs: 1
#
old_ch <- "X75A.Epistemic.Possibility"
new_ch <- "Ch_75A"
levels(wals.original[[old_ch]])
wals.original[[new_ch]] <- wals.original[[old_ch]]
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) unlist(strsplit(x," "))[1])
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) ifelse(x==2,1,0))
#head(na.exclude(wals[c(old_ch,new_ch)]),20)
#head(wals.original[c(old_ch,new_ch)])
#head(na.exclude(wals.original[c(old_ch,new_ch)]),20)

#################################################################################################
# 78A Coding of Evidentiality:6 cat. (non-continuous)
#   binarization: no grammatical evidentials, evidential particles: 0
#                 verbal affix or clitic, part of the tense system: 1
#   excluded: modal morpheme (can be either modal verb or bound morpheme), mixed

old_ch <- "X78A.Coding.of.Evidentiality"
new_ch <- "Ch_78A"
#wals[wals[[old_ch]]=="9 Exclusively borderline case-marking",]
levels(wals.original[[old_ch]])
wals.original[[new_ch]] <- wals.original[[old_ch]]
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) unlist(strsplit(x," "))[1])
levels(wals.original[[new_ch]])[levels(wals.original[[new_ch]])==5] <- NA
levels(wals.original[[new_ch]])[levels(wals.original[[new_ch]])==6] <- NA
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) ifelse(x==1|x==4,0,1))
#head(na.exclude(wals[c(old_ch,new_ch)]),20)
#head(wals.original[c(old_ch,new_ch)])
#head(na.exclude(wals.original[c(old_ch,new_ch)]),20)

#################################################################################################
# 94A Subordination: 5 cat. (non-continuous)
#   binarization: subordination suffix: 1
#                 all others: 0
#   removed: 5 Mixed

old_ch <- "X94A.Order.of.Adverbial.Subordinator.and.Clause"
new_ch <- "Ch_94A"
levels(wals.original[[old_ch]])
wals.original[[new_ch]] <- wals.original[[old_ch]]
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) unlist(strsplit(x," "))[1])
levels(wals.original[[new_ch]])[levels(wals.original[[new_ch]])==5] <- NA
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) ifelse(x==4,1,0))
#head(na.exclude(wals[c(old_ch,new_ch)]),20)
#head(wals.original[c(old_ch,new_ch)])
#head(na.exclude(wals.original[c(old_ch,new_ch)]),20)

#################################################################################################
# 101A Expression of Pronominal Subjects: 6 cat. (non-continuous)
#   binarization: obligatory pronouns in subj. position, subj. pronouns in different position, optional pronouns in subj. position: 0
#                 subject affixes on verbs, subject clitics on variable host: 1
#   excluded: mixed	

old_ch <- "X101A.Expression.of.Pronominal.Subjects"
new_ch <- "Ch_101A"
levels(wals.original[[old_ch]])
wals.original[[new_ch]] <- wals.original[[old_ch]]
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) unlist(strsplit(x," "))[1])
levels(wals.original[[new_ch]])[levels(wals.original[[new_ch]])==6] <- NA
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) ifelse(x==2|x==3,1,0))
#head(na.exclude(wals[c(old_ch,new_ch)]),20)
#head(wals.original[c(old_ch,new_ch)])
#head(na.exclude(wals.original[c(old_ch,new_ch)]),20)

#################################################################################################
# 102A Verbal Person Marking: 5 cat. (partially continuous)
#    recategorization: 1: "No person marking", 
#                      2: "only A argument"/"only P argument"/"A or P argument", 3: "A and P argument" 
#
old_ch <- "X102A.Verbal.Person.Marking"
new_ch <- "Ch_102A"
levels(wals.original[[old_ch]])
wals.original[[new_ch]] <- wals.original[[old_ch]]
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) unlist(strsplit(x," "))[1])
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function(x) {x <- recode(x,"3=2; 4=2; 5=3"); x}) 
#head(na.exclude(wals[c(old_ch,new_ch)]),20)
#head(wals.original[c(old_ch,new_ch)])
#head(na.exclude(wals.original[c(old_ch,new_ch)]),20)

#################################################################################################
# 111A Nonperiphrastic causative constructions: 4 cat. (non-continuous)
#   binarization: Neither morph nor compound, compound but not morph.: 0
#                 morph. but not compound: 1
#   excluded: "Both morph. and compound type"

old_ch <- "X111A.Nonperiphrastic.Causative.Constructions"
new_ch <- "Ch_111A"
levels(wals.original[[old_ch]])
wals.original[[new_ch]] <- wals.original[[old_ch]]
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) unlist(strsplit(x," "))[1])
levels(wals.original[[new_ch]])[levels(wals.original[[new_ch]])==4] <- NA
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) ifelse(x==2,1,0))
#head(na.exclude(wals[c(old_ch,new_ch)]),20)
#head(wals.original[c(old_ch,new_ch)])
#head(na.exclude(wals.original[c(old_ch,new_ch)]),20)

#################################################################################################
# 112A Negative Morphemes: 6 cat. (non-continuous)
#   binarization: negative particle, negative word, negative auxiliary verb: 0
#                 negative affix: 1
#   exclude: double negation, variation between negative word and affix

old_ch <- "X112A.Negative.Morphemes"
new_ch <- "Ch_112A"
levels(wals.original[[old_ch]])
wals.original[[new_ch]] <- wals.original[[old_ch]]
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) unlist(strsplit(x," "))[1])
levels(wals.original[[new_ch]])[levels(wals.original[[new_ch]])==6] <- NA
levels(wals.original[[new_ch]])[levels(wals.original[[new_ch]])==5] <- NA
levels(wals.original[[new_ch]]) <- sapply(levels(wals.original[[new_ch]]),
                                          FUN =function (x) ifelse(x==1,1,0))
#head(na.exclude(wals[c(old_ch,new_ch)]),20)
#head(wals.original[c(old_ch,new_ch)])
#head(na.exclude(wals.original[c(old_ch,new_ch)]),20)


#######################################################################################################
# Scaling
#################################################################################################

#wals.original[39:66] == wals[39:66]
wals.original$meanVal <- apply(wals.original[39:66], 1, FUN = function(x) mean(as.numeric(x),na.rm = T)) 
#round(wals.original$meanVal, digits =3) == round(wals$meanVal, digits=3)

# Number of Chapters
wals.original$NumChap <- 28 - apply(is.na(wals.original[,39:66]),1,sum)
wals.original$NumChap == wals$numChap

# No feature scaling
wals.scaled <- wals.original
wals.scaled$meanVal <- apply(wals.scaled[39:66], 1, FUN = function(x) mean(as.numeric(x),na.rm = T)) 
colnames(wals.scaled)[2]="iso" #"iso_code"
write.csv(wals.scaled[c("iso", "NumChap", "meanVal")],
          "/Users/Tanya/Dropbox/MorphCompl/Data/WALS/WALS_final_short_NoScaling.csv", row.names = F)
unique(wals.scaled$iso)

# Feature scaling
f_scale <- function(x) as.numeric(x) /max(as.numeric(x), na.rm = TRUE)
wals.scaled <- wals.original
wals.scaled[39:66] <- apply(wals.original[39:66], 2 , FUN = f_scale) 
wals.scaled$meanVal <- apply(wals.scaled[39:66], 1, FUN = function(x) mean(as.numeric(x),na.rm = T)) 
colnames(wals.scaled)[2]="iso" #"iso_code"
write.csv(wals.scaled[c("iso", "NumChap", "meanVal")],
          "/Users/Tanya/Dropbox/MorphCompl/Data/WALS/WALS_final_short_ScalingTo1.csv", row.names = F)
unique(wals.scaled$iso)
