library(topicmodels)
library(tm)
library(ggplot2)
library(reshape2)
library(tidytext)
library(SnowballC)
library(quanteda)
library(quanteda.textmodels)
library(seededlda)
library(tidyverse)

load(file = "Full_PS1.Robj")

################## for topic modelling trying seeded LDA

longDescriptions <- Full_PS1 |>
  filter(str_detect(Full_PS1$exhibitionDescription, "\\d\\.\\s\\w+")) |>
  select(exhibitID, exhibitionDescription) |>
  mutate(exhibitionDescription = str_remove_all(
    exhibitionDescription,
    regex("ps1|p\\.s\\.1|P\\.S\\.1|PS1", ignore_case = TRUE)
  ))

longDescriptions <- longDescriptions[!duplicated(longDescriptions), ]

# fill in missing IDs

longDescriptions <- longDescriptions |> mutate(exhibitID = as.numeric(exhibitID)) |>
  mutate(exhibitID = case_when(
    str_detect(exhibitionDescription, "Xefirotarch") ~ 114, 
    str_detect(exhibitionDescription, "Gnuform") ~ 75, 
    str_detect(exhibitionDescription, "Iwamoto") ~ 44, 
    str_detect(exhibitionDescription, "Eliasson") ~ 31,
    str_detect(exhibitionDescription, "Toranzo") ~ 5581,
    str_detect(exhibitionDescription, "Viejo") ~ 5634,
    str_detect(exhibitionDescription, "Escobar") ~ 5654,
    str_detect(exhibitionDescription, "Tiravanija:") ~ 5692, 
    str_detect(exhibitionDescription, "Claflin") ~ 5854,
    str_detect(exhibitionDescription, "Lawson") & str_detect(exhibitionDescription, "Ethiopia") ~ 5452,
    str_detect(exhibitionDescription, "Rashid") ~ 5583,
    str_detect(exhibitionDescription, "Claflin") ~ 5854,
    str_detect(exhibitionDescription, "Susiraja") ~ 5635,
    str_detect(exhibitionDescription, "rasqua") ~ 5709,
    str_detect(exhibitionDescription, "Pacita") & str_detect(exhibitionDescription, "spring") ~ 5744,
    str_detect(exhibitionDescription, "Ixil") ~ 5747,
    str_detect(exhibitionDescription, "Jasmine") ~ 5804,
    str_detect(exhibitionDescription, "Ceremonies") ~ 5817,
    str_detect(exhibitionDescription, "Ceccaldi") ~ 5853,
    str_detect(exhibitionDescription, "Obomsawin") ~ 5855, 
    
    TRUE ~ exhibitID
  ))

#####

archKeywords <- c("architect", "structur", "urban", "furniture", "typography")

drawKeywords <- c("draw", "sketch", "paper", "print", "illustrat", "collag")

filmKeywords <- c("film", "video", "documentar", "movie", "cinema", "role", "screen")

perfKeywords <- c("perform", "music", "record", "poet", "danc", "video")

pandsKeywords <- c("paint", "sculpt", "structur", "brush")

photoKeywords <- c("photo", "photograph", "pictur", "film", "camera")

otherKeywords <- c("instal", "multi", "fashion", "interdisc")

dept_dict <- dictionary(list(
  "Architecture and Design" = archKeywords,
  "Drawings and Prints" = drawKeywords,
  "Film" = filmKeywords,
  "Media and Performance" = perfKeywords,
  "Painting and Sculpture" = pandsKeywords,
  "Photography" = photoKeywords, 
  "other" = otherKeywords
))


tokens <- tokens(
  longDescriptions$exhibitionDescription,
  remove_punct = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE,
  remove_separators = TRUE
)
tokens <- tokens_tolower(tokens)
tokens <- tokens_remove(tokens, stopwords("en"))
tokens <- tokens_wordstem(tokens)

ps1_fillers <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "nov", "dec", 
                 "p.s.1", "contemporary", "art", "center", "presents", "exhibition", "exhibit", "work", 
                 "works", "artist", "new", "york", "clocktower", "museum", "archive", "ist", "moma", "curate",
                 "curator", "collect", "greater", "curat", "includ", "will", "ing", "studio", "ion", "program", "associ", "also", "well", "citi", 
                 "los", "angel", "p.s.", "ps1", "p.s")

fillers_stem <- wordStem(ps1_fillers)

tokens <- tokens_remove(tokens, fillers_stem)

tokens <- tokens_keep(tokens, min_nchar = 3)

dfm <- dfm(tokens)

set.seed(444) 
seededModel <- textmodel_seededlda(dfm, dictionary = dept_dict, residual = F)

topics <- topics(seededModel)

terms(seededModel, 10)

doc_topic_probs <- seededModel$theta

dept_stm <- colnames(doc_topic_probs)[max.col(doc_topic_probs)]

longDescriptions$dept_stm <- dept_stm

longDescriptions |> group_by(dept_stm) |> summarise((n()/nrow(longDescriptions))*100)

############ groups based on departments (if else statement)

archKeywords <- c("architect", "plan", "architectur")

drawKeywords <- c("draw", "notebook", "sketch", "paper")

filmKeywords <- c("film", "video", "documentar")

perfKeywords <- c("perform")

pandsKeywords <- c("paint", "sculpt")

photoKeywords <- c("photo", "photograph")

longDescriptions <- longDescriptions |>
  mutate(dept_hand = NA) |>
  mutate(exhibitionDescription = str_to_lower(exhibitionDescription))


for (i in 1:nrow(longDescriptions)) {
  if (str_detect(longDescriptions[i, 2], "architect") | str_detect(longDescriptions[i, 2], "plan")) {
    longDescriptions[i,4] = "Architecture and Design"
  } else if (str_detect(longDescriptions[i, 2], "film") | str_detect(longDescriptions[i, 2], "video")) {
    longDescriptions[i,4] = "Film"
  } else if (str_detect(longDescriptions[i, 2], "perform")) {
    longDescriptions[i,4] = "Media and Performance"
  } else if (str_detect(longDescriptions[i, 2], "photo")) {
    longDescriptions[i,4] = "Photography"
  } else if (str_detect(longDescriptions[i, 2], "draw") | str_detect(longDescriptions[i, 2], "sketch")) {
    longDescriptions[i,4] = "Drawings and Prints"
  } else if (str_detect(longDescriptions[i, 2], "paint") | str_detect(longDescriptions[i, 2], "sculpt")) {
    longDescriptions[i,4] = "Painting and Sculpture"
  } else {
    NA
  }
}

longDescriptions |> group_by(dept_hand) |> summarise((n()/nrow(longDescriptions))*100)

# write.csv(longDescriptions, "longDescriptions.csv", row.names = FALSE)

# longDescriptions <- read_excel("longDescriptions.xlsx") # 10% "train" set

# confusion matrix: 
table(longDescriptions$dept_stm, longDescriptions$dept_hand) # :(

# next steps: "points" version
# count number of times each word/root in a vocab matches to description, whichever category has most "points" 

archCount <- c("architect", "structur", "urban", "furniture", "typography")

drawCount <- c("draw", "notebook", "sketch", "paper", "print", "illustrat", "collag")

filmCount <- c("film", "video", "documentar", "movie", "cinema", "role", "screen")

mandpCount <- c("perform", "music", "record", "poet", "danc", "video")

pandsCount <- c("paint", "sculpt", "structur", "brush")

photoCount <- c("photo", "photograph", "pictur", "film", "camera")

otherCount <- c("instal", "multi", "fashion", "interdisc")

sum(str_count(longDescriptions[1,2], pandsCount))

write.csv(longDescriptions, "longDescriptions0413.csv", row.names = FALSE)

longDescriptions <- read.csv("~/thesis/longDescriptions0413.csv")
longDescriptions <- longDescriptions |> select(exhibitID, exhibitionDescription, dept_stm, dept_hand, dept_train)

dept_count <- character(length = nrow(longDescriptions))

for (i in 1:nrow(longDescriptions)) {
  ARcount <- sum(str_count(longDescriptions[i,2], archCount))
  FLcount<- sum(str_count(longDescriptions[i,2], filmCount))
  MPcount <- sum(str_count(longDescriptions[i,2], mandpCount))
  PScount <- sum(str_count(longDescriptions[i,2], pandsCount))
  PHcount <- sum(str_count(longDescriptions[i,2], photoCount))
  OTcount <- sum(str_count(longDescriptions[i,2], otherCount))
  DPcount <- sum(str_count(longDescriptions[i,2], drawCount))
  
  category <- c("Architecture and Design", "Film", "Media and Performance", "Painting and Sculpture", "Photography", "other", "Drawings and Prints")
  counts <- c(ARcount, FLcount, MPcount, PScount, PHcount, OTcount, DPcount)
  
  rank <- tibble(category, counts) |>
    arrange(desc(by = counts))
  
  dept_count[i] <- if (rank[1,2] == 0) {"other"} else
    if (rank[1,2] == rank[2,2]) {
      paste("TIED:", str_sub(rank[1,1], end = 5L), "OR", str_sub(rank[2,1], end = 5L), sep = " ")
    } else {rank[1,1]}
  
  longDescriptions[i,6] <- dept_count[i]
  
}

longDescriptions <- longDescriptions |>
  rename(dept_count = '...6')


#longDescriptions0317 <- longDescriptions0317 |>
#  mutate(dept_train = ifelse(dept_count == dept_stm & dept_count == dept_hand, dept_count, dept_train))


#longDescriptions0317 <- read.csv("~/thesis/longDescriptions0317.csv")

# confusion matrix: 
table(longDescriptions0317$dept_stm, longDescriptions0317$dept_count) # compare to seeded lda method
table(longDescriptions0317$dept_hand, longDescriptions0317$dept_count) # compare to ifelse method

nrow(longDescriptions0317 |>
       filter(dept_count == dept_stm)) # 115 agreements

nrow(longDescriptions0317 |>
       filter(dept_count == dept_hand)) # 93 agreements

view(longDescriptions0317 |>
       filter(dept_count == dept_stm & dept_count == dept_hand)) # 40 where all agree

longDescriptions0317 |>
  filter(is.na(dept_train)) |>
  filter(!(dept_count == dept_stm | dept_count == dept_hand)) |>
  view()

# hand assigned all for accuracy

# longDescriptions <- read.csv("~/thesis/longDescriptions0413.csv")

# get entries to say "Archi|Photo" instead of "TIED: Archi OR Photo"
longDescriptions <- longDescriptions |>
  mutate(dept_count = ifelse(str_detect(dept_count, "^TIED"),
                             sapply(str_extract_all(dept_count, "Archi|Drawi|Paint|Photo|other|Film|Media"), paste, collapse = "|"),
                             dept_count))

# accuracy of each method: 
longDescriptions |>
  filter(str_detect(dept_stm, dept_train)) |>
  nrow()/356 ## seeded LDA: 16% correct

longDescriptions |>
  mutate(dept_hand = ifelse(is.na(dept_hand), "other", dept_hand)) |>
  filter(str_detect(dept_hand, dept_train)) |>
  nrow()/356 ## if-else statements: 31.46% correct

longDescriptions |>
  mutate(dept_count = ifelse(str_detect(dept_count, "^TIED"),
                             sapply(str_extract_all(dept_count, "Archi|Drawi|Paint|Photo|other|Film|Media"), paste, collapse = "|"),
                             dept_count)) |>
  filter(dept_train == dept_count) |>
  nrow()/356 ## counting method: 53.37% correct

# add back onto Full_PS1

Full_PS1 <- read_excel("Full_PS1.xlsx") 

joinDescriptions <- longDescriptions |>
  rename(department = dept_train) |>
  select(exhibitID, department)


Full_PS1 <- Full_PS1 |>
  mutate(exhibitID = as.numeric(exhibitID)) |>
  left_join(y = joinDescriptions, by = "exhibitID")

# 2510/8182 have a department (8182 artist and exhibition pairs)
# 356/1090 have department (1090 unique exhibitions)

# found an issue with the way ageDuring is calculated, fixing that: 
Full_PS1 <- Full_PS1 |> 
  mutate(ageDuring = ifelse(artistYOD < year(start), as.numeric(artistYOD) - as.numeric(artistYOB), year(start) - as.numeric(artistYOB)))

save(Full_PS1, file = "Full_PS1.Robj")
write.csv(Full_PS1, "Full_PS1.csv", row.names = FALSE)
