library(tidyverse)
library(stringr)
library(lubridate)
library(fpp3)
library(car)
library(ordinal)
load(file = "Full_PS1.Robj")
load(file = "PS1_Artists.Robj")
# MoMAArtworks <- read.csv("~/Artworks.txt")
# MoMAArtists <- read.csv("~/Artists.txt")

###### Regression prep ######

MoMAArtworks <- MoMAArtworks |>
  select(Title, Artist, Nationality, Gender, ArtistYOB, ArtistYOD, ArtDate, Medium, 
         CreditLine, Classification, Department, DateAcquired)


# get rid of all parentheses

MoMAArtworks <- MoMAArtworks |>
  mutate(Nationality = str_extract(Nationality, "\\w+"), 
         Gender = str_extract(Gender, "\\w+"), 
         ArtistYOB = str_extract(ArtistYOB, "\\d+"), 
         ArtistYOD = str_extract(ArtistYOD, "\\d+"), 
         DateAcquired = as.Date(DateAcquired)
  ) |>
  mutate(Gender = case_when(
    str_detect(Artist, "\\w,\\s\\w") ~ "unknown/other",
    Gender == "female" ~ "female",
    Gender == "male" ~ "male", 
    Gender == "gender" ~ "unknown/other", 
    Gender == "non" ~ "unknown/other", 
    Gender == "transgender" ~ "unknown/other",
    is.na(Gender) ~ "unknown/other"
  ))

MoMAArtworks <- MoMAArtworks[!duplicated(MoMAArtworks), ]


# add true/false for if the artist has a work bought 1. ever, 
AcqEver <- logical(nrow(Full_PS1))

for (i in 1:nrow(Full_PS1)) {
  endRange <- dmy("10 June 2025") # latest acquisition date in MoMA df
  
  interval <- lubridate::interval(Full_PS1$start[i], endRange)
  
  inRange <- MoMAArtworks |> 
    filter(DateAcquired %within% interval)
  
  AcqEver[i] <- Full_PS1$artistName[i] %in% inRange$Artist
}

Full_PS1$AcqEver <- AcqEver

# 2. within 6 months of the END of PS1 exhibit, 
Acq6M <- logical(nrow(Full_PS1))

for (i in 1:nrow(Full_PS1)) {
  endRange <- Full_PS1$end[i] + months(6)
  
  interval <- lubridate::interval(Full_PS1$start[i], endRange)
  
  inRange <- MoMAArtworks |> 
    filter(DateAcquired %within% interval)
  
  Acq6M[i] <- Full_PS1$artistName[i] %in% inRange$Artist
}

Full_PS1$Acq6M <- Acq6M

table(Full_PS1$AcqEver, Full_PS1$Acq6M)

# 315 acquisitions within 6 months of exhibition at PS1

# 3. within 3 years of END of PS1 exhibit
Acq3Y <- logical(nrow(Full_PS1))

for (i in 1:nrow(Full_PS1)) {
  endRange <- Full_PS1$end[i] + years(3)
  
  interval <- lubridate::interval(Full_PS1$start[i], endRange)
  
  inRange <- MoMAArtworks |> 
    filter(DateAcquired %within% interval)
  
  Acq3Y[i] <- Full_PS1$artistName[i] %in% inRange$Artist
}

Full_PS1$Acq3Y <- Acq3Y


table(Full_PS1$AcqEver, Full_PS1$Acq3Y)


# based on eda results, create continent groups - N. America, Europe, Other

Full_PS1 <- Full_PS1 |>
  mutate(artistContinent = case_when(
    Full_PS1$artistNationality %in% c(
      "German","British","Italian","French","Swiss","Austrian","Belgian","Dutch",
      "Swedish","Irish","Spanish","Polish","Russian","Scottish","Serbian","Croatian",
      "Icelandic","Lithuanian","Norwegian","Albanian","Danish","Romanian","Bosnian",
      "Greek","Hungarian","Bulgarian","Finnish","Slovak","Slovenian","Welsh",
      "Czech","Luxembourger"
    ) ~ "Europe",
    
    Full_PS1$artistNationality %in% c(
      "Japanese","Chinese","Israeli","Iraqi","Thai","Korean","South Korean",
      "Palestinian","Iranian","Kuwaiti","Turkish","Lebanese","Filipino",
      "Georgian","Indian","Taiwanese","Kurdish","Malaysian","Pakistani",
      "South African","Nigerian","Egyptian","Algerian","Congolese","Ethiopian",
      "Ghanaian","Ivorian","Malian","Moroccan","Sudanese","Mozambican",
      "Namibian","Senegalese","Ugandan","Zimbabwean", "Brazilian","Argentine",
      "Chilean","Colombian","Uruguayan","Venezuelan", "Australian","New Zealander"
    ) ~ "Other",
    
    Full_PS1$artistNationality %in% c(
      "American","Canadian","Mexican","Cuban","Puerto Rican",
      "Native American","Bahamian","Guatemalan","Costa Rican","Carribean"
    ) ~ "North America",
    
    Full_PS1$artistNationality == "NA" ~ NA,
    
    TRUE ~ artistNationality
  ))

save(PS1_Artists, file = "PS1_Artists.Robj")
save(Full_PS1, file = "Full_PS1.Robj")
save(MoMAArtworks, file = "MoMAArtworks.Robj")

# have to clean up NAs for logistic: 

Full_PS1 <- Full_PS1 |>
  mutate(artistContinent = case_when(
    artistContinent == "North America" ~ "0. North America", 
    artistContinent == "Europe" ~ "2. Europe", 
    artistContinent == "Other" ~ "3. Other",
    artistContinent == "unknown" ~ "1. Unknown", 
    is.na(artistContinent) ~ "1. Unknown"
  ))

Full_PS1 <- Full_PS1 |>
  mutate(department = case_when(
    exhibitID == 5654 ~ "Media and Performance", 
    TRUE ~ department
  )) |>
  filter(!(is.na(start)))


# imputing age

for (i in 1:nrow(Full_PS1)) {
  if (!(is.na(Full_PS1$ageDuring[i]))) {
    Full_PS1$ageImputed[i] = Full_PS1$ageDuring[i]
  } else {
    Full_PS1$ageImputed[i] = Full_PS1 |> 
      filter(year(start) == year(Full_PS1$start[i])) |>
      summarise(meanAge = mean(ageDuring, na.rm = TRUE)) |>
      pull(meanAge)
  }
}


PreviousWork <- logical(nrow(Full_PS1))

for (i in 1:nrow(Full_PS1)) {
  startDate <- dmy("19 November 1929") # earliest acquisition date in MoMA df
  
  interval <- lubridate::interval(startDate, Full_PS1$start[i])
  
  inRange <- MoMAArtworks |> 
    filter(DateAcquired %within% interval)
  
  PreviousWork[i] <- Full_PS1$artistName[i] %in% inRange$Artist
}

Full_PS1$PreviousWork <- PreviousWork

table(Full_PS1$AcqEver, Full_PS1$PreviousWork)
# 1343 artist/exhibition pairs where there were only acquisitions made after PS1 exhibition
# 1133 artist/exhibition pairs where there acquisitions made before PS1 exhibition


Full_PS1 <- Full_PS1 |> mutate(horizon = 2025-year(start))

save(Full_PS1, file = "Full_PS1.Robj")

###### logistic regression: ######

# for pre and post 1997-10
summary(glm(Acq6M ~ daysExhibited + artistContinent + ageDuring + AvgGenderConf + department, data = Full_PS1, family = "binomial"))

PreMerger_PS1 <- Full_PS1 |>
  filter(yearmonth(start) < yearmonth("Oct 1997")) 
# summary(glm(Acq6M ~ daysExhibited + artistContinent + (ageDuring:ageBin) + AvgGenderConf, data = PreMerger_PS1, family = "binomial"))
# summary(glm(Acq3Y ~ daysExhibited + artistContinent + (ageDuring:ageBin) + AvgGenderConf, data = PreMerger_PS1, family = "binomial"))
# summary(glm(AcqEver ~ daysExhibited + artistContinent + (ageDuring:ageBin) + AvgGenderConf, data = PreMerger_PS1, family = "binomial"))

vif(glm(Acq6M ~ daysExhibited + artistContinent + ageImputed + AvgGenderConf + PreviousWork + horizon, data = PreMerger_PS1, family = "binomial"))
vif(glm(Acq6M ~ daysExhibited + artistContinent + ageImputed + AvgGenderConf + PreviousWork + department + horizon, data = PostMerger_PS1, family = "binomial"))
# no concerning values

# start with post-merger:

PostMerger_PS1 <- Full_PS1 |>
  filter(yearmonth(start) >= yearmonth("Oct 1997")) 
summary(glm(Acq6M ~ daysExhibited + artistContinent + ageImputed + AvgGenderConf + department + PreviousWork + horizon, data = PostMerger_PS1, family = "binomial"))
summary(glm(Acq3Y ~ daysExhibited + artistContinent + ageImputed + AvgGenderConf + department + PreviousWork + horizon, data = PostMerger_PS1, family = "binomial"))
summary(glm(AcqEver ~ daysExhibited + artistContinent + ageImputed + AvgGenderConf + department + PreviousWork + horizon, data = PostMerger_PS1, family = "binomial"))

# remove department

# variable selection since there are so many "useless" variables: 
step(glm(Acq6M ~ daysExhibited + artistContinent + ageImputed + AvgGenderConf + PreviousWork + horizon, data = Full_PS1, family = "binomial"))
step(glm(Acq3Y ~ daysExhibited + artistContinent + ageImputed + AvgGenderConf + PreviousWork + horizon, data = Full_PS1, family = "binomial"))
step(glm(AcqEver ~ daysExhibited + artistContinent + ageImputed + AvgGenderConf + PreviousWork + horizon, data = Full_PS1, family = "binomial"))

# reduced models
summary(glm(Acq6M ~ daysExhibited + artistContinent + PreviousWork, data = Full_PS1, family = "binomial"))
summary(glm(Acq3Y ~ daysExhibited + artistContinent + AvgGenderConf + PreviousWork, data = Full_PS1, family = "binomial"))
summary(glm(AcqEver ~ daysExhibited + artistContinent + ageImputed + PreviousWork + horizon, data = Full_PS1, family = "binomial"))

chisq.test(table(Full_PS1$artistContinent, Full_PS1$PreviousWork))
# statistically significant association between artistContinent and PreviousWork

prop.table(table(Full_PS1$artistContinent, Full_PS1$PreviousWork), 1)
# Almost all in Unknown category (99.49%) are FALSE for previous work

# still look at pre-and post- merger DFs for comparison's sake
PreMerger_PS1 <- Full_PS1 |>
  filter(yearmonth(start) < yearmonth("Oct 1997"))
step(glm(Acq6M ~ daysExhibited + artistContinent + ageImputed + AvgGenderConf + PreviousWork, data = PreMerger_PS1, family = "binomial"), trace = FALSE)
step(glm(Acq3Y ~ daysExhibited + artistContinent + ageImputed + AvgGenderConf + PreviousWork, data = PreMerger_PS1, family = "binomial"), trace = FALSE)
step(glm(AcqEver ~ daysExhibited + artistContinent + ageImputed + AvgGenderConf + PreviousWork, data = PreMerger_PS1, family = "binomial"), trace = FALSE)

summary(glm(Acq6M ~ artistContinent + PreviousWork, data = PreMerger_PS1, family = "binomial"))
summary(glm(Acq3Y ~ artistContinent + AvgGenderConf + PreviousWork, data = PreMerger_PS1, family = "binomial"))
summary(glm(AcqEver ~ artistContinent + ageImputed + PreviousWork, data = PreMerger_PS1, family = "binomial"))

PostMerger_PS1 <- Full_PS1 |>
  filter(yearmonth(start) >= yearmonth("Oct 1997"))
step(glm(Acq6M ~ daysExhibited + artistContinent + ageImputed + AvgGenderConf + PreviousWork, data = PostMerger_PS1, family = "binomial"), trace = FALSE)
step(glm(Acq3Y ~ daysExhibited + artistContinent + ageImputed + AvgGenderConf + PreviousWork, data = PostMerger_PS1, family = "binomial"), trace = FALSE)
step(glm(AcqEver ~ daysExhibited + artistContinent + ageImputed + AvgGenderConf + PreviousWork, data = PostMerger_PS1, family = "binomial"), trace = FALSE)

summary(glm(Acq6M ~ daysExhibited + artistContinent + PreviousWork, data = PostMerger_PS1, family = "binomial"))
summary(glm(Acq3Y ~ daysExhibited + artistContinent + AvgGenderConf + PreviousWork, data = PostMerger_PS1, family = "binomial"))
summary(glm(AcqEver ~ daysExhibited + artistContinent + ageImputed + PreviousWork, data = PostMerger_PS1, family = "binomial"))

###### cumulative log link logistic regression model (CLM) ######

# Set up a column with all three responses in it

Full_PS1 <- Full_PS1 |>
  mutate(AcqLink = case_when(
    Acq6M ~ 0,
    Acq3Y ~ 1,
    AcqEver ~ 2,
    !(AcqEver) ~ 3
  )) 

Full_PS1$AcqLink <- as.factor(Full_PS1$AcqLink)

# clm(), ordinal package

summary(clm(AcqLink ~ daysExhibited + PreviousWork + artistContinent + ageImputed + AvgGenderConf + horizon, data = Full_PS1))


anova(clm(AcqLink ~ daysExhibited + PreviousWork + artistContinent + ageImputed + AvgGenderConf + horizon, data = Full_PS1), clm(AcqLink ~ 1, data = Full_PS1)) # need to find 44 NAs

# confusion matrix for logit response = EVER vs NEVER

fulllogit <- glm(AcqEver ~ daysExhibited + artistContinent + ageImputed + PreviousWork + horizon, data = Full_PS1, family = "binomial")

logitpred <- predict(fulllogit, type = "response")

logitpred <- ifelse(logitpred >= 0.5, TRUE, FALSE)

table(Pred = logitpred, True = Full_PS1$AcqEver) # 86.24% accurate

# confusion matrix for clm 

fullclm <- clm(AcqLink ~ daysExhibited + PreviousWork + artistContinent + ageImputed + AvgGenderConf + horizon, data = Full_PS1)

clmpred <- predict(fullclm, type = "class")$fit

table(Pred = clmpred, True = Full_PS1$AcqLink)

# model is never predicting Acq6M, and almost never predicts Acq3Y. 74.25% accurate
# not really distinguishing the acquired categories, and is pushing most things into never acquired
# Acq6M is lowest category by far, 1/5 the size of AcqEver, so this makes some sense

# binary model more accurate; ever vs never is easier than six months vs three years vs any point after

save(PS1_Artists, file = "PS1_Artists.Robj")
save(Full_PS1, file = "Full_PS1.Robj")
save(MoMAArtworks, file = "MoMAArtworks.Robj")