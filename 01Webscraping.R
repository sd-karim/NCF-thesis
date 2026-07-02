library(tidyverse)
library(rvest)

####### MULTIPLE ARTISTS AND ARTISTS LISTS ########

html_MA <- readLines("ps1/PS1MulipleArtistExhibits.txt")

html_MA <- paste(html_MA, collapse = "\n")

html_MA <- strsplit(html_MA, "\n#+\n")[[1]] |> str_sub(start = 4L)

html_MA[1] |> read_html() |> html_elements("title") |> html_text2()

# loop to get all titles into a vector
title = character(length = length(html_MA))

for (i in 1:length(html_MA)) {
  title[i] <- html_MA[i] |> read_html() |>html_elements("title") |> html_text2() |>
    str_sub(end = -8L)
} 


exhibitID = numeric(length = length(html_MA))

for (i in 1:length(html_MA)) {
  exhibitID[i] <- html_MA[i] |> read_html() |> html_element("link") |> html_attr("href") |> str_extract("\\d\\d\\d\\d")
}

start = character(length = length(html_MA))

for (i in 1:length(html_MA)) {
  start[i] <- html_MA[i] |> read_html() |> html_elements("script[type='application/ld+json']") |> html_text() |>
    str_extract("startDate.:\\s.\\d\\d\\d\\d-\\d\\d-\\d\\d") |>
    str_sub(start = 14L)
} 

end = character(length = length(html_MA))

for (i in 1:length(html_MA)) {
  end[i] <- html_MA[i] |> read_html() |> html_elements("script[type='application/ld+json']") |> html_text() |>
    str_extract("endDate.:\\s.\\d\\d\\d\\d-\\d\\d-\\d\\d") |>
    str_sub(start = 12L)
} 

exhibitionDescription = character(length = length(html_MA))

for (i in 1:length(html_MA)) {
  exhibitionDescription[i] <- html_MA[i] |> read_html() |> html_element("meta[property='og:description']") |> html_attr("content") 
}

exhibits_MA <- tibble(
  exhibitID, title, start = ymd(start), end = ymd(end)
)

exhibits_MA <- exhibits_MA |>
  mutate(exhibitionDescription = exhibitionDescription)

# join on artists
artists_MA <- readLines("ps1/PS1ArtistsLists.txt")

artists_MA <- paste(artists_MA, collapse = "\n")

artists_MA <- strsplit(artists_MA, "\n#+\n")[[1]] |> str_sub(start = 4L)

artists_MA[1] |> read_html() |> html_elements("h3") |> html_elements("span")  |> html_text2()

# loop to get into multiple dfs, then bind rows

ArtistLists <- vector("list", length(artists_MA))

for (i in seq_along(artists_MA)) {
  page <- read_html(artists_MA[i])
  
  artists <- page |>                       
    html_elements('li a[href^="/artists/"]') |>  
    html_elements(xpath = "..") 
  
  ArtistLists[[i]] <- tibble(
    exhibitID = page |>
      html_element("artist-filters-form") |> html_attr("param-exhibition-id"),
    
    artistName = artists |>
      html_element("h3") |> html_element("span") |> html_text2(),
    
    artistBio = artists |>
      html_element("p") |> html_element("span.balance-text") |> html_text2()
    
  )
}

# bind into one df
FullArtistLists <- bind_rows(ArtistLists)


####### SINGLE ARTIST EXHIBITS ########


html_SA <- readLines("ps1/PS1SingleArtist.txt", warn = FALSE)

html_SA <- paste(html_SA, collapse = "\n")

html_SA <- strsplit(html_SA, "\n#+\n")[[1]] |> str_sub(start = 4L)


# loop to get all titles into a vector
title = character(length = length(html_SA))

for (i in 1:length(html_SA)) {
  title[i] <- html_SA[i] |> read_html() |>html_elements("title") |> html_text2() |>
    str_sub(end = -8L)
} 


exhibitID = numeric(length = length(html_SA))

for (i in 1:length(html_SA)) {
  exhibitID[i] <- html_SA[i] |> read_html() |> html_element("link") |> html_attr("href") |> str_extract("\\d\\d\\d\\d")
}

start = character(length = length(html_SA))

for (i in 1:length(html_SA)) {
  start[i] <- html_SA[i] |> read_html() |> html_elements("script[type='application/ld+json']") |> html_text() |>
    str_extract("startDate.:\\s.\\d\\d\\d\\d-\\d\\d-\\d\\d") 
  str_sub(start = 14L)
} 

end = character(length = length(html_SA))

for (i in 1:length(html_SA)) {
  end[i] <- html_SA[i] |> read_html() |> html_elements("script[type='application/ld+json']") |> html_text() |>
    str_extract("endDate.:\\s.\\d\\d\\d\\d-\\d\\d-\\d\\d") |>
    str_sub(start = 12L)
} 

artistName = character(length = length(html_SA))

for (i in 1:length(html_SA)) {
  artistName[i] <- html_SA[i] |> read_html() |> html_element(".artist-term--in-list__title__text")  |> html_text2() 
}

artistBio = character(length = length(html_SA))

for (i in 1:length(html_SA)) {
  artistBio[i] <- html_SA[i] |> read_html() |> html_element(".artist-term--in-list__info__text")  |> html_text2() 
}

exhibitionDescription = character(length = length(html_SA))

for (i in 1:length(html_SA)) {
  exhibitionDescription[i] <- html_SA[i] |> read_html() |> html_element("meta[property='og:description']") |> html_attr("content") 
}

SA_exhibits <- tibble(
  exhibitID, artistName, artistBio, title, start = ymd(start), end = ymd(end), exhibitionDescription
)


# join
ps1_MA <- FullArtistLists |>
  full_join(exhibits_MA, by = "exhibitID", relationship = "many-to-many") |>
  filter(!(is.na(artistName) & is.na(title)))

ps1_MA[duplicated(ps1_MA) | duplicated(ps1_MA, fromLast = TRUE), ]

####### combine MA and SA with bind_rows #######

Full_PS1 <- bind_rows(ps1_MA, SA_exhibits)

####### new variables: nationality, year of birth, gender
Full_PS1 <- Full_PS1 |>
  mutate(daysExhibited = (end-start)+1)

Full_PS1 <- Full_PS1 |> 
  mutate(artistNationality = ifelse(str_detect(artistBio, "and") | str_detect(artistBio, "Born"), str_extract(artistBio, "[:alpha:]+(?=,)"),str_extract(artistBio, "[:alpha:]+\\s?\\w*(?=,)"))
  ) |> 
  mutate(artistYOB = as.numeric(str_extract(artistBio, "\\d\\d\\d\\d")))|>
  mutate(ageDuring = year(start) - artistYOB) |>
  filter(!(is.na(artistName) & is.na(artistBio))) |>
  mutate(artistYOD = as.numeric(str_extract(artistBio, "\\d\\d\\d\\d$"))) |>
  mutate(artistYOD = ifelse(artistYOD == artistYOB, NA, artistYOD)) |> # keep YOD only if artist has a YOD, otherwise NA
  mutate(ageDuring = ifelse(artistYOD < year(start), artistYOD - artistYOB, ageDuring)) # if artist is dead at time of exhibition, age = age of death

Full_PS1 <- Full_PS1[!duplicated(Full_PS1), ] # removed duplicate rows

# group in cities that are listed as nationalities:

Full_PS1 <- Full_PS1 |> 
  mutate(artistNationality = case_when(
    artistNationality %in% c("Albuquerque","Baltimore","Chicago","Columbus","Houston",
                             "Jamaican American","Jessica Gispert","Joplin","Kansas City",
                             "Long Branch","Los Angeles","New Orleans","Orleans","Palm Beach",
                             "Philadelphia","Pittsburgh","Queens","San Diego","Scranton",
                             "Spring Hill","USA","Washington","Fairborn","Houma","Nuyorican",
                             "Lancaster", "Vietnamese American") ~ "American",
    
    artistNationality %in% c("Anishinabe","Mohawk","Nations","of Indians","Quinte Mohawk") ~ "Native American",
    
    artistNationality %in% c("American Canadian","Canada","Toronto") ~ "Canadian",
    
    artistNationality == "born Nigerian" ~ "Nigerian",
    artistNationality %in% c("Cuban American","Havana") ~ "Cuban",
    artistNationality == "Germany" ~ "German",
    artistNationality == "Italy" ~ "Italian",
    artistNationality == "Japan" ~ "Japanese",
    artistNationality == "Lithuania" ~ "Lithuanian",
    artistNationality == "Palestinian origin" ~ "Palestinian",
    artistNationality == "Zealander" ~ "New Zealander",
    artistNationality == "Bayamon" ~ "Puerto Rican",
    artistNationality == "African" ~ "South African",
    artistNationality == "Chew" ~ NA,
    
    TRUE ~ artistNationality
  ))

# join with Artists df to get nationality, gender: 

Artists <- read.csv("~/Artists.txt")

Artists_base <- Artists |>
  select(DisplayName, Nationality, Gender)

Full_PS1 <- Full_PS1 |>
  left_join(y = Artists_base, by = c("artistName" = "DisplayName"))

save(Full_PS1, file = "Full_PS1.Robj")

write.csv(Full_PS1, "Full_PS1.csv", row.names = FALSE)