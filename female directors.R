setwd("C:/Users/danid/Dropbox/datos de mi?rcoles")

library(tidyverse)
library(rjson)
library(Hmisc)
library(stringr)
library(magrittr)
library(stringi)

netflix_titles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')

# I downloaded a names dataset in https://data.world/arunbabu/gender-by-names  
names_df <- read.csv("gender_refine-csv.csv")

names_df <- names_df %>% 
  filter(gender!=3) %>%
  mutate(gender=ifelse(gender==0, "F", "M"))

names_df$score=NULL

## Separating first names of directors and filtering out movies

netflix_titles <- netflix_titles %>% 
  mutate(separate_strings= strsplit(director, "\\, |\\,| "))


netflix_titles$director_first_name <- NA

for (i in 1:nrow(netflix_titles)) {
  netflix_titles$director_first_name[i] <- netflix_titles$separate_strings[[i]] %>% extract2(1)
}


movies <- netflix_titles %>% filter(type=="Movie")

## Matching the movies database with the names database

names_df <- names_df %>% mutate(director_first_name=name) %>% select(-name)

movies <- movies %>% left_join(names_df, by="director_first_name") 

movies <- movies %>% filter(is.na(director)==FALSE)

## Checking missing values

missing <- movies %>% filter(is.na(gender)) %>%
  select(director, director_first_name, country) %>% 
  unique()

countries <- missing %>% group_by(country) %>% count()

## Female directors

female_directors <- movies %>% filter(gender=="F")

total_directors <- female_directors %>% group_by(director) %>%
  count()

write.csv2(female_directors %>% select(-c(separate_strings:gender)), "Female directors_prev.csv", row.names=F)


library(leaflet)
library(countrycode)

dat <- read.csv2("Female directors_prev.csv") %>%
  select(-show_id, -type) %>%
  select(director, title, country, description,
         description, release_year, duration, listed_in, cast) %>%
  mutate(duration=parse_number(duration))

write.csv2(dat, "Female directors.csv", row.names = F)

dat <- read.csv2("Female directors_prev.csv") 

dat <- dat %>% separate(country, sep=",", into = c(
  'country_1',
  'country_2',
  'country_3',
  'country_4',
  'country_5',
  'country_6'))



dat$countrycode <- countryname(dat$country_1, "iso3c")

c <- readr::read_csv("https://gist.githubusercontent.com/tadast/8827699/raw/f5cac3d42d16b78348610fc4ec301e9234f82821/countries_codes_and_coordinates.csv")

colnames(c)[3] <- "countrycode"

c <- c %>% 
  select(countrycode, `Latitude (average)`, `Longitude (average)`)
colnames(c) <- c("countrycode", "lat", "lng")

dat <- dat %>% left_join(c, by="countrycode")

dat <- dat %>% unique()

write.csv2(dat, "Female directors_mapa.csv", row.names=F)

  


