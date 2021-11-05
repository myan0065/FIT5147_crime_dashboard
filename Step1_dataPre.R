library(readxl)
library(tidyverse)
library(sf)
library(polite)
library(rvest)


## ----aus-cleaning-----------------------------------------------------------------------------------------------
# nsw
nsw <- read_excel("data/2. Offenders, states and territories.xls", sheet = 3, range = "A43:Y59")

nsw <- nsw %>%
  select(-(2:13))

names(nsw) <- c("offence", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")
nsw$offence <- gsub("[[:digit:]]", "", nsw$offence) %>%
  str_trim()

nsw$offence <- str_remove(nsw$offence, "\\([a-z]")
nsw$offence <- str_remove(nsw$offence, "\\)")

nsw <- nsw[-9, ]

nsw$"2008" <- as.numeric(nsw$"2008")
nsw$"2009" <- as.numeric(nsw$"2009")
nsw$"2010" <- as.numeric(nsw$"2010")
nsw$"2011" <- as.numeric(nsw$"2011")
nsw$"2012" <- as.numeric(nsw$"2012")

nsw <- nsw %>%
  pivot_longer(cols = "2008":"2019",
               names_to = "year",
               values_to = "rate") %>%
  mutate(state = "NSW")


# vic
vic <- read_excel("data/2. Offenders, states and territories.xls", sheet = 4, range = "A43:Y59")

vic <- vic %>%
  select(-(2:13))

names(vic) <- c("offence", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")
vic$offence <- gsub("[[:digit:]]", "", vic$offence) %>%
  str_trim()

vic$offence <- str_remove(vic$offence, "\\([a-z]")
vic$offence <- str_remove(vic$offence, "\\)")


vic <- vic[-9, ]

vic$"2008" <- as.numeric(vic$"2008")
vic$"2009" <- as.numeric(vic$"2009")
vic$"2010" <- as.numeric(vic$"2010")
vic$"2011" <- as.numeric(vic$"2011")
vic$"2012" <- as.numeric(vic$"2012")
vic$"2013" <- as.numeric(vic$"2013")

vic <- vic %>%
  pivot_longer(cols = "2008":"2019",
               names_to = "year",
               values_to = "rate") %>%
  mutate(state = "VIC")


# qld
qld <- read_excel("data/2. Offenders, states and territories.xls", sheet = 5, range = "A43:Y59")

qld  <- qld  %>%
  select(-(2:13))

names(qld) <- c("offence", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")
qld$offence <- gsub("[[:digit:]]", "", qld$offence) %>%
  str_trim()

qld$offence <- str_remove(qld$offence, "\\([a-z]")
qld$offence <- str_remove(qld$offence, "\\)")


qld <- qld[-9, ]

qld$"2008" <- as.numeric(qld$"2008")
qld$"2009" <- as.numeric(qld$"2009")
qld$"2010" <- as.numeric(qld$"2010")
qld$"2011" <- as.numeric(qld$"2011")
qld$"2012" <- as.numeric(qld$"2012")
qld$"2013" <- as.numeric(qld$"2013")

qld <- qld %>%
  pivot_longer(cols = "2008":"2019",
               names_to = "year",
               values_to = "rate") %>%
  mutate(state = "QLD")

# SA
sa <- read_excel("data/2. Offenders, states and territories.xls", sheet = 6, range = "A43:Y59")

sa <- sa %>%
  select(-(2:13))


names(sa) <- c("offence", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")
sa$offence <- gsub("[[:digit:]]", "", sa$offence) %>%
  str_trim()

sa$offence <- str_remove(sa$offence, "\\([a-z]")
sa$offence <- str_remove(sa$offence, "\\)")


sa<- sa[-9, ]

sa$"2008" <- as.numeric(sa$"2008")
sa$"2009" <- as.numeric(sa$"2009")
sa$"2010" <- as.numeric(sa$"2010")
sa$"2011" <- as.numeric(sa$"2011")
sa$"2012" <- as.numeric(sa$"2012")
sa$"2013" <- as.numeric(sa$"2013")

sa <- sa %>%
  pivot_longer(cols = "2008":"2019",
               names_to = "year",
               values_to = "rate") %>%
  mutate(state = "SA")


# WA
wa <- read_excel("data/2. Offenders, states and territories.xls", sheet = 7, range = "A41:Y56")

wa <- wa %>%
  select(-(2:13))

names(wa) <- c("offence", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")
wa$offence <- gsub("[[:digit:]]", "", wa$offence) %>%
  str_trim()

wa$offence <- str_remove(wa$offence, "\\([a-z]")
wa$offence <- str_remove(wa$offence, "\\)")


wa <- wa %>%
  pivot_longer(cols = "2008":"2019",
               names_to = "year",
               values_to = "rate") %>%
  mutate(state = "WA")


# TAS
tas <- read_excel("data/2. Offenders, states and territories.xls", sheet = 8, range = "A41:Y56")

tas <- tas %>%
  select(-(2:13))

names(tas) <- c("offence", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")
tas$offence <- gsub("[[:digit:]]", "", tas$offence) %>%
  str_trim()

tas$offence <- str_remove(tas$offence, "\\([a-z]")
tas$offence <- str_remove(tas$offence, "\\)")

tas <- tas %>%
  pivot_longer(cols = "2008":"2019",
               names_to = "year",
               values_to = "rate") %>%
  mutate(state = "TAS")



# NT
nt <- read_excel("data/2. Offenders, states and territories.xls", sheet = 9, range = "A41:Y56")

nt <- nt %>%
  select(-(2:13))

names(nt) <- c("offence", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")
nt$offence <- gsub("[[:digit:]]", "", nt$offence) %>%
  str_trim()

nt$offence <- str_remove(nt$offence, "\\([a-z]")
nt$offence <- str_remove(nt$offence, "\\)")

nt <- nt %>%
  pivot_longer(cols = "2008":"2019",
               names_to = "year",
               values_to = "rate") %>%
  mutate(state = "NT")



# ACT
act <- read_excel("data/2. Offenders, states and territories.xls", sheet = 10, range = "A41:Y56")

act <- act %>%
  select(-(2:13))

names(act) <- c("offence", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")
act$offence <- gsub("[[:digit:]]", "", act$offence) %>%
  str_trim()

act$offence <- str_remove(act$offence, "\\([a-z]")
act$offence <- str_remove(act$offence, "\\)")

act <- act %>%
  pivot_longer(cols = "2008":"2019",
               names_to = "year",
               values_to = "rate") %>%
  mutate(state = "ACT")



# Join data
states <- do.call("rbind", list(act, nsw, nt, qld, sa, tas, vic, wa))



## ----age-cleaning-----------------------------------------------------------------------------------------------

# age data person
age <- read_excel("data/1. Offenders, Australia.xls", sheet = 6, range = "A39:Y51")

age <- age %>%
  select(-(2:13))

names(age) <- c("age", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")

age_p <- age %>%
  pivot_longer(cols = "2008":"2019",
               names_to = "year",
               values_to = "rate")


# age data gender
## male
age_m <- read_excel("data/1. Offenders, Australia.xls", sheet = 6, range = "A7:Y19")

age_m <- age_m %>%
  select(-(2:13))

names(age_m) <- c("age", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")

age_m <- age_m %>%
  pivot_longer(cols = "2008":"2019",
               names_to = "year",
               values_to = "rate") %>%
  mutate(sex = "male")


## female
age_f <- read_excel("data/1. Offenders, Australia.xls", sheet = 6, range = "A23:Y35")

age_f <-  age_f %>%
  select(-(2:13))

names(age_f) <- c("age", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")

age_f <- age_f %>%
  pivot_longer(cols = "2008":"2019",
               names_to = "year",
               values_to = "rate") %>%
  mutate(sex = "female")

age_sex <- do.call("rbind", list(age_m, age_f))




## ----vic-cleaning-----------------------------------------------------------------------------------------------

# vic cime LGA
vic_crime <- read_excel("data/Data_Tables_LGA_Recorded_Offences_Year_Ending_March_2021.xlsx", sheet = 3)

vic_crime <- vic_crime %>%
  select("Year",
         "Local Government Area",
         "Offence Division",
         "LGA Rate per 100,000 population") %>%
  rename(lga = "Local Government Area",
         offence = "Offence Division",
         rate = "LGA Rate per 100,000 population")

vic_crime$rate <- round(vic_crime$rate, 1)

vic_crime$offence <- str_remove( vic_crime$offence, "offences")
vic_crime$offence <- substring(vic_crime$offence, 2)
vic_crime$offence <- trimws(vic_crime$offence)

## vic crime map
vic_map <- st_read("data/VIC_LGA_POLYGON_shp/vic_lga.shp")

vic_map <- vic_map %>%
  select("ABB_NAME", "geometry") %>%
  rename(lga = "ABB_NAME")



## ---------------------------------------------------------------------------------------------------------------
aus_map <- st_read("data/1259030001_ste11aaust_shape/STE11aAust.shp")

aus_map1 <- aus_map %>%
  mutate(state = c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT", "OTHER"))

################
library(rmapshaper)
aus_map1_simple<-rmapshaper::ms_simplify(aus_map1, keep = 0.01, keep_shapes = TRUE)
vic_map_simple<-rmapshaper::ms_simplify(vic_map, keep = 0.01, keep_shapes = TRUE)


save(aus_map1_simple,
     vic_map_simple,
     vic_map,
     vic_crime,
     age_sex,
     age_p,
     states,
	 file='dataNeeded.RData')




###### vic crime suburbs
vic_crime1 <- read_excel("data/Data_Tables_LGA_Recorded_Offences_Year_Ending_March_2021.xlsx", sheet = 4)

vic_crime1 <- vic_crime1 %>%
  select("Year",
         "Local Government Area",
         "Suburb/Town Name",
         "Offence Division",
         "Offence Count") %>%
  rename(lga = "Local Government Area",
         suburb = "Suburb/Town Name",
         offence = "Offence Division",
         count = "Offence Count")

vic_crime1$offence <- str_remove( vic_crime1$offence, "offences")
vic_crime1$offence <- substring(vic_crime1$offence, 2)
vic_crime1$offence <- trimws(vic_crime1$offence)

# web scapping
## suburb scores - web scraping

web <- bow("https://www.domain.com.au/liveable-melbourne/melbournes-most-liveable-suburbs-2019/melbournes-307-suburbs-ranked-for-liveability-2019-898676/")
web_data <- scrape(web)

web1 <- web_data %>%
html_nodes("h3 strong") %>%
html_text()

web1 <- tibble(suburb = web1) %>%
  mutate(rank = 1:307)

web1$suburb <- gsub("[[:digit:]]", "", web1$suburb)
web1$suburb <- gsub("\\.", "", web1$suburb)
web1$suburb <- trimws(web1$suburb)

q3_1 <- vic_crime1 %>%
  filter(Year %in% c("2020", "2021")) %>%
  group_by(suburb) %>%
  summarise(total = sum(count))

q3_2 <- q3_1 %>%
  left_join(web1,
            by = "suburb") %>%
  filter(!is.na(rank))

save(vic_crime1,q3_2,file='lmNeeded.RData')




