library(tidyverse)

###INCIDENCE
get_spectrum_data <- function(country, indicator, age, sex) {
  
  url <- paste0("https://naomi2023.azurewebsites.net/api/v1/data?period=2022-4&country=", country, "&areaLevel=0&areaLevel=1&areaLevel=2&areaLevel=3", "&indicator=", indicator, "&ageGroup=", age, "&sex=", sex )
  print(url)
  data_pull <- read.csv(as.character(url)) %>%
    mutate(country = country) %>%
    mutate(indicator = indicator) %>%
    mutate(age = age) %>%
    mutate(sex = sex)
  
}

incData <- data.frame(matrix(ncol = 9, nrow = 0))
cols <- c("level", "area", "mean", "lower", "upper", "country", "indicator", "age", "sex")
colnames(incData) <- cols

countries_included <- c("ANG",
                        "BDI",
                        "BWA",
                        "CIV",
                        "CMR",
                        "COG",
                        "HTI", 
                        "KEN", 
                        "LSO", 
                        "MOZ", 
                        "MWI", 
                        "NAM",
                        "NER",
                        "RWA", 
                        "ESW",
                        "TGO",
                        "TZA", 
                        "UGA", 
                        "ZAF", 
                        "ZMB", 
                        "ZWE") 

indicators <- c("incidence")

ages <- c("Y010_014",
          "Y015_019",
          "Y020_024",
          "Y025_029")

sexes <- c("female")


for (i in countries_included){
  for (j in indicators) {
    for (k in ages) {
      for (l in sexes) {
        new_incData <- get_spectrum_data(i, j, k, l)
        print(new_incData) 
        incData <- rbind(incData, new_incData)
      }
    }
  }
}

incData_SAF <- data.frame(matrix(ncol = 9, nrow = 0))
cols <- c("level", "area", "mean", "lower", "upper", "country", "indicator", "age", "sex")
colnames(incData_SAF) <- cols


countries_included_ZAF <- c("ZAF")


get_spectrum_data_SAF <- function(country, indicator, age, sex) {
  
  url <- paste0("https://naomi2023.azurewebsites.net/api/v1/data?period=2023-3&country=", country, "&areaLevel=0&areaLevel=1&areaLevel=2&areaLevel=3", "&indicator=", indicator, "&ageGroup=", age, "&sex=", sex )
  print(url)
  data_pull <- read.csv(as.character(url)) %>%
    mutate(country = country) %>%
    mutate(indicator = indicator) %>%
    mutate(age = age) %>%
    mutate(sex = sex)
  
}

for (i in countries_included_ZAF){
  for (j in indicators) {
    for (k in ages) {
      for (l in sexes) {
        new_incData_SAF <- get_spectrum_data_SAF(i, j, k, l)
        print(new_incData_SAF) 
        incData_SAF <- rbind(incData_SAF, new_incData_SAF)
      }
    }
  }
}


incData1b <- rbind(incData,
                   incData_SAF)

incData2 <- incData1b %>%
  dplyr::select(-c("lower",
                   "upper",
                   "sex",
                   "indicator")) %>%
  rename(inc = mean)

rm(incData)
rm(incData_SAF)
rm(incData1b)
rm(new_incData)
rm(new_incData_SAF)

###PREVALENCE
get_spectrum_data <- function(country, indicator, age, sex) {
  
  url <- paste0("https://naomi2023.azurewebsites.net/api/v1/data?period=2022-4&country=", country, "&areaLevel=0&areaLevel=1&areaLevel=2&areaLevel=3", "&indicator=", indicator, "&ageGroup=", age, "&sex=", sex )
  print(url)
  data_pull <- read.csv(as.character(url)) %>%
    mutate(country = country) %>%
    mutate(indicator = indicator) %>%
    mutate(age = age) %>%
    mutate(sex = sex)
  
}

prevData <- data.frame(matrix(ncol = 9, nrow = 0))
cols <- c("level", "area", "mean", "lower", "upper", "country", "indicator", "age", "sex")
colnames(prevData) <- cols

countries_included <- c("ANG",
                        "BDI",
                        "BWA", 
                        "CIV",
                        "CMR",
                        "COG",
                        "HTI", 
                        "KEN", 
                        "LSO", 
                        "MOZ", 
                        "MWI", 
                        "NAM",
                        "NER",
                        "RWA", 
                        "ESW",
                        "TGO",
                        "TZA", 
                        "UGA", 
                        "ZAF", 
                        "ZMB", 
                        "ZWE") 

indicators <- c("prevalence")

ages <- c("Y010_014",
          "Y015_019",
          "Y020_024",
          "Y025_029")

sexes <- c("female")


for (i in countries_included){
  for (j in indicators) {
    for (k in ages) {
      for (l in sexes) {
        new_prevData <- get_spectrum_data(i, j, k, l)
        print(new_prevData) 
        prevData <- rbind(prevData, new_prevData)
      }
    }
  }
}

prevData_SAF <- data.frame(matrix(ncol = 9, nrow = 0))
cols <- c("level", "area", "mean", "lower", "upper", "country", "indicator", "age", "sex")
colnames(prevData_SAF) <- cols


countries_included_ZAF <- c("ZAF")


get_spectrum_data_SAF <- function(country, indicator, age, sex) {
  
  url <- paste0("https://naomi2023.azurewebsites.net/api/v1/data?period=2023-3&country=", country, "&areaLevel=0&areaLevel=1&areaLevel=2&areaLevel=3", "&indicator=", indicator, "&ageGroup=", age, "&sex=", sex )
  print(url)
  data_pull <- read.csv(as.character(url)) %>%
    mutate(country = country) %>%
    mutate(indicator = indicator) %>%
    mutate(age = age) %>%
    mutate(sex = sex)
  
}

for (i in countries_included_ZAF){
  for (j in indicators) {
    for (k in ages) {
      for (l in sexes) {
        new_prevData_SAF <- get_spectrum_data_SAF(i, j, k, l)
        print(new_prevData_SAF) 
        prevData_SAF <- rbind(prevData_SAF, new_prevData_SAF)
      }
    }
  }
}


prevData1b <- rbind(prevData,
                    prevData_SAF)

prevData2 <- prevData1b %>%
  dplyr::select(-c("lower",
                   "upper",
                   "sex",
                   "indicator")) %>%
  rename(prev = mean)

rm(prevData)
rm(prevData_SAF)
rm(prevData1b)
rm(new_prevData)
rm(new_prevData_SAF)

###OLDER MALE ART COVERAGE
get_spectrum_data <- function(country, indicator, age, sex) {
  
  url <- paste0("https://naomi2023.azurewebsites.net/api/v1/data?period=2022-4&country=", country, "&areaLevel=0&areaLevel=1&areaLevel=2&areaLevel=3", "&indicator=", indicator, "&ageGroup=", age, "&sex=", sex )
  print(url)
  data_pull <- read.csv(as.character(url)) %>%
    mutate(country = country) %>%
    mutate(indicator = indicator) %>%
    mutate(age = age) %>%
    mutate(sex = sex)
  
}

maleARTData <- data.frame(matrix(ncol = 9, nrow = 0))
cols <- c("level", "area", "mean", "lower", "upper", "country", "indicator", "age", "sex")
colnames(maleARTData) <- cols

countries_included <- c("ANG",
                        "BDI",
                        "BWA", 
                        "CIV",
                        "CMR",
                        "COG",
                        "HTI", 
                        "KEN", 
                        "LSO", 
                        "MOZ", 
                        "MWI", 
                        "NAM",
                        "NER",
                        "RWA", 
                        "ESW",
                        "TGO",
                        "TZA", 
                        "UGA", 
                        "ZAF", 
                        "ZMB", 
                        "ZWE")  

indicators <- c("art_coverage")

ages <- c("Y025_049")

sexes <- c("male")


for (i in countries_included){
  for (j in indicators) {
    for (k in ages) {
      for (l in sexes) {
        new_maleARTData <- get_spectrum_data(i, j, k, l)
        print(new_maleARTData) 
        maleARTData <- rbind(maleARTData, new_maleARTData)
      }
    }
  }
}

maleARTData_SAF <- data.frame(matrix(ncol = 9, nrow = 0))
cols <- c("level", "area", "mean", "lower", "upper", "country", "indicator", "age", "sex")
colnames(maleARTData_SAF) <- cols


countries_included_ZAF <- c("ZAF")


get_spectrum_data_SAF <- function(country, indicator, age, sex) {
  
  url <- paste0("https://naomi2023.azurewebsites.net/api/v1/data?period=2023-3&country=", country, "&areaLevel=0&areaLevel=1&areaLevel=2&areaLevel=3", "&indicator=", indicator, "&ageGroup=", age, "&sex=", sex )
  print(url)
  data_pull <- read.csv(as.character(url)) %>%
    mutate(country = country) %>%
    mutate(indicator = indicator) %>%
    mutate(age = age) %>%
    mutate(sex = sex)
  
}

for (i in countries_included_ZAF){
  for (j in indicators) {
    for (k in ages) {
      for (l in sexes) {
        new_maleARTData_SAF <- get_spectrum_data_SAF(i, j, k, l)
        print(new_maleARTData_SAF) 
        maleARTData_SAF <- rbind(maleARTData_SAF, new_maleARTData_SAF)
      }
    }
  }
}


maleARTData1b <- rbind(maleARTData,
                       maleARTData_SAF)

maleARTData2 <- maleARTData1b %>%
  dplyr::select(-c("lower",
                   "upper",
                   "sex",
                   "indicator")) %>%
  rename(ART = mean)

rm(maleARTData)
rm(maleARTData_SAF)
rm(maleARTData1b)
rm(new_maleARTData)
rm(new_maleARTData_SAF)

###OLDER MALE PREV
get_spectrum_data <- function(country, indicator, age, sex) {
  
  url <- paste0("https://naomi2023.azurewebsites.net/api/v1/data?period=2022-4&country=", country, "&areaLevel=0&areaLevel=1&areaLevel=2&areaLevel=3", "&indicator=", indicator, "&ageGroup=", age, "&sex=", sex )
  print(url)
  data_pull <- read.csv(as.character(url)) %>%
    mutate(country = country) %>%
    mutate(indicator = indicator) %>%
    mutate(age = age) %>%
    mutate(sex = sex)
  
}

malePrevData <- data.frame(matrix(ncol = 9, nrow = 0))
cols <- c("level", "area", "mean", "lower", "upper", "country", "indicator", "age", "sex")
colnames(malePrevData) <- cols

countries_included <- c("ANG",
                        "BDI",
                        "BWA", 
                        "CIV",
                        "CMR",
                        "COG",
                        "HTI", 
                        "KEN", 
                        "LSO", 
                        "MOZ", 
                        "MWI", 
                        "NAM",
                        "NER",
                        "RWA", 
                        "ESW",
                        "TGO",
                        "TZA", 
                        "UGA", 
                        "ZAF", 
                        "ZMB", 
                        "ZWE")  

indicators <- c("prevalence")

ages <- c("Y025_049")

sexes <- c("male")


for (i in countries_included){
  for (j in indicators) {
    for (k in ages) {
      for (l in sexes) {
        new_malePrevData <- get_spectrum_data(i, j, k, l)
        print(new_malePrevData) 
        malePrevData <- rbind(malePrevData, new_malePrevData)
      }
    }
  }
}

malePrevData_SAF <- data.frame(matrix(ncol = 9, nrow = 0))
cols <- c("level", "area", "mean", "lower", "upper", "country", "indicator", "age", "sex")
colnames(malePrevData_SAF) <- cols


countries_included_ZAF <- c("ZAF")


get_spectrum_data_SAF <- function(country, indicator, age, sex) {
  
  url <- paste0("https://naomi2023.azurewebsites.net/api/v1/data?period=2023-3&country=", country, "&areaLevel=0&areaLevel=1&areaLevel=2&areaLevel=3", "&indicator=", indicator, "&ageGroup=", age, "&sex=", sex )
  print(url)
  data_pull <- read.csv(as.character(url)) %>%
    mutate(country = country) %>%
    mutate(indicator = indicator) %>%
    mutate(age = age) %>%
    mutate(sex = sex)
  
}

for (i in countries_included_ZAF){
  for (j in indicators) {
    for (k in ages) {
      for (l in sexes) {
        new_malePrevData_SAF <- get_spectrum_data_SAF(i, j, k, l)
        print(new_malePrevData_SAF) 
        malePrevData_SAF <- rbind(malePrevData_SAF, new_malePrevData_SAF)
      }
    }
  }
}


malePrevData1b <- rbind(malePrevData,
                        malePrevData_SAF)

malePrevData2 <- malePrevData1b %>%
  dplyr::select(-c("lower",
                   "upper",
                   "age",
                   "sex",
                   "indicator")) %>%
  rename(malePrev = mean)

rm(malePrevData)
rm(malePrevData_SAF)
rm(malePrevData1b)
rm(new_malePrevData)
rm(new_malePrevData_SAF)

###SEX RATIO
get_spectrum_data <- function(country, indicator, age, sex) {
  
  url <- paste0("https://naomi2023.azurewebsites.net/api/v1/data?period=2022-4&country=", country, "&areaLevel=0&areaLevel=1&areaLevel=2&areaLevel=3", "&indicator=", indicator, "&ageGroup=", age, "&sex=", sex )
  print(url)
  data_pull <- read.csv(as.character(url)) %>%
    mutate(country = country) %>%
    mutate(indicator = indicator) %>%
    mutate(age = age) %>%
    mutate(sex = sex)
  
}

get_spectrum_data_ZAF <- function(country, indicator, age, sex) {
  
  url <- paste0("https://naomi2023.azurewebsites.net/api/v1/data?period=2023-3&country=", country, "&areaLevel=0&areaLevel=1&areaLevel=2&areaLevel=3", "&indicator=", indicator, "&ageGroup=", age, "&sex=", sex )
  print(url)
  data_pull <- read.csv(as.character(url)) %>%
    mutate(country = country) %>%
    mutate(indicator = indicator) %>%
    mutate(age = age) %>%
    mutate(sex = sex)
  
}


data <- data.frame(matrix(ncol = 9, nrow = 0))
cols <- c("level", "area", "mean", "lower", "upper", "country", "indicator", "age", "sex")
colnames(data) <- cols


countries_included <- c("MOZ")


countries_included <- c("ANG",
                        "BDI",
                        "BWA", 
                        "CIV",
                        "CMR",
                        "COG",
                        "HTI", 
                        "KEN", 
                        "LSO", 
                        "MOZ", 
                        "MWI", 
                        "NAM",
                        "NER",
                        "RWA", 
                        "ESW",
                        "TGO",
                        "TZA", 
                        "UGA", 
                        "ZAF", 
                        "ZMB", 
                        "ZWE") 

indicators <- c("population")

ages <- c("Y015_049", 
          "Y015_064")

sexes <- c("female",
           "male")


for (i in countries_included){
  for (j in indicators) {
    for (k in ages) {
      for (l in sexes) {
        new_data <- get_spectrum_data(i, j, k, l)
        print(new_data) 
        data <- rbind(data, new_data)
      }
    }
  }
}

countries_included_ZAF <- c("ZAF")

data_ZAF <- data.frame(matrix(ncol = 9, nrow = 0))
colnames(data_ZAF) <- cols

for (i in countries_included_ZAF){
  for (j in indicators) {
    for (k in ages) {
      for (l in sexes) {
        new_data_ZAF <- get_spectrum_data_ZAF(i, j, k, l)
        print(new_data_ZAF) 
        data_ZAF <- rbind(data_ZAF, new_data_ZAF)
      }
    }
  }
}

data1b <- rbind(data,
                data_ZAF)

# data1b %>%
#   group_by(country,
#            age,
#            sex) %>%
#   summarize(count=n(),
#             sum=sum(mean)) %>% 
#   filter(sum == 0) %>%
#   View()


data2 <- data1b %>%
  dplyr::select(-c("lower",
                   "upper"))

data3 <- data2 %>%
  dplyr::select(-c("indicator")) %>%
  pivot_wider(
    names_from = sex,
    values_from = mean
  ) 

data3$sexRatio <- data3$male / data3$female

data4 <- data3 %>%
  dplyr::select(-c("male",
                   "female")) %>%
  pivot_wider(
    names_from = age,
    values_from = sexRatio
  )


data_sexRatio <- data4

rm(data)
rm(data_ZAF)
rm(data1b)
rm(data2)
rm(data3)
rm(data4)
rm(data5)
rm(new_data)
rm(new_data_ZAF)

data_sexRatio1 <-  data_sexRatio
data_sexRatio1$age <- "Y010_014"
data_sexRatio2 <-  data_sexRatio
data_sexRatio2$age <- "Y015_019"
data_sexRatio3 <-  data_sexRatio
data_sexRatio3$age <- "Y020_024"
data_sexRatio4 <-  data_sexRatio
data_sexRatio4$age <- "Y025_029"

data_sexRatioAll <- dplyr::bind_rows(data_sexRatio1,
                                     data_sexRatio2,
                                     data_sexRatio3,
                                     data_sexRatio4)
rm(data_sexRatio)
rm(data_sexRatio1)
rm(data_sexRatio2)
rm(data_sexRatio3)
rm(data_sexRatio4)

###########

data_inc_prev <- left_join(incData2,
                                 prevData2,
                                 by = c("level",
                                        "area",
                                        "country",
                                        "age"))



data_inc_prev_sexratio <- left_join(data_inc_prev,
                                    data_sexRatioAll,
                                    by = c("level",
                                           "area",
                                           "country",
                                           "age"))

rm(data_inc_prev)
rm(data_sexRatioAll)

maleARTData21 <-  maleARTData2
maleARTData21$age <- "Y010_014"
maleARTData22 <-  maleARTData2
maleARTData22$age <- "Y015_019"
maleARTData23 <-  maleARTData2
maleARTData23$age <- "Y020_024"
maleARTData24 <-  maleARTData2
maleARTData24$age <- "Y025_029"

data_maleARTAll <- dplyr::bind_rows(maleARTData21,
                                    maleARTData22,
                                    maleARTData23,
                                    maleARTData24)

rm(maleARTData2)
rm(maleARTData21)
rm(maleARTData22)
rm(maleARTData23)
rm(maleARTData24)


malePrevData21 <-  malePrevData2
malePrevData21$age <- "Y010_014"
malePrevData22 <-  malePrevData2
malePrevData22$age <- "Y015_019"
malePrevData23 <-  malePrevData2
malePrevData23$age <- "Y020_024"
malePrevData24 <-  malePrevData2
malePrevData24$age <- "Y025_029"

data_malePrevAll <- dplyr::bind_rows(malePrevData21,
                                     malePrevData22,
                                     malePrevData23,
                                     malePrevData24)

rm(malePrevData2)
rm(malePrevData21)
rm(malePrevData22)
rm(malePrevData23)
rm(malePrevData24)




data_inc_prev_sexratio_maleprev <- left_join(data_inc_prev_sexratio,
                                             data_malePrevAll,
                                             by = c("level",
                                                    "area",
                                                    "country",
                                                    "age"))

data_all <- left_join(data_inc_prev_sexratio_maleprev,
                      data_maleARTAll,
                      by = c("level",
                             "area",
                             "country",
                             "age")) %>%
  rename(maleART = ART)




rm(data_malePrevAll)
rm(data_maleARTAll)
rm(data_inc_prev_sexratio_maleprev)
rm(data_inc_prev_sexratio)

dataFinal <- data_all %>%
  dplyr::filter(level != 0)

rm(data_all)


dataFinal_goodEncoding <- dataFinal %>%
  dplyr::filter(!is.na(maleART))

rm(dataFinal_goodEncoding)
###########


dataFinal$DROP <- "N"

data <- dataFinal %>%
  mutate(
    DROP = case_when(
      (country == "ANG" & level != 1) ~ "Y",
      (country == "BDI" & level != 2) ~ "Y",
      (country == "BWA" & level != 3) ~ "Y",
      (country == "CIV" & level != 2) ~ "Y",
      (country == "CMR" & level != 3) ~ "Y",
      (country == "COG" & level != 2) ~ "Y",
      (country == "HTI" & level != 2) ~ "Y",
      (country == "KEN" & level != 2) ~ "Y",
      (country == "LSO" & level != 1) ~ "Y",
      (country == "MOZ" & level != 3) ~ "Y",
      (country == "MWI" & level != 5) ~ "Y",
      (country == "NAM" & level != 2) ~ "Y",
      (country == "NER" & level != 1) ~ "Y",
      (country == "RWA" & level != 2) ~ "Y",
      (country == "ESW" & level != 1) ~ "Y",
      (country == "TGO" & level != 2) ~ "Y",
      (country == "TZA" & level != 4) ~ "Y",
      (country == "UGA" & level != 3) ~ "Y",
      (country == "ZAF" & level != 2) ~ "Y",
      (country == "ZMB" & level != 2) ~ "Y",
      (country == "ZWE" & level != 2) ~ "Y",
      TRUE ~ as.character("N")
    )
  ) %>%
  filter(DROP != "Y") %>%
  dplyr::select(-DROP)

data <- data %>%
  rename(sexRatio15_49 = Y015_049,
         sexRatio15_64 = Y015_064)


data_DHS <-read.csv("data/DHSData.csv")
data_DHS_Togo <-read.csv("data/Togo.csv") %>%
  dplyr::select(c("country",
                  "Characteristic",
                  "area",
                  "Median.age.at.first.sexual.intercourse..Women...20.49",
                  "Median.age.at.first.sexual.intercourse..Women...25.49",
                  "level",
                  "age")) %>%
  rename(DHS.area = Characteristic,
         Age.at.First.Sexual.Experience..Women.age.20.49 = Median.age.at.first.sexual.intercourse..Women...20.49,
         Age.at.First.Sexual.Experience..Women.age.25.49= Median.age.at.first.sexual.intercourse..Women...25.49)

data_DHS_Niger <-read.csv("data/Niger.csv") %>%
  dplyr::select(c("Country",
                  "Characteristic",
                  "area",
                  "Median.age.at.first.sexual.intercourse..Women...20.49",
                  "Median.age.at.first.sexual.intercourse..Women...25.49",
                  "level",
                  "age")) %>%
  rename(DHS.area = Characteristic,
         country = Country,
         Age.at.First.Sexual.Experience..Women.age.20.49 = Median.age.at.first.sexual.intercourse..Women...20.49,
         Age.at.First.Sexual.Experience..Women.age.25.49= Median.age.at.first.sexual.intercourse..Women...25.49)

data_DHS_Angola <-read.csv("data/Angola.csv") %>%
  dplyr::select(c("Country",
                  "Characteristic",
                  "area",
                  "Median.age.at.first.sexual.intercourse..Women...20.49",
                  "Median.age.at.first.sexual.intercourse..Women...25.49",
                  "level",
                  "age")) %>%
  rename(DHS.area = Characteristic,
         country = Country,
         Age.at.First.Sexual.Experience..Women.age.20.49 = Median.age.at.first.sexual.intercourse..Women...20.49,
         Age.at.First.Sexual.Experience..Women.age.25.49 = Median.age.at.first.sexual.intercourse..Women...25.49)

data_DHS_Burundi <-read.csv("data/Burundi.csv") %>%
  dplyr::select(c("Country",
                  "Characteristic",
                  "area",
                  "Median.age.at.first.sexual.intercourse..Women...20.49",
                  "Median.age.at.first.sexual.intercourse..Women...25.49",
                  "level",
                  "age")) %>%
  rename(DHS.area = Characteristic,
         country = Country,
         Age.at.First.Sexual.Experience..Women.age.20.49 = Median.age.at.first.sexual.intercourse..Women...20.49,
         Age.at.First.Sexual.Experience..Women.age.25.49 = Median.age.at.first.sexual.intercourse..Women...25.49)

data_DHS_Cameroon <-read.csv("data/Cameroon.csv") %>%
  dplyr::select(c("Characteristic",
                  "area",
                  "Median.age.at.first.sexual.intercourse..Women...20.49",
                  "Median.age.at.first.sexual.intercourse..Women...25.49",
                  "level",
                  "age")) %>%
  rename(DHS.area = Characteristic,
         Age.at.First.Sexual.Experience..Women.age.20.49 = Median.age.at.first.sexual.intercourse..Women...20.49,
         Age.at.First.Sexual.Experience..Women.age.25.49 = Median.age.at.first.sexual.intercourse..Women...25.49)


data_DHS_Congo <-read.csv("data/Congo.csv") %>%
  dplyr::select(c("Characteristic",
                  "area",
                  "Median.age.at.first.sexual.intercourse..Women...20.49",
                  "Median.age.at.first.sexual.intercourse..Women...25.49",
                  "level",
                  "age")) %>%
  rename(DHS.area = Characteristic,
         Age.at.First.Sexual.Experience..Women.age.20.49 = Median.age.at.first.sexual.intercourse..Women...20.49,
         Age.at.First.Sexual.Experience..Women.age.25.49 = Median.age.at.first.sexual.intercourse..Women...25.49)

data_DHS_Togo$country <- "TGO"
data_DHS_Niger$country <- "NER"
data_DHS_Angola$country <- "ANG"
data_DHS_Burundi$country <- "BDI"
data_DHS_Cameroon$country <- "CMR"
data_DHS_Congo$country <- "COG"

data_DHS_combined <- rbind(data_DHS,
                           data_DHS_Togo)

data_DHS_combined <- rbind(data_DHS_combined,
                           data_DHS_Niger)

data_DHS_combined <- rbind(data_DHS_combined,
                           data_DHS_Angola)

data_DHS_combined <- rbind(data_DHS_combined,
                           data_DHS_Burundi)

data_DHS_combined <- rbind(data_DHS_combined,
                           data_DHS_Cameroon)

data_DHS_combined <- rbind(data_DHS_combined,
                           data_DHS_Congo)

data_DHS_combined$DROP <- "N"

malawi3 <- c("Blantyre",
             "Lilongwe",
             "Zomba")


data_DHS2 <- data_DHS_combined %>%
  mutate(
    level = case_when(
      (country == "RWA" & area == "Western Province") ~ 1,
      (country == "LSO" & DHS.area == "Total") ~ 0,
      (country == "ZWE" & area == "Mashonaland Central") ~ 1,
      (country == "MWI" & area %in% malawi3) ~ 5,
      TRUE ~  as.numeric(level)
    )
  ) %>%
  mutate(
    DROP = case_when(
      (country == "ANG" & level != 1) ~ "Y",
      (country == "BDI" & level != 2) ~ "Y",
      (country == "BWA" & level != 3) ~ "Y",
      (country == "CIV" & level != 2) ~ "Y",
      (country == "CMR" & level != 3) ~ "Y",
      (country == "COG" & level != 2) ~ "Y",
      (country == "HTI" & level != 2) ~ "Y",
      (country == "KEN" & level != 2) ~ "Y",
      (country == "LSO" & level != 1) ~ "Y",
      (country == "MOZ" & level != 3) ~ "Y",
      (country == "MWI" & level != 5) ~ "Y",
      (country == "NAM" & level != 2) ~ "Y",
      (country == "NER" & level != 1) ~ "Y",
      (country == "RWA" & level != 2) ~ "Y",
      (country == "ESW" & level != 1) ~ "Y",
      (country == "TGO" & level != 2) ~ "Y",
      (country == "TZA" & level != 4) ~ "Y",
      (country == "UGA" & level != 3) ~ "Y",
      (country == "ZAF" & level != 2) ~ "Y",
      (country == "ZMB" & level != 2) ~ "Y",
      (country == "ZWE" & level != 2) ~ "Y",
      TRUE ~ as.character("N")
    )
  ) %>%
  filter(DROP != "Y") %>%
  dplyr::select(-DROP) %>%
  mutate(
    age = case_when(
      age == "Y010_Y014" ~ "Y010_014",
      age == "Y015_Y019" ~ "Y015_019",
      age == "Y020_Y024" ~ "Y020_024",
      age == "Y025_Y029" ~ "Y025_029",
      age == "Y010_014" ~ "Y010_014",
      age == "Y015_019" ~ "Y015_019",
      age == "Y020_024" ~ "Y020_024",
      age == "Y025_029" ~ "Y025_029",
      TRUE ~ as.character("CHECK")
    )
  ) %>%
  mutate(
    area = case_when(
      area == "Blantyre city" ~ "Blantyre City",
      area == "Butha-Buthe" ~ "Butha-buthe",
      area == "Lilongwe city" ~ "Lilongwe City",
      area == "Port-Au-Prince" ~ "Port-au-prince",
      area == "San-pédro" ~ "San-pedro",
      area == "Thaba-Tseka" ~ "Thaba-tseka",
      area == "Zomba city" ~ "Zomba City",
      area == "Mzimba City" ~ "Mzuzu City",
      area == "Bouénza" ~ "Bouenza",
      area == "Lékoumou" ~ "Lekoumou",
      TRUE ~ as.character(area)
    )
  )


## Blantyre level 5 from data, values equiv to Blantyre city


########

joined <- dplyr::full_join(data,
                           data_DHS2,
                           dplyr::join_by(area == area,
                                          age == age,
                                          country == country),
                           keep = TRUE)


joined2NAsX <- joined %>%
  filter(!(country.x %in% c("BWA",
                            "ESW"))) %>%
  filter(!(is.na(area.x) & area.y=="")) %>%
  filter(is.na(area.x)) %>%
  filter(!(area.y == "Niger" & DHS.area == "Total"))

joined2NAsY <- joined %>%
  filter(!(country.x %in% c("BWA",
                            "ESW"))) %>%
  filter(!(is.na(area.x) & area.y=="")) %>%
  filter(is.na(area.y)) %>%
  filter(!(area.y == "Niger" & DHS.area == "Total"))

joined2 <- joined %>% 
  filter(!(is.na(area.x) & area.y=="")) %>%
  rename(sexualDebut20_49 = Age.at.First.Sexual.Experience..Women.age.20.49,
         sexualDebut25_49 = Age.at.First.Sexual.Experience..Women.age.25.49,
         area = area.x,
         age = age.x,
         level = level.x,
         country = country.x) %>%
  dplyr::select(-c(area.y,
                   age.y,
                   level.y,
                   DHS.area,
                   country.y)) %>%
  filter(!is.na(country)) %>%
  filter(area != "Nhamatanda")


data <- joined2
data$inc <- data$inc*1000
data$prev <- data$prev*100
data$malePrev <- data$malePrev*100
data$maleART <- data$maleART*100

write.csv(data, "data/CountryModelsData.csv")
