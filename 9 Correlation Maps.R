library(sf)
library(tidyverse)
library(colorspace)
library(RColorBrewer)

Africa.sf <- sf::st_read(dsn = "data/geo-data/afr_g2014_2013_0.shp")
hti0.sf <- sf::st_read(dsn = "data/geo-data/geojson/clean/hti.geojson") %>%
  filter(area_level == 0)

Africa.sf.excluded <- c("AGO",
                       "BDI",
                       "BWA",
                       "CIV",
                       "CMR",
                       "COG",
                       "KEN",
                       "LSO",
                       "MOZ",
                       "MWI",
                       "NAM",
                       "NER",
                       "RWA",
                       "SWZ",
                       "TGO",
                       "TZA",
                       "UGA",
                       "ZAF",
                       "ZMB",
                       "ZWE")

Africa.sf.excluded.sexualDebut <- c("AGO",
                        "BDI",
                        "CIV",
                        "CMR",
                        "COG",
                        "KEN",
                        "LSO",
                        "MOZ",
                        "MWI",
                        "NAM",
                        "NER",
                        "RWA",
                        "TGO",
                        "TZA",
                        "UGA",
                        "ZAF",
                        "ZMB",
                        "ZWE")

Africa.sf.exclude <- Africa.sf %>%
  filter(!(ISO3 %in% Africa.sf.excluded))

Africa.sf.exclude.sexualDebut <- Africa.sf %>%
  filter(!(ISO3 %in% Africa.sf.excluded.sexualDebut))

duplicates <- c(2718,
                2726,
                2733,
                2754,
                2762,
                2769,
                2790,
                2798,
                2805,
                2826,
                2834,
                2841)

allData <- read.csv("data/CountryModelsData.csv") %>%
  filter(!(X %in% duplicates & area %in% c("Blantyre", "Zomba", "Lilongwe"))) %>%
  filter(age != "Y010_014")

allData1519 <- allData %>%
  filter(age == "Y015_019")
allData2024 <- allData %>%
  filter(age == "Y020_024")
allData2529 <- allData %>%
  filter(age == "Y025_029")

corrMapData <- allData %>%
  group_by(country) %>%
  summarize(cor_inc_prev=cor(inc, prev),
            cor_inc_mPrev=cor(inc, malePrev),
            cor_inc_mART=cor(inc, maleART),
            cor_inc_sRatio=cor(inc, sexRatio15_64),
            cor_inc_sDebut=cor(inc, sexualDebut25_49),
            n = n()) %>% 
  mutate(
    country = case_when(
      country == "ANG" ~ "AGO",
      country == "CDI" ~ "CIV",
      country == "ESW" ~ "SWZ",
      TRUE ~ as.character(country)
    )
  )

corrMapData1519 <- allData1519 %>%
  group_by(country) %>%
  summarize(cor_inc_prev1519=cor(inc, prev),
            cor_inc_mPrev1519=cor(inc, malePrev),
            cor_inc_mART1519=cor(inc, maleART),
            cor_inc_sRatio1519=cor(inc, sexRatio15_64),
            cor_inc_SDebut1519=cor(inc, sexualDebut25_49),
            n1519 = n()) %>% 
  mutate(
    country = case_when(
      country == "ANG" ~ "AGO",
      country == "CDI" ~ "CIV",
      country == "ESW" ~ "SWZ",
      TRUE ~ as.character(country)
    )
  )

corrMapData2024 <- allData2024 %>%
  group_by(country) %>%
  summarize(cor_inc_prev2024=cor(inc, prev),
            cor_inc_mPrev2024=cor(inc, malePrev),
            cor_inc_mART2024=cor(inc, maleART),
            cor_inc_sRatio2024=cor(inc, sexRatio15_64),
            cor_inc_SDebut2024=cor(inc, sexualDebut25_49),
            n2024 = n()) %>% 
  mutate(
    country = case_when(
      country == "ANG" ~ "AGO",
      country == "CDI" ~ "CIV",
      country == "ESW" ~ "SWZ",
      TRUE ~ as.character(country)
    )
  )

corrMapData2529 <- allData2529 %>%
  group_by(country) %>%
  summarize(cor_inc_prev2529=cor(inc, prev),
            cor_inc_mPrev2529=cor(inc, malePrev),
            cor_inc_mART2529=cor(inc, maleART),
            cor_inc_sRatio2529=cor(inc, sexRatio15_64),
            cor_inc_SDebut2529=cor(inc, sexualDebut25_49),
            n2529 = n()) %>% 
  mutate(
    country = case_when(
      country == "ANG" ~ "AGO",
      country == "CDI" ~ "CIV",
      country == "ESW" ~ "SWZ",
      TRUE ~ as.character(country)
    )
  )

rm(allData)
rm(allData1519)
rm(allData2024)
rm(allData2529)

Africa <- full_join(Africa.sf,
                    corrMapData,
                    dplyr::join_by(ISO3 == country))

Africa <- full_join(Africa,
                    corrMapData1519,
                    dplyr::join_by(ISO3 == country))

Africa <- full_join(Africa,
                    corrMapData2024,
                    dplyr::join_by(ISO3 == country))

Africa <- full_join(Africa,
                    corrMapData2529,
                    dplyr::join_by(ISO3 == country))

                          
Haiti <- full_join(hti0.sf,
                    corrMapData,
                    dplyr::join_by(area_id == country))

Haiti <- full_join(Haiti,
                   corrMapData1519,
                   dplyr::join_by(area_id == country))

Haiti <- full_join(Haiti,
                   corrMapData2024,
                   dplyr::join_by(area_id == country))

Haiti <- full_join(Haiti,
                   corrMapData2529,
                   dplyr::join_by(area_id == country))

# Africa <- Africa %>%
#   filter(ISO3 != "HTI")

Haiti <- Haiti %>%
  filter(area_id == "HTI")



annoyingPaletteJustWorkCorrelation <- function(x, y, z) {
  if (y == "Africa") {
    location <- Africa.sf
  } else if (y == "Haiti") {
    location <- hti0.sf
  }

  G9 <- x %>%
    filter(cor_disp < -0.81)
  G8 <- x %>%
    filter(cor_disp > -0.81 & cor_disp < -0.61)
  G7 <- x %>%
    filter(cor_disp > -0.61 & cor_disp < -0.41)
  G6 <- x %>%
    filter(cor_disp > -0.41 & cor_disp < -0.21)
  G5 <- x %>%
    filter(cor_disp > -0.21 & cor_disp < 0.19)
  G4 <- x %>%
    filter(cor_disp > 0.19 & cor_disp < 0.39)
  G3 <- x %>%
    filter(cor_disp > 0.39 & cor_disp < 0.59)
  G2 <- x %>%
    filter(cor_disp > 0.59 & cor_disp < 0.79)
  G1 <- x %>%
    filter(cor_disp > 0.79)
  
  if(z == "Africa") {
  map <- ggplot() +
    geom_sf(data = st_as_sf(Africa.sf.exclude),
            fill = "#d3d3d3",
            color = "black",
            linewidth = 0.5) +
    geom_sf(data = G1,
            fill = "#B24D8E",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G2,
            fill = "#DE7EB3",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G3,
            fill = "#EFB6D2",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G4,
            fill = "#FAE1EA",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G5,
            fill = "#FBFCFC",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G6,
            fill = "#DCEDCD",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G7,
            fill = "#B4D999",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G8,
            fill = "#8CB855",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G9,
            fill = "#6E8D4C",
            color = "#7f7f7f",
            linewidth = 0.1) +
    theme_void() +
    geom_sf(data = st_as_sf(location), 
            fill = NA, 
            color = "black", 
            linewidth = 0.5) +
    theme(legend.position = "none")
  } else if (z == "Haiti") {
    map <- ggplot() +
      geom_sf(data = G1,
              fill = "#B24D8E",
              color = "#7f7f7f",
              linewidth = 0.1) +
      geom_sf(data = G2,
              fill = "#DE7EB3",
              color = "#7f7f7f",
              linewidth = 0.1) +
      geom_sf(data = G3,
              fill = "#EFB6D2",
              color = "#7f7f7f",
              linewidth = 0.1) +
      geom_sf(data = G4,
              fill = "#FAE1EA",
              color = "#7f7f7f",
              linewidth = 0.1) +
      geom_sf(data = G5,
              fill = "#FBFCFC",
              color = "#7f7f7f",
              linewidth = 0.1) +
      geom_sf(data = G6,
              fill = "#DCEDCD",
              color = "#7f7f7f",
              linewidth = 0.1) +
      geom_sf(data = G7,
              fill = "#B4D999",
              color = "#7f7f7f",
              linewidth = 0.1) +
      geom_sf(data = G8,
              fill = "#8CB855",
              color = "#7f7f7f",
              linewidth = 0.1) +
      geom_sf(data = G9,
              fill = "#6E8D4C",
              color = "#7f7f7f",
              linewidth = 0.1) +
      theme_void() +
      geom_sf(data = st_as_sf(location), 
              fill = NA, 
              color = "black", 
              linewidth = 0.5) +
      theme(legend.position = "none")
    
  } 
  return(map)
}




#PREVALENCE
###NON-HAITIAN
Africa$cor_disp <- Africa$cor_inc_prev

prevalence_main <- annoyingPaletteJustWorkCorrelation(Africa,
                                                 "Africa",
                                                 "Africa")

prevalence_main

ggsave("output/corr_prevalence_main.png", dpi = 300, width = 12, height = 12)


Africa$cor_disp <- Africa$cor_inc_prev1519

prevalence_main1519 <- annoyingPaletteJustWorkCorrelation(Africa,
                                                          "Africa",                                                  
                                                          "Africa")

prevalence_main1519

ggsave("output/corr_prevalence_main_1519.png", dpi = 300, width = 12, height = 12)

Africa$cor_disp <- Africa$cor_inc_prev2024

prevalence_main2024 <- annoyingPaletteJustWorkCorrelation(Africa,
                                                          "Africa",                                                  
                                                          "Africa")

prevalence_main2024

ggsave("output/corr_prevalence_main_2024.png", dpi = 300, width = 12, height = 12)


Africa$cor_disp <- Africa$cor_inc_prev2529

prevalence_main2529 <- annoyingPaletteJustWorkCorrelation(Africa,
                                                          "Africa",
                                                          "Africa")

prevalence_main2529

ggsave("output/corr_prevalence_main_2529.png", dpi = 300, width = 12, height = 12)

###HAITI
Haiti$cor_disp <- Haiti$cor_inc_prev

prevalence_HTI <- annoyingPaletteJustWorkCorrelation(Haiti,
                                                     "Haiti",
                                                     "Haiti")

prevalence_HTI

ggsave("output/corr_prevalence_HTI.png", dpi = 300, width = 4, height = 4)

Haiti$cor_disp <- Haiti$cor_inc_prev1519

prevalence_HTI1519 <- annoyingPaletteJustWorkCorrelation(Haiti,
                                                         "Haiti",
                                                         "Haiti")

prevalence_HTI1519

ggsave("output/corr_prevalence_HTI1519.png", dpi = 300, width = 4, height = 4)

Haiti$cor_disp <- Haiti$cor_inc_prev2024

prevalence_HTI2024 <- annoyingPaletteJustWorkCorrelation(Haiti,
                                                         "Haiti",
                                                         "Haiti")

prevalence_HTI2024

ggsave("output/corr_prevalence_HTI2024.png", dpi = 300, width = 4, height = 4)

Haiti$cor_disp <- Haiti$cor_inc_prev2529

prevalence_HTI2529 <- annoyingPaletteJustWorkCorrelation(Haiti,
                                                         "Haiti",
                                                         "Haiti")

prevalence_HTI2529

ggsave("output/corr_prevalence_HTI2529.png", dpi = 300, width = 4, height = 4)




#MALE ART
###NON-HAITIAN
Africa$cor_disp <- Africa$cor_inc_mART

maleART_main <- annoyingPaletteJustWorkCorrelation(Africa,
                                                   "Africa",
                                                   "Africa")

maleART_main

ggsave("output/corr_maleART_main.png", dpi = 300, width = 12, height = 12)


Africa$cor_disp <- Africa$cor_inc_mART1519

maleART_main1519 <- annoyingPaletteJustWorkCorrelation(Africa,
                                                       "Africa",
                                                       "Africa")

maleART_main1519

ggsave("output/corr_maleART_main_1519.png", dpi = 300, width = 12, height = 12)

Africa$cor_disp <- Africa$cor_inc_mART2024

maleART_main2024 <- annoyingPaletteJustWorkCorrelation(Africa,
                                                       "Africa",
                                                       "Africa")

maleART_main2024

ggsave("output/corr_maleART_main_2024.png", dpi = 300, width = 12, height = 12)


Africa$cor_disp <- Africa$cor_inc_mART2529

maleART_main2529 <- annoyingPaletteJustWorkCorrelation(Africa,
                                                       "Africa",
                                                       "Africa")

maleART_main2529

ggsave("output/corr_maleART_main_2529.png", dpi = 300, width = 12, height = 12)

###HAITI
Haiti$cor_disp <- Haiti$cor_inc_mART

maleART_HTI <- annoyingPaletteJustWorkCorrelation(Haiti,
                                                  "Haiti",
                                                  "Haiti")

maleART_HTI

ggsave("output/corr_maleART_HTI.png", dpi = 300, width = 4, height = 4)

Haiti$cor_disp <- Haiti$cor_inc_mART1519

maleART_HTI1519 <- annoyingPaletteJustWorkCorrelation(Haiti,
                                                      "Haiti",
                                                      "Haiti")

maleART_HTI1519

ggsave("output/corr_maleART_HTI1519.png", dpi = 300, width = 4, height = 4)

Haiti$cor_disp <- Haiti$cor_inc_mART2024

maleART_HTI2024 <- annoyingPaletteJustWorkCorrelation(Haiti,
                                                      "Haiti",
                                                      "Haiti")

maleART_HTI2024

ggsave("output/corr_maleART_HTI2024.png", dpi = 300, width = 4, height = 4)

Haiti$cor_disp <- Haiti$cor_inc_mART2529

maleART_HTI2529 <- annoyingPaletteJustWorkCorrelation(Haiti,
                                                      "Haiti",
                                                      "Haiti")

maleART_HTI2529

ggsave("output/corr_maleART_HTI2529.png", dpi = 300, width = 4, height = 4)



#MALE PREVALENCE
###NON-HAITIAN
Africa$cor_disp <- Africa$cor_inc_mPrev

malePrev_main <- annoyingPaletteJustWorkCorrelation(Africa,
                                                    "Africa",
                                                    "Africa")

malePrev_main

ggsave("output/corr_malePrev_main.png", dpi = 300, width = 12, height = 12)

Africa$cor_disp <- Africa$cor_inc_mPrev1519

malePrev_main1519 <- annoyingPaletteJustWorkCorrelation(Africa,
                                                    "Africa",
                                                    "Africa")

malePrev_main1519

ggsave("output/corr_malePrev_main_1519.png", dpi = 300, width = 12, height = 12)

Africa$cor_disp <- Africa$cor_inc_mPrev2024

malePrev_main2024 <- annoyingPaletteJustWorkCorrelation(Africa,
                                                    "Africa",
                                                    "Africa")

malePrev_main2024

ggsave("output/corr_malePrev_main_2024.png", dpi = 300, width = 12, height = 12)

Africa$cor_disp <- Africa$cor_inc_mPrev2529

malePrev_main2529 <- annoyingPaletteJustWorkCorrelation(Africa,
                                                    "Africa",
                                                    "Africa")

malePrev_main2529

ggsave("output/corr_malePrev_main_2529.png", dpi = 300, width = 12, height = 12)


###HAITI
Haiti$cor_disp <- Haiti$cor_inc_mPrev

malePrev_HTI <- annoyingPaletteJustWorkCorrelation(Haiti,
                                                   "Haiti",
                                                   "Haiti")

malePrev_HTI

ggsave("output/corr_malePrev_HTI.png", dpi = 300, width = 4, height = 4)

Haiti$cor_disp <- Haiti$cor_inc_mPrev1519

malePrev_HTI1519 <- annoyingPaletteJustWorkCorrelation(Haiti,
                                                        "Haiti",
                                                        "Haiti")
malePrev_HTI1519

ggsave("output/corr_malePrev_HTI_1519.png", dpi = 300, width = 4, height = 4)

Haiti$cor_disp <- Haiti$cor_inc_mPrev2024

malePrev_HTI2024 <- annoyingPaletteJustWorkCorrelation(Haiti,
                                                       "Haiti",
                                                       "Haiti")

malePrev_HTI2024

ggsave("output/corr_malePrev_HTI_2024.png", dpi = 300, width = 4, height = 4)

Haiti$cor_disp <- Haiti$cor_inc_mPrev2529

malePrev_HTI2529 <- annoyingPaletteJustWorkCorrelation(Haiti,
                                                       "Haiti",
                                                       "Haiti")

malePrev_HTI2529

ggsave("output/corr_malePrev_HTI_2529.png", dpi = 300, width = 4, height = 4)




#AGE OF SEXUAL DEBUT
###NON-HAITIAN
Africa.sD <- Africa %>%
  filter(ISO3 != "BWA" & ISO3 != "ESW")

Africa.sD$cor_disp <- Africa.sD$cor_inc_sDebut

sexualDebut_main <- annoyingPaletteJustWorkCorrelation(Africa.sD,
                                                       "Africa",
                                                       "Africa")

sexualDebut_main

ggsave("output/corr_sexualDebut_main.png", dpi = 300, width = 12, height = 12)


Africa.sD$cor_disp <- Africa.sD$cor_inc_SDebut1519

sexualDebut_main1519 <- annoyingPaletteJustWorkCorrelation(Africa.sD,
                                                           "Africa",
                                                           "Africa")

sexualDebut_main1519

ggsave("output/corr_sexualDebut_main_1519.png", dpi = 300, width = 12, height = 12)

Africa.sD$cor_disp <- Africa.sD$cor_inc_SDebut2024

sexualDebut_main2024 <- annoyingPaletteJustWorkCorrelation(Africa.sD,
                                                           "Africa",
                                                           "Africa")

sexualDebut_main2024

ggsave("output/corr_sexualDebut_main_2024.png", dpi = 300, width = 12, height = 12)

Africa.sD$cor_disp <- Africa.sD$cor_inc_SDebut2529

sexualDebut_main2529 <- annoyingPaletteJustWorkCorrelation(Africa.sD,
                                                           "Africa",
                                                           "Africa")

sexualDebut_main2529

ggsave("output/corr_sexualDebut_main_2529.png", dpi = 300, width = 12, height = 12)


###HAITI
Haiti$cor_disp <- Haiti$cor_inc_sDebut

sexualDebut_HTI <- annoyingPaletteJustWorkCorrelation(Haiti,
                                                      "Haiti",
                                                      "Haiti")

sexualDebut_HTI

ggsave("output/corr_sexualDebut_HTI.png", dpi = 300, width = 4, height = 4)

Haiti$cor_disp <- Haiti$cor_inc_SDebut1519


sexualDebut_HTI1519 <- annoyingPaletteJustWorkCorrelation(Haiti,
                                                          "Haiti",
                                                          "Haiti")

sexualDebut_HTI1519

ggsave("output/corr_sexualDebut_HTI_1519.png", dpi = 300, width = 4, height = 4)


Haiti$cor_disp <- Haiti$cor_inc_SDebut2024


sexualDebut_HTI2024 <- annoyingPaletteJustWorkCorrelation(Haiti,
                                                          "Haiti",
                                                          "Haiti")

sexualDebut_HTI2024

ggsave("output/corr_sexualDebut_HTI_2024.png", dpi = 300, width = 4, height = 4)

Haiti$cor_disp <- Haiti$cor_inc_SDebut2529

sexualDebut_HTI2529 <- annoyingPaletteJustWorkCorrelation(Haiti,
                                                          "Haiti",
                                                          "Haiti")

sexualDebut_HTI2529

ggsave("output/corr_sexualDebut_HTI_2529.png", dpi = 300, width = 4, height = 4)


#SEX RATIO
###NON-HAITIAN
Africa$cor_disp <- Africa$cor_inc_sRatio

sexRatio_main <- annoyingPaletteJustWorkCorrelation(Africa,
                                                    "Africa",
                                                    "Africa")

sexRatio_main

ggsave("output/corr_sexRatio_main.png", dpi = 300, width = 12, height = 12)



Africa$cor_disp <- Africa$cor_inc_sRatio1519

sexRatio_main1519 <- annoyingPaletteJustWorkCorrelation(Africa,
                                                        "Africa",
                                                        "Africa")

sexRatio_main1519

ggsave("output/corr_sexRatio_main_1519.png", dpi = 300, width = 12, height = 12)

Africa$cor_disp <- Africa$cor_inc_sRatio2024

sexRatio_main2024 <- annoyingPaletteJustWorkCorrelation(Africa,
                                                        "Africa",
                                                        "Africa")

sexRatio_main2024

ggsave("output/corr_sexRatio_main_2024.png", dpi = 300, width = 12, height = 12)

Africa$cor_disp <- Africa$cor_inc_sRatio2529

sexRatio_main2529 <- annoyingPaletteJustWorkCorrelation(Africa,
                                                        "Africa",
                                                        "Africa")

sexRatio_main2529

ggsave("output/corr_sexRatio_main_2529.png", dpi = 300, width = 12, height = 12)


###HAITI
Haiti$cor_disp <- Haiti$cor_inc_sRatio

sexRatio_HTI <- annoyingPaletteJustWorkCorrelation(Haiti,
                                                    "Haiti",
                                                    "Haiti")

sexRatio_HTI

ggsave("output/corr_sexRatio_HTI.png", dpi = 300, width = 4, height = 4)

Haiti$cor_disp <- Haiti$cor_inc_sRatio1519


sexRatio_HTI1519 <- annoyingPaletteJustWorkCorrelation(Haiti,
                                                       "Haiti",
                                                       "Haiti")

sexRatio_HTI1519

ggsave("output/corr_sexRatio_HTI_1519.png", dpi = 300, width = 4, height = 4)


Haiti$cor_disp <- Haiti$cor_inc_sRatio2024


sexRatio_HTI2024 <- annoyingPaletteJustWorkCorrelation(Haiti,
                                                       "Haiti",
                                                       "Haiti")

sexRatio_HTI2024

ggsave("output/corr_sexRatio_HTI_2024.png", dpi = 300, width = 4, height = 4)

Haiti$cor_disp <- Haiti$cor_inc_SDebut2529

sexRatio_HTI2529 <- annoyingPaletteJustWorkCorrelation(Haiti,
                                                       "Haiti",
                                                       "Haiti")

sexRatio_HTI2529

ggsave("output/corr_sexRatio_HTI_2529.png", dpi = 300, width = 4, height = 4)
