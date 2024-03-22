library(sf)
library(tidyverse)
library(colorspace)
library(RColorBrewer)

Africa.sf <- sf::st_read(dsn = "data/geo-data/afr_g2014_2013_0.shp")
hti0.sf <- sf::st_read(dsn = "data/geo-data/geojson/clean/hti.geojson") %>%
  filter(area_level == 0)

duplicates <- c(1517,
                1589,
                1553,
                1625,
                1502,
                1538,
                1574,
                1610,
                1510,
                1546,
                1582,
                1618)

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

Africa <- Africa %>%
  filter(ISO3 != "HTI")

Haiti <- Haiti %>%
  filter(area_id == "HTI")
# 
# NotHTI_NAs <- geom_sf(data = st_as_sf(Africa),
#                       fill = "#928E85",
#                       color = NA,
#                       linewidth = 0.5)
#   



br <- c(-1.00, -.80, -.60, -.40, -.20, .20, .40, .60, .80, 1.00)

pal <- hcl.colors(9, "PiYG", rev = TRUE, alpha = 0.7)

#PREVALENCE
###NON-HAITIAN


NotHTI_NAs <- geom_sf(data = st_as_sf(Africa),
                      fill = "#928E85",
                      color = NA,
                      linewidth = 0.5)


Africa$disp_corr <- cut(Africa$cor_inc_prev,
                        breaks = br,
                        dig.lab = 5)

prevalence_main <- ggplot(data = Africa, 
                          aes(fill = disp_corr)) +
  NotHTI_NAs +
  geom_sf(data = Africa,
          aes(fill = disp_corr),
          color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Africa, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

prevalence_main

ggsave("output/corr_prevalence_main.png", dpi = 300, width = 12, height = 12)



Africa$disp_corr <- cut(Africa$cor_inc_prev1519,
                        breaks = br,
                        dig.lab = 5)

prevalence_main1519 <- ggplot(data = Africa, 
                             aes(fill = disp_corr)) +
  geom_sf(data = Africa,
          aes(fill = disp_corr),
          color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Africa, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

prevalence_main1519

ggsave("output/corr_prevalence_main_1519.png", dpi = 300, width = 12, height = 12)

Africa$disp_corr <- cut(Africa$cor_inc_prev2024,
                        breaks = br,
                        dig.lab = 5)

prevalence_main2024 <- ggplot(data = Africa, 
                              aes(fill = disp_corr)) +
  geom_sf(data = Africa,
          aes(fill = disp_corr),
          color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Africa, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

prevalence_main2024

ggsave("output/corr_prevalence_main_2024.png", dpi = 300, width = 12, height = 12)

Africa$disp_corr <- cut(Africa$cor_inc_prev2529,
                        breaks = br,
                        dig.lab = 5)

prevalence_main2529 <- ggplot(data = Africa, 
                              aes(fill = disp_corr)) +
  geom_sf(data = Africa,
          aes(fill = disp_corr),
          color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Africa, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

prevalence_main2529

ggsave("output/corr_prevalence_main_2529.png", dpi = 300, width = 12, height = 12)


###HAITI
Haiti$disp_corr <- cut(Haiti$cor_inc_prev,
                        breaks = br,
                        dig.lab = 5)

prevalence_HTI <- ggplot(data = Haiti, 
                          aes(fill = disp_corr)) +
  geom_sf(color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Haiti, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

prevalence_HTI

ggsave("output/corr_prevalence_HTI.png", dpi = 300, width = 4, height = 4)

Haiti$disp_corr <- cut(Haiti$cor_inc_prev1519,
                       breaks = br,
                       dig.lab = 5)


prevalence_HTI1519 <- ggplot(data = Haiti, 
                             aes(fill = disp_corr)) +
  geom_sf(color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Haiti, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

prevalence_HTI1519

ggsave("output/corr_prevalence_HTI_1519.png", dpi = 300, width = 4, height = 4)


Haiti$disp_corr <- cut(Haiti$cor_inc_prev2024,
                       breaks = br,
                       dig.lab = 5)


prevalence_HTI2024 <- ggplot(data = Haiti, 
                             aes(fill = disp_corr)) +
  geom_sf(color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Haiti, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

prevalence_HTI2024

ggsave("output/corr_prevalence_HTI_2024.png", dpi = 300, width = 4, height = 4)

Haiti$disp_corr <- cut(Haiti$cor_inc_prev2529,
                       breaks = br,
                       dig.lab = 5)

prevalence_HTI2529 <- ggplot(data = Haiti, 
                             aes(fill = disp_corr)) +
  geom_sf(color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Haiti, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

prevalence_HTI2529

ggsave("output/corr_prevalence_HTI_2529.png", dpi = 300, width = 4, height = 4)




#MALE ART
###NON-HAITIAN
Africa$disp_corr <- cut(Africa$cor_inc_mART,
                        breaks = br,
                        dig.lab = 5)

maleART_main <- ggplot(data = Africa, 
                          aes(fill = disp_corr)) +
  geom_sf(data = Africa,
          aes(fill = disp_corr),
          color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Africa, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

maleART_main

ggsave("output/corr_maleART_main.png", dpi = 300, width = 12, height = 12)



Africa$disp_corr <- cut(Africa$cor_inc_mART1519,
                        breaks = br,
                        dig.lab = 5)

maleART_main1519 <- ggplot(data = Africa, 
                              aes(fill = disp_corr)) +
  geom_sf(data = Africa,
          aes(fill = disp_corr),
          color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Africa, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

maleART_main1519

ggsave("output/corr_maleART_main_1519.png", dpi = 300, width = 12, height = 12)

Africa$disp_corr <- cut(Africa$cor_inc_mART2024,
                        breaks = br,
                        dig.lab = 5)

maleART_main2024 <- ggplot(data = Africa, 
                              aes(fill = disp_corr)) +
  geom_sf(data = Africa,
          aes(fill = disp_corr),
          color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Africa, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

maleART_main2024

ggsave("output/corr_maleART_main_2024.png", dpi = 300, width = 12, height = 12)

Africa$disp_corr <- cut(Africa$cor_inc_mART2529,
                        breaks = br,
                        dig.lab = 5)

maleART_main2529 <- ggplot(data = Africa, 
                              aes(fill = disp_corr)) +
  geom_sf(data = Africa,
          aes(fill = disp_corr),
          color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Africa, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

maleART_main2529

ggsave("output/corr_maleART_main_2529.png", dpi = 300, width = 12, height = 12)


###HAITI
Haiti$disp_corr <- cut(Haiti$cor_inc_mART,
                       breaks = br,
                       dig.lab = 5)

maleART_HTI <- ggplot(data = Haiti, 
                         aes(fill = disp_corr)) +
  geom_sf(color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Haiti, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

maleART_HTI

ggsave("output/corr_maleART_HTI.png", dpi = 300, width = 4, height = 4)

Haiti$disp_corr <- cut(Haiti$cor_inc_mART1519,
                       breaks = br,
                       dig.lab = 5)


maleART_HTI1519 <- ggplot(data = Haiti, 
                             aes(fill = disp_corr)) +
  geom_sf(color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Haiti, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

maleART_HTI1519

ggsave("output/corr_maleART_HTI_1519.png", dpi = 300, width = 4, height = 4)


Haiti$disp_corr <- cut(Haiti$cor_inc_mART2024,
                       breaks = br,
                       dig.lab = 5)


maleART_HTI2024 <- ggplot(data = Haiti, 
                             aes(fill = disp_corr)) +
  geom_sf(color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Haiti, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

maleART_HTI2024

ggsave("output/corr_maleART_HTI_2024.png", dpi = 300, width = 4, height = 4)

Haiti$disp_corr <- cut(Haiti$cor_inc_mART2529,
                       breaks = br,
                       dig.lab = 5)

maleART_HTI2529 <- ggplot(data = Haiti, 
                             aes(fill = disp_corr)) +
  geom_sf(color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Haiti, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

maleART_HTI2529

ggsave("output/corr_maleART_HTI_2529.png", dpi = 300, width = 4, height = 4)






#MALE PREVALENCE
###NON-HAITIAN
Africa$disp_corr <- cut(Africa$cor_inc_mPrev,
                        breaks = br,
                        dig.lab = 5)

malePrev_main <- ggplot(data = Africa, 
                       aes(fill = disp_corr)) +
  geom_sf(data = Africa,
          aes(fill = disp_corr),
          color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Africa, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

malePrev_main

ggsave("output/corr_malePrev_main.png", dpi = 300, width = 12, height = 12)



Africa$disp_corr <- cut(Africa$cor_inc_mPrev1519,
                        breaks = br,
                        dig.lab = 5)

malePrev_main1519 <- ggplot(data = Africa, 
                           aes(fill = disp_corr)) +
  geom_sf(data = Africa,
          aes(fill = disp_corr),
          color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Africa, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

malePrev_main1519

ggsave("output/corr_malePrev_main_1519.png", dpi = 300, width = 12, height = 12)

Africa$disp_corr <- cut(Africa$cor_inc_mPrev2024,
                        breaks = br,
                        dig.lab = 5)

malePrev_main2024 <- ggplot(data = Africa, 
                           aes(fill = disp_corr)) +
  geom_sf(data = Africa,
          aes(fill = disp_corr),
          color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Africa, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

malePrev_main2024

ggsave("output/corr_malePrev_main_2024.png", dpi = 300, width = 12, height = 12)

Africa$disp_corr <- cut(Africa$cor_inc_mPrev2529,
                        breaks = br,
                        dig.lab = 5)

malePrev_main2529 <- ggplot(data = Africa, 
                           aes(fill = disp_corr)) +
  geom_sf(data = Africa,
          aes(fill = disp_corr),
          color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Africa, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

malePrev_main2529

ggsave("output/corr_malePrev_main_2529.png", dpi = 300, width = 12, height = 12)


###HAITI
Haiti$disp_corr <- cut(Haiti$cor_inc_mPrev,
                       breaks = br,
                       dig.lab = 5)

malePrev_HTI <- ggplot(data = Haiti, 
                      aes(fill = disp_corr)) +
  geom_sf(color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Haiti, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

malePrev_HTI

ggsave("output/corr_malePrev_HTI.png", dpi = 300, width = 4, height = 4)

Haiti$disp_corr <- cut(Haiti$cor_inc_mPrev1519,
                       breaks = br,
                       dig.lab = 5)


malePrev_HTI1519 <- ggplot(data = Haiti, 
                          aes(fill = disp_corr)) +
  geom_sf(color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Haiti, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

malePrev_HTI1519

ggsave("output/corr_malePrev_HTI_1519.png", dpi = 300, width = 4, height = 4)


Haiti$disp_corr <- cut(Haiti$cor_inc_mPrev2024,
                       breaks = br,
                       dig.lab = 5)


malePrev_HTI2024 <- ggplot(data = Haiti, 
                          aes(fill = disp_corr)) +
  geom_sf(color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Haiti, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

malePrev_HTI2024

ggsave("output/corr_malePrev_HTI_2024.png", dpi = 300, width = 4, height = 4)

Haiti$disp_corr <- cut(Haiti$cor_inc_mPrev2529,
                       breaks = br,
                       dig.lab = 5)

malePrev_HTI2529 <- ggplot(data = Haiti, 
                          aes(fill = disp_corr)) +
  geom_sf(color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Haiti, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

malePrev_HTI2529

ggsave("output/corr_malePrev_HTI_2529.png", dpi = 300, width = 4, height = 4)








#AGE OF SEXUAL DEBUT
###NON-HAITIAN
Africa$disp_corr <- cut(Africa$cor_inc_sDebut,
                        breaks = br,
                        dig.lab = 5)

sexualDebut_main <- ggplot(data = Africa, 
                        aes(fill = disp_corr)) +
  geom_sf(data = Africa,
          aes(fill = disp_corr),
          color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Africa, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

sexualDebut_main

ggsave("output/corr_sexualDebut_main.png", dpi = 300, width = 12, height = 12)



Africa$disp_corr <- cut(Africa$cor_inc_SDebut1519,
                        breaks = br,
                        dig.lab = 5)

sexualDebut_main1519 <- ggplot(data = Africa, 
                            aes(fill = disp_corr)) +
  geom_sf(data = Africa,
          aes(fill = disp_corr),
          color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Africa, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

sexualDebut_main1519

ggsave("output/corr_sexualDebut_main_1519.png", dpi = 300, width = 12, height = 12)

Africa$disp_corr <- cut(Africa$cor_inc_SDebut2024,
                        breaks = br,
                        dig.lab = 5)

sexualDebut_main2024 <- ggplot(data = Africa, 
                            aes(fill = disp_corr)) +
  geom_sf(data = Africa,
          aes(fill = disp_corr),
          color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Africa, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

sexualDebut_main2024

ggsave("output/corr_sexualDebut_main_2024.png", dpi = 300, width = 12, height = 12)

Africa$disp_corr <- cut(Africa$cor_inc_SDebut2529,
                        breaks = br,
                        dig.lab = 5)

sexualDebut_main2529 <- ggplot(data = Africa, 
                            aes(fill = disp_corr)) +
  geom_sf(data = Africa,
          aes(fill = disp_corr),
          color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Africa, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

sexualDebut_main2529

ggsave("output/corr_sexualDebut_main_2529.png", dpi = 300, width = 12, height = 12)


###HAITI
Haiti$disp_corr <- cut(Haiti$cor_inc_sDebut,
                       breaks = br,
                       dig.lab = 5)

sexualDebut_HTI <- ggplot(data = Haiti, 
                       aes(fill = disp_corr)) +
  geom_sf(color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Haiti, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

sexualDebut_HTI

ggsave("output/corr_sexualDebut_HTI.png", dpi = 300, width = 4, height = 4)

Haiti$disp_corr <- cut(Haiti$cor_inc_SDebut1519,
                       breaks = br,
                       dig.lab = 5)


sexualDebut_HTI1519 <- ggplot(data = Haiti, 
                           aes(fill = disp_corr)) +
  geom_sf(color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Haiti, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

sexualDebut_HTI1519

ggsave("output/corr_sexualDebut_HTI_1519.png", dpi = 300, width = 4, height = 4)


Haiti$disp_corr <- cut(Haiti$cor_inc_SDebut2024,
                       breaks = br,
                       dig.lab = 5)


sexualDebut_HTI2024 <- ggplot(data = Haiti, 
                           aes(fill = disp_corr)) +
  geom_sf(color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Haiti, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

sexualDebut_HTI2024

ggsave("output/corr_sexualDebut_HTI_2024.png", dpi = 300, width = 4, height = 4)

Haiti$disp_corr <- cut(Haiti$cor_inc_SDebut2529,
                       breaks = br,
                       dig.lab = 5)

sexualDebut_HTI2529 <- ggplot(data = Haiti, 
                           aes(fill = disp_corr)) +
  geom_sf(color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Haiti, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

sexualDebut_HTI2529

ggsave("output/corr_sexualDebut_HTI_2529.png", dpi = 300, width = 4, height = 4)


#SEX RATIO
###NON-HAITIAN
Africa$disp_corr <- cut(Africa$cor_inc_sRatio,
                        breaks = br,
                        dig.lab = 5)

sexRatio_main <- ggplot(data = Africa, 
                           aes(fill = disp_corr)) +
  geom_sf(data = Africa,
          aes(fill = disp_corr),
          color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Africa, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

sexRatio_main

ggsave("output/corr_sexRatio_main.png", dpi = 300, width = 12, height = 12)



Africa$disp_corr <- cut(Africa$cor_inc_sRatio1519,
                        breaks = br,
                        dig.lab = 5)

sexRatio_main1519 <- ggplot(data = Africa, 
                               aes(fill = disp_corr)) +
  geom_sf(data = Africa,
          aes(fill = disp_corr),
          color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Africa, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

sexRatio_main1519

ggsave("output/corr_sexRatio_main_1519.png", dpi = 300, width = 12, height = 12)

Africa$disp_corr <- cut(Africa$cor_inc_sRatio2024,
                        breaks = br,
                        dig.lab = 5)

sexRatio_main2024 <- ggplot(data = Africa, 
                               aes(fill = disp_corr)) +
  geom_sf(data = Africa,
          aes(fill = disp_corr),
          color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Africa, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

sexRatio_main2024

ggsave("output/corr_sexRatio_main_2024.png", dpi = 300, width = 12, height = 12)

Africa$disp_corr <- cut(Africa$cor_inc_sRatio2529,
                        breaks = br,
                        dig.lab = 5)

sexRatio_main2529 <- ggplot(data = Africa, 
                               aes(fill = disp_corr)) +
  geom_sf(data = Africa,
          aes(fill = disp_corr),
          color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Africa, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

sexRatio_main2529

ggsave("output/corr_sexRatio_main_2529.png", dpi = 300, width = 12, height = 12)


###HAITI
Haiti$disp_corr <- cut(Haiti$cor_inc_sRatio,
                       breaks = br,
                       dig.lab = 5)

sexRatio_HTI <- ggplot(data = Haiti, 
                          aes(fill = disp_corr)) +
  geom_sf(color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Haiti, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

sexRatio_HTI

ggsave("output/corr_sexRatio_HTI.png", dpi = 300, width = 4, height = 4)

Haiti$disp_corr <- cut(Haiti$cor_inc_sRatio1519,
                       breaks = br,
                       dig.lab = 5)


sexRatio_HTI1519 <- ggplot(data = Haiti, 
                              aes(fill = disp_corr)) +
  geom_sf(color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Haiti, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

sexRatio_HTI1519

ggsave("output/corr_sexRatio_HTI_1519.png", dpi = 300, width = 4, height = 4)


Haiti$disp_corr <- cut(Haiti$cor_inc_sRatio2024,
                       breaks = br,
                       dig.lab = 5)


sexRatio_HTI2024 <- ggplot(data = Haiti, 
                              aes(fill = disp_corr)) +
  geom_sf(color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Haiti, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

sexRatio_HTI2024

ggsave("output/corr_sexRatio_HTI_2024.png", dpi = 300, width = 4, height = 4)

Haiti$disp_corr <- cut(Haiti$cor_inc_sRatio2529,
                       breaks = br,
                       dig.lab = 5)

sexRatio_HTI2529 <- ggplot(data = Haiti, 
                              aes(fill = disp_corr)) +
  geom_sf(color = "white",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    # Legend
                    guides(none)) +
  geom_sf(data = Haiti, fill = NA, color = "black", linewidth = 0.5) +
  theme_void() +
  theme(legend.position = "none")

sexRatio_HTI2529

ggsave("output/corr_sexRatio_HTI_2529.png", dpi = 300, width = 4, height = 4)
