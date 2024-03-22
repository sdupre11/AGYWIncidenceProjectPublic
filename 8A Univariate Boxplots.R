#This script assumes that the data processes in Univariate Maps.R have been run already.

library(ggbeeswarm)


allData1 <- allData %>%
  filter(inc < 0.5)
allData2 <- allData %>%
  filter(inc > 0.5 & inc < 1)
allData3 <- allData %>%
  filter(inc > 1 & inc < 2)
allData4 <- allData %>%
  filter(inc > 2 & inc < 5)
allData5 <- allData %>%
  filter(inc > 5 & inc < 10)
allData6 <- allData %>%
  filter(inc > 10 & inc < 15)
allData7 <- allData %>%
  filter(inc > 15)

ggplot() +
  geom_point(data = allData1,
             aes(x = age,
             y = inc),
             position = "jitter",
             color = "#FEE5D9") +
  geom_point(data = allData2,
             aes(x = age,
                 y = inc),
             position = "jitter",
             color = "#FABAA1") +
  geom_point(data = allData3,
             aes(x = age,
                 y = inc),
             position = "jitter",
             color = "#F69274") +
  geom_point(data = allData4,
             aes(x = age,
                 y = inc),
             position = "jitter",
             color = "#F26A4D") +
  geom_point(data = allData5,
             aes(x = age,
                 y = inc),
             position = "jitter",
             color = "#EF3D2C") +
  geom_point(data = allData6,
             aes(x = age,
                 y = inc),
             position = "jitter",
             color = "#CA2027") +
  geom_point(data = allData7,
             aes(x = age,
                 y = inc),
             position = "jitter",
             color = "#9A1B1F") +
  geom_boxplot(
    data = allData,
    aes(x = age,
        y = inc),
    color = "black",
    fill = NA) + 
  coord_cartesian(ylim = c(0, 25)) +
  theme_minimal()

ggsave("output/incidence_boxplot.png", dpi = 300, width = 6, height = 6)


allData1 <- allData %>%
  filter(prev < 2)
allData2 <- allData %>%
  filter(prev > 2 & prev < 4)
allData3 <- allData %>%
  filter(prev > 4 & prev < 6)
allData4 <- allData %>%
  filter(prev > 6 & prev < 8)
allData5 <- allData %>%
  filter(prev > 8 & prev < 10)
allData6 <- allData %>%
  filter(prev > 10 & prev < 20)
allData7 <- allData %>%
  filter(prev > 20)

ggplot() +
  geom_point(data = allData1,
             aes(x = age,
                 y = prev),
             position = "jitter",
             color = "#F0F3FA") +
  geom_point(data = allData2,
             aes(x = age,
                 y = prev),
             position = "jitter",
             color = "#C7DBEE") +
  geom_point(data = allData3,
             aes(x = age,
                 y = prev),
             position = "jitter",
             color = "#9ECAE1") +
  geom_point(data = allData4,
             aes(x = age,
                 y = prev),
             position = "jitter",
             color = "#6AADD6") +
  geom_point(data = allData5,
             aes(x = age,
                 y = prev),
             position = "jitter",
             color = "#4392C6") +
  geom_point(data = allData6,
             aes(x = age,
                 y = prev),
             position = "jitter",
             color = "#2372B5") +
  geom_point(data = allData7,
             aes(x = age,
                 y = prev),
             position = "jitter",
             color = "#194791") +
  geom_boxplot(
    data = allData,
    aes(x = age,
        y = prev),
    color = "black",
    fill = NA) +
  coord_cartesian(ylim = c(0, 45)) +
  theme_minimal()

ggsave("output/prevalence_boxplot.png", dpi = 300, width = 6, height = 6)



allData1 <- allData %>%
  filter(maleART < 40)
allData2 <- allData %>%
  filter(maleART > 40 & maleART < 50)
allData3 <- allData %>%
  filter(maleART > 50 & maleART < 60)
allData4 <- allData %>%
  filter(maleART > 60 & maleART < 70)
allData5 <- allData %>%
  filter(maleART > 70 & maleART < 80)
allData6 <- allData %>%
  filter(maleART > 80 & maleART < 90)
allData7 <- allData %>%
  filter(maleART > 90)

ggplot() +
  geom_point(data = allData1,
             aes(x = age,
                 y = maleART),
             position = "jitter",
             color = "#EEF6E9") +
  geom_point(data = allData2,
             aes(x = age,
                 y = maleART),
             position = "jitter",
             color = "#C8E4BF") +
  geom_point(data = allData3,
             aes(x = age,
                 y = maleART),
             position = "jitter",
             color = "#A2D39A") +
  geom_point(data = allData4,
             aes(x = age,
                 y = maleART),
             position = "jitter",
             color = "#76C375") +
  geom_point(data = allData5,
             aes(x = age,
                 y = maleART),
             position = "jitter",
             color = "#41AB5D") +
  geom_point(data = allData6,
             aes(x = age,
                 y = maleART),
             position = "jitter",
             color = "#258B45") +
  geom_point(data = allData7,
             aes(x = age,
                 y = maleART),
             position = "jitter",
             color = "#005A32") +
  geom_boxplot(
    data = allData,
    aes(x = age,
        y = maleART),
    color = "black",
    fill = NA) + 
  coord_cartesian(ylim = c(0, 100)) +
  theme_minimal()

ggsave("output/maleART_boxplot.png", dpi = 300, width = 6, height = 6)

allData1 <- allData %>%
  filter(malePrev < 2)
allData2 <- allData %>%
  filter(malePrev > 2 & malePrev < 4)
allData3 <- allData %>%
  filter(malePrev > 4 & malePrev < 6)
allData4 <- allData %>%
  filter(malePrev > 6 & malePrev < 8)
allData5 <- allData %>%
  filter(malePrev > 8 & malePrev < 10)
allData6 <- allData %>%
  filter(malePrev > 10 & malePrev < 20)
allData7 <- allData %>%
  filter(malePrev > 20)

ggplot() +
  geom_point(data = allData1,
             aes(x = age,
                 y = malePrev),
             position = "jitter",
             color = "#F0F3FA") +
  geom_point(data = allData2,
             aes(x = age,
                 y = malePrev),
             position = "jitter",
             color = "#C7DBEE") +
  geom_point(data = allData3,
             aes(x = age,
                 y = malePrev),
             position = "jitter",
             color = "#9ECAE1") +
  geom_point(data = allData4,
             aes(x = age,
                 y = malePrev),
             position = "jitter",
             color = "#6AADD6") +
  geom_point(data = allData5,
             aes(x = age,
                 y = malePrev),
             position = "jitter",
             color = "#4392C6") +
  geom_point(data = allData6,
             aes(x = age,
                 y = malePrev),
             position = "jitter",
             color = "#2372B5") +
  geom_point(data = allData7,
             aes(x = age,
                 y = malePrev),
             position = "jitter",
             color = "#194791") +
  geom_boxplot(
    data = allData,
    aes(x = age,
        y = malePrev),
    color = "black",
    fill = NA) +
  coord_cartesian(ylim = c(0, 40)) +
  theme_minimal()


ggsave("output/maleprevalence_boxplot.png", dpi = 300, width = 6, height = 6)


allData1 <- allData %>%
  filter(sexualDebut25_49 < 15)
allData2 <- allData %>%
  filter(sexualDebut25_49 > 15 & sexualDebut25_49 < 16)
allData3 <- allData %>%
  filter(sexualDebut25_49 > 16 & sexualDebut25_49 < 17)
allData4 <- allData %>%
  filter(sexualDebut25_49 > 17 & sexualDebut25_49 < 18)
allData5 <- allData %>%
  filter(sexualDebut25_49 > 18 & sexualDebut25_49 < 19)
allData6 <- allData %>%
  filter(sexualDebut25_49 > 19 & sexualDebut25_49 < 20)
allData7 <- allData %>%
  filter(sexualDebut25_49 > 20)

ggplot() +
  geom_point(data = allData1,
             aes(x = age,
                 y = sexualDebut25_49),
             position = "jitter",
             color = "#FFEDDF") +
  geom_point(data = allData2,
             aes(x = age,
                 y = sexualDebut25_49),
             position = "jitter",
             color = "#FED1A2") +
  geom_point(data = allData3,
             aes(x = age,
                 y = sexualDebut25_49),
             position = "jitter",
             color = "#FAAD6C") +
  geom_point(data = allData4,
             aes(x = age,
                 y = sexualDebut25_49),
             position = "jitter",
             color = "#F68C40") +
  geom_point(data = allData5,
             aes(x = age,
                 y = sexualDebut25_49),
             position = "jitter",
             color = "#F06A22") +
  geom_point(data = allData6,
             aes(x = age,
                 y = sexualDebut25_49),
             position = "jitter",
             color = "#D84C27") +
  geom_point(data = allData7,
             aes(x = age,
                 y = sexualDebut25_49),
             position = "jitter",
             color = "#8B301D") +
  geom_boxplot(
    data = allData,
    aes(x = age,
        y = sexualDebut25_49),
    color = "black",
    fill = NA) + 
  coord_cartesian(ylim = c(0, 25)) +
  theme_minimal()





ggsave("output/sexualdebut_boxplot.png", dpi = 300, width = 6, height = 6)


allData1 <- allData %>%
  filter(sexRatio15_64 < 0.8)
allData2 <- allData %>%
  filter(sexRatio15_64 > 0.8 & sexRatio15_64 < 0.9)
allData3 <- allData %>%
  filter(sexRatio15_64 > 0.9 & sexRatio15_64 < 0.95)
allData4 <- allData %>%
  filter(sexRatio15_64 > 0.95 & sexRatio15_64 < 1.05)
allData5 <- allData %>%
  filter(sexRatio15_64 > 1.05 & sexRatio15_64 < 1.1)
allData6 <- allData %>%
  filter(sexRatio15_64 > 1.1 & sexRatio15_64 < 1.2)
allData7 <- allData %>%
  filter(sexRatio15_64 > 1.2)

ggplot() +
  geom_point(data = allData1,
             aes(x = age,
                 y = sexRatio15_64),
             position = "jitter",
             color = "#D73127") +
  geom_point(data = allData2,
             aes(x = age,
                 y = sexRatio15_64),
             position = "jitter",
             color = "#F68D5B") +
  geom_point(data = allData3,
             aes(x = age,
                 y = sexRatio15_64),
             position = "jitter",
             color = "#FDDF90") +
  geom_point(data = allData4,
             aes(x = age,
                 y = sexRatio15_64),
             position = "jitter",
             color = "#FDF8C1") +
  geom_point(data = allData5,
             aes(x = age,
                 y = sexRatio15_64),
             position = "jitter",
             color = "#E0F3F8") +
  geom_point(data = allData6,
             aes(x = age,
                 y = sexRatio15_64),
             position = "jitter",
             color = "#92BFDA") +
  geom_point(data = allData7,
             aes(x = age,
                 y = sexRatio15_64),
             position = "jitter",
             color = "#4575B5") +
  geom_boxplot(
    data = allData,
    aes(x = age,
        y = sexRatio15_64),
    color = "black",
    fill = NA) + 
  coord_cartesian(ylim = c(0.6, 1.6)) +
  theme_minimal()





ggsave("output/sexratio_boxplot.png", dpi = 300, width = 6, height = 6)





#############################
base_gg_main <- geom_sf(data = st_as_sf(Africa.sf), 
        fill = NA, 
        color = "black", 
        linewidth = 0.5)


ggplot(data = sf.noHTI2024, 
                   aes(fill = cut(sexRatio15_64,
                                  breaks = c(0,0.8,0.9,0.95,1.05,1.1,1.2,100),
                                  dig.lab = 5))) +
  geom_sf(color = "#7f7f7f",
          linewidth = 0.1) +
  scale_fill_brewer(type = "qual",
                    palette = "RdYlBu",
                    na.value = NA,
                    name = "ART",
                    direction=1
  ) +
  theme_void() +
  base_gg_main



