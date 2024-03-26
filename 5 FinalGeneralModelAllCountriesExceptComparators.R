library(hrbrthemes)
library(tidyverse)

excluded <- c("BWA",
              "NER",
              "COG",
              "CMR",
              "BDI",
              "TGO",
              "ANG",
              "ESW"
              )

allData1519 <- allData %>%
  filter(age == "Y015_019") %>%
  filter(!(country %in% excluded))
allData2024 <- allData %>%
  filter(age == "Y020_024") %>%
  filter(!(country %in% excluded))
allData2529 <- allData %>%
  filter(age == "Y025_029") %>%
  filter(!(country %in% excluded))
allData1529 <- allData %>%
  filter(age != "Y010_014") %>%
  filter(!(country %in% excluded))


model.1529 <- lm(inc ~ as.factor(age) + prev + maleART + malePrev + sexualDebut25_49, data = allData1529)
summary(model.1529)
confint(model.1529)

model.1519 <- lm(inc ~ prev + maleART + malePrev + sexualDebut25_49, data = allData1519)
summary(model.1519)
confint(model.1519)

model.2024 <- lm(inc ~ prev + maleART + malePrev + sexualDebut25_49, data = allData2024)
summary(model.2024)
confint(model.2024)

model.2529 <- lm(inc ~ prev + maleART + malePrev + sexualDebut25_49, data = allData2529)
summary(model.2529)
confint(model.2529)









