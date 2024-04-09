##Setup Data

allData1519 <- allData %>%
  filter(age == "Y015_019")

##ZIM

model.1529.ZMB <- allData %>%
  filter(age != "Y010_014" & country == "ZMB") %>%
  lm(data = ., inc ~ as.factor(age) + prev + maleART + malePrev + sexualDebut25_49 + sexRatio15_64)
summary(model.1529.ZMB) 
confint(model.1529.ZMB)

model.1519.ZMB <- allData %>%
  filter(age != "Y015_019" & country == "ZMB") %>%
  lm(data = ., inc ~ prev + maleART + malePrev + sexualDebut25_49 + sexRatio15_64)
summary(model.1519.ZMB) 
confint(model.1519.ZMB)

model.2024.ZMB <- allData %>%
  filter(age != "Y020_024" & country == "ZMB") %>%
  lm(data = ., inc ~ prev + maleART + malePrev + sexualDebut25_49 + sexRatio15_64)
summary(model.2024.ZMB) 
confint(model.2024.ZMB)

model.2529.ZMB <- allData %>%
  filter(age != "Y025_029" & country == "ZMB") %>%
  lm(data = ., inc ~ prev + maleART + malePrev + sexualDebut25_49 + sexRatio15_64)
summary(model.2529.ZMB) 
confint(model.2529.ZMB)

##UGA

model.1529.UGA <- allData %>%
  filter(age != "Y010_014" & country == "UGA") %>%
  lm(data = ., inc ~ as.factor(age) + prev + maleART + malePrev + sexualDebut25_49 + sexRatio15_64)
summary(model.1529.UGA) 
confint(model.1529.UGA)

model.1519.UGA <- allData %>%
  filter(age != "Y015_019" & country == "UGA") %>%
  lm(data = ., inc ~ prev + maleART + malePrev + sexualDebut25_49 + sexRatio15_64)
summary(model.1519.UGA) 
confint(model.1519.UGA)

model.2024.UGA <- allData %>%
  filter(age != "Y020_024" & country == "UGA") %>%
  lm(data = ., inc ~ prev + maleART + malePrev + sexualDebut25_49 + sexRatio15_64)
summary(model.2024.UGA) 
confint(model.2024.UGA)

model.2529.UGA <- allData %>%
  filter(age != "Y025_029" & country == "UGA") %>%
  lm(data = ., inc ~ prev + maleART + malePrev + sexualDebut25_49 + sexRatio15_64)
summary(model.2529.UGA) 
confint(model.2529.UGA)




