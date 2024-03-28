#I've done a bit of tweaking here, so just do a last run-through of the dataframe production steps to make sure the correct countries and covariates are being used for your tests

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

#Only test countries
data1529.novel <- allData %>%
  filter(country %in% excluded) %>%
  filter(!(country %in% c("BWA", "ESW"))) %>%
  filter(age != "Y010_014")

#Data by age group, main corpus
allData1014 <- allData %>%
  filter(age == "Y010_014") %>%
  filter(country == "ZAF")
allData1519 <- allData %>%
  filter(age == "Y015_019") %>%
  filter(!(country %in% excluded)) %>%
  filter(country != "NER")
allData2024 <- allData %>%
  filter(age == "Y020_024") %>%
  filter(!(country %in% excluded)) %>%
  filter(country != "NER")
allData2529 <- allData %>%
  filter(age == "Y025_029") %>%
  filter(!(country %in% excluded)) %>%
  filter(country != "NER")
allData1529 <- allData %>%
  filter(age != "Y010_014") %>%
  filter(!(country %in% excluded)) %>%
  filter(country != "NER")



#Each age group model
model.1014.ZAF <- lm(inc ~ prev + maleART + malePrev + sexualDebut25_49 + sexRatio15_64, data = allData1014)
summary(model.1014.ZAF)

# model.1529 <- lm(inc ~ prev + maleART + malePrev + sexualDebut25_49, data = allData1529)
# summary(model.1529)

#Including age
model.1529.incAge <- lm(inc ~ as.factor(age) + prev + maleART + malePrev + sexualDebut25_49, data = allData1529)
summary(model.1529.incAge)


model.1519 <- lm(inc ~ prev + maleART + malePrev + sexualDebut25_49, data = allData1519)
summary(model.1519)

model.2024 <- lm(inc ~ prev + maleART + malePrev + sexualDebut25_49, data = allData2024)
summary(model.2024)

model.2529 <- lm(inc ~ prev + maleART + malePrev + sexualDebut25_49, data = allData2529)
summary(model.2529)



#Testing 10Perc

allSampling <- allData1529 %>% 
  add_column(sortPos = runif(nrow(.))) %>%
  arrange(desc(sortPos)) %>% 
  filter(age != "Y010_014")

tenPercSample <- allSampling %>%
  slice_max(sortPos, n=314)

ninetyPercSample <- allSampling %>%
  slice_min(sortPos, n=2830)

model.90perc <- lm(inc ~ as.factor(age) + prev + maleART + malePrev + sexualDebut25_49, data = ninetyPercSample)
summary(model.90perc)

tenPercSample$predictedInc <- predict(model.90perc,
                                      tenPercSample)

plot(tenPercSample$inc, tenPercSample$predictedInc)
cor.test(tenPercSample$inc, tenPercSample$predictedIn)



#Testing Angola
data1529.noAngola <- allData1529 %>%
  filter(country != "ANG")
data1529.yesAngola <- allData1529 %>%
  filter(country == "ANG")


model.noAngola <- lm(inc ~ as.factor(age) + prev + maleART + malePrev + sexualDebut25_49, data = data1529.noAngola)
summary(model.noAngola)

data1529.yesAngola$predictedInc <- predict(model.noAngola,
           data1529.yesAngola)


plot(data1529.yesAngola$inc, data1529.yesAngola$predictedInc)
cor.test(data1529.yesAngola$inc, data1529.yesAngola$predictedIn)

data1529.yesAngola$predResids <- data1529.yesAngola$inc - data1529.yesAngola$predictedInc
mean(data1529.yesAngola$predResids)

data1529.yesAngola$incDiff <- ((data1529.yesAngola$inc - data1529.yesAngola$predictedInc)/data1529.yesAngola$inc)*100
mean(data1529.yesAngola$incDiff)

data1529.yesAngola$incAbsPercDiff <- (abs((data1529.yesAngola$inc - data1529.yesAngola$predictedInc)/((data1529.yesAngola$inc + data1529.yesAngola$predictedInc)/2)))*100
mean(data1529.yesAngola$incAbsPercDiff)


ggplot(data1529.yesAngola, aes(x=inc, y=predictedInc)) + 
  geom_point(size=2) +
  theme_ipsum() #+
  #geom_abline()

hist(data1529.yesAngola$predResids)
hist(data1529.yesAngola$incDiff)
hist(data1529.yesAngola$incAbsPercDiff)



#Testing Togo
data1529.noTogo <- allData1529 %>%
  filter(country != "TGO")
data1529.yesTogo <- allData1529 %>%
  filter(country == "TGO")

model.noTogo <- lm(inc ~ as.factor(age) + prev + maleART + malePrev + sexualDebut25_49, data = data1529.noTogo)
summary(model.noTogo)

data1529.yesTogo$predictedInc <- predict(model.noTogo,
                                           data1529.yesTogo)


plot(data1529.yesTogo$inc, data1529.yesTogo$predictedInc)
cor.test(data1529.yesTogo$inc, data1529.yesTogo$predictedIn)

#Testing Niger
data1529.noNiger <- allData1529 %>%
  filter(country != "ANG")
data1529.yesNiger <- allData1529 %>%
  filter(country == "NER")

model.noNiger <- lm(inc ~ as.factor(age) + prev + maleART + malePrev + sexualDebut25_49, data = data1529.noNiger)
summary(model.noNiger)

data1529.yesNiger$predictedInc <- predict(model.noNiger,
                                           data1529.yesNiger)

plot(data1529.yesNiger$inc, data1529.yesNiger$predictedInc)
cor.test(data1529.yesNiger$inc, data1529.yesNiger$predictedIn)



#Testing Burundi
data1529.yesBurundi <- allData1529 %>%
  filter(country == "BDI") %>%
  filter(area != "Bugarama ")

data1529.yesBurundi$predictedInc <- predict(model.1529.incAge,
                                            data1529.yesBurundi)


plot(data1529.yesBurundi$inc, data1529.yesBurundi$predictedInc)
cor.test(data1529.yesBurundi$inc, data1529.yesBurundi$predictedIn)

data1529.yesBurundi$predResids <- data1529.yesBurundi$inc - data1529.yesBurundi$predictedInc
mean(data1529.yesBurundi$predResids)

data1529.yesBurundi$incDiff <- ((data1529.yesBurundi$inc - data1529.yesBurundi$predictedInc)/data1529.yesBurundi$inc)*100
mean(data1529.yesBurundi$incDiff)

data1529.yesBurundi$incAbsPercDiff <- (abs((data1529.yesBurundi$inc - data1529.yesBurundi$predictedInc)/((data1529.yesBurundi$inc + data1529.yesBurundi$predictedInc)/2)))*100
mean(data1529.yesBurundi$incAbsPercDiff)


ggplot(data1529.yesBurundi, aes(x=inc, y=predictedInc)) + 
  geom_point(size=2) +
  theme_ipsum() #+
#geom_abline()

hist(data1529.yesBurundi$predResids)
hist(data1529.yesBurundi$incDiff)
hist(data1529.yesBurundi$incAbsPercDiff)



#Testing Cameroon
data1529.yesCameroon <- data1529.novel %>%
  filter(country == "CMR")

data1529.yesCameroon$predictedInc <- predict(model.1529.incAge,
                                             data1529.yesCameroon)


plot(data1529.yesCameroon$inc, data1529.yesCameroon$predictedInc)
cor.test(data1529.yesCameroon$inc, data1529.yesCameroon$predictedIn)

data1529.yesCameroon$predResids <- data1529.yesCameroon$inc - data1529.yesCameroon$predictedInc
mean(data1529.yesCameroon$predResids)

data1529.yesCameroon$incDiff <- ((data1529.yesCameroon$inc - data1529.yesCameroon$predictedInc)/data1529.yesCameroon$inc)*100
mean(data1529.yesCameroon$incDiff)

data1529.yesCameroon$incAbsPercDiff <- (abs((data1529.yesCameroon$inc - data1529.yesCameroon$predictedInc)/((data1529.yesCameroon$inc + data1529.yesCameroon$predictedInc)/2)))*100
mean(data1529.yesCameroon$incAbsPercDiff)


ggplot(data1529.yesCameroon, aes(x=inc, y=predictedInc)) + 
  geom_point(size=2) +
  theme_ipsum() #+
#geom_abline()

hist(data1529.yesCameroon$predResids)
hist(data1529.yesCameroon$incDiff)
hist(data1529.yesCameroon$incAbsPercDiff)



#Testing Congo
data1529.yesCongo <- data1529.novel %>%
  filter(country == "COG")

data1529.yesCongo$predictedInc <- predict(model.1529.incAge,
                                          data1529.yesCongo)


plot(data1529.yesCongo$inc, data1529.yesCongo$predictedInc)
cor.test(data1529.yesCongo$inc, data1529.yesCongo$predictedInc)

data1529.yesCongo$predResids <- data1529.yesCongo$inc - data1529.yesCongo$predictedInc
mean(data1529.yesCongo$predResids)

data1529.yesCongo$incDiff <- ((data1529.yesCongo$inc - data1529.yesCongo$predictedInc)/data1529.yesCongo$inc)*100
mean(data1529.yesCongo$incDiff)

data1529.yesCongo$incAbsPercDiff <- (abs((data1529.yesCongo$inc - data1529.yesCongo$predictedInc)/((data1529.yesCongo$inc + data1529.yesCongo$predictedInc)/2)))*100
mean(data1529.yesCongo$incAbsPercDiff)


ggplot(data1529.yesCongo, aes(x=inc, y=predictedInc)) + 
  geom_point(size=2) +
  theme_ipsum() #+
#geom_abline()

hist(data1529.yesCongo$predResids)
hist(data1529.yesCongo$incDiff)
hist(data1529.yesCongo$incAbsPercDiff)



#Testing All Novel Countries
data1529.novel$predictedInc <- predict(model.1529.incAge,
                                       data1529.novel)



# Extend the regression lines beyond the domain of the data
ggplot(data1529.novel, aes(x=inc, y=predictedInc, color=country)) + geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines

ggplot(data1529.novel, aes(x=inc, y=predictedInc, color=country)) + geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE)

ggplot(data1529.novel, aes(x=country, y=inc)) + 
  geom_boxplot() +
  scale_colour_hue(l=50)


plot(data1529.novel$inc, data1529.novel$predictedInc)
cor.test(data1529.novel$inc, data1529.novel$predictedIn)

data1529.novel$predResids <- data1529.novel$inc - data1529.novel$predictedInc
mean(data1529.novel$predResids)

data1529.novel$incDiff <- ((data1529.novel$inc - data1529.novel$predictedInc)/data1529.novel$inc)*100
mean(data1529.novel$incDiff)

data1529.novel$incAbsPercDiff <- ((abs(data1529.novel$inc - data1529.novel$predictedInc)/((data1529.novel$inc + data1529.novel$predictedInc)/2)))*100
mean(data1529.novel$incAbsPercDiff)


ggplot(data1529.novel, aes(x=inc, y=predictedInc)) + 
  geom_point(size=2) +
  theme_ipsum() #+
#geom_abline()

hist(data1529.novel$predResids)
hist(data1529.novel$incDiff)
hist(data1529.novel$incAbsPercDiff)
