library(tmap)

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


all.sf2.1519.resids <- full_join(all.sf.resids, 
                          allData1519,
                          dplyr::join_by(area == area,
                                         level == level)) %>%
  filter(area != "Nhamatanda")

all.sf2.2024.resids <- full_join(all.sf.resids, 
                          allData2024,
                          dplyr::join_by(area == area,
                                         level == level)) %>%
  filter(area != "Nhamatanda")

all.sf2.2529.resids <- full_join(all.sf.resids, 
                          allData2529,
                          dplyr::join_by(area == area,
                                         level == level)) %>%
  filter(area != "Nhamatanda")

all.sf2.1529.resids <- full_join(all.sf.resids, 
                          allData1529,
                          dplyr::join_by(area == area,
                                         level == level)) %>%
  filter(area != "Nhamatanda")


#15 to 19s
model1519 <- lm(inc ~ prev + maleART + malePrev + sexualDebut25_49, data = allData1519)
summary(model1519)

plot(density(resid(model1519)))
qqnorm(resid(model1519)) # A quantile normal plot - good for checking normality
qqline(resid(model1519))

resids1519 <- residuals(model1519)

map.resids <- cbind(all.sf2.1519.resids, resids1519) 



#20 to 24s
model2024 <- lm(inc ~ prev + maleART + malePrev + sexualDebut25_49, data = allData2024)
summary(model2024)

plot(density(resid(model2024)))
qqnorm(resid(model2024)) # A quantile normal plot - good for checking normality
qqline(resid(model2024))

resids2024 <- residuals(model2024)

map.resids <- cbind(map.resids, resids2024) 



#25 to 29s
model2529 <- lm(inc ~ prev + maleART + malePrev + sexualDebut25_49, data = allData2529)
summary(model2529)

plot(density(resid(model2529)))
qqnorm(resid(model2529)) # A quantile normal plot - good for checking normality
qqline(resid(model2529))

resids2529 <- residuals(model2529)

map.resids <- cbind(map.resids, resids2529) 


#15 to 29s
model1529 <- lm(inc ~ as.factor(age) + prev + maleART + malePrev + sexualDebut25_49, data = allData1529)
summary(model1529)

plot(density(resid(model1529)))
qqnorm(resid(model1529)) # A quantile normal plot - good for checking normality
qqline(resid(model1529))

resids1529 <- residuals(model1529) %>% as.data.frame()
nums <- rep(1:1045, each = 3)

resids1529.2 <- cbind(resids1529, nums) 

resids1529.3 <- resids1529.2 %>%
  group_by(nums) %>%
  summarize(resids1529mean = mean(.)) %>%
  select(-nums) %>%
  rename(resids1529 = resids1529mean)


map.resids <- cbind(map.resids, resids1529.3) 


map.resids.valid <- map.resids %>%
  st_make_valid() 


map.resids.valid.HTI <- map.resids.valid %>%
  filter(country == "HTI")

map.resids.valid.NotHTI <- map.resids.valid %>%
  filter(country != "HTI")

qtm(map.resids.valid.HTI, 
    fill = "resids1519",
    title = "Haiti Residuals - Aged 15 to 19")

qtm(map.resids.valid.NotHTI, 
    fill = "resids1519",
    title = "Residuals - Aged 15 to 19")

qtm(map.resids.valid.HTI, 
    fill = "resids2024",
    title = "Haiti Residuals - Aged 20 to 24")

qtm(map.resids.valid.NotHTI, 
    fill = "resids2024",
    title = "Residuals - Aged 20 to 24")

qtm(map.resids.valid.HTI, 
    fill = "resids2529",
    title = "Haiti Residuals - Aged 25 to 29")

qtm(map.resids.valid.NotHTI, 
    fill = "resids2529",
    title = "Residuals - Aged 25 to 29")

saveRDS(map.resids.valid.HTI, file = "data/geo-data/resids.sf.HTI.RDS")
saveRDS(map.resids.valid.NotHTI, file = "data/geo-data/resids.sf.NotHTI.RDS")




library(tidyverse)
library(sf)
library(ggtext)

Africa.sf <- sf::st_read(dsn = "data/geo-data/afr_g2014_2013_0.shp")

###Residuals
map.resids.valid.HTI <- readRDS('data/geo-data/resids.sf.HTI.RDS')
map.resids.valid.NotHTI <- readRDS('data/geo-data/resids.sf.NotHTI.RDS')

min(map.resids.valid.NotHTI$resids1519)
max(map.resids.valid.NotHTI$resids1519)
mean(map.resids.valid.NotHTI$resids1519)
min(map.resids.valid.NotHTI$resids2024)
max(map.resids.valid.NotHTI$resids2024)
mean(map.resids.valid.NotHTI$resids2024)
min(map.resids.valid.NotHTI$resids2529)
max(map.resids.valid.NotHTI$resids2529)
mean(map.resids.valid.NotHTI$resids2529)

min(map.resids.valid.HTI$resids1519)
max(map.resids.valid.HTI$resids1519)
mean(map.resids.valid.HTI$resids1519)
min(map.resids.valid.HTI$resids2024)
max(map.resids.valid.HTI$resids2024)
mean(map.resids.valid.HTI$resids2024)
min(map.resids.valid.HTI$resids2529)
max(map.resids.valid.HTI$resids2529)
mean(map.resids.valid.HTI$resids2529)

hist(map.resids.valid.NotHTI$resids1519)
hist(map.resids.valid.NotHTI$resids2024)
hist(map.resids.valid.NotHTI$resids2529)
hist(map.resids.valid.HTI$resids1519)
hist(map.resids.valid.HTI$resids2024)
hist(map.resids.valid.HTI$resids2529)

br <- c(-10, -4, -1, -0.5, 0.5, 1, 4, 10)

map.resids.valid.NotHTI$disp_resids <- cut(map.resids.valid.NotHTI$resids1519,
                                           breaks = br,
                                           dig.lab = 5)

# Create custom labels - e.g. (0k-10k]
labs <- c(-10, -4, -1, -0.5, 0.5, 1, 4, 10)
labs_plot <- ("")

pal <- hcl.colors(7, "PRGn", rev = TRUE, alpha = 0.7)

base_gg_main <- ggplot() +
  geom_sf(data = st_as_sf(Africa.sf), 
          fill = NA, 
          color = "black", 
          linewidth = 0.2)

residuals_main_1519 <- base_gg_main +
  geom_sf(data = map.resids.valid.NotHTI,
          aes(fill = disp_resids),
          color = "#7f7f7f",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    label = labs_plot,
                    # Legend
                    guide = guide_legend(direction = "vertical",
                                         nrow = 1,
                                         label.position = "bottom")) +
  # Theme
  theme_void()

residuals_main_1519

ggsave("output/residuals_main_1519.png", dpi = 300, width = 12, height = 12)

map.resids.valid.NotHTI$disp_resids <- cut(map.resids.valid.NotHTI$resids2024,
                                           breaks = br,
                                           dig.lab = 5)

residuals_main_2024 <- base_gg_main +
  geom_sf(data = map.resids.valid.NotHTI,
          aes(fill = disp_resids),
          color = "#7f7f7f",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    label = labs_plot,
                    # Legend
                    guide = guide_legend(direction = "vertical",
                                         nrow = 1,
                                         label.position = "bottom")) +
  # Theme
  theme_void()

residuals_main_2024

ggsave("output/residuals_main_2024.png", dpi = 300, width = 12, height = 12)


map.resids.valid.NotHTI$disp_resids <- cut(map.resids.valid.NotHTI$resids2529,
                                           breaks = br,
                                           dig.lab = 5)


residuals_main_2529 <- base_gg_main +
  geom_sf(data = map.resids.valid.NotHTI,
          aes(fill = disp_resids),
          color = "#7f7f7f",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    label = labs_plot,
                    # Legend
                    guide = guide_legend(direction = "vertical",
                                         nrow = 1,
                                         label.position = "bottom")) +
  # Theme
  theme_void()

residuals_main_2529

ggsave("output/residuals_main_2529.png", dpi = 300, width = 12, height = 12)

#Haiti
#########

map.resids.valid.HTI$disp_resids <- cut(map.resids.valid.HTI$resids1519,
                                        breaks = br,
                                        dig.lab = 5)

base_gg_haiti <- ggplot() +
  geom_sf(data = st_as_sf(hti0.sf), 
          fill = NA, 
          color = "black", 
          linewidth = 0.2)

residuals_haiti_1519 <- base_gg_haiti +
  geom_sf(data = map.resids.valid.HTI,
          aes(fill = disp_resids),
          color = "#7f7f7f",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    label = NULL,
                    # Legend
                    guide = NULL) +
  # Theme
  theme_void()

residuals_haiti_1519

ggsave("output/residuals_haiti_1519.png", dpi = 300, width = 4, height = 4)


map.resids.valid.HTI$disp_resids <- cut(map.resids.valid.HTI$resids2024,
                                        breaks = br,
                                        dig.lab = 5)

residuals_haiti_2024 <- base_gg_haiti +
  geom_sf(data = map.resids.valid.HTI,
          aes(fill = disp_resids),
          color = "#7f7f7f",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    label = NULL,
                    # Legend
                    guide = NULL) +
  # Theme
  theme_void()

residuals_haiti_2024

ggsave("output/residuals_haiti_2024.png", dpi = 300, width = 4, height = 4)

map.resids.valid.HTI$disp_resids <- cut(map.resids.valid.HTI$resids2529,
                                        breaks = br,
                                        dig.lab = 5)


residuals_haiti_2529 <- base_gg_haiti +
  geom_sf(data = map.resids.valid.HTI,
          aes(fill = disp_resids),
          color = "#7f7f7f",
          linewidth = 0.1) +
  scale_fill_manual("",
                    values = pal,
                    drop = FALSE,
                    na.value = NA,
                    label = NULL,
                    # Legend
                    guide = NULL) +
  # Theme
  theme_void()

residuals_haiti_2529

ggsave("output/residuals_haiti_2529.png", dpi = 300, width = 4, height = 4)

