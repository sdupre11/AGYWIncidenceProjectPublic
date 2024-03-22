library(sf)
library(tidyverse)
library(colorspace)

Africa.sf <- sf::st_read(dsn = "data/geo-data/afr_g2014_2013_0.shp")
hti0.sf <- sf::st_read(dsn = "data/geo-data/geojson/clean/hti.geojson") %>%
  filter(area_level == 0)

Angola.sf <- sf::st_read(dsn = "data/geo-data/geoBoundaries-AGO-ADM1.shp") %>%
  rename(area_name = shapeName) %>%
  select(-c("shapeISO",
            "shapeID",
            "shapeGroup",
            "shapeType"))

Angola.sf$area_level <- 1
Angola.sf$area_level_label <- NA
Angola.sf$area_id <- NA
Angola.sf$parent_area_id <- NA
Angola.sf$spectrum_region_code <- NA
Angola.sf$area_sort_order <- NA
Angola.sf$center_x <- NA
Angola.sf$center_y <- NA
Angola.sf$name <- NA

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
  filter(!(X %in% duplicates & area %in% c("Blantyre", "Zomba", "Lilongwe")))

bwa.sf <- sf::st_read(dsn = "data/geo-data/geojson/clean/bwa.geojson") %>%
  filter(area_level == 3)
civ.sf <- sf::st_read(dsn = "data/geo-data/geojson/clean/civ.geojson") %>%
  filter(area_level == 2)
hti.sf <- sf::st_read(dsn = "data/geo-data/geojson/clean/hti.geojson") %>%
  filter(area_level == 2)
ken.sf <- sf::st_read(dsn = "data/geo-data/geojson/clean/ken.geojson") %>%
  filter(area_level == 2)
lso.sf <- sf::st_read(dsn = "data/geo-data/geojson/clean/lso.geojson") %>%
  filter(area_level == 1)
moz.sf <- sf::st_read(dsn = "data/geo-data/geojson/clean/moz.geojson") %>%
  filter(area_level == 3)
mwi.sf <- sf::st_read(dsn = "data/geo-data/geojson/clean/mwi.geojson") %>%
  filter(area_level == 5)
nam.sf <- sf::st_read(dsn = "data/geo-data/geojson/clean/nam.geojson") %>%
  filter(area_level == 2)
ner.sf <- sf::st_read(dsn = "data/geo-data/geojson/clean/ner.geojson") %>%
  filter(area_level == 1)
rwa.sf <- sf::st_read(dsn = "data/geo-data/geojson/clean/rwa.geojson") %>%
  filter(area_level == 2)
tgo.sf <- sf::st_read(dsn = "data/geo-data/geojson/clean/tgo.geojson") %>%
  filter(area_level == 2)
tza.sf <- sf::st_read(dsn = "data/geo-data/geojson/clean/tza.geojson") %>%
  filter(area_level == 4)
uga.sf <- sf::st_read(dsn = "data/geo-data/geojson/clean/uga.geojson") %>%
  filter(area_level == 3)
zaf.sf <- sf::st_read(dsn = "data/geo-data/geojson/clean/zaf.geojson") %>%
  filter(area_level == 2)
zmb.sf <- sf::st_read(dsn = "data/geo-data/geojson/clean/zmb.geojson") %>%
  filter(area_level == 2)
zwe.sf <- sf::st_read(dsn = "data/geo-data/geojson/clean/zwe.geojson") %>%
  filter(area_level == 2)

my_list <- list(Angola.sf,
                bwa.sf,
                civ.sf,
                hti.sf,
                ken.sf,
                lso.sf,
                moz.sf,
                mwi.sf,
                nam.sf,
                ner.sf,
                rwa.sf,
                tgo.sf,
                tza.sf,
                uga.sf,
                zaf.sf,
                zmb.sf,
                zwe.sf)

all.sf <- do.call(rbind, my_list)

all.sf <- all.sf %>%
  select(-c(area_level_label,
            parent_area_id,
            spectrum_region_code,
            area_sort_order,
            center_x,
            center_y,
            area_id,
            name)) %>%
  rename(area = area_name,
         level = area_level) %>%
  mutate(
    area = case_when(
      (area == "!Nami#nus") ~ "!nami#nus",
      (area == "A Nzo DM") ~ "A Nzo Dm",
      (area == "Abobo-Est") ~ "Abobo-est",
      (area == "Abobo-Ouest") ~ "Abobo-ouest",
      (area == "Acul-du-Nord") ~ "Acul-du-nord",
      (area == "Adjame-Plateau-Attecoube") ~ "Adjame-plateau-attecoube",
      (area == "Amajuba DM") ~ "Amajuba Dm",
      (area == "Amathole DM") ~ "Amathole Dm",
      (area == "Anse-à-Veau") ~ "Anse-à-veau",
      (area == "Anse-d'Ainault") ~ "Anse-d'ainault",
      (area == "Arusha CC") ~ "Arusha Cc",
      (area == "Arusha DC") ~ "Arusha Dc",
      (area == "Babati DC") ~ "Babati Dc",
      (area == "Babati TC") ~ "Babati Tc",
      (area == "Bagamoyo DC") ~ "Bagamoyo Dc",
      (area == "Bahi DC") ~ "Bahi Dc",
      (area == "Bariadi DC") ~ "Bariadi Dc",
      (area == "Bariadi TC") ~ "Bariadi Tc",
      (area == "Bas-Mono") ~ "Bas-mono",
      (area == "Belle-Anse") ~ "Belle-anse",
      (area == "Biharamulo DC") ~ "Biharamulo Dc",
      (area == "Bojanala Platinum DM") ~ "Bojanala Platinum Dm",
      (area == "Bouake-Nord-Est") ~ "Bouake-nord-est",
      (area == "Bouake-Nord-Ouest") ~ "Bouake-nord-ouest",
      (area == "Bouake-Sud") ~ "Bouake-sud",
      (area == "Buchosa DC") ~ "Buchosa Dc",
      (area == "Buffalo City MM") ~ "Buffalo City Mm",
      (area == "Buhigwe DC") ~ "Buhigwe Dc",
      (area == "Bukoba DC") ~ "Bukoba Dc",
      (area == "Bukoba MC") ~ "Bukoba Mc",
      (area == "Bukombe DC") ~ "Bukombe Dc",
      (area == "Bumbuli DC") ~ "Bumbuli Dc",
      (area == "Bunda DC") ~ "Bunda Dc",
      (area == "Bunda TC") ~ "Bunda Tc",
      (area == "Busega DC") ~ "Busega Dc",
      (area == "Busokelo DC") ~ "Busokelo Dc",
      (area == "Butha-Buthe") ~ "Butha-buthe",
      (area == "Butiama DC") ~ "Butiama Dc",
      (area == "C Hani DM") ~ "C Hani Dm",
      (area == "Cap-Haïtien") ~ "Cap-haïtien",
      (area == "Cape Town MM") ~ "Cape Town Mm",
      (area == "Cape Winelands DM") ~ "Cape Winelands Dm",
      (area == "Capricorn DM") ~ "Capricorn Dm",
      (area == "Central Karoo DM") ~ "Central Karoo Dm",
      (area == "Cerca la Source") ~ "Cerca La Source",
      (area == "Chalinze DC") ~ "Chalinze Dc",
      (area == "Chamwino DC") ~ "Chamwino Dc",
      (area == "Chato DC") ~ "Chato Dc",
      (area == "Chemba DC") ~ "Chemba Dc",
      (area == "Chunya DC") ~ "Chunya Dc",
      (area == "Cocody-Bingerville") ~ "Cocody-bingerville",
      (area == "Croix-des-Bouquets") ~ "Croix-des-bouquets",
      (area == "Dodoma MC") ~ "Dodoma Mc",
      (area == "Dr K Kaunda DM") ~ "Dr K Kaunda Dm",
      (area == "Ehlanzeni DM") ~ "Ehlanzeni Dm",
      (area == "Ekurhuleni MM") ~ "Ekurhuleni Mm",
      (area == "Elgeyo-Marakwet") ~ "Elgeyo-marakwet",
      (area == "Est-Mono") ~ "Est-mono",
      (area == "eThekwini MM") ~ "Ethekwini Mm",
      (area == "Fezile Dabi DM") ~ "Fezile Dabi Dm",
      (area == "Fort portal City") ~ "Fort Portal City",
      (area == "Fort-Liberté") ~ "Fort-liberté",
      (area == "Frances Baard DM") ~ "Frances Baard Dm",
      (area == "G Sibande DM") ~ "G Sibande Dm",
      (area == "Gairo DC") ~ "Gairo Dc",
      (area == "Garden Route DM") ~ "Garden Route Dm",
      (area == "Geita DC") ~ "Geita Dc",
      (area == "Geita TC") ~ "Geita Tc",
      (area == "Grand-Bassam") ~ "Grand-bassam",
      (area == "Grand-Lahou") ~ "Grand-lahou",
      (area == "Grande-Rivière du Nord") ~ "Grande-rivière Du Nord",
      (area == "Gros-Morne") ~ "Gros-morne",
      (area == "Hai DC") ~ "Hai Dc",
      (area == "Hanang DC") ~ "Hanang Dc",
      (area == "Handeni DC") ~ "Handeni Dc",
      (area == "Handeni TC") ~ "Handeni Tc",
      (area == "Harry Gwala DM") ~ "Harry Gwala Dm",
      (area == "Ifakara TC") ~ "Ifakara Tc",
      (area == "Igunga DC") ~ "Igunga Dc",
      (area == "Ikungi DC") ~ "Ikungi Dc",
      (area == "Ilala MC") ~ "Ilala Mc",
      (area == "Ileje DC") ~ "Ileje Dc",
      (area == "iLembe DM") ~ "Ilembe Dm",
      (area == "Ilemela MC") ~ "Ilemela Mc",
      (area == "Ilha de Moçambique") ~ "Ilha De Moçambique",
      (area == "Iramba DC") ~ "Iramba Dc",
      (area == "Iringa DC") ~ "Iringa Dc",
      (area == "Iringa MC") ~ "Iringa Mc",
      (area == "Itigi DC") ~ "Itigi Dc",
      (area == "Itilima DC") ~ "Itilima Dc",
      (area == "J T Gaetsewe DM") ~ "J T Gaetsewe Dm",
      (area == "Joe Gqabi DM") ~ "Joe Gqabi Dm",
      (area == "Johannesburg MM") ~ "Johannesburg Mm",
      (area == "Kahama TC") ~ "Kahama Tc",
      (area == "Kakonko DC") ~ "Kakonko Dc",
      (area == "Kalambo DC") ~ "Kalambo Dc",
      (area == "Kaliua DC") ~ "Kaliua Dc",
      (area == "KaMavota") ~ "Kamavota",
      (area == "KaMaxakeni") ~ "Kamaxakeni",
      (area == "KaMpfumu") ~ "Kampfumu",
      (area == "KaMubukwana") ~ "Kamubukwana",
      (area == "KaNyaka") ~ "Kanyaka",
      (area == "Karagwe DC") ~ "Karagwe Dc",
      (area == "Karatu DC") ~ "Karatu Dc",
      (area == "Kasulu DC") ~ "Kasulu Dc",
      (area == "Kasulu TC") ~ "Kasulu Tc",
      (area == "KaTembe") ~ "Katembe",
      (area == "Kibaha DC") ~ "Kibaha Dc",
      (area == "Kibaha TC") ~ "Kibaha Tc",
      (area == "Kibiti DC") ~ "Kibiti Dc",
      (area == "Kibondo DC") ~ "Kibondo Dc",
      (area == "Kigamboni MC") ~ "Kigamboni Mc",
      (area == "Kigoma DC") ~ "Kigoma Dc",
      (area == "Kigoma Ujiji MC") ~ "Kigoma Ujiji Mc",
      (area == "Kilindi DC") ~ "Kilindi Dc",
      (area == "Kilolo DC") ~ "Kilolo Dc",
      (area == "Kilombero DC") ~ "Kilombero Dc",
      (area == "Kilosa DC") ~ "Kilosa Dc",
      (area == "Kilwa DC") ~ "Kilwa Dc",
      (area == "King Cetshwayo DM") ~ "King Cetshwayo Dm",
      (area == "Kinondoni MC") ~ "Kinondoni Mc",
      (area == "Kisarawe DC") ~ "Kisarawe Dc",
      (area == "Kishapu DC") ~ "Kishapu Dc",
      (area == "Kiteto DC") ~ "Kiteto Dc",
      (area == "Kondoa DC") ~ "Kondoa Dc",
      (area == "Kondoa TC") ~ "Kondoa Tc",
      (area == "Kongwa DC") ~ "Kongwa Dc",
      (area == "Korogwe DC") ~ "Korogwe Dc",
      (area == "Korogwe TC") ~ "Korogwe Tc",
      (area == "Koun-Fao") ~ "Koun-fao",
      (area == "Kwimba DC") ~ "Kwimba Dc",
      (area == "Kyela DC") ~ "Kyela Dc",
      (area == "Kyerwa DC") ~ "Kyerwa Dc",
      (area == "Lejweleputswa DM") ~ "Lejweleputswa Dm",
      (area == "Lindi DC") ~ "Lindi Dc",
      (area == "Lindi MC") ~ "Lindi Mc",
      (area == "Liwale DC") ~ "Liwale Dc",
      (area == "Longido DC") ~ "Longido Dc",
      (area == "Ludewa DC") ~ "Ludewa Dc",
      (area == "Lushoto DC") ~ "Lushoto Dc",
      (area == "M'Batto") ~ "M'batto",
      (area == "M'Bengue") ~ "M'bengue",
      (area == "Madaba DC") ~ "Madaba Dc",
      (area == "Madi okollo") ~ "Madi Okollo",
      (area == "Mafia DC") ~ "Mafia Dc",
      (area == "Mafinga TC") ~ "Mafinga Tc",
      (area == "Maganja da Costa") ~ "Maganja Da Costa",
      (area == "Magu DC") ~ "Magu Dc",
      (area == "Makambako TC") ~ "Makambako Tc",
      (area == "Makete DC") ~ "Makete Dc",
      (area == "Malinyi DC") ~ "Malinyi Dc",
      (area == "Mangaung MM") ~ "Mangaung Mm",
      (area == "Manyoni DC") ~ "Manyoni Dc",
      (area == "Masasi DC") ~ "Masasi Dc",
      (area == "Masasi TC") ~ "Masasi Tc",
      (area == "Maswa DC") ~ "Maswa Dc",
      (area == "Mbarali DC") ~ "Mbarali Dc",
      (area == "Mbeya CC") ~ "Mbeya Cc",
      (area == "Mbeya DC") ~ "Mbeya Dc",
      (area == "Mbinga DC") ~ "Mbinga Dc",
      (area == "Mbinga TC") ~ "Mbinga Tc",
      (area == "Mbogwe DC") ~ "Mbogwe Dc",
      (area == "Mbozi DC") ~ "Mbozi Dc",
      (area == "Mbulu DC") ~ "Mbulu Dc",
      (area == "Mbulu TC") ~ "Mbulu Tc",
      (area == "Meatu DC") ~ "Meatu Dc",
      (area == "Meru DC") ~ "Meru Dc",
      (area == "Missenyi DC") ~ "Missenyi Dc",
      (area == "Misungwi DC") ~ "Misungwi Dc",
      (area == "Mkalama DC") ~ "Mkalama Dc",
      (area == "Mkinga DC") ~ "Mkinga Dc",
      (area == "Mkuranga DC") ~ "Mkuranga Dc",
      (area == "Mlele DC") ~ "Mlele Dc",
      (area == "Mocímboa da Praia") ~ "Mocímboa Da Praia",
      (area == "Môle-Saint-Nicolas") ~ "Môle-saint-nicolas",
      (area == "Momba DC") ~ "Momba Dc",
      (area == "Monduli DC") ~ "Monduli Dc",
      (area == "Mopani DM") ~ "Mopani Dm",
      (area == "Morogoro DC") ~ "Morogoro Dc",
      (area == "Morogoro MC") ~ "Morogoro Mc",
      (area == "Moshi DC") ~ "Moshi Dc",
      (area == "Moshi MC") ~ "Moshi Mc",
      (area == "Moyen-Mono") ~ "Moyen-mono",
      (area == "Mpanda DC") ~ "Mpanda Dc",
      (area == "Mpanda TC") ~ "Mpanda Tc",
      (area == "Mpimbwe DC") ~ "Mpimbwe Dc",
      (area == "Mpwapwa DC") ~ "Mpwapwa Dc",
      (area == "Msalala DC") ~ "Msalala Dc",
      (area == "Mtwara DC") ~ "Mtwara Dc",
      (area == "Mtwara Mikindani MC") ~ "Mtwara Mikindani Mc",
      (area == "Mufindi DC") ~ "Mufindi Dc",
      (area == "Muheza DC") ~ "Muheza Dc",
      (area == "Muleba DC") ~ "Muleba Dc",
      (area == "Musoma DC") ~ "Musoma Dc",
      (area == "Musoma MC") ~ "Musoma Mc",
      (area == "Mvomero DC") ~ "Mvomero Dc",
      (area == "Mwanga DC") ~ "Mwanga Dc",
      (area == "N Mandela Bay MM") ~ "N Mandela Bay Mm",
      (area == "Nacala-a-Velha") ~ "Nacala-a-velha",
      (area == "Nachingwea DC") ~ "Nachingwea Dc",
      (area == "Nairobi (County)") ~ "Nairobi (county)",
      (area == "Namakwa DM") ~ "Namakwa Dm",
      (area == "Namtumbo DC") ~ "Namtumbo Dc",
      (area == "Nanyamba TC") ~ "Nanyamba Tc",
      (area == "Nanyumbu DC") ~ "Nanyumbu Dc",
      (area == "Newala DC") ~ "Newala Dc",
      (area == "Newala TC") ~ "Newala Tc",
      (area == "Ngaka Modiri Molema DM") ~ "Ngaka Modiri Molema Dm",
      (area == "Ngara DC") ~ "Ngara Dc",
      (area == "Ngorongoro DC") ~ "Ngorongoro Dc",
      (area == "Njombe DC") ~ "Njombe Dc",
      (area == "Njombe TC") ~ "Njombe Tc",
      (area == "Nkangala DM") ~ "Nkangala Dm",
      (area == "Nkasi DC") ~ "Nkasi Dc",
      (area == "Nsimbo DC") ~ "Nsimbo Dc",
      (area == "Nyamagana MC") ~ "Nyamagana Mc",
      (area == "Nyang'hwale DC") ~ "Nyang'hwale Dc",
      (area == "Nyasa DC") ~ "Nyasa Dc",
      (area == "Nzega DC") ~ "Nzega Dc",
      (area == "Nzega TC") ~ "Nzega Tc",
      (area == "O Tambo DM") ~ "O Tambo Dm",
      (area == "Overberg DM") ~ "Overberg Dm",
      (area == "Pangani DC") ~ "Pangani Dc",
      (area == "Pixley ka Seme DM") ~ "Pixley Ka Seme Dm",
      (area == "Plaine de Mo") ~ "Plaine De Mo",
      (area == "Port-au-Prince") ~ "Port-au-prince",
      (area == "Port-Bouet-Vridi") ~ "Port-bouet-vridi",
      (area == "Port-de-Paix") ~ "Port-de-paix",
      (area == "Port-Salut") ~ "Port-salut",
      (area == "Rombo DC") ~ "Rombo Dc",
      (area == "Rorya DC") ~ "Rorya Dc",
      (area == "Ruangwa DC") ~ "Ruangwa Dc",
      (area == "Rufiji DC") ~ "Rufiji Dc",
      (area == "Rungwe DC") ~ "Rungwe Dc",
      (area == "Ruth Segomotsi Mompati DM") ~ "Ruth Segomotsi Mompati Dm",
      (area == "Saint-Louis du Nord") ~ "Saint-louis Du Nord",
      (area == "Saint-Marc") ~ "Saint-marc",
      (area == "Saint-Raphaël") ~ "Saint-raphaël",
      (area == "Same DC") ~ "Same Dc",
      (area == "San-Pedro") ~ "San-pedro",
      (area == "Sarah Baartman DM") ~ "Sarah Baartman Dm",
      (area == "Sedibeng DM") ~ "Sedibeng Dm",
      (area == "seguela") ~ "Seguela",
      (area == "Sekhukhune DM") ~ "Sekhukhune Dm",
      (area == "Sengerema DC") ~ "Sengerema Dc",
      (area == "Serengeti DC") ~ "Serengeti Dc",
      (area == "Shinyanga DC") ~ "Shinyanga Dc",
      (area == "Shinyanga MC") ~ "Shinyanga Mc",
      (area == "Siha DC") ~ "Siha Dc",
      (area == "Sikonge DC") ~ "Sikonge Dc",
      (area == "Simanjiro DC") ~ "Simanjiro Dc",
      (area == "Singida DC") ~ "Singida Dc",
      (area == "Singida MC") ~ "Singida Mc",
      (area == "Songea DC") ~ "Songea Dc",
      (area == "Songea MC") ~ "Songea Mc",
      (area == "Songwe DC") ~ "Songwe Dc",
      (area == "Sumbawanga DC") ~ "Sumbawanga Dc",
      (area == "Sumbawanga MC") ~ "Sumbawanga Mc",
      (area == "T Mofutsanyana DM") ~ "T Mofutsanyana Dm",
      (area == "Tabora MC") ~ "Tabora Mc",
      (area == "Taita-Taveta") ~ "Taita-taveta",
      (area == "Tandahimba DC") ~ "Tandahimba Dc",
      (area == "Tanga CC") ~ "Tanga Cc",
      (area == "Tarime DC") ~ "Tarime Dc",
      (area == "Tarime TC") ~ "Tarime Tc",
      (area == "Temeke MC") ~ "Temeke Mc",
      (area == "Thaba-Tseka") ~ "Thaba-tseka",
      (area == "Tharaka-Nithi") ~ "Tharaka-nithi",
      (area == "Trans-Nzoia") ~ "Trans-nzoia",
      (area == "Treichville-Marcory") ~ "Treichville-marcory",
      (area == "Trou-du-Nord") ~ "Trou-du-nord",
      (area == "Tshwane MM") ~ "Tshwane Mm",
      (area == "Tunduma TC") ~ "Tunduma Tc",
      (area == "Tunduru DC") ~ "Tunduru Dc",
      (area == "Ubungo MC") ~ "Ubungo Mc",
      (area == "Ugu DM") ~ "Ugu Dm",
      (area == "Ukerewe DC") ~ "Ukerewe Dc",
      (area == "Ulanga DC") ~ "Ulanga Dc",
      (area == "uMgungundlovu DM") ~ "Umgungundlovu Dm",
      (area == "Umkhanyakude DM") ~ "Umkhanyakude Dm",
      (area == "UMP") ~ "Ump",
      (area == "Umzinyathi DM") ~ "Umzinyathi Dm",
      (area == "Urambo DC") ~ "Urambo Dc",
      (area == "Ushetu DC") ~ "Ushetu Dc",
      (area == "Uthukela DM") ~ "Uthukela Dm",
      (area == "Uvinza DC") ~ "Uvinza Dc",
      (area == "Uyui DC") ~ "Uyui Dc",
      (area == "Vhembe DM") ~ "Vhembe Dm",
      (area == "Wanging'ombe DC") ~ "Wanging'ombe Dc",
      (area == "Waterberg DM") ~ "Waterberg Dm",
      (area == "West Coast DM") ~ "West Coast Dm",
      (area == "West Rand DM") ~ "West Rand Dm",
      (area == "Xai-Xai") ~ "Xai-xai",
      (area == "Xhariep DM") ~ "Xhariep Dm",
      (area == "Yakasse-Attobrou") ~ "Yakasse-attobrou",
      (area == "Yopougon-Est") ~ "Yopougon-est",
      (area == "Yopougon-Ouest-Songon") ~ "Yopougon-ouest-songon",
      (area == "ZF Mgcawu DM") ~ "Zf Mgcawu Dm",
      (area == "Zululand DM") ~ "Zululand Dm",
      (area == "BENGO") ~ "Bengo",
      (area == "BENGUELA") ~ "Benguela",
      (area == "BIÉ") ~ "Bié",
      (area == "CABINDA") ~ "Cabinda",
      (area == "CUANDO CUBANGO") ~ "Cuando Cubango",
      (area == "CUANZA NORTE") ~ "Cuanza Norte",
      (area == "CUANZA SUL") ~ "Cuanza Sul",
      (area == "CUNENE") ~ "Cunene",
      (area == "HUAMBO") ~ "Huambo",
      (area == "HUÍLA") ~ "Huíla",
      (area == "LUANDA") ~ "Luanda",
      (area == "LUNDA NORTE") ~ "Lunda Norte",
      (area == "LUNDA SUL") ~ "Lunda Sul",
      (area == "MALANJE") ~ "Malanje",
      (area == "MOXICO") ~ "Moxico",
      (area == "NAMIBE") ~ "Namibe",
      (area == "UÍGE") ~ "Uíge",
      (area == "ZAIRE") ~ "Zaire",
      TRUE ~ as.character(area)
    )
  )


allData1519 <- allData %>%
  filter(age == "Y015_019")
allData2024 <- allData %>%
  filter(age == "Y020_024")
allData2529 <- allData %>%
  filter(age == "Y025_029")


all.sf2.1519 <- full_join(all.sf, 
                          allData1519,
                          dplyr::join_by(area == area,
                                         level == level)) %>%
  filter(area != "Nhamatanda")

all.sf2.2024 <- full_join(all.sf, 
                          allData2024,
                          dplyr::join_by(area == area,
                                         level == level)) %>%
  filter(area != "Nhamatanda")

all.sf2.2529 <- full_join(all.sf, 
                          allData2529,
                          dplyr::join_by(area == area,
                                         level == level)) %>%
  filter(area != "Nhamatanda")




sf.noHTI1519 <- all.sf2.1519 %>%
  filter(country != "HTI")
sf.HTI1519 <- all.sf2.1519 %>%
  filter(country == "HTI")
sf.noHTI2024 <- all.sf2.2024 %>%
  filter(country != "HTI")
sf.HTI2024 <- all.sf2.2024 %>%
  filter(country == "HTI")
sf.noHTI2529 <- all.sf2.2529 %>%
  filter(country != "HTI")
sf.HTI2529 <- all.sf2.2529 %>%
  filter(country == "HTI")



HTIorNotHTI <- function(z) {
  if(z == "Haiti") {
    geom_sf(data = st_as_sf(hti0.sf), 
                            fill = NA, 
                            color = "black", 
                            linewidth = 0.5)
  } else {
    geom_sf(data = st_as_sf(Africa.sf), 
                            fill = NA, 
                            color = "black", 
                            linewidth = 0.5)
  }
}

# HTIorNotHTI_NAs <- function(z) {
#   if(z == "Haiti") {
#     geom_sf(data = st_as_sf(hti0.sf), 
#             fill = NA, 
#             color = "black", 
#             linewidth = 0.5)
#   } else {
#     geom_sf(data = st_as_sf(Africa.sf), 
#             fill = "#928E85", 
#             color = NA, 
#             linewidth = 0.5)
#   }
# }

univariates <- function(w, x, y, z="Other") {
  
  base_gg_main <- HTIorNotHTI(z)
  
  # base_gg_main_NAs <- HTIorNotHTI_NAs(z)

  if(x == "inc") {
    basePlot <- ggplot(data = w, 
                       aes(fill = cut(inc,
                                      breaks = c(0,0.5, 1,2,5,10,15,100),
                                      dig.lab = 5)))
  } else if (x =="prev") {
    basePlot <- ggplot(data = w, 
                       aes(fill = cut(prev,
                                      breaks = c(0,2,4,6,8,10,20,100),
                                      dig.lab = 5)))
  } else if (x =="maleART") {
    basePlot <- ggplot(data = w, 
                       aes(fill = cut(maleART,
                                      breaks = c(0,40,50,60,70,80,90,100),
                                      dig.lab = 5)))
  } else if (x =="malePrev") {
    basePlot <- ggplot(data = w, 
                       aes(fill = cut(malePrev,
                                      breaks = c(0,5,10,15,20,25,30,100),
                                      dig.lab = 5)))
  } else if (x =="sexualDebut25_49") {
    basePlot <- ggplot(data = w, 
                       aes(fill = cut(sexualDebut25_49,
                                      breaks = c(0,15,16,17,18,19,20,100),
                                      dig.lab = 5)))
  } else if (x =="sexRatio15_64") {
    basePlot <- ggplot(data = w, 
                       aes(fill = cut(sexRatio15_64,
                                      breaks = c(0,0.8,0.9,0.95,1.05,1.1,1.2,100),
                                      dig.lab = 5)))
  }
  
  basePlot +
  # base_gg_main_NAs +
  geom_sf(color = "#7f7f7f",
          linewidth = 0.1) +
  scale_fill_brewer(type = "qual",
                    palette = y,
                    na.value = NA,
                    name = "Incidence",
                    direction=1,
                    guides(none)
                    ) +
  theme_void() +
  base_gg_main +
  theme(legend.position = "none")

}

#INCIDENCE
###NON-HAITIAN
incidence_main_1519 <-univariates(sf.noHTI1519,
                                  "inc",
                                  "Reds")
            

incidence_main_1519

ggsave("output/incidence_main_1519.png", dpi = 300, width = 12, height = 12)

incidence_main_2024 <-univariates(sf.noHTI2024,
                                  "inc",
                                  "Reds")


incidence_main_2024

ggsave("output/incidence_main_2024.png", dpi = 300, width = 12, height = 12)

incidence_main_2529 <-univariates(sf.noHTI2529,
                                  "inc",
                                  "Reds")


incidence_main_2529

ggsave("output/incidence_main_2529.png", dpi = 300, width = 12, height = 12)

###HAITI
incidence_HTI_1519 <-univariates(sf.HTI1519,
                                  "inc",
                                  "Reds",
                                 "Haiti")

incidence_HTI_1519

ggsave("output/incidence_HTI_1519.png", dpi = 300, width = 4, height = 4)


incidence_HTI_2024 <-univariates(sf.HTI2024,
                                 "inc",
                                 "Reds",
                                 "Haiti")

incidence_HTI_2024

ggsave("output/incidence_HTI_2024.png", dpi = 300, width = 4, height = 4)


incidence_HTI_2529 <-univariates(sf.HTI2529,
                                 "inc",
                                 "Reds",
                                 "Haiti")

incidence_HTI_2529

ggsave("output/incidence_HTI_2529.png", dpi = 300, width = 4, height = 4)

PREVALENCE
###NON-HAITIAN
prevalence_main_1519 <-univariates(sf.noHTI1519,
                                  "prev",
                                  "Blues")


prevalence_main_1519

ggsave("output/prevalence_main_1519.png", dpi = 300, width = 12, height = 12)

prevalence_main_2024 <-univariates(sf.noHTI2024,
                                  "prev",
                                  "Blues")


prevalence_main_2024

ggsave("output/prevalence_main_2024.png", dpi = 300, width = 12, height = 12)

prevalence_main_2529 <-univariates(sf.noHTI2529,
                                  "prev",
                                  "Blues")


prevalence_main_2529

ggsave("output/prevalence_main_2529.png", dpi = 300, width = 12, height = 12)

###HAITI
prevalence_HTI_1519 <-univariates(sf.HTI1519,
                                 "prev",
                                 "Blues",
                                 "Haiti")

prevalence_HTI_1519

ggsave("output/prevalence_HTI_1519.png", dpi = 300, width = 4, height = 4)


prevalence_HTI_2024 <-univariates(sf.HTI1519,
                                 "prev",
                                 "Blues",
                                 "Haiti")

prevalence_HTI_2024

ggsave("output/prevalence_HTI_2024.png", dpi = 300, width = 4, height = 4)


prevalence_HTI_2529 <-univariates(sf.HTI2529,
                                 "prev",
                                 "Blues",
                                 "Haiti")

prevalence_HTI_2529

ggsave("output/prevalence_HTI_2529.png", dpi = 300, width = 4, height = 4)


#MALE ART
###NON-HAITIAN
maleART_main <-univariates(sf.noHTI1519,
                                   "maleART",
                                   "Greens")


maleART_main

ggsave("output/maleART_main.png", dpi = 300, width = 12, height = 12)


###HAITI
maleART_HTI <-univariates(sf.HTI1519,
                                  "maleART",
                                  "Greens",
                                  "Haiti")

maleART_HTI

ggsave("output/maleART_HTI.png", dpi = 300, width = 4, height = 4)


#MALE PREVALENCE
###NON-HAITIAN
malePrev_main <-univariates(sf.noHTI1519,
                           "malePrev",
                           "Blues")


malePrev_main

ggsave("output/malePrevalence_main.png", dpi = 300, width = 12, height = 12)


###HAITI
malePrev_HTI <-univariates(sf.HTI1519,
                          "malePrev",
                          "Blues",
                          "Haiti")

malePrev_HTI

ggsave("output/malePrevalence_HTI.png", dpi = 300, width = 4, height = 4)

#AGE OF SEXUAL DEBUT
###NON-HAITIAN


sf.noHTI1519_noBWA <- sf.noHTI1519 %>%
  filter(country != "BWA")

sexualDebut_main <-univariates(sf.noHTI1519_noBWA,
                                   "sexualDebut25_49",
                                   "Oranges")

sexualDebut_main

ggsave("output/sexualDebut_main.png", dpi = 300, width = 12, height = 12)


###HAITI
sexualDebut_HTI <-univariates(sf.HTI1519,
                                  "sexualDebut25_49",
                                  "Oranges",
                                  "Haiti")

sexualDebut_HTI

ggsave("output/sexualDebut_HTI.png", dpi = 300, width = 4, height = 4)



#SEX RATIO
###NON-HAITIAN
sexRatio_main <-univariates(sf.noHTI1519,
                            "sexRatio15_64",
                            "RdYlBu")


sexRatio_main

ggsave("output/sexRatio_main.png", dpi = 300, width = 12, height = 12)


###HAITI
sexRatio_HTI <-univariates(sf.HTI1519,
                           "sexRatio15_64",
                           "RdYlBu",
                           "Haiti")

sexRatio_HTI

ggsave("output/sexRatio_HTI.png", dpi = 300, width = 4, height = 4)

