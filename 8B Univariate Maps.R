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
  filter(!(X %in% duplicates & area %in% c("Blantyre", "Zomba", "Lilongwe")))

bwa.sf <- sf::st_read(dsn = "data/geo-data/geojson/clean/bwa.geojson") %>%
  filter(area_level == 3)
civ.sf <- sf::st_read(dsn = "data/geo-data/geojson/clean/civ.geojson") %>%
  filter(area_level == 2)
cog.sf <- sf::st_read(dsn = "data/geo-data/geojson/clean/cog.geojson") %>%
  filter(area_level == 2)
esw.sf <- sf::st_read(dsn = "data/geo-data/geojson/clean/esw.geojson") %>%
  filter(area_level == 1)
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

my_list.full <- list(Angola.sf,
                     bwa.sf,
                     civ.sf,
                     cog.sf,
                     esw.sf,
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

all.sf <- do.call(rbind, my_list.full)

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

annoyingPaletteJustWorkInc <- function(x, y) {
  
  if (y == "Africa") {
    location <- Africa.sf
  } else if (y == "Haiti") {
    location <- hti0.sf
  }
  
  G1 <- x %>%
    filter(inc < 0.5)
  G2 <- x %>%
    filter(inc > 0.5 & inc < 1)
  G3 <- x %>%
    filter(inc > 1 &  inc < 2)
  G4 <- x %>%
    filter(inc > 2 & inc < 5)
  G5 <- x %>%
    filter(inc > 5 & inc < 10)
  G6 <- x %>%
    filter(inc > 10 & inc < 15)
  G7 <- x %>%
    filter(inc > 15)
  
  map <- ggplot() +
    geom_sf(data = G1,
            fill = "#FEE5D9",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G2,
            fill = "#FABAA1",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G3,
            fill = "#F69274",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G4,
            fill = "#F26A4D",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G5,
            fill = "#EF3D2C",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G6,
            fill = "#CA2027",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G7,
            fill = "#9A1B1F",
            color = "#7f7f7f",
            linewidth = 0.1) +
    theme_void() +
    geom_sf(data = st_as_sf(location), 
            fill = NA, 
            color = "black", 
            linewidth = 0.5) +
    theme(legend.position = "none")
  
  return(map)
}

# 
# HTIorNotHTI <- function(z) {
#   if(z == "Haiti") {
#     geom_sf(data = st_as_sf(hti0.sf), 
#                             fill = NA, 
#                             color = "black", 
#                             linewidth = 0.2)
#   } else {
#     geom_sf(data = st_as_sf(Africa.sf), 
#                             fill = NA, 
#                             color = "black", 
#                             linewidth = 0.2)
#   }
# }
# 
# # HTIorNotHTI_NAs <- function(z) {
# #   if(z == "Haiti") {
# #     geom_sf(data = st_as_sf(hti0.sf), 
# #             fill = NA, 
# #             color = "black", 
# #             linewidth = 0.5)
# #   } else {
# #     geom_sf(data = st_as_sf(Africa.sf), 
# #             fill = "#928E85", 
# #             color = NA, 
# #             linewidth = 0.5)
# #   }
# # }
# 
# univariates <- function(w, x, y, z="Other") {
#   
#   base_gg_main <- HTIorNotHTI(z)
#   
#   # base_gg_main_NAs <- HTIorNotHTI_NAs(z)
# 
#   if(x == "inc") {
#     basePlot <- ggplot(data = w, 
#                        aes(fill = cut(inc,
#                                       breaks = c(0,0.5, 1,2,5,10,15,100),
#                                       dig.lab = 5)))
#   } else if (x =="prev") {
#     basePlot <- ggplot(data = w, 
#                        aes(fill = cut(prev,
#                                       breaks = c(0,2,4,6,8,10,20,100),
#                                       dig.lab = 5)))
#   } else if (x =="maleART") {
#     basePlot <- ggplot(data = w, 
#                        aes(fill = cut(maleART,
#                                       breaks = c(0,40,50,60,70,80,90,100),
#                                       dig.lab = 5)))
#   } else if (x =="malePrev") {
#     basePlot <- ggplot(data = w, 
#                        aes(fill = cut(malePrev,
#                                       breaks = c(0,5,10,15,20,25,30,100),
#                                       dig.lab = 5)))
#   } else if (x =="sexualDebut25_49") {
#     basePlot <- ggplot(data = w, 
#                        aes(fill = cut(sexualDebut25_49,
#                                       breaks = c(0,15,16,17,18,19,20,100),
#                                       dig.lab = 5)))
#   } else if (x =="sexRatio15_64") {
#     basePlot <- ggplot(data = w, 
#                        aes(fill = cut(sexRatio15_64,
#                                       breaks = c(0,0.8,0.9,0.95,1.05,1.1,1.2,100),
#                                       dig.lab = 5)))
#   }
#   
#   basePlot +
#   geom_sf(color = "#7f7f7f",
#   linewidth = 0.1) +
#   scale_fill_brewer(type = "qual",
#                     palette = y,
#                     na.value = NA,
#                     name = "Incidence",
#                     direction=1,
#                     guides(none)
#                     ) +
#   theme_void() +
#   base_gg_main +
#   theme(legend.position = "none")
# 
# }

#INCIDENCE
###NON-HAITIAN
incidence_main_1519 <-annoyingPaletteJustWorkInc(sf.noHTI1519,
                                  "Africa")
            

incidence_main_1519

ggsave("output/incidence_main_1519.png", dpi = 300, width = 12, height = 12)

incidence_main_2024 <-annoyingPaletteJustWorkInc(sf.noHTI2024,
                                  "Africa")


incidence_main_2024

ggsave("output/incidence_main_2024.png", dpi = 300, width = 12, height = 12)

incidence_main_2529 <-annoyingPaletteJustWorkInc(sf.noHTI2529,
                                  "Africa")


incidence_main_2529

ggsave("output/incidence_main_2529.png", dpi = 300, width = 12, height = 12)

###HAITI
incidence_HTI_1519 <- annoyingPaletteJustWorkInc(sf.HTI1519,
                                                 "Haiti")

incidence_HTI_1519


ggsave("output/incidence_HTI_1519.png", dpi = 300, width = 4, height = 4)


incidence_HTI_2024 <-annoyingPaletteJustWorkInc(sf.HTI2024,
                                                "Haiti")

incidence_HTI_2024

ggsave("output/incidence_HTI_2024.png", dpi = 300, width = 4, height = 4)


incidence_HTI_2529 <-annoyingPaletteJustWorkInc(sf.HTI2529,
                                                "Haiti")

incidence_HTI_2529

ggsave("output/incidence_HTI_2529.png", dpi = 300, width = 4, height = 4)

#PREVALENCE

annoyingPaletteJustWorkPrev <- function(x, y) {
  
  if (y == "Africa") {
    location <- Africa.sf
  } else if (y == "Haiti") {
    location <- hti0.sf
  }
  
  G1 <- x %>%
    filter(prev < 2)
  G2 <- x %>%
    filter(prev > 2 & prev < 4)
  G3 <- x %>%
    filter(prev > 4 &  prev < 6)
  G4 <- x %>%
    filter(prev > 6 & prev < 8)
  G5 <- x %>%
    filter(prev > 8 & prev < 10)
  G6 <- x %>%
    filter(prev > 10 & prev < 20)
  G7 <- x %>%
    filter(prev > 20)
  
  map <- ggplot() +
    geom_sf(data = G1,
            fill = "#F0F3FA",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G2,
            fill = "#C7DBEE",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G3,
            fill = "#9ECAE1",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G4,
            fill = "#6AADD6",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G5,
            fill = "#4392C6",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G6,
            fill = "#2372B5",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G7,
            fill = "#194791",
            color = "#7f7f7f",
            linewidth = 0.1) +
    theme_void() +
    geom_sf(data = st_as_sf(location), 
            fill = NA, 
            color = "black", 
            linewidth = 0.5) +
    theme(legend.position = "none")
  
  return(map)
}

###NON-HAITIAN
prevalence_main_1519 <-annoyingPaletteJustWorkPrev(sf.noHTI1519,
                                  "Africa")


prevalence_main_1519

ggsave("output/prevalence_main_1519.png", dpi = 300, width = 12, height = 12)

prevalence_main_2024 <-annoyingPaletteJustWorkPrev(sf.noHTI2024,
                                  "Africa")


prevalence_main_2024

ggsave("output/prevalence_main_2024.png", dpi = 300, width = 12, height = 12)

prevalence_main_2529 <-annoyingPaletteJustWorkPrev(sf.noHTI2529,
                                  "Africa")


prevalence_main_2529

ggsave("output/prevalence_main_2529.png", dpi = 300, width = 12, height = 12)

###HAITI
prevalence_HTI_1519 <-annoyingPaletteJustWorkPrev(sf.HTI1519,
                                                  "Haiti")

prevalence_HTI_1519

ggsave("output/prevalence_HTI_1519.png", dpi = 300, width = 4, height = 4)


prevalence_HTI_2024 <-annoyingPaletteJustWorkPrev(sf.HTI1519,
                                                  "Haiti")

prevalence_HTI_2024

ggsave("output/prevalence_HTI_2024.png", dpi = 300, width = 4, height = 4)


prevalence_HTI_2529 <-annoyingPaletteJustWorkPrev(sf.HTI2529,
                                                  "Haiti")

prevalence_HTI_2529

ggsave("output/prevalence_HTI_2529.png", dpi = 300, width = 4, height = 4)


#MALE ART
annoyingPaletteJustWorkMaleART <- function(x, y) {
  
  if (y == "Africa") {
    location <- Africa.sf
  } else if (y == "Haiti") {
    location <- hti0.sf
  }

  G1 <- x %>%
    filter(maleART < 40)
  G2 <- x %>%
    filter(maleART > 40 & maleART < 50)
  G3 <- x %>%
    filter(maleART > 50 &  maleART < 60)
  G4 <- x %>%
    filter(maleART > 60 & maleART < 70)
  G5 <- x %>%
    filter(maleART > 70 & maleART < 80)
  G6 <- x %>%
    filter(maleART > 80 & maleART < 90)
  G7 <- x %>%
    filter(maleART > 90)
  
  map <- ggplot() +
    geom_sf(data = G1,
            fill = "#EEF6E9",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G2,
            fill = "#C8E4BF",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G3,
            fill = "#A2D39A",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G4,
            fill = "#76C375",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G5,
            fill = "#41AB5D",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G6,
            fill = "#258B45",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G7,
            fill = "#005A32",
            color = "#7f7f7f",
            linewidth = 0.1) +
    theme_void() +
    geom_sf(data = st_as_sf(location), 
            fill = NA, 
            color = "black", 
            linewidth = 0.5) +
    theme(legend.position = "none")
  
  return(map)
}


###NON-HAITIAN
maleART_main <-annoyingPaletteJustWorkMaleART(sf.noHTI1519,
                                   "Africa")


maleART_main

ggsave("output/maleART_main.png", dpi = 300, width = 12, height = 12)


###HAITI
maleART_HTI <-annoyingPaletteJustWorkMaleART(sf.HTI1519,
                                  "Haiti")

maleART_HTI

ggsave("output/maleART_HTI.png", dpi = 300, width = 4, height = 4)


#MALE PREVALENCE
annoyingPaletteJustWorkMalePrev <- function(x, y) {
  if (y == "Africa") {
    location <- Africa.sf
  } else if (y == "Haiti") {
    location <- hti0.sf
  }

  G1 <- x %>%
    filter(malePrev < 5)
  G2 <- x %>%
    filter(malePrev > 5 & malePrev < 10)
  G3 <- x %>%
    filter(malePrev > 10 & malePrev < 15)
  G4 <- x %>%
    filter(malePrev > 15 & malePrev < 20)
  G5 <- x %>%
    filter(malePrev > 20 & malePrev < 25)
  G6 <- x %>%
    filter(malePrev > 25 & malePrev < 30)
  G7 <- x %>%
    filter(malePrev > 30)
  
  map <- ggplot() +
    geom_sf(data = G1,
            fill = "#F0F3FA",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G2,
            fill = "#C7DBEE",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G3,
            fill = "#9ECAE1",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G4,
            fill = "#6AADD6",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G5,
            fill = "#4392C6",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G6,
            fill = "#2372B5",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G7,
            fill = "#194791",
            color = "#7f7f7f",
            linewidth = 0.1) +
    theme_void() +
    geom_sf(data = st_as_sf(location), 
            fill = NA, 
            color = "black", 
            linewidth = 0.5) +
    theme(legend.position = "none")
  
  return(map)
}
###NON-HAITIAN
malePrev_main <-annoyingPaletteJustWorkMalePrev(sf.noHTI1519,
                           "Africa")


malePrev_main

ggsave("output/malePrevalence_main.png", dpi = 300, width = 12, height = 12)


###HAITI
malePrev_HTI <-annoyingPaletteJustWorkMalePrev(sf.HTI1519,
                          "Haiti")

malePrev_HTI

ggsave("output/malePrevalence_HTI.png", dpi = 300, width = 4, height = 4)

#AGE OF SEXUAL DEBUT
annoyingPaletteJustWorksexualDebut <- function(x, y) {
  if (y == "Africa") {
    location <- Africa.sf
  } else if (y == "Haiti") {
    location <- hti0.sf
  }
  
  G1 <- x %>%
    filter(sexualDebut25_49 < 15)
  G2 <- x %>%
    filter(sexualDebut25_49 >= 15 & sexualDebut25_49 < 16)
  G3 <- x %>%
    filter(sexualDebut25_49 >= 16 & sexualDebut25_49 < 17)
  G4 <- x %>%
    filter(sexualDebut25_49 >= 17 & sexualDebut25_49 < 18)
  G5 <- x %>%
    filter(sexualDebut25_49 >= 18 & sexualDebut25_49 < 19)
  G6 <- x %>%
    filter(sexualDebut25_49 >= 19 & sexualDebut25_49 < 20)
  G7 <- x %>%
    filter(sexualDebut25_49 >= 20)
  
  map <- ggplot() +
    geom_sf(data = G1,
            fill = "#FFEDDF",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G2,
            fill = "#FED1A2",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G3,
            fill = "#FAAD6C",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G4,
            fill = "#F68C40",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G5,
            fill = "#F06A22",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G6,
            fill = "#D84C27",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G7,
            fill = "#8B301D",
            color = "#7f7f7f",
            linewidth = 0.1) +
    theme_void() +
    geom_sf(data = st_as_sf(location), 
            fill = NA, 
            color = "black", 
            linewidth = 0.5) +
    theme(legend.position = "none")
  
  return(map)
}
###NON-HAITIAN


sf.noHTI1519_noBWAorESW <- sf.noHTI1519 %>%
  filter(country != "BWA") %>%
  filter(country != "ESW")

sexualDebut_main <-annoyingPaletteJustWorksexualDebut(sf.noHTI1519_noBWAorESW,
                                   "Africa")

sexualDebut_main

ggsave("output/sexualDebut_main.png", dpi = 300, width = 12, height = 12)


###HAITI
sexualDebut_HTI <-annoyingPaletteJustWorksexualDebut(sf.HTI1519,
                                  "Haiti")

sexualDebut_HTI

ggsave("output/sexualDebut_HTI.png", dpi = 300, width = 4, height = 4)



#SEX RATIO
annoyingPaletteJustWorksSexRatio <- function(x, y) {
  if (y == "Africa") {
    location <- Africa.sf
  } else if (y == "Haiti") {
    location <- hti0.sf
  }
  
  G1 <- x %>%
    filter(sexRatio15_64 < 0.8)
  G2 <- x %>%
    filter(sexRatio15_64 > 0.8 & sexRatio15_64 < 0.9)
  G3 <- x %>%
    filter(sexRatio15_64 > 0.9 & sexRatio15_64 < 0.95)
  G4 <- x %>%
    filter(sexRatio15_64 > 0.95 & sexRatio15_64 < 1.05)
  G5 <- x %>%
    filter(sexRatio15_64 > 1.05 & sexRatio15_64 < 1.1)
  G6 <- x %>%
    filter(sexRatio15_64 > 1.1 & sexRatio15_64 < 1.2)
  G7 <- x %>%
    filter(sexRatio15_64 > 1.2)
  
  map <- ggplot() +
    geom_sf(data = G1,
            fill = "#D73127",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G2,
            fill = "#F68D5B",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G3,
            fill = "#FDDF90",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G4,
            fill = "#FDF8C1",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G5,
            fill = "#E0F3F8",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G6,
            fill = "#92BFDA",
            color = "#7f7f7f",
            linewidth = 0.1) +
    geom_sf(data = G7,
            fill = "#4575B5",
            color = "#7f7f7f",
            linewidth = 0.1) +
    theme_void() +
    geom_sf(data = st_as_sf(location), 
            fill = NA, 
            color = "black", 
            linewidth = 0.5) +
    theme(legend.position = "none")
  
  return(map)
}
###NON-HAITIAN
sexRatio_main <-annoyingPaletteJustWorksSexRatio(sf.noHTI1519,
                            "Africa")


sexRatio_main

ggsave("output/sexRatio_main.png", dpi = 300, width = 12, height = 12)


###HAITI
sexRatio_HTI <-annoyingPaletteJustWorksSexRatio(sf.HTI1519,
                           "Haiti")

sexRatio_HTI

ggsave("output/sexRatio_HTI.png", dpi = 300, width = 4, height = 4)

