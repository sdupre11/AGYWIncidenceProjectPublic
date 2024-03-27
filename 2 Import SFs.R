library(tidyverse)
library(geojsonsf)


get_naomi_geographies <- function(iso3, destdir) {
  f_name <- paste0(iso3, ".geojson")
  
  download.file(paste0("https://naomi2023.azurewebsites.net/api/v1/boundaries?country=",
                       toupper(iso3)), 
                paste0("data/geo-data/geojson/unclean/",
                       f_name))
  
}

get_naomi_geographies("ANG")
get_naomi_geographies("BDI")
get_naomi_geographies("BWA")
get_naomi_geographies("CIV")
get_naomi_geographies("CMR")
get_naomi_geographies("COG")
get_naomi_geographies("ESW")
get_naomi_geographies("HTI")
get_naomi_geographies("KEN")
get_naomi_geographies("LSO")
get_naomi_geographies("MOZ")
get_naomi_geographies("MWI")
get_naomi_geographies("NAM")
get_naomi_geographies("NER")
get_naomi_geographies("RWA")
get_naomi_geographies("ESW")
get_naomi_geographies("TGO")
get_naomi_geographies("TZA")
get_naomi_geographies("UGA")
get_naomi_geographies("ZAF")
get_naomi_geographies("ZMB")
get_naomi_geographies("ZWE")


clean_downloaded_geojsons <- function() {
  geojson_files <- paste0("data/geo-data/geojson/unclean/", 
                          list.files("data/geo-data/geojson/unclean/", 
                                     pattern = "*.geojson$", 
                                     recursive = TRUE))
  
  sapply(geojson_files, function(geojson) {
    geojson_string <- read_file(geojson)
    
    i <- regexpr("(?<=unclean/).{3}(?=.geojson)", 
                 geojson, 
                 perl = TRUE)
    
    iso3 <- tolower(substr(geojson, 
                           i, 
                           i + 2))
    
    out_path <- paste0("data/geo-data/geojson/clean/", iso3, ".geojson")
    
    file_exists <- grepl(paste0(iso3, ".geojson"), list.files("data/geo-data/geojson/clean/"), 
                         fixed = TRUE)
    
    if (!any(file_exists)) {
      out_string <- gsub("^.{0,7}", "", 
                         gsub(".{0,1}$", "", geojson_string))
      
      writeLines(out_string, 
                 file(out_path))
    }
  }) 
}



clean_downloaded_geojsons()

