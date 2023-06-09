## code to prepare `DATASET` dataset goes here

setwd("C:/Users/rb501745/OneDrive - Dalhousie University/BTN/BTN/data-raw")

osterfjord<-sf::st_read("fjord.shp")
usethis::use_data(osterfjord, overwrite = TRUE) %>%
  sf::st_transform(32633)

vosso<-sf::st_read("Boniteringspolygon.shp")
usethis::use_data(vosso, overwrite = TRUE) %>%
  sf::st_transform(32633)

suldal<-sf::st_read("suldalmap.kml") %>% dplyr::select(Name)
usethis::use_data(vosso, overwrite = TRUE)

aurland<-sf::st_read("aurland.shp")
usethis::use_data(aurland, overwrite = TRUE)

storel<-readRDS("sl.RDS")
usethis::use_data(storel, overwrite = T)

