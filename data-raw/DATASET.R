## code to prepare `DATASET` dataset goes here

setwd("C:/Users/rb501745/OneDrive - Dalhousie University/BTN/BTN/data-raw")

osterfjord<-sf::st_read("fjord.shp")
usethis::use_data(osterfjord, overwrite = TRUE)

vosso<-sf::st_read("Boniteringspolygon.shp")
usethis::use_data(vosso, overwrite = TRUE)

suldal<-sf::st_read("suldalmap.kml")
usethis::use_data(vosso, overwrite = TRUE)
