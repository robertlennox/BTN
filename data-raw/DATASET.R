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

require(sf)
require(raster)
require(tidyverse)
storel<-readRDS("sl.RDS")
storel<-raster::rasterFromXYZ(storel)
storel[storel==0]<-NA
crs(storel)<-"+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
storel<-projectRaster(storel, crs="+proj=utm +zone=33 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

usethis::use_data(storel, overwrite = T)

isfjorden<-readRDS("isfjorden.RDS")

usethis::use_data(isfjorden, overwrite=T)

orkla<-readRDS("JOdata.RDS")

usethis::use_data(orkla, overwrite=T)

