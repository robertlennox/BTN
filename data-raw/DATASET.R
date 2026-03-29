## code to prepare `DATASET` dataset goes here

require(sf)


sf::sf_use_s2(FALSE)
osterfjord<-readRDS("osterfjord.RDS")
saveRDS(osterfjord, "inst/extdata/osterfjord.rds")

vosso<-readRDS("vosso.RDS")
saveRDS(vosso, "inst/extdata/vosso.rds")


ost_bathy<-readRDS("osterfjord-bathy.RDS")
saveRDS(osterfjord-bathy, "inst/extdata/osterfjord-bathy.rds")


suldal<-readRDS("suldal.RDS")
saveRDS(suldal, "inst/extdata/suldal.rds")

aurland<-readRDS("aurland.RDS")
saveRDS(aurland, "inst/extdata/aurland.rds")

require(sf)
require(raster)
require(tidyverse)
storel<-readRDS("sl.RDS")
storel<-raster::rasterFromXYZ(storel)
storel[storel==0]<-NA
crs(storel)<-"+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
storel<-projectRaster(storel, crs="+proj=utm +zone=33 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

saveRDS(storel, "inst/extdata/storel.rds")


isfjorden<-readRDS("isfjorden.RDS")

saveRDS(isfjorden, "inst/extdata/isfjorden.rds")

orkla<-readRDS("JOdata.RDS")

usethis::use_data(orkla, overwrite=T)

otter<-readRDS("otter.RDS")

usethis::use_data(otter, overwrite=T)

require(tidyverse)
require(sf)
require(rnaturalearth)
require(rnaturalearthhires)
no<-rnaturalearthhires::countries10 %>%
  dplyr::filter(SOV_A3=="NOR") %>%
  st_as_sf() %>%
  st_crop(ymin=57, ymax=72, xmin=5, xmax=32)

usethis::use_data(no, overwrite=T)

radio<-readRDS("salmon-radio.RDS")
usethis::use_data(radio, overwrite=T)

oxytrout<-readRDS("acceleration_MO2hrkg.RDS")
usethis::use_data(oxytrout, overwrite=T)

pace<-readRDS("pace-wp1.RDS")
usethis::use_data(pace, overwrite=T)

isfjorden2<-readRDS("newisfjorden.RDS")
saveRDS(isfjorden2, "inst/extdata/isfjorden2.rds")




