## code to prepare `DATASET` dataset goes here

setwd("C:/Users/rb501745/OneDrive - Dalhousie University/BTN/BTN/data-raw")

require(sf)
sf::sf_use_s2(FALSE)
osterfjord<-readRDS("osterfjord.RDS")
saveRDS(osterfjord, "inst/extdata/osterfjord.rds")

