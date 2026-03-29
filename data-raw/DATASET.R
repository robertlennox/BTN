## code to prepare `DATASET` dataset goes here

require(sf)
sf::sf_use_s2(FALSE)
osterfjord<-readRDS("osterfjord.RDS")
saveRDS(osterfjord, "inst/extdata/osterfjord.rds")

