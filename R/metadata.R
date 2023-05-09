#'
#' functions to get receiver and tagging metadata into session
#'
#' @name smoltify
#' @import data.table
#' @import magrittr
#' @import dplyr
#' @import tidyr
#' @param meta is an object brought in from the tagging metadata
#' @param receivers is an object brought in from the receivers sheet
#' @param detections is an object derived from the read_tb function
#' @export

if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

smoltify<-function(meta, receivers, detections) {
  ID <- n_ID <- key <- value <- Vendor <- Transmitter <- tagCodeType <- NULL
  oid <- lon <- lat <- value <- start <- end <- NULL
  Station <- Receiver <- Habitat <- dmy <- sensor <- Spp <- NULL
  Angler <- Project <- fate <- fatedate <- TL <- NULL


  m<-meta %>%
    dplyr::mutate(ID=as.numeric(.data$ID)) %>%
    dplyr::distinct(.data$Vendor, .data$tagCodeType, .data$ID, .data$n_ID) %>%
    dplyr::mutate(n_ID=n_ID-1) %>%
    dplyr::mutate(ID2=dplyr::case_when(.data$n_ID==0 ~ NA_real_,
                                       .data$n_ID>0 ~ ID+1)) %>%
    dplyr::mutate(ID3=dplyr::case_when(.data$n_ID>=2 ~ .data$ID+2,
                                       T~NA_real_)) %>%
    dplyr::mutate(ID4=dplyr::case_when(.data$n_ID==3 ~ .data$ID+3,
                                       T~NA_real_)) %>%
    dplyr::mutate(ID3=dplyr::case_when(.data$n_ID>=2 ~ .data$ID+2,
                                       T~NA_real_)) %>%
    dplyr::mutate(oid=.data$ID) %>%
    dplyr::select(-.data$n_ID) %>%
    tidyr::gather(key, value, -oid, -Vendor) %>%
    dplyr::rename(ID=oid) %>%
    right_join(meta %>% mutate(ID=as.numeric(.data$ID))) %>%
    dplyr::mutate(key=case_when(grepl("-AT", .data$Transmitter) &
                                  value-ID==0 ~ "temp",
                                grepl("A-LP", .data$Transmitter) ~ "accel",
                                grepl("-AT", .data$Transmitter) &
                                  value-ID==1 ~ "accel",
                                grepl("-DT", .data$Transmitter) &
                                  value-ID==0 ~ "depth",
                                grepl("-DT", .data$Transmitter) &
                                  value-ID==1 ~ "temp",
                                grepl("MT-", .data$Transmitter) &
                                  value-ID==0 ~ "not eaten",
                                grepl("MT-", .data$Transmitter) &
                                  value-ID==1 ~ "eaten",
                                grepl("MT-", .data$Transmitter) &
                                  value-ID==2 ~ "temp",
                                grepl("MT-", .data$Transmitter) &
                                  value-ID==3 ~ "temp2",
                                grepl("-T", .data$Transmitter) &
                                  value-ID==0 ~ "temp",
                                grepl("-P", .data$Transmitter) &
                                  value-ID==0 ~ "not eaten",
                                grepl("-P", .data$Transmitter) &
                                  value-ID==1 ~ "eaten",
                                grepl("-ADT", .data$Transmitter) &
                                  value-ID==0 ~ "temp",
                                grepl("-ADT", .data$Transmitter) &
                                  value-ID==1 ~ "accel",
                                grepl("-ADT", .data$Transmitter) &
                                  value-ID==2 ~ "depth",
                                grepl("-DAT", .data$Transmitter) &
                                  value-ID==0 ~ "depth",
                                grepl("-DAT", .data$Transmitter) &
                                  value-ID==1 ~ "accel",
                                grepl("-DAT", .data$Transmitter) &
                                  value-ID==2 ~ "temp",
                                grepl("-R", .data$Transmitter) &
                                  value-ID==0 ~ "range",
                                grepl("-D", .data$Transmitter) &
                                  value-ID==0 ~ "depth",
                                !grepl("-", .data$Transmitter)~"ID")) %>%
    dplyr::filter(!is.na(.data$key) | .data$Vendor=="Vemco") %>%
    dplyr::rename(sensor=.data$key, oid=.data$ID, ID=.data$value) %>%
    mutate(.data$dmy=lubridate::dmy(.data$dmy)) %>%
    mutate(.data$fatedate=lubridate::parse_date_time(.data$fatedate, c("dmy", "dmy_HM")))

  rec<-receivers %>%
    as_tibble %>%
    dplyr::filter(!is.na(.data$lon)) %>%
    sf::st_as_sf(., coords=c("lon", "lat")) %>%
    sf::st_set_crs(4326) %>%
    sf::st_transform(32633) %>%
    as(., "Spatial") %>%
    as_tibble %>%
    dplyr::rename(lon=coords.x1, lat=coords.x2)

  receiver_locations<-seq.Date(as.Date("2020-01-01"),
                               as.Date(Sys.Date()), by="day") %>%
    tidyr::expand_grid(., rec) %>%
    dplyr::mutate(end=dplyr::case_when(end=="" |
                                         is.na(.data$end)~ Sys.Date(), T~lubridate::dmy(.data$end))) %>%
    dplyr::rename(value=1) %>%
    dplyr::filter(.data$value>lubridate::dmy(.data$start) & .data$value<.data$end) %>%
    dplyr::select(.data$value, .data$Receiver, .data$Station, .data$Habitat,
                  .data$depth, .data$sync, .data$lon, .data$lat) %>%
    dplyr::rename(dti=.data$value, serial=.data$Receiver)

  dets<-detections %>%
    dplyr::mutate(dti=lubridate::date(.data$dt)) %>%
    left_join(receiver_locations, by=c("serial", "dti")) %>%
    dplyr::left_join(m %>%
                       dplyr::mutate(ID=as.integer(ID)) %>%
                       dplyr::rename(Project=5) %>%
                       dplyr::select(ID, oid, tagCodeType, dmy, sensor, Spp, TL, Angler,
                                     fate, fatedate,
                                     Project, Transmitter, "Capture site", "Release Site"),
                     by=c("ID", "tagCodeType"))
  return(dets)

}





