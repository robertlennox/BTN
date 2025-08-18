#'
#' functions to get receiver and tagging metadata into session
#'
#' @name smoltify_otn for when you do not want to eliminate the dead fish
#' @import data.table
#' @import magrittr
#' @import dplyr
#' @import tidyr
#' @param meta is an object brought in from the tagging metadata using the gsheet::gsheet2tbl function
#' @param receivers is an object brought in from the receivers sheet using the gsheet::gsheet2tbl function
#' @export

if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))



smoltify_otn<-function(meta, receivers, detections) {ID <-  n_ID <-  key <- value <- Vendor <- Transmitter <- tagCodeType <- NULL
oid <- lon <- lat <- value <- start <- end <- NULL
Station <- Receiver <- Habitat <- dmy <- sensor <- SCIENTIFIC_NAME <- NULL
Angler <- Project <- fate <- fatedate <- TL <- NULL

meta <- meta %>%
  dplyr::rename(ID=.data$TAG_ID_CODE,
                Vendor=TAG_MANUFACTURER,
                dmy=UTC_RELEASE_DATE_TIME,
                eq_temp=temp_int,
                eq_depth=depth_int,
                eq_accel=accel_int,
                redeploy=REDEPLOY,
                rs="RELEASE_LOCATION",
                Spp="SCIENTIFIC_NAME",
                FL="LENGTH (m)",
                TL="LENGTH2 (m)",
                Sex=SEX,
                Transmitter=.data$TAG_MODEL) %>%
  dplyr::mutate(ID = as.numeric(.data$ID))

m<-meta %>%
  dplyr::distinct(
    .data$ID,
    .data$Vendor,
    .data$tagCodeType,
    .data$ID,
    .data$n_ID,
    .data$Transmitter,
    .data$redeploy,
    .data$eq_temp,
    .data$eq_depth,
    .data$eq_accel) %>%
  dplyr::mutate(n_ID = n_ID - 1) %>%
  dplyr::mutate(ID2 = dplyr::case_when(.data$n_ID == 0 ~ NA_real_, .data$n_ID > 0 ~ ID + 1)) %>%
  dplyr::mutate(ID3 = dplyr::case_when(.data$n_ID >=2 ~ .data$ID + 2, T ~ NA_real_)) %>%
  dplyr::mutate(ID4 = dplyr::case_when(.data$n_ID ==3 ~ .data$ID + 3, T ~ NA_real_)) %>%
  dplyr::mutate(ID3 = dplyr::case_when(.data$n_ID >=2 ~ .data$ID + 2, T ~ NA_real_)) %>%
  dplyr::mutate(oid = .data$ID) %>%
  dplyr::select(-.data$n_ID) %>%
  dplyr::select(Vendor, Transmitter, ID, ID2, ID3, ID4, oid, tagCodeType) %>%
  tidyr::gather(key, value, -tagCodeType, -Transmitter, -Vendor, -oid) %>%
  dplyr::select(oid, key, value) %>%
  arrange(value) %>%
  dplyr::rename(ID = oid) %>%
  right_join(meta %>%
               mutate(ID = as.numeric(.data$ID))) %>%
  dplyr::mutate(key = case_when(grepl("-AT",.data$Transmitter) & value - ID == 0 ~ "temp",
                                grepl("A-LP",.data$Transmitter) ~ "accel",
                                grepl("-AT", .data$Transmitter) & value - ID == 1 ~ "accel",
                                grepl("-DT", .data$Transmitter) & value - ID == 0 ~ "depth",
                                grepl("-DT", .data$Transmitter) & value - ID == 1 ~ "temp",
                                grepl("MT-", .data$Transmitter) & value - ID == 0 ~ "not eaten",
                                grepl("MT-", .data$Transmitter) & value - ID == 1 ~ "eaten",
                                grepl("MT-", .data$Transmitter) & value - ID == 2 ~ "temp",
                                grepl("MT-", .data$Transmitter) & value - ID == 3 ~ "temp2",
                                grepl("-T", .data$Transmitter) &  value - ID == 0 ~ "temp",
                                grepl("-P", .data$Transmitter) &  value - ID == 0 ~ "not eaten",
                                grepl("-P", .data$Transmitter) &  value - ID == 1 ~ "eaten",
                                grepl("-TAD", .data$Transmitter) & value - ID == 0 ~ "temp",
                                grepl("-TAD", .data$Transmitter) & value - ID == 1 ~ "accel",
                                grepl("-TAD", .data$Transmitter) & value - ID == 2 ~ "depth",
                                grepl("-DAT", .data$Transmitter) & value - ID == 0 ~ "depth",
                                grepl("-DAT", .data$Transmitter) & value - ID == 1 ~ "accel",
                                grepl("-DAT", .data$Transmitter) & value - ID == 2 ~ "temp",
                                grepl("-DTA", .data$Transmitter) & value - ID == 0 ~ "depth",
                                grepl("-DTA", .data$Transmitter) & value - ID == 1 ~ "temp",
                                grepl("-DTA", .data$Transmitter) & value - ID == 2 ~ "accel",
                                grepl("-R", .data$Transmitter) & value - ID == 0 ~ "range",
                                grepl("-D", .data$Transmitter) & value - ID == 0 ~ "depth",
                                !grepl("-", .data$Transmitter) ~ "ID")) %>%
  dplyr::filter(!is.na(.data$key) | .data$Vendor == "Vemco") %>%
  dplyr::rename(sensor = .data$key, oid = .data$ID,
                ID = .data$value) %>% mutate(dmy = lubridate::dmy(.data$dmy)) %>%
  mutate(fatedate = lubridate::parse_date_time(.data$fatedate,
                                               c("dmy", "dmy_HM")))

rec <-
  receivers %>% as_tibble %>% dplyr::filter(!is.na(.data$lon)) %>%
  sf::st_as_sf(., coords = c("lon", "lat")) %>% sf::st_set_crs(4326) %>%
  sf::st_transform(32633) %>% methods::as(., "Spatial") %>%
  as_tibble %>% dplyr::rename(lon = .data$coords.x1, lat = .data$coords.x2)

receiver_locations <-
  seq.Date(as.Date("2020-01-01"), as.Date(Sys.Date()),
           by = "day") %>% tidyr::expand_grid(., rec) %>% dplyr::mutate(end = dplyr::case_when(
             .data$end ==
               "" |
               is.na(.data$end) ~ Sys.Date(),
             T ~ lubridate::dmy(.data$end)
           )) %>%
  dplyr::rename(value = 1) %>% dplyr::filter(.data$value >
                                               lubridate::dmy(.data$start) &
                                               .data$value < .data$end) %>%
  dplyr::select(
    .data$value,
    .data$Receiver,
    .data$Station,
    .data$Habitat,
    .data$depth,
    .data$sync,
    .data$lon,
    .data$lat
  ) %>% dplyr::rename(dti = .data$value, serial = .data$Receiver)

redeploys <- m %>% dplyr::filter(.data$redeploy == T) %>%
  distinct(.data$ID, .data$dmy)

dets <- detections %>%
  dplyr::mutate(dti = lubridate::date(.data$dt)) %>%
  left_join(receiver_locations, by = c("serial", "dti")) %>%
  dplyr::left_join(
    m %>% dplyr::filter(is.na(redeploy)) %>%
      dplyr::mutate(ID = as.integer(ID)) %>%
      dplyr::select(
        ID,
        oid,
        tagCodeType,
        dmy,
        sensor,
        Spp,
        TL,
        FL,
        fate,
        fatedate,
        Sex,
        Project,
        Transmitter,
        rs,
        eq_temp,
        eq_depth,
        eq_accel,
        temp_slope,
        depth_slope,
        accel_slope
      ),
    by = c("ID", "tagCodeType")
  ) %>%
  dplyr::filter(lubridate::date(.data$dt) >= .data$dmy) %>%
  dplyr::filter(.data$dt < fatedate | is.na(.data$fatedate)) %>%
  bind_rows(
    detections %>% dplyr::filter(ID %in% redeploys$ID) %>%
      left_join(redeploys) %>% dplyr::filter(lubridate::date(.data$dt) >=
                                               dmy) %>% dplyr::select(-.data$dmy) %>% dplyr::mutate(dti = lubridate::date(.data$dt)) %>%
      left_join(receiver_locations, by = c("serial", "dti")) %>%
      dplyr::left_join(
        m %>% dplyr::filter(.data$redeploy ==
                              T) %>% dplyr::mutate(ID = as.integer(ID))  %>% dplyr::select(
                                ID,
                                oid,
                                tagCodeType,
                                dmy,
                                sensor,
                                Spp,
                                TL,
                                FL,
                                fate,
                                fatedate,
                                Sex,
                                Project,
                                Transmitter,
                                rs,
                                eq_temp,
                                eq_depth,
                                eq_accel,
                                temp_slope,
                                depth_slope,
                                accel_slope
                              ),
        by = c("ID", "tagCodeType")
      ) %>% dplyr::filter(lubridate::date(.data$dt) >=
                            .data$dmy) %>% dplyr::filter(.data$dt < fatedate |
                                                           is.na(.data$fatedate))
  )
dets <- dets %>% mutate(
  Data = case_when(
    eq_depth == 1000 &
      is.na(depth_slope) &
      sensor == "depth" ~ ((Data * 25) - 1000) * 0.01,
    eq_depth !=
      1000 &
      is.na(depth_slope) &
      sensor == "depth" ~ (Data / 255) * eq_depth,
    sensor ==
      "accel" &
      is.na(accel_slope) ~ (Data / 255) * eq_accel,
    sensor == "temp" & is.na(temp_slope) ~ (Data / 255) *
      eq_temp,
    sensor=="temp" & !is.na(temp_slope) ~ as.numeric(eq_temp) + (Data*as.numeric(temp_slope)),
    sensor=="depth" & !is.na(depth_slope) ~ as.numeric(eq_depth) + (Data*as.numeric(depth_slope)),
    sensor=="accel" & !is.na(accel_slope) ~ as.numeric(eq_accel) + (Data*as.numeric(accel_slope)),
    T ~ Data
  )
) %>% dplyr::select(-eq_temp,-eq_accel,-eq_depth, -temp_slope, -depth_slope, -accel_slope)

dets<-dets %>%
  dplyr::select(Project, dt, dt_utc, epo, frac, oid, ID, Spp, serial, x=lon, y=lat, temperature, noise, sensor, Data, TL, FL, dmy, fate, fatedate) %>%
  mutate(TL=as.numeric(TL))

return(dets)}
