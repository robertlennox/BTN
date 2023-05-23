#'
#' hatch is a function developed with code partly contributed by Eli Pickholtz to read in and manipulate Thelma DB files
#'
#' @name hatch
#' @import data.table
#' @import magrittr
#' @import dplyr
#' @import tidyr
#' @import DBI
#' @param tbdb_file is the name of the thelma database e.g. "PACE.tbdb"

hatch = function(tbdb_file) {
  tbrSerialNo <- tagID <- epo <- frac <- usTimestamp <- tagSNR <- tagCodeType <- NULL
  temperature <- tbrSerialNo <- ambientNoise <- ambientNoisePeak <- secTimestampUTC  <- NULL
  tagData <- dt_utc <- dt_sec_utc  <- NULL
con = DBI::dbConnect(RSQLite::SQLite(), dbname = tbdb_file)

  #To see all available tables:
  #print(DBI::dbListObjects(con))

  detections = data.table::setDT(DBI::dbReadTable(con, 'TagReadings'))[,
                                                                       .(serial=tbrSerialNo,
                                                                         ID=tagID,
                                                                         epo = secTimestampUTC,
                                                                         Data=tagData,
                                                                         tagCodeType,
                                                                         frac=usTimestamp/1000000,
                                                                         SNR=tagSNR
                                                                       )][,
                                                                          dt_utc := as.POSIXct(epo, origin = '1970-01-01', tz = 'UTC')]


  sensors = data.table::setDT(DBI::dbReadTable(con, 'TbrSensorReadings'))[,
                                                                          .(serial=tbrSerialNo,
                                                                            dt_sec_utc = secTimestampUTC,
                                                                            temperature = (temperature - 50) / 10,
                                                                            noise=ambientNoise,
                                                                            peak=ambientNoisePeak)
  ][, dt_utc := as.POSIXct(dt_sec_utc, origin = '1970-01-01', tz = 'UTC')]

  DBI::dbDisconnect(con)

  dets<- detections %>%
    dplyr::mutate(dt=lubridate::with_tz(.data$dt_utc, "Europe/Oslo")) %>%
    dplyr::select(.data$dt, .data$dt_utc, .data$epo, .data$frac,
                  .data$serial, .data$tagCodeType, .data$ID, .data$Data) %>%
    dplyr::mutate(dti=lubridate::round_date(.data$dt, "10 mins")) %>%
    dplyr::left_join(sensors %>%
                       dplyr::mutate(dt=lubridate::with_tz(.data$dt_utc, "Europe/Oslo")) %>%
                       dplyr::select(.data$dt, .data$serial, .data$temperature, .data$noise) %>%
                       dplyr::mutate(dti=lubridate::round_date(.data$dt, "10 mins")) %>%
                       dplyr::select(-.data$dt),
                     by=c("dti", "serial")) %>%
    dplyr::select(-.data$dti)
  list(d=dets)

  beepr::beep(2)
  return(dets)

}


