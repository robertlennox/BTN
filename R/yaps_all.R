#'
#' function to read in and manipulate Thelma DB
#'
#' @name hatch
#' @import data.table
#' @import magrittr
#' @import dplyr
#' @import tidyr
#' @param fish_detecions is the object containing the YAPS formatted fish detections
#' @param sync_model is the chosen synchronization model from YAPS
#' @param transmitter_ID is the transmitter ID desired to be positioned
#' @param rbi_min is the minimum time of the tag transmissions, default is 60
#' @param rbi_max is the maximim time of the tag transmissions, default is 120
#' @param fish_detecions is the object containing the YAPS formatted fish detections
#' @param nruns is the number of YAPS fits desired, default is 5

#' @examples
#' yaps_all(fish_detections=dets, sync_model=sync, transmitter_ID=4000)
#'
#' @export

yaps_all<-function(fish_detections, sync_model, transmitter_ID,
                   rbi_min=60, rbi_max=120, nruns=5){
  tryCatch({

    dat <- applySync(toa=fish_detections %>% # sync model for one download
                       dplyr::filter(lubridate::date(ts)==date) %>%
                       droplevels() %>%
                       data.table::setDT %>%
                       split(.$tag) %>%
                       purrr::pluck(transmitter_ID),
                     hydros=hydros,
                     sync_model=sync_model)

    hydros_yaps <- data.table::data.table(sync_model$pl$TRUE_H)
    colnames(hydros_yaps) <- c('hx','hy','hz')

    toa <- yaps::getToaYaps(synced_dat=dat,
                      hydros=hydros_yaps,
                      pingType='rbi',
                      rbi_min=rbi_min,
                      rbi_max=rbi_max)

    ####
    # YAPS
    ##

    magicYAPS<-function(x){
      tryCatch({yaps::runYaps(
        getInp(hydros_yaps,
               toa,
               E_dist="Mixture",
               n_ss=2,
               pingType="rbi",
               sdInits=1,
               rbi_min=rbi_min,
               rbi_max=rbi_max,
               ss_data_what="est",
               bbox=NULL),
        silent=F,
        tmb_smartsearch=TRUE,
        maxIter=5000)},
        error=function(e){NA})}

    YAPS_list<-nruns %>%
      rerun(magicYAPS())

    magic<-YAPS_list %>%  # the magic number
      purrr::map(purrr::pluck(4)) %>% # get the AIC cols
      purrr::map(purrr::pluck(1)) %>% # take the number
      bind_cols() %>% # make a df
      t() %>% # oops wrong order
      as_tibble %>% # obv
      dplyr::filter(V1==min(V1)) %>% # get the min val
      as.numeric# make it a number

    final_track<-YAPS_list %>%
      purrr::discard(., ~any(is.na(.x))) %>%
      purrr::compact() %>%
      purr::keep(., as_mapper(~.x$obj %>% # keep only the run with the lowest AIC
                          purrr::pluck(1) == magic)) %>%
      purrr::pluck(1) %>%
      purrr::pluck(8) %>%
      tidyr::as_tibble %>%
      dplyr::mutate(dat %>% dplyr::distinct(tag))},
    error=function(e){NA})
}
