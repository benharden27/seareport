#' Create a station summary sheet from packaged data
#'
#' @param output
#'
#' @return
#' @export
#'
#' @examples
create_summary <- function(data, cruiseID = NULL, tz = 0, inc_ss = FALSE) {

  if (is.null(cruiseID)) {
    cruiseID <- stringr::str_extract(data$neuston$station,".*(?=-[0-9]{3})")[1]
  }

  neuston <- tibble::tibble(station = stringr::str_extract(data$neuston$station,".*(?=-NT)"),
                            nlon = data$neuston$lon,
                            nlat = data$neuston$lat,
                            ndttm = data$neuston$dttm_in)

  ii <- which(!duplicated(data$hydro$station))
  hydro <- tibble::tibble(station = stringr::str_extract(data$hydro$station[ii],".*(?=-HC)"),
                          hlon = data$hydro$lon[ii],
                          hlat = data$hydro$lat[ii],
                          hdttm = data$hydro$dttm[ii])


  ctd <- tibble::tibble(station = paste0(cruiseID,"-",stringr::str_replace_all(format(sea::get_ctd_meta(data$ctd),trim = F,width=3)," ","0")),
                        clon = sea::get_ctd_meta(data$ctd,"longitude"),
                        clat = sea::get_ctd_meta(data$ctd,"latitude"),
                        cdttm = lubridate::as_datetime(sea::get_ctd_meta(data$ctd,"time")))


  for (i in 1:nrow(ctd)) {
    ctd$cdttm[i] <- data$elg$dttm[which.min(oce::geodDist(data$elg$lon,data$elg$lat,ctd$clon[i],ctd$clat[i]))+60*24] + tz * 60 * 60
  }

  all_stat <- dplyr::full_join(neuston, hydro, by = "station")
  all_stat <- dplyr::full_join(all_stat, ctd, by = "station")
  if(inc_ss) {
    ss <- tibble::tibble(station = paste0(cruiseID,"-",stringr::str_extract(data$surfsamp$station,".*(?=-[A-Za-z])")),
                         slon = data$surfsamp$lon,
                         slat = data$surfsamp$lat,
                         sdttm = data$surfsamp$dttm_local)

  } else {
    ss <- tibble::tibble(station = "dummy",
                         slon = NA,
                         slat = NA,
                         sdttm = NA)
  }
  all_stat <- dplyr::full_join(all_stat, ss, by = "station")
  all_stat <- all_stat[all_stat$station!="dummy", ]
  all_stat <- dplyr::arrange(all_stat, station)
  all_stat <- dplyr::rowwise(all_stat) %>% dplyr::mutate(dttm = min(ndttm,cdttm,hdttm,sdttm,na.rm = T),
                                                         i = which.min(c(ndttm,cdttm,hdttm,sdttm)))
  all_stat <- dplyr::mutate(all_stat, lon = NA, lat = NA)
  for (i in 1:nrow(all_stat)) {
    all_stat$lon[i] <- dplyr::select(all_stat[i,],nlon,clon,hlon,slon)[[all_stat[i,]$i]]
    all_stat$lat[i] <- dplyr::select(all_stat[i,],nlat,clat,hlat,slat)[[all_stat[i,]$i]]
  }



#   dttm <- lon <- lat <- rep(NA,nrow(all_stat))
#   for (i in 1:nrow(all_stat)) {
#     if (!is.na(all_stat$ndttm[i]) | !is.na(all_stat$cdttm[i]) | !is.na(all_stat$hdttm[i])) {
#       dttm[i] <- min(c(all_stat$ndttm[i],all_stat$cdttm[i],all_stat$hdttm[i]),na.rm = T)
#       ii <- which.min(c(all_stat$ndttm[i],all_stat$cdttm[i],all_stat$hdttm[i]))
#       if (ii == 1) {
#         lon[i] <- all_stat$nlon[i]
#         lat[i] <- all_stat$nlat[i]
#       } else if (ii == 2) {
#         lon[i] <- all_stat$clon[i]
#         lat[i] <- all_stat$clat[i]
#       } else if (ii == 3) {
#         lon[i] <- all_stat$hlon[i]
#         lat[i] <- all_stat$hlat[i]
#       } else {
#         lon[i] <- all_stat$slon[i]
#         lat[i] <- all_stat$slat[i]
#       }
#     }
#   }
#
# #
# #   for (i in 1:nrow(all_stat)) {
# #     if(is.na(all_stat$ndttm[i]) & !is.na(all_stat$cdttm[i])) {
# #       dttm[i] <- all_stat$cdttm[i]
# #       lon[i] <- all_stat$clon[i]
# #       lat[i] <- all_stat$clat[i]
# #     } else if (!is.na(all_stat$ndttm[i]) & is.na(all_stat$cdttm[i])) {
# #       dttm[i] <- all_stat$ndttm[i]
# #       lon[i] <- all_stat$nlon[i]
# #       lat[i] <- all_stat$nlat[i]
# #     } else if (!is.na(all_stat$ndttm[i]) & !is.na(all_stat$cdttm[i])) {
# #       dttm[i] <- min(c(all_stat$ndttm[i],all_stat$cdttm[i]))
# #       ii <- which.min(c(all_stat$ndttm[i],all_stat$cdttm[i]))
# #       if (ii == 1) {
# #         lon[i] <- all_stat$nlon[i]
# #         lat[i] <- all_stat$nlat[i]
# #       } else {
# #         lon[i] <- all_stat$clon[i]
# #         lat[i] <- all_stat$clat[i]
# #       }
# #     }
# #   }

  # dttm <- lubridate::as_datetime(dttm)


  if(inc_ss) {
    all_stat <- dplyr::transmute(all_stat, station, dttm = dttm,
                               lon = lon, lat = lat,
                               ctd = !is.na(cdttm) & is.na(hdttm),
                               hc = !is.na(hdttm),
                               nt = !is.na(ndttm),
                               ss = !is.na(sdttm))

  } else {
    all_stat <- dplyr::transmute(all_stat, station, dttm = dttm,
                                 lon = lon, lat = lat,
                                 ctd = !is.na(cdttm) & is.na(hdttm),
                                 hc = !is.na(hdttm),
                                 nt = !is.na(ndttm))
  }

  return(all_stat)

}
