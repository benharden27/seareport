
#' Create a station summary sheet from packaged data
#'
#' @param output
#'
#' @return
#' @export
#'
#' @examples
create_summary <- function(data) {

  cruiseID <- stringr::str_extract(data$neuston$station,".*(?=-[0-9]{3})")[1]

  neuston <- tibble::tibble(station = stringr::str_extract(data$neuston$station,".*(?=-NT)"),
                            nlon = data$neuston$lon,
                            nlat = data$neuston$lat,
                            ndttm = data$neuston$dttm)

  ii <- which(!duplicated(data$hydro$station))
  hydro <- tibble::tibble(station = stringr::str_extract(data$hydro$station[ii],".*(?=-HC)"),
                          hlon = data$hydro$lon[ii],
                          hlat = data$hydro$lat[ii],
                          hdttm = data$hydro$dttm[ii])


  ctd <- tibble::tibble(station = paste0(cruiseID,"-",
                                         stringr::str_replace_all(
                                           format(sea::get_ctd_meta(data$ctd),
                                                  trim = F,width=3),
                                           " ","0")),
                        clon = sea::get_ctd_meta(data$ctd,"longitude"),
                        clat = sea::get_ctd_meta(data$ctd,"latitude"),
                        cdttm = lubridate::as_datetime(
                          sea::get_ctd_meta(data$ctd,"time")))



  all_stat <- dplyr::full_join(neuston,hydro,by = "station")
  all_stat <- dplyr::full_join(all_stat,ctd, by = "station")

  all_stat <- dplyr::arrange(all_stat,station)

  dttm <- rep(NA,nrow(all_stat))
  for (i in 1:nrow(all_stat)) {
    if(is.na(all_stat$ndttm[i]) & !is.na(all_stat$cdttm[i])) {
      dttm[i] <- all_stat$cdttm[i]
    } else if (!is.na(all_stat$ndttm[i]) & is.na(all_stat$cdttm[i])) {
      dttm[i] <- all_stat$ndttm[i]
    } else if (!is.na(all_stat$ndttm[i]) & !is.na(all_stat$cdttm[i])) {
      dttm[i] <- min(c(all_stat$ndttm[i],all_stat$cdttm[i]))
    }
  }

  dttm <- lubridate::as_datetime(dttm)

  all_stat <- dplyr::transmute(all_stat, station, dttm = dttm,
                               ctd = !is.na(cdttm) & is.na(hdttm),
                               hc = !is.na(hdttm),
                               nt = !is.na(ndttm))

}
