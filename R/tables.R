#' Print a table of data
#'
#' @param dfo
#' @param caption
#' @param SigF
#' @param secondRow
#' @param thirdRow
#' @param save
#' @param fileout
#'
#' @return
#' @export
#'
#' @examples
print_table <- function(dfo, SigF = NULL, caption = NULL, hline.after = c(0),
                        secondRow = NULL, thirdRow = NULL,
                        save = FALSE, fileout = "table_output.tex", ...) {

  # Create the xtable
  out_table <- xtable::xtable(dfo, caption = caption)

  # align all colums to be centered
  xtable::align(out_table) <- rep("c", ncol(dfo)+1)

  # change number of Significant Figures if defined
  if(!is.null(SigF))
    xtable::digits(out_table) <- SigF

  # Add additional header rows as required
  add.to.row <- list(pos = list(0), command = NULL)

  if(!is.null(secondRow)) {
    secondRowAdd <- paste0(paste(secondRow, collapse=' & '), " \\\\ \n")
  } else {
    secondRowAdd <- NULL
  }
  if(!is.null(thirdRow)) {
    thirdRowAdd <- paste0(paste(thirdRow, collapse=' & '), " \\\\ \n")
  } else {
    thirdRowAdd <- NULL
  }

  # Comile the command for printing the table
  command <- paste0(secondRowAdd,
                    thirdRowAdd,
                    "\\hline\n\\endfirsthead\n",
                    paste0(paste(colnames(dfo), collapse=' & '), " \\\\ \n"),
                    secondRowAdd,
                    thirdRowAdd,
                    "\\hline\n\\endhead\n",
                    "\\hline\n",
                    "\\multicolumn{", ncol(dfo), "}{l}",
                    "{\\footnotesize Continued on next page}\n",
                    "\\endfoot\n",
                    "\\endlastfoot\n")

  add.to.row$command <- command

  # Print the table
  out <- xtable::print.xtable(out_table, hline.after=hline.after, add.to.row = add.to.row,
                              tabular.environment = "longtable",
                              floating = FALSE,
                              include.rownames = FALSE,
                              caption.placement = 'top',
                              comment = FALSE, ...)
  if(save) {
    fileout <- gsub(" ","",fileout)
    fileConn<-file(fileout)
    writeLines(out, fileConn)
    close(fileConn)
  }

}


#' Make a formated neuston table
#'
#' @param df
#' @param caption
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
table_neuston <- function(df, caption = NULL, ...) {

  dfo <- dplyr::select(df,station, dttm_in,lat, lon,
                       temp, sal, fluor, tow_dist, biomass, biodens)
  dfo <- dplyr::mutate(dfo,
                       dttm_in = format(dfo$dttm_in,"%Y-%m-%d %H:%M"),
                       biodens = biodens * 1000)
  dfo <- tibble::add_column(dfo,
                            moon = paste0(df$moon_phase*100,"% (",stringr::str_to_title(df$moon_mode),")"),
                            .after = 4)


  colnames(dfo) <- c("Station","Time","Lat","Lon","Moon Phase","Temperature","Salinity","Chl-a","Tow Area","Zooplankton","Zooplankton")
  secondRow <-c("","[local]","[$^\\circ$N]","[$^\\circ$E]","[\\% full]","[$^\\circ$C]","","Fluoroesence","[m$^2$]","Biovolume","Density")
  thirdRow <- c("","","","","","","","[Volts]","","[mL]","[$\\mu$L/km$^2$]")
  SigF <- c(20,20,20,2,2,20,1,2,2,0,1,2)

  print_table(dfo,caption=caption,SigF=SigF,secondRow=secondRow,thirdRow=thirdRow,...)

}


#' Make formated table of Neuston nekton
#'
#' @param df
#' @param caption
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
table_nekton <- function(df, caption = NULL, ...) {

  dfo <- dplyr::select(df, station,
                       phyl_num, lept_num, halo_num, myct_num,
                       tar, nekt_total_vol, gelat_vol, plas_density,
                       sarg_piece_tot,sarg_mass_tot)

  colnames(dfo) <- c("Station","Phyl","Lept","Halo","Myct","Tar","Nekton > 2 cm","Gelatinous > 2cm","Plastic Density","Sargassum Pieces","Sargassum Mass")
  secondRow <- c("","[\\#]","[\\#]","[\\#]","[\\#]","[\\#]","[mL]","[mL]","[\\#/km$^2$]","[\\#]","[g]")
  SigF <- c(20,20,0,0,0,0,0,1,1,0,0,0)

  print_table(dfo,caption=caption,SigF=SigF,secondRow=secondRow,...)

}


#' Make a Hydrowork Table
#'
#' @param df
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
table_hydro <- function(df, caption = NULL, ...) {

  dfo <- dplyr::select(df,station, dttm, lon,lat, bottle, z, no3, po4, sio2, pH, alk, chla, temp, sal)
  dfo <- dplyr::mutate(dfo, dttm = format(dfo$dttm,"%Y-%m-%d %H:%M"))

  rep_station <- duplicated(dfo$station)
  dfo$station[rep_station] <- NA
  dfo$dttm[rep_station] <- NA
  dfo$lon[rep_station] <- NA
  dfo$lat[rep_station] <- NA

  hline_vec <- c(which(rep_station==F)-1,nrow(dfo))

  colnames(dfo) <- c('Station','Time',"Lat","Lon",'Bottle','Depth','NO$_3^{-1}$','PO$_4^{-3}$','Si0$_2^-2$','pH','Alk','Chl-a','Temp','Sal')
  secondRow <- c('','(local)',"$^\\circ$N","$^\\circ$E","",'[m]','[$\\mu$M]','[$\\mu$M]','[$\\mu$M]','','','[mg/L]','[$^\\circ$C]','')
  SigF <- c(20,20,2,2,20,0,2,2,2,2,2,3,1,2)

  emptyCols <- colSums(is.na(dfo)) == nrow(dfo)
  dfo<- dfo[!emptyCols]
  secondRow <- secondRow[!emptyCols]
  SigF <- SigF[!emptyCols]
  SigF <- append(20,SigF)

  print_table(dfo, SigF=SigF, secondRow=secondRow, caption = caption, hline.after = hline_vec, ...)

}



