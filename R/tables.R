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
                              comment = FALSE,
                              sanitize.text.function = function(x) {x},
                              ...)
  if(save) {
    fileout <- gsub(" ","",fileout)
    fileConn<-file(fileout)
    writeLines(out, fileConn)
    close(fileConn)
  }

}


#' Plot a table summary
#'
#' @param file
#' @param caption
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
table_summary <- function(file, caption = NULL, ...) {

  dfa <- sea::read_datasheet(file)
  df <- sea::read_datasheet(file,skip=1)
  names(df) <- names(dfa)
  names(df) <- stringr::str_replace_all(names(df)," ",".")

  xi <- sort(append(grep('Meter.Net',names(df)),grep('Meter.Net',names(df))+1))
  df$Meter.Net <- substr(gsub('[0-9]','X',gsub('0','',as.character(rowSums((!is.na(df[,xi]))+0)))),1,1)
  xi <- sort(append(grep('Secchi.Disk',names(df)),grep('Secchi.Disk',names(df))+1))
  df$Secchi.Disk <- substr(gsub('[0-9]','X',gsub('0','',as.character(rowSums((!is.na(df[,xi]))+0)))),1,1)
  xi <- sort(append(grep('Hydrocast',names(df)),grep('Hydrocast',names(df))+1))
  df$Hydrocast <- substr(gsub('[0-9]','X',gsub('0','',as.character(rowSums((!is.na(df[,xi])&df[,xi]!='')+0)))),1,1)
  xi <- sort(append(grep('Free.CTD',names(df)),grep('Free.CTD',names(df))+1))
  df$Free.CTD <- substr(gsub('[0-9]','X',gsub('0','',as.character(rowSums((!is.na(df[,xi]))+0)))),1,1)
  xi <- sort(append(grep('RBR',names(df)),grep('RBR',names(df))+1))
  if(any(!is.na(df[,xi]))) {
    # for (i in xi) {
    #   df[nchar(df[,i])==0,i] <- NA
    # }
    df$RBR <- substr(gsub('[0-9]','X',gsub('0','',as.character(rowSums((!is.na(df[,xi]))+0)))),1,1)
  } else {
    df$RBR <- rep('',nrow(df))
  }
  xi <- sort(append(grep('Shipek',names(df)),grep('Shipek',names(df))+1))
  df$Shipek.Grab <- substr(gsub('[0-9]','X',gsub('0','',as.character(rowSums((!is.na(df[,xi]))+0)))),1,1)
  xi <- sort(append(grep('Phyto',names(df)),grep('Phyto',names(df))+1))
  df$Phyto.Net <- substr(gsub('[A-Z0-9]','X',gsub('0','',as.character(rowSums((!is.na(df[,xi]))+0)))),1,1)
  xi <- sort(append(grep('Hydrophone',names(df)),grep('Hydrophone',names(df))+1))
  df$hydrophone <- substr(gsub('[A-Z0-9]','X',gsub('0','',as.character(rowSums((!is.na(df[,xi]))+0)))),1,1)

  Time <- as.numeric(df[[grep('Start',names(df))[1]]])
  Time <- format(Time,format="%H:%M")

  Date <- format(df$Date,format="%Y-%m-%d")
  # Create Output data frame
  dfo <- tibble::tibble(Station = df$Station.Number,
                        Date = Date,
                        Time = Time,
                        Lon = df$LonDisplay,
                        Lat = df$LatDisplay,
                        NT = toupper(df$Neuston.Tow),
                        MN = toupper(df$Meter.Net),
                        PN = toupper(df$Phyto.Net),
                        HC = toupper(df$Hydrocast),
                        CTD = toupper(df$Free.CTD),
                        RBR = toupper(df$RBR),
                        SG = toupper(df$Shipek.Grab),
                        SD = toupper(df$Secchi.Disk),
                        HP = toupper(df$hydrophone),
                        # SS = df$Surface.Station,
                        genLoc = df$General.Locale)

  colnames(dfo) <- c('Station','Date','Time','Longitude','Latitude','NT','MN','PN','HC','CTD','RBR','SG','SD','HP','General Locale')

  dfo<- dfo[!sapply(dfo, function (k) all(k==''))]

  print_table(dfo,caption = caption, ...)

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
  # dfo <- tibble::add_column(dfo,
  #                           moon = paste0(df$moon_phase*100,"% (",stringr::str_to_title(df$moon_mode),")"),
  #                           .after = 4)


  colnames(dfo) <- c("Station","Time","Lat","Lon","Temperature","Salinity","Chl-a","Tow Area","Zooplankton","Zooplankton")
  secondRow <-c("","[local]","[$^\\circ$N]","[$^\\circ$E]","[$^\\circ$C]","","Fluoroesence","[m$^2$]","Biovolume","Density")
  thirdRow <- c("","","","","","","[Volts]","","[mL]","[$\\mu$L/km$^2$]")
  SigF <- c(20,20,20,2,2,1,2,2,0,1,2)

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

  dfo <- dplyr::select(df,station, dttm, lat,lon, bottle, z, no3, po4, sio2, pH, alk, chla, temp, sal)
  dfo <- dplyr::mutate(dfo, dttm = format(dfo$dttm,"%Y-%m-%d %H:%M"))

  rep_station <- duplicated(dfo$station)
  dfo$station[rep_station] <- NA
  dfo$dttm[rep_station] <- NA
  dfo$lon[rep_station] <- NA
  dfo$lat[rep_station] <- NA

  hline_vec <- c(which(rep_station==F)-1,nrow(dfo))

  colnames(dfo) <- c('Station','Time',"Lat","Lon",'Bottle','Depth','NO$_3^{-2}$','PO$_4^{-3}$','Si0$_2^-2$','pH','Alk','Chl-a','Temp','Sal')
  secondRow <- c('','(local)',"$^\\circ$N","$^\\circ$E","",'[m]','[$\\mu$M]','[$\\mu$M]','[$\\mu$M]','','','[mg/L]','[$^\\circ$C]','')
  SigF <- c(20,20,2,2,20,0,2,2,2,2,2,3,1,2)

  emptyCols <- colSums(is.na(dfo)) == nrow(dfo)
  dfo<- dfo[!emptyCols]
  secondRow <- secondRow[!emptyCols]
  SigF <- SigF[!emptyCols]
  SigF <- append(20,SigF)

  print_table(dfo, SigF=SigF, secondRow=secondRow, caption = caption, hline.after = hline_vec, ...)

}

#' Creates surface station summary table
#'
#' @param filename Path of the file to be read in
#' @param saveLoc Folder to save the output
#' @export
#' @examples
table_surfsamp <- function(df, caption = NULL, ...) {

  dfo <- dplyr::select(df, station, dttm_local, lat, lon, no3, po4, sio2, pH, alk, chla, temp, sal)
  dfo <- dplyr::mutate(dfo, dttm_local = format(dfo$dttm_local,"%Y-%m-%d %H:%M"))

  colnames(dfo) <- c('Station','Time',"Lat","Lon",'NO$_3^{-2}$','PO$_4^{-3}$','Si0$_2^-2$','pH','Alk','Chl-a','Temp','Sal')
  secondRow <- c('','(local)',"$^\\circ$N","$^\\circ$E",'[$\\mu$M]','[$\\mu$M]','[$\\mu$M]','','','[mg/L]','[$^\\circ$C]','')
  SigF <- c(20,20,2,2,2,2,2,2,2,3,1,2)

  emptyCols <- colSums(is.na(dfo)) == nrow(dfo)
  dfo<- dfo[!emptyCols]
  secondRow <- secondRow[!emptyCols]
  SigF <- SigF[!emptyCols]
  SigF <- append(20,SigF)

  print_table(dfo, SigF=SigF, secondRow=secondRow, caption = caption, ...)

}

