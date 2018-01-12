#######
# Make LATEX tables from SEA data
#######


#' Does the final table printing and saving after data has been edited
#'
#' Should only be called from one of the SEA table functions
#'
#' @param dfo data frame of data ready to be exported
#' @param caption caption of table.
#' @param SigF vector of significant figures for each column
#' @param secondRow List of entries for second row of table headings if needed
#' @param thirdRow List of entries for third row of table headings if needed
#' @param fileout name and location of the file to be exported
#' @export
#' @examples
#' printTable()
printTable <- function(dfo,caption="",SigF=NULL,secondRow=NULL,thirdRow=NULL,fileout="~/Desktop/tableOutput.tex") {

  outTable<-xtable(dfo,caption = caption)
  align(outTable) <- rep("c", ncol(dfo)+1)
  if(!is.null(SigF)) {
    digits(outTable) <- SigF
  }


  add.to.row <- list(pos = list(0), command = NULL)

  if(!is.null(secondRow)) {
    secondRowAdd <- paste0(paste(secondRow,collapse=' & ')," \\\\ \n")
  } else {
    secondRowAdd <- NULL
  }
  if(!is.null(thirdRow)) {
    thirdRowAdd <- paste0(paste(thirdRow,collapse=' & ')," \\\\ \n")
  } else {
    thirdRowAdd <- NULL
  }
  command <- paste0(secondRowAdd,
                    thirdRowAdd,
                    "\\hline\\n\\endfirsthead\n",
                    paste0(paste(colnames(dfo),collapse=' & ')," \\\\ \n"),
                    secondRowAdd,
                    thirdRowAdd,
                    "\\hline\n\\endhead\n",
                    "\\hline\n",
                    "\\multicolumn{", ncol(dfo), "}{l}",
                    "{\\footnotesize Continued on next page}\n",
                    "\\endfoot\n",
                    "\\endlastfoot\n")

  add.to.row$command <- command

  a<-print(outTable, hline.after=c(0), add.to.row = add.to.row,
           tabular.environment = "longtable",
           floating=F,
           include.rownames = FALSE,
           caption.placement = 'top')

  fileout <- gsub(" ","",fileout)
  fileConn<-file(fileout)
  writeLines(a, fileConn)
  close(fileConn)
}



#' Creates station summary table
#'
#' @param filename Path of the file to be read in
#' @param saveLoc Folder to save the output
#' @export
#' @examples
#' tableStatSum()
tableStatSum <- function(filename,saveLoc = "~/Desktop") {

  file <- tail(strsplit(filename,'/')[[1]],1)
  ext <- tools::file_ext(file)

  dfa <- readSEAxls(filename,rplcsv = T)
  df <- readSEAxls(filename,skip=1,rplcsv = T)
  names(df) <- names(dfa)
  names(df) <- gsub(' ','.',names(df))

  xi <- sort(append(grep('Meter.Net',names(df)),grep('Meter.Net',names(df))+1))
  df$Meter.Net <- substr(gsub('[0-9]','X',gsub('0','',as.character(rowSums((!is.na(df[,xi]))+0)))),1,1)
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


  Time <- df[,grep('Start',names(df))[1]]
  Time <- format(Time,format="%H:%M")

  Date <- format(df$Date,format="%Y-%m-%d")
  # Create Output data frame
  dfo <- data.frame(Station = df$Station.Number,
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
                    HP = toupper(df$hydrophone),
                    SS = df$Surface.Station,
                    genLoc = df$General.Locale,
                    stringsAsFactors =F)

  colnames(dfo) <- c('Station','Date','Time','Longitude','Latitude','NT','MN','PN','HC','CTD','RBR','SG','HP','SS','General Locale')

  dfo<- dfo[!sapply(dfo, function (k) all(k==''))]
  caption <- paste0("\\label{stationSummary} Summary of oceanographic sampling stations for SEA Cruise ",cruiseID,". [",dim(dfo)[1]," Stations]")

  fileout <- file.path(saveLoc,gsub(ext,'tex',file))

  printTable(dfo,caption=caption,fileout=fileout)

}


#' Creates Hydrocast table
#'
#' @param filename Path of the file to be read in
#' @param saveLoc Folder to save the output
#' @export
#' @examples
#' tableHydro()
tableHydro <- function(filename,saveLoc = "~/Desktop") {

  file <- tail(strsplit(filename,'/')[[1]],1)
  ext <- tools::file_ext(file)

  df <- readSEAxls(filename,rplcsv = T)
  names(df) <- gsub(' ','.',names(df))

  Time <- df[,grep('Start',names(df))[1]]
  Time <- format(Time,format="%H:%M")
  Date <- format(df$Date,format="%Y-%m-%d")

  Bottled <- as.character(round(as.numeric(df[,grep("Corr",names(df))])))
  Bottled[is.na(Bottled)&df$Bottle!=13] <- 'DNF'

  df$Bottle[grep('SS',df$Bottle)] <- "13"
  Bottled[is.na(Bottled)&df$Bottle==13] <- '0'

  dfo <- data.frame(Station=df$Station,
                    Date=Date,
                    Time=Time,
                    Bottle=df$Bottle,
                    Bottled = Bottled ,
                    NO3=df[grep("Nitrate.*uM",names(df))],
                    PO4=df[grep("PO4.*uM",names(df))],
                    pH=df[grep("pH",names(df))],
                    Chla=df[grep("Chl.*a.*ug",names(df))],
                    Temp=df[grep("Temp.*deg",names(df))],
                    Sal=df[grep("Salinity.*psu",names(df))],
                    stringsAsFactors =F)
  colnames(dfo) <- c('Station','Date','Time','Bottle','Bottle Depth','NO3','PO4','pH','Chl-a','Temperature','Salinity')
  secondRow <- c('','','(local)','','[m]','[uM]','[uM]','','[mg/L]','[degC]','')
  SigF <- c(20,20,20,20,0,0,2,2,2,3,1,2)

  emptyCols <- (sapply(dfo, function (k) all(k==''))|sapply(dfo, function (k) all(is.na(k))))
  dfo<- dfo[!emptyCols]
  secondRow <- secondRow[!emptyCols]
  SigF <- SigF[!emptyCols]
  caption = paste0("\\label{hydrowork} Hydrocast station data for SEA Cruise ",cruiseID,". Locations as in Table \\ref{stationSummary}")

  fileout <- file.path(saveLoc,gsub(ext,'tex',file))
  printTable(dfo,caption=caption,SigF=SigF,secondRow=secondRow,fileout=fileout)
}



#' Creates CTD summary table
#'
#' @param filename Path of the file to be read in
#' @param saveLoc Folder to save the output
#' @export
#' @examples
#' tableCTD()
tableCTD <- function(filename,saveLoc = "~/Desktop") {
  file <- tail(strsplit(filename,'/')[[1]],1)
  ext <- tools::file_ext(file)

  df <- readSEAxls(filename,rplcsv = T)
  names(df) <- gsub(' ','.',names(df))

  Time <- df[,grep('Time',names(df))[1]]
  Time <- format(Time,format="%H:%M")
  Date <- format(df$Date,format="%Y-%m-%d")

  dfo <- data.frame(Station=df$Station,
                  Date=Date,
                  Time=Time,
                  Temp=df[grep("Temp",names(df))],
                  Sal=df[grep("Sal",names(df))],
                  Chla=df[grep("Fluor",names(df))],
                  dep=df[grep("Cast.*Depth",names(df))],
                  stringsAsFactors = F)

  colnames(dfo) <- c('Station','Date','Time','Surf. Temperature','Surf. Salinity','Surf. Chl-a Fluoro','Cast Depth')
  secondRow <- c('','','(local)','[degC]','','[Volts]','[m]')
  SigF <- c(20,20,20,20,1,2,2,0)
  caption <- paste0("\\label{ctdwork} CTD station data for SEA Cruise ",cruiseID,". Locations as in Table \\ref{stationSummary}")
  fileout <- file.path(saveLoc,gsub(ext,'tex',file))

  printTable(dfo,caption=caption,SigF=SigF,secondRow=secondRow,fileout=fileout)

}


#' Creates surface station summary table
#'
#' @param filename Path of the file to be read in
#' @param saveLoc Folder to save the output
#' @export
#' @examples
#' tableSS()
tableSS <- function(filename,saveLoc = "~/Desktop") {
  file <- tail(strsplit(filename,'/')[[1]],1)
  ext <- tools::file_ext(file)

  df <- readSEAxls(filename,rplcsv = T)
  names(df) <- gsub(' ','.',names(df))

  Time <- df[,grep('Time',names(df))[1]]
  Time <- format(Time,format="%H:%M")
  Date <- format(df$Date,format="%Y-%m-%d")

  Nit <- as.numeric(df[,grep('NO3',names(df))[1]])
  Phos <- as.numeric(df[,grep('PO4',names(df))[1]])
  chla <- as.numeric(df[,grep('^(Chl.a)',names(df))[1]])
  pH <- as.numeric(df[,grep('pH',names(df))[1]])
  Alk <- as.numeric(df[,grep('^(?=.*Alk)(?!.*Carb).*$',names(df),perl=T)])

  dfo = data.frame(Station=df$Station,
                   Date=Date,
                   Time=Time,
                   Lon=df$LonDisplay,
                   Lat=df$LatDisplay,
                   Temp=df[grep('Temp.*oC',names(df))],
                   Sal=df[grep('Salinity',names(df))],
                   stringsAsFactors = F)
  colnames(dfo) <- c('Station','Date','Time','Longitude','Latitude','Temperature','Salinity')
  secondRow <- c('','','(local)','','','[degC]','')

  possVar <- c('chla','Nit','Phos','pH','Alk')
  nameVar <- c('Chl-a','NO3','PO4','pH','Total Alk.')
  unitVar <- c('[mg/L]','[uM]','[uM]','','[Meq/L]')
  sigFig<-c(3,3,3,3,3)
  usedVar <- usedSig <- usedUnit <-NULL
  initCol <- colnames(dfo)
  initSig <- c(20,20,20,20,20,20,1,2)
  for (i in 1:length(possVar)) {
    if(length(which(!is.na(get(possVar[i]))))>0) {
      dfo <- cbind(dfo,get(possVar[i]))
      usedVar <- append(usedVar,nameVar[i])
      usedSig <- append(usedSig,sigFig[i])
      usedUnit <- append(usedUnit,unitVar[i])
    }
  }
  SigF <- append(initSig,usedSig)
  colnames(dfo) <- append(initCol,usedVar)
  fileout <- file.path(saveLoc,gsub(ext,'tex',file))
  caption = paste0("\\label{surfsamp} Surface station data for SEA Cruise ",cruiseID)

  printTable(dfo,caption=caption,SigF=SigF,secondRow=secondRow,fileout=fileout)

}


#' Creates Neuston tow summary table
#'
#' Creates two tables - one for overview and the second for nekton data
#'
#' @param filename Path of the file to be read in
#' @param saveLoc Folder to save the output
#' @export
#' @examples
#' tableNeuston)
tableNeuston <- function(filename,saveLoc = "~/Desktop") {

  file <- tail(strsplit(filename,'/')[[1]],1)
  ext <- tools::file_ext(file)

  df <- readSEAxls(filename,rplcsv = T)
  names(df) <- gsub(' ','.',names(df))

  Time <- df[,grep('Time',names(df))[1]]
  Time <- format(Time,format="%H:%M")
  Date <- format(df$Date,format="%Y-%m-%d")

  Moon <- df[,grep("Moon",names(df))]
  if(mean(Moon,na.rm=T)<1) {Moon<-Moon*100}

  # OVERVIEW DATA
  dfo <- data.frame(Station=df$Station,
                    Date=Date,
                    Time=Time,
                    Moon=Moon,
                    Temp=as.numeric(df[,grep("Temp",names(df))]),
                    Sal=as.numeric(df[,grep("Salinity",names(df))]),
                    Chla=as.numeric(df[,grep("Fluor",names(df))]),
                    TowA=as.numeric(df[,grep("Tow.*Distance",names(df))]),
                    BioV=as.numeric(df[,grep("Zoo.*Biomass",names(df))]),
                    Den=as.numeric(df[,grep("Density",names(df))])*1000,
                    stringsAsFactors = F)

  colnames(dfo) <- c('Station','Date','Time','Moon Phase','Temperature','Salinity','Chl-a','Tow Area','Zooplankton','Zooplankton')
  secondRow <-c('','','(local)','[\\% full]','[degC]','','Fluoroesence','[$m^2$]','Biovolume','Density')
  thirdRow <- c('','','','','','','[Volts]','','[mL]','[$uL/m^2$]')
  SigF <- c(20,20,20,20,0,1,2,2,0,1,2)
  caption <- paste0("\\label{neuston1} Neuston tow hydrographic data for SEA Cruise ",cruiseID,". Locations as in Table \\ref{stationSummary}")

  fileout <- file.path(saveLoc,gsub(paste0(".",ext),'_1.tex',file))

  printTable(dfo,caption=caption,SigF=SigF,secondRow=secondRow,thirdRow=thirdRow,fileout=fileout)

  # NEKTON
  dfo <- data.frame(Station=df$Station,
                    Phyl=as.numeric(df[,grep("Phyll.*#",names(df))]),
                    Lept=as.numeric(df[,grep("Lepto.*#",names(df))]),
                    Halo=as.numeric(df[,grep("Halob.*#",names(df))]),
                    Myct=as.numeric(df[,grep("Mycto.*#",names(df))]),
                    Plas1=as.numeric(df[,grep("Plastic.*pellets",names(df))]),
                    Plas2=as.numeric(df[,grep("Plastic.*pieces",names(df))]),
                    Tar=as.numeric(df[,grep("Tar",names(df))]),
                    Nekt=as.numeric(df[,grep("Total.*Nekton.*ml",names(df))]),
                    Gel=as.numeric(df[,grep("Gelat.*ml",names(df))]),
                    stringsAsFactors = F)

  colnames(dfo) <- c('Station','Phyl','Lept','Halo','Myct','Plastic Pellets','Plastic Pieces','Tar','Nekton > 2cm','Gelatinous > 2cm')
  secondRow <- c('','[\\#]','[\\#]','[\\#]','[\\#]','[\\#]','[\\#]','[\\#]','[mL]','[mL]')
  SigF <- c(20,20,0,0,0,0,0,0,0,1,1)
  caption <- paste0("\\label{neuston2} Neuston tow biological data for  SEA Cruise ",cruiseID,". Locations as in Table \\ref{stationSummary}")

  fileout <- file.path(saveLoc,gsub(paste0(".",ext),'_2.tex',file))
  printTable(dfo,caption=caption,SigF=SigF,secondRow=secondRow,fileout=fileout)
}



#' Creates 100 count data summary table
#'
#' Creates table in two parts due to length of dataset
#'
#' @param filename Path of the file to be read in
#' @param saveLoc Folder to save the output
#' @export
#' @examples
#' table100Count()
table100Count <- function(filename,saveLoc = "~/Desktop") {

  file <- tail(strsplit(filename,'/')[[1]],1)
  ext <- tools::file_ext(file)

  dfn <- readSEAxls(filename,rplcsv = T,sheet=2)
  df <- readSEAxls(filename,rplcsv = T,sheet=2,skip=1)
  names(df) <- gsub(' ','.',names(dfn)[1:dim(df)[2]])

  Time <- df[,grep('Time',names(df))[1]]
  Time <- format(Time,format="%H:%M")
  Date <- format(df$Date,format="%Y-%m-%d")


  dfo <- data.frame(Station=df$Station,
                    Date=Date,
                    Time=Time,
                    Cnid=as.numeric(df[,grep("Cnida",names(df))]),
                    Siph=as.numeric(df[,grep("Sipho",names(df))]),
                    Cten=as.numeric(df[,grep("Cteno",names(df))]),
                    Pter=as.numeric(df[,grep("Ptero",names(df))]),
                    Nud=as.numeric(df[,grep("Nudib",names(df))]),
                    Other=as.numeric(df[,grep("Other.*Snail",names(df))]),
                    Poly=as.numeric(df[,grep("Polycheate",names(df))]),
                    Chaet=as.numeric(df[,grep("Chaet",names(df))]),
                    Cop=as.numeric(df[,grep("Copep",names(df))]),
                    Gam=as.numeric(df[,grep("Gamma",names(df))]),
                    Hyp=as.numeric(df[,grep("Hyper",names(df))]),
                    Crab=as.numeric(df[,grep("Zoea",names(df))]) + as.numeric(df[,grep("Megal",names(df))]),
                    stringsAsFactors = F)

  secondRow <- rep('',ncol(dfo))
  secondRow[3]<-'[local]'
  secondRow[9]<-'Snail'
  secondRow[ncol(dfo)] <- 'Larv.'
  SigF <- rep(0,ncol(dfo)+1)
  caption <-paste0("\\label{100count1} Zooplankton 100 count data for SEA Cruise ",cruiseID," (part 1). Locations as in Table \\ref{stationSummary}")
  fileout <- file.path(saveLoc,gsub(paste0(".",ext),'_100Count_1.tex',file))

  printTable(dfo,caption=caption,SigF=SigF,secondRow=secondRow,fileout=fileout)


  ### SECOND PART

  Other <- as.numeric(rowSums(df[,grep("^Other$|^Other[_]+",names(df))],na.rm=T))
  dfo <- data.frame(Station=df$Station,
                    Date=Date,
                    Time=Time,
                    Shr=as.numeric(df[,grep("Shrimp",names(df))]),
                    Lob=as.numeric(df[,grep("Lobster",names(df))]),
                    Mys=as.numeric(df[,grep("Mysid",names(df))]),
                    Euph=as.numeric(df[,grep("Euph",names(df))]),
                    Stom=as.numeric(df[,grep("Stoma",names(df))]),
                    Ostr=as.numeric(df[,grep("Ostra",names(df))]),
                    Iso=as.numeric(df[,grep("Isopo",names(df))]),
                    Salp=as.numeric(df[,grep("Salps",names(df))]),
                    Fish=as.numeric(df[,grep("Fish.*Larva",names(df))]),
                    Fishe=as.numeric(df[,grep("Fish.*Eggs",names(df))]),
                    Other=Other,
                    Shan=as.numeric(df[,grep("Shannon",names(df))]),
                    stringsAsFactors = F)

  cn <- colnames(dfo)
  cn[grep('Fishe',cn)] <- 'Fish'
  cn[grep('Shan',cn)] <- 'Shannon-Weiner'
  colnames(dfo) <- cn
  secondRow <- rep('',ncol(dfo))
  secondRow[c(4,5,8,12)]<-'Larv.'
  secondRow[13] <- 'Eggs'
  secondRow[ncol(dfo)] <- 'Diversity Index'
  secondRow[3] <- '[local]'
  SigF <- rep(0,ncol(dfo)+1)
  SigF[ncol(dfo)+1] <- 2
  caption <- paste0("\\label{100count2} Zooplankton 100 count data for SEA Cruise ",cruiseID," (part 2). Locations as in Table \\ref{stationSummary}")

  fileout <- file.path(saveLoc,gsub(paste0(".",ext),'_100Count_2.tex',file))
  printTable(dfo,caption=caption,SigF=SigF,secondRow=secondRow,fileout=fileout)

}


# FIXME: Everything after this needs reworking.
#
#
#
# ## METER NET
# filename <-  paste0(cruiseID,'_meterwork.xlsm')
# system(paste('xlsx2csv',file.path(shipfold,filename),'-s 1 >',file.path(outfold,gsub('.xlsm','_1.csv',filename)),sep=' '))
# system(paste('xlsx2csv',file.path(shipfold,filename),'-s 2 >',file.path(outfold,gsub('.xlsm','_2.csv',filename)),sep=' '))
#
#
#
#
# # METER NET OVERVIEW
# df <- read.csv(file.path(outfold,gsub('.xlsm','_1.csv',filename)),stringsAsFactors = F)
# nrows <- which(nchar(as.character(df$Station))==0)[1]-1
# df <- df[1:nrows,]
# df$Date <- gsub('\\[\\$-409\\]','',df$Date)
#
# Time <- df$Time
# Time[!grepl(':',Time)] <- paste0('00:',gsub(' ','0',format(round(as.numeric(Time[!grepl(':',Time)])*1440),width=2)))
#
#
# dfo <- data.frame(Station=df$Station,
#                   Date=df$Date,
#                   Time=Time,
#                   Temp=df$Temp..oC.,
#                   Sal=df$Salinity..psu.,
#                   Chla=df$Fluoro..chl.a.,
#                   Towd=df$Tow.depth..m.,
#                   TowD=df$Tow.Length..m.,
#                   TowV=df$Tow.Volume..m3.,
#                   BioV=df$Zoop.Biomass..ml.,
#                   Den=as.numeric(df$Zpl.Density..ml.m3.)*1000)
#
# colnames(dfo) <- c('Station','Date','Time','Temperature','Salinity','Chl-a','Tow','Tow','Tow','Zooplankton','Zooplankton')
# secondRow <-c('','','(local)','[degC]','','Fluoroesence','Depth','Length','Volume','Biovolume','Density')
# thirdRow <- c('','','','','','[Volts]','[m]','[m]','[$m^3]','[mL]','[$uL/m^3$]')
# SigF <- c(20,20,20,20,1,2,2,0,0,1,1,2)
# outTable<-xtable(dfo,caption = paste0("\\label{meter1} 1-meter and 2-meter net tow hydrographic data for SEA Cruise ",cruiseID,". Locations as in Table \\ref{stationSummary}"))
# align(outTable) <- rep("c", ncol(dfo)+1)
# digits(outTable) <- SigF
#
# add.to.row <- list(pos = list(0), command = NULL)
# command <- paste0(paste0(paste(secondRow,collapse=' & ')," \\\\ \n"),
#                   paste0(paste(thirdRow,collapse=' & ')," \\\\ \n"),
#                   "\\hline\\n\\endfirsthead\n",
#                   paste0(paste(colnames(dfo),collapse=' & ')," \\\\ \n"),
#                   paste0(paste(secondRow,collapse=' & ')," \\\\ \n"),
#                   paste0(paste(thirdRow,collapse=' & ')," \\\\ \n"),
#                   "\\hline\n\\endhead\n",
#                   "\\hline\n",
#                   "\\multicolumn{", ncol(dfo), "}{l}",
#                   "{\\footnotesize Continued on next page}\n",
#                   "\\endfoot\n",
#                   "\\endlastfoot\n")
# # command <- paste0(paste0(paste(secondRow,collapse=' & ')," \\\\ \n"),
# #                   paste0(paste(thirdRow,collapse=' & ')," \\\\ \n"),
# #                   "\\hline\n\\endhead\n",
# #                   "\\hline\n",
# #                   "\\multicolumn{", ncol(dfo), "}{l}",
# #                   "{\\footnotesize Continued on next page}\n",
# #                   "\\endfoot\n",
# #                   "\\endlastfoot\n")
# add.to.row$command <- command
#
# a<-print(outTable, hline.after=c(0), add.to.row = add.to.row,
#          tabular.environment = "longtable",
#          floating=F,
#          include.rownames = FALSE,
#          caption.placement = 'top')
#
# fileout <- gsub(paste0(cruiseID,'_'),'',gsub('.xlsm','_1.tex',filename))
# fileConn<-file(file.path(tablefold,fileout))
# writeLines(a, fileConn)
# close(fileConn)
#
#
#
#
# # NEKTON
#
# dfo <- data.frame(Station=df$Station,
#                   Phyl=df$Phyllosoma..,
#                   Lept=df$Leptocephali..,
#                   Halo=df$Halobates..,
#                   Myct=df$Myctophids..,
#                   Plas1=df$Plastic.pellets,
#                   Plas2=df$Plastic.pieces,
#                   Tar=df$Tar,
#                   Nekt=df$Total.Nekton.ml,
#                   Gel=df$Geltanious.ml)
#
# colnames(dfo) <- c('Station','Phyl','Lept','Halo','Myct','Plastic Pellets','Plastic Pieces','Tar','Nekton > 2cm','Gelatinous > 2cm')
# secondRow <- c('','[\\#]','[\\#]','[\\#]','[\\#]','[\\#]','[\\#]','[\\#]','[mL]','[mL]')
# SigF <- c(20,20,0,0,0,0,0,0,0,1,1)
#
# outTable<-xtable(dfo,caption = paste0("\\label{meter2} 1-meter and 2-meter net tow biological data for SEA Cruise ",cruiseID,". Locations as in Table \\ref{stationSummary}"))
# align(outTable) <- rep("c", ncol(dfo)+1)
# digits(outTable) <- SigF
#
# add.to.row <- list(pos = list(0), command = NULL)
# command <- paste0(paste0(paste(secondRow,collapse=' & ')," \\\\ \n"),
#                   "\\hline\\n\\endfirsthead\n",
#                   paste0(paste(colnames(dfo),collapse=' & ')," \\\\ \n"),
#                   paste0(paste(secondRow,collapse=' & ')," \\\\ \n"),
#                   "\\hline\n\\endhead\n",
#                   "\\hline\n",
#                   "\\multicolumn{", ncol(dfo), "}{l}",
#                   "{\\footnotesize Continued on next page}\n",
#                   "\\endfoot\n",
#                   "\\endlastfoot\n")
# # command <- paste0(paste0(paste(secondRow,collapse=' & ')," \\\\ \n"),
# #                   "\\hline\n\\endhead\n",
# #                   "\\hline\n",
# #                   "\\multicolumn{", ncol(dfo), "}{l}",
# #                   "{\\footnotesize Continued on next page}\n",
# #                   "\\endfoot\n",
# #                   "\\endlastfoot\n")
# add.to.row$command <- command
#
# a<-print(outTable, hline.after=c(0), add.to.row = add.to.row,
#          tabular.environment = "longtable",
#          floating=F,
#          include.rownames = FALSE,
#          caption.placement = 'top')
#
# fileout <- gsub(paste0(cruiseID,'_'),'',gsub('.xlsm','_2.tex',filename))
# fileConn<-file(file.path(tablefold,fileout))
# writeLines(a, fileConn)
# close(fileConn)
#
#


# ## PHYTONETS
#
# filename <-  paste0(cruiseID,'_phytowork.xlsm')
# if(file.exists(filename)) {
#   system(paste('xlsx2csv',file.path(shipfold,filename),file.path(outfold,gsub('.xlsm','.csv',filename)),sep=' '))
#   df <- read.csv(file.path(outfold,gsub('.xlsm','.csv',filename)),stringsAsFactors = F)
#   lasti <- which(df$Date=='')[1]-1
#   df <- df[1:lasti,]
#   df$Date <- gsub('\\[\\$-409\\]','',df$Date)
#   Time <- df$Time
#   Time[!grepl(':',Time)] <- paste0('00:',gsub(' ','0',format(round(as.numeric(Time[!grepl(':',Time)])*1440),width=2)))
#
#
#   dfo <- data.frame(Station=df$Station,
#                     Date=df$Date,
#                     Time=Time,
#                     Temp=df$Temp..oC.,
#                     Sal = df$Salinity..psu.,
#                     Chla=df$Fluoro..raw.,
#                     dia=df$X..Diatoms*100,
#                     din=df$X..Dinoflagellates*100,
#                     oth=df$X..Other*100)
#
#   colnames(dfo) <- c('Station','Date','Time','Temperature','Salinity','Chl-a Fluoroesence','Diatom','Dinoflagellate','Other')
#   secondRow <- c('','','(local)','[degC]','','[Volts]','[\\%]','[\\%]','[\\%]')
#   SigF <- c(20,20,20,20,1,2,2,0,0,0)
#
#   outTable<-xtable(dfo,caption = paste0("\\label{phytowork} Phytoplankton net data for ",cruiseID,". Locations as in Table \\ref{stationSummary}"))
#   align(outTable) <- rep("c", ncol(dfo)+1)
#   digits(outTable) <- SigF
#
#   add.to.row <- list(pos = list(0), command = NULL)
#   command <- paste0(paste0(paste(secondRow,collapse=' & ')," \\\\ \n"),
#                     "\\hline\\n\\endfirsthead\n",
#                     paste0(paste(colnames(dfo),collapse=' & ')," \\\\ \n"),
#                     paste0(paste(secondRow,collapse=' & ')," \\\\ \n"),
#                     "\\hline\n\\endhead\n",
#                     "\\hline\n",
#                     "\\multicolumn{", ncol(dfo), "}{l}",
#                     "{\\footnotesize Continued on next page}\n",
#                     "\\endfoot\n",
#                     "\\endlastfoot\n")
#   # command <- paste0(paste0(paste(secondRow,collapse=' & ')," \\\\ \n"),
#   #                   "\\hline\n\\endhead\n",
#   #                   "\\hline\n",
#   #                   "\\multicolumn{", ncol(dfo), "}{l}",
#   #                   "{\\footnotesize Continued on next page}\n",
#   #                   "\\endfoot\n",
#   #                   "\\endlastfoot\n")
#   add.to.row$command <- command
#
#   a<-print(outTable, hline.after=c(0), add.to.row = add.to.row,
#            tabular.environment = "longtable",
#            floating=F,
#            include.rownames = FALSE,
#            caption.placement = 'top')
#
#   fileout <- gsub(paste0(cruiseID,'_'),'',gsub('.xlsm','.tex',filename))
#   fileConn<-file(file.path(tablefold,fileout))
#   writeLines(a, fileConn)
#   close(fileConn)
#
# }
#
# ## SHIPEK
#
# filename <-  paste0(cruiseID,'_shipekwork.xlsm')
#
# if(file.exists(filename)) {
#   system(paste('xlsx2csv',file.path(shipfold,filename),file.path(outfold,gsub('.xlsm','.csv',filename)),sep=' '))
#   df <- read.csv(file.path(outfold,gsub('.xlsm','.csv',filename)),stringsAsFactors = F)
#   lasti <- which(df$Date=='')[1]-1
#   df <- df[1:lasti,]
#   df$Date <- gsub('\\[\\$-409\\]','',df$Date)
#
#   Time <- df$Time
#   Time[!grepl(':',Time)] <- paste0('00:',gsub(' ','0',format(round(as.numeric(Time[!grepl(':',Time)])*1440),width=2)))
#
#
#   dfo <- data.frame(Station=df$Station,
#                     Date=df$Date,
#                     Time=Time,
#                     Temp=df$Temp..oC.,
#                     Sal = df$Sal..psu.,
#                     Chla=df$Fluoro...chl.a.,
#                     dep=df$Bottom.Depth..m.,
#                     comm=df$Comments)
#
#   colnames(dfo) <- c('Station','Date','Time','Temperature','Salinity','Chl-a Fluoro','Depth','Comments')
#   secondRow <- c('','','(local)','[degC]','','[Volts]','[m]','')
#   SigF <- c(20,20,20,20,1,2,2,0,0)
#
#   outTable<-xtable(dfo,caption = paste0("\\label{shipek} Shipek grab data for SEA Cruise ",cruiseID,". Locations as in Table \\ref{stationSummary}"))
#   align(outTable) <- rep("c", ncol(dfo)+1)
#   digits(outTable) <- SigF
#
#   add.to.row <- list(pos = list(0), command = NULL)
#   command <- paste0(paste0(paste(secondRow,collapse=' & ')," \\\\ \n"),
#                     "\\hline\\n\\endfirsthead\n",
#                     paste0(paste(colnames(dfo),collapse=' & ')," \\\\ \n"),
#                     paste0(paste(secondRow,collapse=' & ')," \\\\ \n"),
#                     "\\hline\n\\endhead\n",
#                     "\\hline\n",
#                     "\\multicolumn{", ncol(dfo), "}{l}",
#                     "{\\footnotesize Continued on next page}\n",
#                     "\\endfoot\n",
#                     "\\endlastfoot\n")
#   # command <- paste0(paste0(paste(secondRow,collapse=' & ')," \\\\ \n"),
#   #                   "\\hline\n\\endhead\n",
#   #                   "\\hline\n",
#   #                   "\\multicolumn{", ncol(dfo), "}{l}",
#   #                   "{\\footnotesize Continued on next page}\n",
#   #                   "\\endfoot\n",
#   #                   "\\endlastfoot\n")
#   add.to.row$command <- command
#
#   a<-print(outTable, hline.after=c(0), add.to.row = add.to.row,
#            tabular.environment = "longtable",
#            floating=F,
#            include.rownames = FALSE,
#            caption.placement = 'top')
#
#   fileout <- gsub(paste0(cruiseID,'_'),'',gsub('.xlsm','.tex',filename))
#   fileConn<-file(file.path(tablefold,fileout))
#   writeLines(a, fileConn)
#   close(fileConn)
# }
