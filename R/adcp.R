

#' Read in adcp data files in the format exported before conversion to ODV
#'
#' This needs to be updated to instread read directly from the .ENS files to skip steps in on-board processing
#'
#' @param filein .txt file to read from
#' @keywords
#' @export
#' @examples
#' readSEAadcp()
#'
readSEAadcp <- function(filein) {

  # reads in the file as individual lines
  r <- readLines(filein)

  # finds the header line index and return headerline
  hi <- grep('^Ens.*Eas',r)
  headline <- r[hi]

  # find the indexes of u, v, lon and lat from titles in headerline
  ui <- grep('Eas',strsplit(headline,'\t')[[1]])
  vi <- grep('Nor',strsplit(headline,'\t')[[1]])
  loni <- grep('FLon',strsplit(headline,'\t')[[1]])
  lati <- grep('FLat',strsplit(headline,'\t')[[1]])

  # read the table in from 3 lines past the header onwards
  a<-read.table(filein,skip=hi+3,header=FALSE,sep='\t')

  # extract u and v, lon and lat
  u <- as.matrix(a[ui])
  v <- as.matrix(a[vi])
  lon <- c(a[loni])[[1]]
  lat <- c(a[lati])[[1]]

  # ensure that u and v are output as numeric
  u <- apply(u,2,as.numeric)
  v <- apply(v,2,as.numeric)
  lon <- as.numeric(lon)
  lat <- as.numeric(lat)

  return(list(u=u,v=v,lon=lon,lat=lat))
}


#'
#' As readSEAadcp, this needs to be updated to instread read directly from the .ENS files to skip steps in on-board processing
#'
#' @param foldin folder containing all the .txt file to read from
#' @keywords
#' @export
#' @examples
#' readSEAadcp_all()
#'
readSEAadcp_all <- function(foldin) {
  files <- list.files(path=foldin, pattern="^ADCP.*30.txt$")
  if(length(files)==0) {
    files <- list.files(path=foldin, pattern="^ADCP.*60.txt$")
  }
  comp <- unlist(strsplit(gsub(cruiseID,paste(cruiseID,'-',sep=''),files),'[-_]'))
  ii <- grep(cruiseID,comp)
  filenum <- as.numeric(comp[ii+1])
  startnum <- as.numeric(comp[ii+2])
  ord <- sort(filenum+startnum/(max(startnum)+1),index.return=T)
  files <- files[ord$ix]

  u <- v <- lon <- lat <- NULL

  for (i in 1:length(files)) {
    cat(i)
    filein <- file.path(foldin,files[i])
    cat(filein,"\n")
    X <- readSEAadcp(filein)
    u<-rbind(u,X$u)
    v<-rbind(v,X$v)
    lon<-append(lon,X$lon)
    lat<-append(lat,X$lat)
  }

  lon[lon<0&!is.na(lon)] <- lon[lon<0&!is.na(lon)] +360

  return(list(u=u,v=v,lon=lon,lat=lat))
}




