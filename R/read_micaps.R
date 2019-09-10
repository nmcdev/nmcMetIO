#' Read micaps type 3 file (general scatter point).
#'
#' @param filename : file name
#'
#' @return a list include datatime. data.frame (ID,lon,lat,alt,V1,...) and so on.
#'         if read false, return NULL.
#' @export
#'
#' @examples
#'   dataV <- read_micaps_3("Z:\\data\\surface\\jiany_rr\\r20\\17012108.000")
#'
#'
read_micaps_3 <- function(filename) {
  
  # check file, if not exist, then return empty list.
  if (!file.exists(filename)) return(NULL)
  
  # read all lines
  txt <- readr::read_file(filename,locale=readr::locale(encoding="UTF-8"))
  
  # get character vector
  txt <- unlist(strsplit(stringr::str_trim(txt), "+[ \n\t\r]+"))
  
  # extract information
  headInfo <- txt[3]
  
  # date & time
  if (nchar(txt[4]) < 4) {
    year <- as.integer(txt[4]) + 2000
  } else {
    year <- as.integer(txt[4])
  }
  month <- as.integer(txt[5])
  day   <- as.integer(txt[6])
  hour  <- as.integer(txt[7])
  dataTime <- ISOdate(year,month,day,hour=hour)
  
  # level
  level <- as.integer(txt[8])
  
  # extract contour information
  ncnt <- as.integer(txt[9])
  pt <- 10
  if (ncnt > 0) {
    cnValues <- as.numeric(txt[pt:(pt+ncnt-1)])
    pt <- pt + ncnt
  }
  
  # extract smooth and bold value
  smoothCeof <- as.numeric(txt[pt])
  pt <- pt + 1
  boldCeof <- as.numeric(txt[pt])
  pt <- pt + 1
  
  # extract boundary
  nBound = as.integer(txt[pt])
  pt <- pt + 1
  if (nBound > 0) {
    bpoints <- as.numeric(txt[pt:(pt+2*nBound-1)])
    pt <- pt + 2*nBound
  }
  
  # extract number of elements and data
  nElements <- as.integer(txt[pt])
  pt <- pt + 1
  number <- as.integer(txt[pt])
  pt <- pt + 1
  
  # cut data
  txt = txt[pt:(length(txt))]
  
  # reform data to matrix
  dim(txt) <- c(nElements+4,number)
  
  # extract data information to data.frame
  dataV <- data.frame(ID=txt[1,],lon=txt[2,],lat=txt[3,],alt=txt[4,])
  for (ii in 1:nElements) {
    dataV[,paste0("V",ii)] <- txt[ii+4,]
  }
  
  # return data
  return(list(headInfo=headInfo,
              dataTime=dataTime,
              level=level,
              dataV=dataV))
}


#' Read micaps type 4 file (grid).
#'
#' @param filename : file name.
#'
#' @return : a list include:
#'         headInfo, head information
#'         dataTime, data date time
#'         fhour, forecast hours
#'         level, data level
#'         lon, grid longitude
#'         lat, grid laitude
#          dataV, a matrix with dimension c(nlon,nlat)
#' @export
#'
#' @examples
#'
#' dataV <- read_micaps_4("Z:\\data\\newecmwf_grib\\pressure\\17010108.006")
#'
read_micaps_4 <- function(filename) {
  
  # check file, if not exist, then return empty list.
  if (!file.exists(filename)) return(NULL)
  
  # read all lines
  txt <- readr::read_file(filename,locale=readr::locale(encoding="UTF-8"))
  
  # get character vector
  txt <- unlist(strsplit(stringr::str_trim(txt), "+[ \n\t\r]+"))
  
  # extract information
  headInfo <- txt[3]
  
  # date & time
  if (nchar(txt[4]) < 4) {
    year <- as.integer(txt[4]) + 2000
  } else {
    year <- as.integer(txt[4])
  }
  month <- as.integer(txt[5])
  day   <- as.integer(txt[6])
  hour  <- as.integer(txt[7])
  dataTime <- ISOdate(year,month,day,hour=hour)
  fhour <- as.integer(txt[8])
  
  # level
  level <- as.integer(txt[9])
  
  # grid information
  xs <- as.numeric(txt[10])
  ys <- as.numeric(txt[11])
  slon <- as.numeric(txt[12])
  elon <- as.numeric(txt[13])
  slat <- as.numeric(txt[14])
  elat <- as.numeric(txt[15])
  nlon <- as.integer(txt[16])
  nlat <- as.integer(txt[17])
  
  # construct grid
  lon <- slon + seq(0,nlon)*xs
  lat <- slat + seq(0,nlat)*ys
  
  # contour information
  cnInterval <- as.numeric(txt[18])
  cnStart <- as.numeric(txt[19])
  cnEnd <- as.numeric(txt[20])
  
  # smooth and bold value
  smoothCeof <- as.numeric(txt[21])
  boldCeof <- as.numeric(txt[22])
  
  # cut and reform data
  dataV <- as.numeric(txt[23:length(txt)])
  dim(dataV) <- c(nlon,nlat)
  
  # re-arrange latitude order
  if ((lat[2]-lat[1]) < 0) {
    lat <- rev(lat)
    dataV <- dataV[,nlat:1]
  }
  
  # return data
  return(list(headInfo=headInfo,
              dataTime=dataTime,
              fhour=fhour,
              level=level,
              lon=lon,
              lat=lat,
              dataV=dataV))
}


#' Read micaps 14 file.
#'
#' @param fileName : file name.
#'
#' @return A list include draw elements:
#'           lineData,          lines
#'           lineSymData,       line symbols, present trough, front line and so on
#'           symData,           symbols
#'           closedContourData, colosed contour
#'           stationsData,      station situation
#'           weatherRegionData, weather region
#'           fillAreaData,      filled area
#'           notesSymData,      notes symbol
#'           plinesSymData,     line symbols with properties
#'
#' @export
#' @examples
#'     fileName <- "Z:/diamond/update/rr010908.024"
#'     drawElements <- read_micaps_14(fileName)
#'
#'
read_micaps_14 <- function(fileName){
  
  # check file, if not exist, then return empty list.
  if (!file.exists(filename)) return(NULL)
  
  # read all lines
  txt <- readr::read_file(filename,locale=readr::locale(encoding="UTF-8"))
  
  # get character vector
  txt <- unlist(strsplit(stringr::str_trim(txt), "+[ \n\t\r]+"))
  
  # extract information
  headInfo <- txt[3]
  
  # date & time
  if (nchar(txt[4]) < 4) {
    year <- as.integer(txt[4]) + 2000
  } else {
    year <- as.integer(txt[4])
  }
  month <- as.integer(txt[5])
  day   <- as.integer(txt[6])
  hour  <- as.integer(txt[7])
  dataTime <- ISOdate(year,month,day,hour=hour)
  fhour <- as.integer(txt[8])
  
  
  ####Read lines--------------------------------
  
  #locate the line data section
  iLine <- which(toupper(txt) == "LINES:")
  lineData <- NULL
  if (length(iLine) != 0) {
    number <- as.integer(txt[iLine+1])
    iLine  <- iLine + 2
    
    # lines exists
    if (number > 0) {
      # define data
      lineWidth     <- vector("numeric")
      lineXYZnum    <- vector("integer")
      lineXYZ       <- list()
      lineLabelNum  <- vector("integer")
      lineLabel     <- vector("character")
      lineLabelXYZ  <- list()
      
      # loop every line
      for (ii in 1:number) {
        # line width
        lineWidth[[ii]] <- as.numeric(txt[iLine])
        iLine <- iLine + 1
        
        # line xyz point number
        xyzNum <- as.integer(txt[iLine])
        lineXYZnum[[ii]] <- xyzNum
        iLine <- iLine + 1
        
        # line xyz
        xyz <- as.numeric(txt[iLine:(iLine+3*xyzNum-1)])
        lineXYZ[[ii]] <- xyz
        iLine <- iLine + 3*xyzNum
        
        # line label
        lineLabel[[ii]] <- txt[iLine]
        iLine <- iLine + 1
        
        # line label number
        labelNum = as.integer(txt[iLine])
        lineLabelNum[[ii]] <- labelNum
        iLine <- iLine + 1
        
        # label xyz
        if (labelNum > 0) {
          labelXYZ <- as.numeric(txt[iLine:(iLine+3*labelNum-1)])
          dim(labelXYZ) <- c(3, labelNum)
          lineLabelXYZ[[ii]]  <- labelXYZ
          iLine <- iLine + 3*labelNum
        } else {
          lineLabelXYZ[[ii]]  <- list()
        }
      }
      
      # construct line data
      lineData <- list(lineWidth=lineWidth,
                       lineXYZnum=lineXYZnum,
                       lineXYZ=lineXYZ,
                       lineLabelNum=lineLabelNum,
                       lineLabel=lineLabel,
                       lineLabelXYZ=lineLabelXYZ)
    }
  }
  
  
  ####Read line symbols--------------------------------
  
  #locate the line symbols data section
  iLine <- which(toupper(txt) == "LINES_SYMBOL:")
  lineSymData <- NULL
  if (length(iLine) != 0) {
    number <- as.integer(txt[iLine+1])
    iLine  <- iLine + 2
    
    # lines exists
    if (number > 0) {
      # define data
      lineSymCode   <- vector("integer")
      lineSymWidth  <- vector("numeric")
      lineSymXYZNum <- vector("integer")
      lineSymXYZ    <- list()
      
      # loop every line
      for (ii in 1:number) {
        # line symbol code
        lineSymCode[[ii]] <- as.integer(txt[iLine])
        iLine <- iLine + 1
        
        # line symbol width
        lineSymWidth[[ii]] <- as.numeric(txt[iLine])
        iLine <- iLine + 1
        
        # line symbol xyz point number
        xyzNum <- as.integer(txt[iLine])
        lineSymXYZNum[[ii]] <- xyzNum
        iLine <- iLine + 1
        
        # line symbol xyz
        xyz <- as.numeric(txt[iLine:(iLine+3*xyzNum-1)])
        dim(xyz) <- c(3,xyzNum)
        lineSymXYZ[[ii]] <- xyz
        iLine <- iLine + 3*xyzNum
        
        # label xyz
        label <- txt[iLine]
        iLine <- iLine + 1
        
        # line symbol label number
        labelNum <- as.integer(txt[iLine])
        iLine <- iLine + labelNum*3 + 1
      }
      
      # construct line data
      lineSymData <- list(lineSymCode=lineSymCode,
                          lineSymWidth=lineSymWidth,
                          lineSymXYZNum=lineSymXYZNum,
                          lineSymXYZ=lineSymXYZ)
    }
  }
  
  
  ####Read symbol--------------------------------
  
  #locate the symbol data section
  iLine <- which(toupper(txt) == "SYMBOLS:")
  symData <- NULL
  if (length(iLine) != 0) {
    number <- as.integer(txt[iLine+1])
    iLine  <- iLine + 2
    
    # lines exists
    if (number > 0) {
      # define data
      symCode  <- vector("integer")
      symXYZ   <- list()
      symValue <- vector("character")
      
      # loop every line
      for (ii in 1:number) {
        # symbol code
        symCode[[ii]] <- as.integer(txt[iLine])
        iLine <- iLine + 1
        
        # symbol xyz
        xyz <- as.numeric(txt[iLine:(iLine+2)])
        symXYZ[[ii]] <- xyz
        iLine <- iLine + 3
        
        # symbol value
        symValue[[ii]] <- txt[iLine]
        iLine <- iLine + 1
      }
      
      # construct line data
      symData <- list(symCode=symCode,
                      symXYZ=symXYZ,
                      symValue=symValue)
    }
  }
  
  
  ####Read closed contours--------------------------------
  
  iLine <- which(toupper(txt) == "CLOSED_CONTOURS:")
  closedContourData <- NULL
  if (length(iLine) != 0) {
    number <- as.integer(txt[iLine+1])
    iLine  <- iLine + 2
    
    # lines exists
    if (number > 0) {
      # define data
      cnWidth     <- vector("numeric")
      cnXYZnum    <- vector("integer")
      cnXYZ       <- list()
      cnLabelNum  <- vector("integer")
      cnLabel     <- vector("character")
      cnLabelXYZ  <- list()
      
      # loop every line
      for (ii in 1:number) {
        # line width
        cnWidth[[ii]] <- as.numeric(txt[iLine])
        iLine <- iLine + 1
        
        # line xyz point number
        xyzNum <- as.integer(txt[iLine])
        cnXYZnum[[ii]] <- xyzNum
        iLine <- iLine + 1
        
        # line xyz
        xyz <- as.numeric(txt[iLine:(iLine+3*xyzNum-1)])
        dim(xyz) <- c(3,xyzNum)
        cnXYZ[[ii]] <- xyz
        iLine <- iLine + 3*xyzNum
        
        # line label
        cnLabel[[ii]] <- txt[iLine]
        iLine <- iLine + 1
        
        # line label number
        labelNum <- as.integer(txt[iLine])
        cnLabelNum[[ii]] <- labelNum
        iLine <- iLine + 1
        
        # label xyz
        if (labelNum > 0) {
          labelXYZ <- as.numeric(txt[iLine:(iLine+3*labelNum-1)])
          dim(labelXYZ) <- c(3, labelNum)
          cnLabelXYZ[[ii]]  <- cnLabelXYZ
          iLine <- iLine + 3*labelNum
        } else {
          cnLabelXYZ[[ii]]  <- list()
        }
      }
      
      # construct line data
      closedContourData <- list(cnWidth=cnWidth,
                                cnXYZnum=cnXYZnum,
                                cnXYZ=cnXYZ,
                                cnLabelNum=cnLabelNum,
                                cnLabel=cnLabel,
                                cnLabelXYZ=cnLabelXYZ)
    }
  }
  
  
  ####Read station situation--------------------------------
  
  iLine <- which(toupper(txt) == "STATION_SITUATION:")
  stationsData <- NULL
  
  if (length(iLine) != 0) {
    # find data end subscript
    endIndex <- iLine + 1
    while ((!is.na(as.numeric(txt[endIndex])) && (endIndex < length(txt)))) {
      endIndex <- endIndex + 1
    }
    if (endIndex > (iLine+1)) {
      stationsData <- txt[(iLine+1):(endIndex-1)]
      dim(stationsData) <- c(2,length(stationsData)/2)
    }
  }
  
  
  ####Read weather region--------------------------------
  
  #locate the weather region data section
  iLine <- which(toupper(txt) == "WEATHER_REGION:")
  weatherRegionData <- NULL
  if (length(iLine) != 0) {
    number <- as.integer(txt[iLine+1])
    iLine  <- iLine + 2
    
    # lines exists
    if (number > 0) {
      # define data
      weatherRegionCode   <- vector("integer")
      weatherRegionXYZNum <- vector("integer")
      weatherRegionXYZ    <- list()
      
      # loop every line
      for (ii in 1:number) {
        # region code
        weatherRegionCode[[ii]] <- as.integer(txt[iLine])
        iLine <- iLine + 1
        
        # region xyz point number
        xyzNum <- as.integer(txt[iLine])
        weatherRegionXYZNum[[ii]] <- xyzNum
        iLine <- iLine + 1
        
        # symbol xyz
        xyz <- as.numeric(txt[iLine:(iLine+3*xyzNum-1)])
        dim(xyz) <- c(3,xyzNum)
        symXYZ[[ii]] <- xyz
        iLine <- iLine + 3*xyzNum
      }
      
      # construct line data
      weatherRegionData <- list(weatherRegionCode=weatherRegionCode,
                                weatherRegionXYZNum=weatherRegionXYZNum,
                                weatherRegionXYZ=weatherRegionXYZ)
    }
  }
  
  
  ####Read fill data--------------------------------
  
  #locate the fill data section
  iLine <- which(toupper(txt) == "FILLAREA:")
  fillAreaData <- NULL
  if (length(iLine) != 0) {
    number <- as.integer(txt[iLine+1])
    iLine  <- iLine + 2
    
    # lines exists
    if (number > 0) {
      # define data
      fillAreaCode          <- vector("integer")        # code
      fillAreaNum           <- vector("integer")        # line points number
      fillAreaXYZ           <- list()                   # line points
      fillAreaType          <- vector("integer")        # fill area type
      fillAreaColor         <- list()                   # fill area color
      fillAreaFrontColor    <- list()                   # fill area front color
      fillAreaBackColor     <- list()                   # fill area back color
      fillAreaGradientAngle <- vector("numeric")        # fill area gradient angle
      fillAreaGraphicsType  <- vector("integer")        # fill area graphics type
      fillAreaFrame         <- vector("integer")        # fill area frame
      
      # loop every line
      for (ii in 1:number) {
        # code
        fillAreaCode[[ii]] <- as.integer(txt[iLine])
        iLine <- iLine + 1
        
        # xyz point number
        xyzNum <- as.integer(txt[iLine])
        fillAreaNum[[ii]] <- xyzNum
        iLine <- iLine + 1
        
        # xyz points
        xyz <- as.numeric(txt[iLine:(iLine+3*xyzNum-1)])
        dim(xyz) <- c(3,xyzNum)
        fillAreaXYZ[[ii]] <- xyz
        iLine <- iLine + 3*xyzNum
        
        # type
        fillAreaType[[ii]] <- as.integer(txt[iLine])
        iLine <- iLine + 1
        
        # color
        fillAreaColor[[ii]] <- as.integer(txt[iLine:(iLine+3)])
        iLine <- iLine + 4
        
        # front color
        fillAreaFrontColor[[ii]] <- as.integer(txt[iLine:(iLine+3)])
        iLine <- iLine + 4
        
        # back color
        fillAreaBackColor[[ii]] <- as.integer(txt[iLine:(iLine+3)])
        iLine <- iLine + 4
        
        # gradient angle
        fillAreaGradientAngle[[ii]] <- as.numeric(txt[iLine])
        iLine <- iLine + 1
        
        # graphics type
        fillAreaGraphicsType[[ii]] <- as.integer(txt[iLine])
        iLine <- iLine + 1
        
        # draw frame or not
        fillAreaFrame[[ii]] <- as.integer(txt[iLine])
        iLine <- iLine + 1
      }
      
      # construct line data
      fillAreaData <- list(fillAreaCode=fillAreaCode,
                           fillAreaNum=fillAreaNum,
                           fillAreaXYZ=fillAreaXYZ,
                           fillAreaType=fillAreaType,
                           fillAreaColor=fillAreaColor,
                           fillAreaFrontColor=fillAreaFrontColor,
                           fillAreaBackColor=fillAreaBackColor,
                           fillAreaGradientAngle=fillAreaGradientAngle,
                           fillAreaGraphicsType=fillAreaGraphicsType,
                           fillAreaFrame=fillAreaFrame)
    }
  }
  
  
  ####Read notes symbol--------------------------------
  
  #locate the notes symbol data section
  iLine <- which(toupper(txt) == "NOTES_SYMBOL:")
  notesSymData <- NULL
  if (length(iLine) != 0) {
    number <- as.integer(txt[iLine+1])
    iLine  <- iLine + 2
    
    # lines exists
    if (number > 0) {
      # define data
      nSymCode      <- vector("integer")
      nSymXYZ       <- list()
      nSymCharLen   <- vector("integer")
      nSymChar      <- vector("character")
      nSymAngle     <- vector("numeric")
      nSymFontLen   <- vector("integer")
      nSymFontName  <- vector("character")
      nSymFontSize  <- vector("numeric")
      nSymFontType  <- vector("character")
      nSymColor     <- vector("character")
      
      # loop every line
      for (ii in 1:number) {
        # code
        nSymCode[[ii]] <- as.integer(txt[iLine])
        iLine <- iLine + 1
        
        # xyz
        xyz <- as.numeric(txt[iLine:(iLine+2)])
        nSymXYZ[[ii]] <- xyz
        iLine <- iLine + 3
        
        # character length
        nSymCharLen[[ii]] <- as.integer(txt[iLine])
        iLine <- iLine + 1
        
        # characters
        nSymChar[[ii]] <- txt[iLine]
        iLine <- iLine + 1
        
        # character angle
        nSymAngle[[ii]] <- as.numeric(txt[iLine])
        iLine <- iLine + 1
        
        # font length
        nSymFontLen[[ii]] <- as.integer(txt[iLine])
        iLine <- iLine + 1
        
        # font name
        nSymFontName[[ii]] <- txt[iLine]
        iLine <- iLine + 1
        
        # font size
        nSymFontSize[[ii]] <- as.numeric(txt[iLine])
        iLine <- iLine + 1
        
        # font type
        nSymFontType[[ii]] <- txt[iLine]
        iLine <- iLine + 1
        
        # color
        nSymColor[[ii]] <- txt[iLine]
        iLine <- iLine + 1
      }
      
      # construct line data
      notesSymData <- list(nSymCode=nSymCode,
                           nSymXYZ=nSymXYZ,
                           nSymCharLen=nSymCharLen,
                           nSymChar=nSymChar,
                           nSymAngle=nSymAngle,
                           nSymFontLen=nSymFontLen,
                           nSymFontName=nSymFontName,
                           nSymFontSize=nSymFontSize,
                           nSymFontType=nSymFontType,
                           nSymColor=nSymColor)
    }
  }
  
  
  ####Read lines symbols with property--------------------------------
  
  #locate the weather region data section
  iLine <- which(toupper(txt) == "WITHPROP_LINESYMBOLS:")
  plinesSymData <- NULL
  if (length(iLine) != 0) {
    number <- as.integer(txt[iLine+1])
    iLine  <- iLine + 2
    
    # lines exists
    if (number > 0) {
      # define data
      plinesSymCode     <- vector("integer")
      plinesSymWidth    <- vector("numeric")
      plinesSymColor    <- list()
      plinesSymType     <- vector("integer")
      plinesSymShadow   <- vector("integer")
      plinesSymXYZNum   <- vector("integer")
      plinesSymXYZ      <- list()
      plinesSymLabel    <- vector("character")
      plinesSymLabelNum <- vector("integer")
      plinesSymLabelXYZ <- list()
      
      # loop every line
      for (ii in 1:number) {
        # code
        plinesSymCode[[ii]] <- as.integer(txt[iLine])
        iLine <- iLine + 1
        
        # width
        plinesSymWidth[[ii]] <- as.numeric(txt[iLine])
        iLine <- iLine + 1
        
        # color A R G B
        plinesSymColor[[ii]] <- as.integer(txt[iLine:(iLine+3)])
        iLine <- iLine + 4
        
        # type
        plinesSymType[[ii]] <- as.integer(txt[iLine])
        iLine <- iLine + 1
        
        # shadow
        plinesSymShadow[[ii]] <- as.integer(txt[iLine])
        iLine <- iLine + 1
        
        # xyz number
        xyzNum <- as.integer(txt[iLine])
        plinesSymXYZNum[[ii]] <- xyzNum
        iLine <- iLine + 1
        
        # line symbol xyz
        xyz <- as.numeric(txt[iLine:(iLine+3*xyzNum-1)])
        dim(xyz) <- c(3, xyzNum)
        plinesSymXYZ[[ii]] <- xyz
        iLine <- iLine + 3*xyzNum
        
        # symbol label
        plinesSymLabel[[ii]] <- txt[iLine]
        iLine <- iLine + 1
        
        # symbol label number
        labelNum <- as.integer(txt[iLine])
        plinesSymLabelNum[[ii]] <- labelNum
        iLine <- iLine + 1
        
        # label xyz
        if (labelNum > 0) {
          xyz <- as.numeric(txt[iLine:(iLine+3*labelNum-1)])
          dim(xyz) <- c(3, labelNum)
          plinesSymLabelXYZ[[ii]] <- xyz
          iLine <- iLine + 3*labelNum
        } else {
          plinesSymLabelXYZ[[ii]] <- list()
        }
      }
      
      # construct line data
      plinesSymData <- list(plinesSymCode=plinesSymCode,
                            plinesSymWidth=plinesSymWidth,
                            plinesSymColor=plinesSymColor,
                            plinesSymType=plinesSymType,
                            plinesSymShadow=plinesSymShadow,
                            plinesSymXYZNum=plinesSymXYZNum,
                            plinesSymXYZ=plinesSymXYZ,
                            plinesSymLabel=plinesSymLabel,
                            plinesSymLabelNum=plinesSymLabelNum,
                            plinesSymLabelXYZ=plinesSymLabelXYZ)
    }
  }
  
  
  ####return data contents--------------------------------
  
  return(list(headInfo=headInfo,
              dataTime=dataTime,
              fhour=fhour,
              
              lineData=lineData,
              lineSymData=lineSymData,
              symData=symData,
              closedContourData=closedContourData,
              stationsData=stationsData,
              weatherRegionData=weatherRegionData,
              fillAreaData=fillAreaData,
              notesSymData=notesSymData,
              plinesSymData=plinesSymData))
}
