#' Retrieve micaps grid data from cassandra service.
#'
#' @param directory : the directory on the micaps service
#' @param filename : the data filename, if not given, will use the lastest model run.
#' @param filter : the filename filter pattern, when filename=NULL, this will be used to
#'                 find the specified file.
#' @param outList : if TRUE, return list
#' @param cache : if TRUE, cache retrieved data to local directory
#'
#'
#' @return data.table, or list(x, y, z, level, time, fhour)
#' @export
#'
#' @examples
#'   data <- retrieve_micaps_model_grid("ECMWF_HR/TMP/850/", filename="19033020.024")
#'
retrieve_micaps_model_grid <- function(directory, filename=NULL, filter="*.024", outList=FALSE, cache=FALSE){
  
  # check cache file
  if(is.null(filename)){
    filename <- gds_get_latest_filename(directory, filter=filter)
  }
  if(cache){
    cacheFile <- get_cache_file(directory, filename, name="MICAPS_DATA")
    if(file.exists(cacheFile)){
      re <- readRDS(cacheFile)
      return(re)
    }
  }
  
  # retrieve data contents
  msg <- gds_get_content(directory, filename)
  if (is.null(msg)){
    return(NULL)
  }
  
  # define parse funtcion
  parse_bytes <- function(msg, ss, what){
    res <- readBin(msg[ii:(ii+ss-1)], what, size=ss)
    ii <<- ii + ss  # update variables
    return(res)
  }
  ii <- 1
  
  # read head information
  head <- list()
  head[['discriminator']] <- parse_bytes(msg, 4, "character")
  head[['type']] <- parse_bytes(msg, 2, "integer")
  head[['modelName']] <- parse_bytes(msg, 20, "character")
  head[['element']] <- parse_bytes(msg, 50, "character")
  head[['description']] <- parse_bytes(msg, 30, "character")
  head[['level']] <- parse_bytes(msg, 4, "numeric")
  head[['year']] <- parse_bytes(msg, 4, "integer")
  head[['month']] <- parse_bytes(msg, 4, "integer")
  head[['day']] <- parse_bytes(msg, 4, "integer")
  head[['hour']] <- parse_bytes(msg, 4, "integer")
  head[['timezone']] <- parse_bytes(msg, 4, "integer")
  head[['period']] <- parse_bytes(msg, 4, "integer")
  head[['startLongitude']] <- parse_bytes(msg, 4, "numeric")
  head[['endLongitude']] <- parse_bytes(msg, 4, "numeric")
  head[['longitudeGridSpace']] <- parse_bytes(msg, 4, "numeric")
  head[['longitudeGridNumber']] <- parse_bytes(msg, 4, "integer")
  head[['startLatitude']] <- parse_bytes(msg, 4, "numeric")
  head[['endLatitude']] <- parse_bytes(msg, 4, "numeric")
  head[['latitudeGridSpace']] <- parse_bytes(msg, 4, "numeric")
  head[['latitudeGridNumber']] <- parse_bytes(msg, 4, "integer")
  head[['isolineStartValue']] <- parse_bytes(msg, 4, "numeric")
  head[['isolineEndValue']] <- parse_bytes(msg, 4, "numeric")
  head[['isolineSpace']] <- parse_bytes(msg, 4, "numeric")
  head[['perturbationNumber']] <- parse_bytes(msg, 2, "integer")
  head[['ensembleTotalNumber']] <- parse_bytes(msg, 2, "integer")
  head[['minute']] <- parse_bytes(msg, 2, "integer")
  head[['second']] <- parse_bytes(msg, 2, "integer")
  head[['Extent']] <- parse_bytes(msg, 92, "character")
  
  # get required grid information
  data_type <- head[['type']]
  nlon <- head[['longitudeGridNumber']]
  nlat <- head[['latitudeGridNumber']]
  nmem <- head[['ensembleTotalNumber']]
  lon <- 0:(nlon-1)*head[['longitudeGridSpace']] + head[['startLongitude']]
  lat <- 0:(nlat-1)*head[['latitudeGridSpace']] + head[['startLatitude']]
  lev <- head[['level']]
  
  # get time information
  initTime <- ISOdatetime(head[['year']], head[['month']], head[['day']], head[['hour']], 0, 0)
  fhour <- head[['period']]
  time <- initTime + fhour*3600
  
  # read data
  if (nmem == 0){
    if (data_type == 4){                                                   # model scale data
      dims <- c(nlon, nlat)
      nn = prod(dims)
      dataV <- readBin(msg[ii:(ii+nn*4-1)], "numeric", n=nn, size=4)
      dim(dataV) <- dims
      
      # convert latitude to increase order
      if (lat[1] > lat[2]) {
        lat  <- rev(lat)
        dataV <- dataV[,nlat:1]
      }
      
      # return list or data.table, which is consistent to MetR.
      if (outList) {
        re <- list(var=dataV, lon=lon, lat=lat, level=lev,
                   initTime=initTime, time=time, fhour=fhour)
      } else {
        re <- data.table::data.table(lon=rep(lon, length(lat)),
                                     lat=rep(lat, each=length(lon)),
                                     lev=lev, time=time,
                                     initTime=initTime, fhour=fhour,
                                     var1=as.vector(dataV))
        data.table::setkeyv(re, c("lon", "lat", "lev", "time"))
      }
    } else {                                                                # model vector data
      dims <- c(nlon, nlat, 2)
      nn = prod(dims)
      dataV <- readBin(msg[ii:(ii+nn*4-1)], "numeric", n=nn, size=4)
      dim(dataV) <- dims
      
      # convert latitude to increase order
      if (lat[1] > lat[2]) {
        lat  <- rev(lat)
        dataV <- dataV[,nlat:1,]
      }
      
      # return list or data.table, which is consistent to MetR.
      if (outList) {
        re <- list(var1=dataV[,,1], var2=dataV[,,2], lon=lon, lat=lat,
                   level=lev, initTime=initTime, time=time, fhour=fhour)
      } else {
        re <- data.table::data.table(lon=rep(lon, length(lat)),
                                     lat=rep(lat, each=length(lon)),
                                     lev=lev, time=time,
                                     initTime=initTime, fhour=fhour,
                                     var1=as.vector(dataV[,,1]),
                                     var2=as.vector(dataV[,,2]))
        data.table::setkeyv(re, c("lon", "lat", "lev", "time"))
      }
    }
  } else {
    # construct ensemble dimensions
    mem = seq(1:nmem)
    
    # read variable data
    if (data_type == 4){                                                     # ensemble model scale data
      dims <- c(nlon, nlat, nmem)
      nn = prod(dims)
      dataV <- readBin(msg[ii:(ii+nn*4-1)], "numeric", n=nn, size=4)
      dim(dataV) <- dims
      
      # convert latitude to increase order
      if (lat[1] > lat[2]) {
        lat  <- rev(lat)
        dataV <- dataV[,nlat:1,]
      }
      
      # return list or data.table, which is consistent to MetR.
      if (outList) {
        re <- list(var=dataV, lon=lon, lat=lat, mem=mem, level=lev,
                   initTime=initTime, time=time, fhour=fhour)
      } else {
        re <- data.table::data.table(lon=rep(rep(lon, length(lat)), length(mem)),
                                     lat=rep(rep(lat, each=length(lon)), length(mem)),
                                     mem=mem, lev=lev, time=time,
                                     initTime=initTime, fhour=fhour,
                                     var1=as.vector(dataV))
        data.table::setkeyv(re, c("lon", "lat", "lev", "time"))
      }
    } else {                                                                 # ensemble model vector data
      dims <- c(nlon, nlat, nmem, 2)
      nn = prod(dims)
      dataV <- readBin(msg[ii:(ii+nn*4-1)], "numeric", n=nn, size=4)
      dim(dataV) <- dims
      
      # convert latitude to increase order
      if (lat[1] > lat[2]) {
        lat  <- rev(lat)
        dataV <- dataV[,nlat:1,,]
      }
      
      # return list or data.table, which is consistent to MetR.
      if (outList) {
        re <- list(var1=dataV[,,,1], var2=dataV[,,,2], lon=lon, lat=lat, mem=mem,
                   level=lev, initTime=initTime, time=time, fhour=fhour)
      } else {
        re <- data.table::data.table(lon=rep(rep(lon, length(lat)), length(mem)),
                                     lat=rep(rep(lat, each=length(lon)), length(mem)),
                                     mem=mem, lev=lev, time=time,
                                     initTime=initTime, fhour=fhour,
                                     var1=as.vector(dataV[,,,1]),
                                     var2=as.vector(dataV[,,,2]))
        data.table::setkeyv(re, c("lon", "lat", "mem","lev", "time"))
      }
    }
  }
  
  # check cache file
  if(cache){
    saveRDS(re, file=cacheFile)
  }
  
  # return result
  return(re)
}


#' Get model forecast time series.
#'
#' @param directory : the directory on the micaps service
#' @param initTimeStr : the model forecast initial time string
#' @param fhours : the model forecast hours
#' @param allExists : all files should exist, or return None.
#' @param cache : if TRUE, cache retrieved data to local directory
#'
#' @return 
#' @export
#'
#' @examples
#'   initTimeStr <- "20022820"
#'   fhours <- seq(0, 72, by=6)
#'   data <- retrieve_micaps_model_grids("ECMWF_HR/TMP/850/", initTimeStr, fhours)
#'   
retrieve_micaps_model_grids <- function(directory, initTimeStr, fhours, allExists=TRUE, cache=FALSE){
  # loop every forecast time
  data <- NULL
  for(fhour in fhours){
    filename <- paste(initTimeStr, sprintf("%03d", fhour), sep=".")
    dataTemp <- retrieve_micaps_model_grid(directory, filename=filename, cache=cache)
    if(allExists){
      if(is.null(dataTemp)){
        return(NULL)
      }
    }
    data <- rbind(data, dataTemp)
  }
  return(data)
}


#' Map var id to name.
#'
#' @param vid: integer.
#'
#' @return : variable name.
#'
retrieve_micaps_station_data_var_name <- function(vid){
  # define variable name dictionary
  varNames <- list('1'='lon', '2'='lat', '3'='alt', '4'='grade', '5'='type', '21'='name',
                   '201'='Wind_angle', '203'='Wind_speed', '205'='Wind_angle_1m_avg', '207'='Wind_speed_1m_avg',
                   '209'='Wind_angle_2m_avg', '211'='Wind_speed_2m_avg', '213'='Wind_angle_10m_avg', '215'='Wind_speed_10m_avg',
                   '217'='Wind_angle_max', '219'='Wind_speed_max', '221'='Wind_angle_instant', '223'='Wind_speed_instant',
                   '225'='Gust_angle', '227'='Gust_speed', '229'='Gust_angle_6h', '231'='Gust_speed_6h',
                   '233'='Gust_angle_12h', '235'='Gust_speed_12h', '237'='Wind_power', 
                   '401'='Sea_level_pressure', '403'='Pressure_3h_trend', '405'='Pressure_24h_trend',
                   '407'='Station_pressure', '409'='Pressure_max', '411'='Pressure_min', '413'='Pressure',
                   '415'='Pressure_day_avg', '417'='SLP_day_avg', '419'='Hight', '421'='Geopotential_hight',
                   '601'='Temp', '603'='Temp_max', '605'='Temp_min', '607'='Temp_24h_trend', 
                   '609'='Temp_24h_max', '611':'Temp_24h_min', '613'='Temp_dav_avg',
                   '801'='Dewpoint', '803'='Dewpoint_depression', '805'='Relative_humidity',
                   '807'='Relative_humidity_min', '809'='Relative_humidity_day_avg', 
                   '811'='Water_vapor_pressure', '813'='Water_vapor_pressure_day_avg',
                   '1001'='Rain', '1003'='Rain_1h', '1005'='Rain_3h', '1007'='Rain_6h', '1009'='Rain_12h', '1013'='Rain_day',
                   '1015'='Rain_20-08', '1017'='Rain_08-20', '1019'='Rain_20-20', '1021'='Rain_08-08',
                   '1023'='Evaporation', '1025'='Evaporation_large', '1027'='Precipitable_water',
                   '1201'='Vis_1min', '1203'='Vis_10min', '1205'='Vis_min', '1207'='Vis_manual',
                   '1401'='Total_cloud_cover', '1403'='Low_cloud_cover', '1405'='Cloud_base_hight',
                   '1407'='Low_cloud', '1409'='Middle_cloud', '1411'='High_cloud',
                   '1413'='TCC_day_avg', '1415'='LCC_day_avg', '1417'='Cloud_cover', '1419'='Cloud_type',
                   '1601'='Weather_current', '1603'='Weather_past_1', '1605'='Weather_past_2',
                   '2001'='Surface_temp', '2003'='Surface_temp_max', '2005'='Surface_temp_min')
  name <- varNames[[tolower(as.character(vid))]]
  if (is.null(name)) {
    return(paste('var_', as.character(vid), sep=''))
  } else {
    return(name)
  }
}


#' Retrieve micaps station data from cassandra service.
#'
#' @param directory : the directory on the micaps service
#' @param filename : the data filename, if not given, will use the latest model run.
#' @param filter : the filename filter pattern, when filename=NULL, this will be used to
#'                 find the specified file.
#'
#' @return : data.frame of records
#' @export
#'
#' @examples
#'   obs <- retrieve_micaps_station_data("SURFACE/PLOT_NATIONAL/", filename="20190406140000.000")
#'
retrieve_micaps_station_data <- function(directory, filename=NULL, filter="*.000", cache=FALSE){
  # check cache file
  if(is.null(filename)){
    filename <- gds_get_latest_filename(directory, filter=filter)
  }
  if(cache){
    cacheFile <- get_cache_file(directory, filename, name="MICAPS_DATA")
    if(file.exists(cacheFile)){
      records <- readRDS(cacheFile)
      return(records)
    }
  }
  
  # retrieve data contents
  msg <- gds_get_content(directory, filename)
  if (is.null(msg)){
    return(NULL)
  }

  # define parse function
  parse_bytes <- function(msg, ss, what){
    res <- readBin(msg[ii:(ii+ss-1)], what, size=ss)
    ii <<- ii + ss  # update variables
    return(res)
  }
  ii <- 1
  
  #
  # read head information section
  head <- list()
  head[['discriminator']] <- parse_bytes(msg, 4, "character")
  head[['type']] <- parse_bytes(msg, 2, "integer")
  head[['description']] <- parse_bytes(msg, 100, "character")
  head[['level']] <- parse_bytes(msg, 4, "numeric")
  head[['levelDescription']] <- parse_bytes(msg, 50, "character")
  head[['year']] <- parse_bytes(msg, 4, "integer")
  head[['month']] <- parse_bytes(msg, 4, "integer")
  head[['day']] <- parse_bytes(msg, 4, "integer")
  head[['hour']] <- parse_bytes(msg, 4, "integer")
  head[['minute']] <- parse_bytes(msg, 4, "integer")
  head[['second']] <- parse_bytes(msg, 4, "integer")
  head[['timezone']] <- parse_bytes(msg, 4, "integer")
  head[['Extent']] <- parse_bytes(msg, 100, "character")
  
  # get time information
  time <- ISOdatetime(head[['year']], head[['month']], head[['day']],
                      head[['hour']], head[['minute']], head[['second']])
  
  #
  # read data section
  
  # the number of records and record variables
  record_numb <- parse_bytes(msg, 4, "integer")
  record_nvar <- parse_bytes(msg, 2, "integer")
  
  # define variable type map
  # 1: byte, 2: short, 3: int, 4: long, 5: float, 6: double, 7:string
  varTypeMap <- list(
    list(1,"raw"), list(2,"integer"), list(4,"integer"), list(8,"integer"),
    list(4,"numeric"), list(8,"numeric"), list(1,"character"))
  
  # maping variable type and define records data.frame
  varMap <- list()
  records <- tibble::tibble(ID=integer(record_numb)+NA,
                            lon=numeric(record_numb)+NA,
                            lat=numeric(record_numb)+NA,
                            time=time)
  for(i in 1:record_nvar){
    varID <- retrieve_micaps_station_data_var_name(parse_bytes(msg, 2, "integer"))
    varType <- parse_bytes(msg, 2, "integer")
    varMap[[varID]] <- varTypeMap[[varType]]
    records[[varID]] <- do.call(varTypeMap[[varType]][[2]], list(record_numb))
    if (varTypeMap[[varType]][[2]] != 'character' &
        varTypeMap[[varType]][[2]] != 'raw') {
      records[[varID]]  = records[[varID]] + NA
    }
  }
  
  # loop every record to read
  for(i in 1:record_numb){
    records[i,'ID'] <- parse_bytes(msg, 4, "integer")
    records[i,'lon'] <- parse_bytes(msg, 4, "numeric")
    records[i,'lat'] <- parse_bytes(msg, 4, "numeric")
    numb <- parse_bytes(msg, 2, "integer")
    for(j in 1:numb){
      vid <- retrieve_micaps_station_data_var_name(parse_bytes(msg, 2, "integer"))
      records[i,vid] <- parse_bytes(
        msg, varMap[[vid]][[1]], varMap[[vid]][[2]])
    }
  }
  
  # check cache file
  if(cache){
    saveRDS(records, file=cacheFile)
  }
  
  # return record
  return(records)
}
