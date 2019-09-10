#' Get CIMISS server data.
#'
#' @description 
#' Retrieve the CIMISS data using REST API with pure R code.
#' 
#' refer to:
#'    http://10.20.76.55/cimissapiweb/MethodData_list.action
#'
#' @param interfaceId, MUSIC interface id.
#' @param params, named list for MUSIC parameters.
#'
#' @return json list.
#' @export
#'
#' @examples
get_http_result <- function(interfaceId, params){
  
  # set MUSIC server dns and user information
  config = get_config_from_rcfile()
  if (is.null(config)) {
    return(NULL)
  }
  
  # construct URL
  url = paste0('http://', config$CIMISS$DNS, '/cimiss-web/api?userId=', 
               config$CIMISS$USER_ID, '&pwd=', config$CIMISS$PASSWORD,
               '&interfaceId=', interfaceId)
  
  # paste the parameters
  for (name in names(params)) {
    url = paste0(url, '&', name, '=', params[[name]])
  }
  
  # paste data format
  url = paste0(url, '&dataFormat=json')
  
  # read data
  data <- jsonlite::fromJSON(url)
  if (data$returnCode != 0) {
    return(NULL)
  }
  return(data)
}


#' Retrieve station records from CIMISS in region by time.
#'
#' @description
#'     Retrieve data by cimiss music API, You should install MUSIC java client.
#'     1) download MUSIC java client from http://10.20.76.55/cimissapiweb/page/apipages/ClientDocumentRight.jsp
#'     2) set environment variable
#'     CLASSPATH=C:/soft/music-lib/Ice.jar;C:/soft/music-lib/music-client-v1.4.0.jar;C:/soft/music-lib
#'     3) call java function
#'
#' @param times : time for retrieve, 'YYYYMMDDHHMISS,YYYYMMDDHHMISS,...'
#' @param minLat : minimum latitude
#' @param minLon : minimum longitude
#' @param maxLat : maximum latitude
#' @param maxLon : maximum longitude
#' @param dataCode : dataset code, like "SURF_CHN_MUL_HOR", "SURF_CHN_MUL_HOR_N", and so on.
#' @param elements : elements for retrieve, 'ele1,ele2,...'
#'
#' @return data frame of observations.
#' @export
#'
#' @examples
#'   data <- cimiss_obs_in_rect_by_time('20190908000000', 35, 110, 45, 120)
#'
cimiss_obs_in_rect_by_time <- function(times, minLat, minLon, maxLat, maxLon,
                                       dataCode="SURF_CHN_MUL_HOR_N",
                                       elements="Station_Id_C,Datetime,Lat,Lon,TEM"){
  
  # retrieve parameters
  params = list()
  params[["dataCode"]] <- dataCode
  params[["elements"]] <- elements
  params[["times"]] <- times
  params[["minLat"]] <- as.character(minLat)
  params[["minLon"]] <- as.character(minLon)
  params[["maxLat"]] <- as.character(maxLat)
  params[["maxLon"]] <- as.character(maxLon)
  params[["orderby"]] <- "Datetime:ASC"
  interfaceId = "getSurfEleInRectByTime"
  
  # retrieve data
  result <- get_http_result(interfaceId, params)
  if (is.null(result)) return(NULL)
  
  # extract data
  data = result[['DS']]
  
  # return data
  return(data)
}


#' Retrieve station records from CIMISS by time and station ID.
#'
#'     Retrieve data by cimiss music API.
#'     You should install MUSIC java client.
#'     1) download MUSIC java client from http://10.20.76.55/cimissapiweb/page/apipages/ClientDocumentRight.jsp
#'     2) set environment variable
#'     CLASSPATH=C:/soft/music-lib/Ice.jar;C:/soft/music-lib/music-client-v1.4.0.jar;C:/soft/music-lib
#'     3) call java function
#'
#' @param timeRange : time range for retrieve, "[YYYYMMDDHHMISS,YYYYMMDDHHMISS]",
#'                    like, "[201509010000,20150903060000]"
#' @param minLat : minimum latitude
#' @param minLon : minimum longitude
#' @param maxLat : maximum latitude
#' @param maxLon : maximum longitude
#' @param dataCode : dataset code, like "SURF_CHN_MUL_HOR", "SURF_CHN_MUL_HOR_N", and so on.
#' @param elements : elements for retrieve, 'ele1,ele2,...'
#'
#' @return data frame of observations.
#' @export
#'
#' @examples
#'     elements <- "Station_Id_C,Station_Id_d,Station_Name,Station_levl,Datetime,Lat,Lon,PRE_Time_0808"
#'     data <- cimiss_obs_in_Rect_by_time_range("[20060801000000,20060801000000]",35,110,45,120,dataCode="SURF_CHN_MUL_DAY",elements=elements)
#'
cimiss_obs_in_Rect_by_time_range <- function(timeRange, minLat, minLon, maxLat, maxLon,
                                             dataCode="SURF_CHN_MUL_HOR_N",
                                             elements="Station_Id_C,Datetime,Lat,Lon,TEM"){
  
  # retrieve parameters
  params = list()
  params[["dataCode"]] <- dataCode
  params[["elements"]] <- elements
  params[["timeRange"]] <- timeRange
  params[["minLat"]] <- as.character(minLat)
  params[["minLon"]] <- as.character(minLon)
  params[["maxLat"]] <- as.character(maxLat)
  params[["maxLon"]] <- as.character(maxLon)
  params[["orderby"]] <- "Datetime:ASC"
  interfaceId <- "getSurfEleInRectByTimeRange"

  # retrieve data
  result <- get_http_result(interfaceId, params)
  if (is.null(result)) return(NULL)
  
  # extract data
  data = result[['DS']]
  
  # return data
  return(data)
}


#' Retrieve station records from CIMISS by times.
#'
#' @description
#'     Retrieve data by cimiss music API.
#'     You should install MUSIC java client.
#'     1) download MUSIC java client from http://10.20.76.55/cimissapiweb/page/apipages/ClientDocumentRight.jsp
#'     2) set environment variable
#'     CLASSPATH=C:/soft/music-lib/Ice.jar;C:/soft/music-lib/music-client-v1.4.0.jar;C:/soft/music-lib
#'     3) call java function
#'
#' @param times : time for retrieve, 'YYYYMMDDHHMISS,YYYYMMDDHHMISS,...'
#' @param dataCode : dataset code, like "SURF_CHN_MUL_HOR", "SURF_CHN_MUL_HOR_N", and so on.
#' @param elements : elements for retrieve, 'ele1,ele2,...'
#' @param staIds : station ids, 'xxxxx,xxxxx,...'
#'
#' @return data frame of observations.
#' @export
#' @examples \dontrun{
#'   r1 <- cimiss_obs_by_time('20181108000000')
#' }
#'
cimiss_obs_by_time <- function(times,
                               dataCode="SURF_CHN_MUL_HOR_N",
                               elements="Station_Id_C,Station_Id_d,lat,lon,Datetime,TEM"){
  
  # retrieve parameters
  params = list()
  params[["dataCode"]] <- dataCode
  params[["elements"]] <- elements
  params[["times"]] <- times
  params[["orderby"]] <- "Datetime:ASC"
  interfaceId <- "getSurfEleByTime"

  # retrieve data
  result <- get_http_result(interfaceId, params)
  if (is.null(result)) return(NULL)
  
  # extract data
  data = result[['DS']]
  
  # return data
  return(data)
}


#' Retrieve station records from CIMISS by time and station ID.
#'
#' @description
#'     Retrieve data by cimiss music API.
#'     You should install MUSIC java client.
#'     1) download MUSIC java client from http://10.20.76.55/cimissapiweb/page/apipages/ClientDocumentRight.jsp
#'     2) set environment variable
#'     CLASSPATH=C:/soft/music-lib/Ice.jar;C:/soft/music-lib/music-client-v1.4.0.jar;C:/soft/music-lib
#'     3) call java function
#'
#' @param timeRange : time range for retrieve, "[YYYYMMDDHHMISS,YYYYMMDDHHMISS]",
#'                    like, "[201509010000,20150903060000]"
#' @param hourSeparate: hour space, [1, 24], like 6: 0,6,12,18
#' @param minSeparate: minute space, [1, 60], like 10: 0,10,20,30,40,50
#' @param dataCode : dataset code, like "SURF_CHN_MUL_HOR", "SURF_CHN_MUL_HOR_N", and so on.
#' @param elements : elements for retrieve, 'ele1,ele2,...'
#' @param staIds : station ids, 'xxxxx,xxxxx,...'
#'
#' @return data frame of observations.
#' @export
#' @examples \dontrun{
#'   r1 <- cimiss_obs_by_time_range_and_staID("[20060801000000,20060801000000]")
#' }
#'
cimiss_obs_by_time_range_and_staID <- function(timeRange,
                                               hourSeparate=NULL,
                                               minSeparate=NULL,
                                               dataCode="SURF_CHN_MUL_HOR_N",
                                               elements="Station_Id_C,Datetime,TEM",
                                               staIds="54511"){
  
  # retrieve parameters
  params = list()
  params[["dataCode"]] <- dataCode
  params[["elements"]] <- elements
  params[["times"]] <- times
  if (!is.null(hourSeparate)){
    params[["hourSeparate"]] <- as.character(hourSeparate)
  }
  if (!is.null(minSeparate)){
    params[["minSeparate"]] <- as.character(minSeparate)
  }
  params[["staIds"]] <- staIds
  params[["orderby"]] <- "Datetime:ASC"
  interfaceId = "getSurfEleByTimeRangeAndStaID"
  
  # retrieve data
  result <- get_http_result(interfaceId, params)
  if (is.null(result)) return(NULL)
  
  # extract data
  data = result[['DS']]
  
  # return data
  return(data)
}


#' Retrieve station records from CIMISS by time and station ID.
#'
#' @description
#'     Retrieve data by cimiss music API.
#'     You should install MUSIC java client.
#'     1) download MUSIC java client from http://10.20.76.55/cimissapiweb/page/apipages/ClientDocumentRight.jsp
#'     2) set environment variable
#'     CLASSPATH=C:/soft/music-lib/Ice.jar;C:/soft/music-lib/music-client-v1.4.0.jar;C:/soft/music-lib
#'     3) call java function
#'
#' @param times : time for retrieve, 'YYYYMMDDHHMISS,YYYYMMDDHHMISS,...'
#' @param dataCode : dataset code, like "SURF_CHN_MUL_HOR", "SURF_CHN_MUL_HOR_N", and so on.
#' @param elements : elements for retrieve, 'ele1,ele2,...'
#' @param staIds : station ids, 'xxxxx,xxxxx,...'
#'
#' @return data frame of observations.
#' @export
#' @examples \dontrun{
#'   r1 <- cimiss_obs_by_time_and_staID('20181108000000')
#' }
#'
cimiss_obs_by_time_and_staID <- function(times,
                                         dataCode="SURF_CHN_MUL_HOR_N",
                                         elements="Station_Id_C,Datetime,TEM",
                                         staIds="54511"){
  
  # retrieve parameters
  params = list()
  params[["dataCode"]] <- dataCode
  params[["elements"]] <- elements
  params[["times"]] <- times
  params[["staIds"]] <- staIds
  params[["orderby"]] <- "Datetime:ASC"
  interfaceId = "getSurfEleByTimeAndStaID"

  # retrieve data
  result <- get_http_result(interfaceId, params)
  if (is.null(result)) return(NULL)
  
  # extract data
  data = result[['DS']]
  
  # return data
  return(data)
}


#' retrieve model point forecast data from CIMISS.
#'
#' @param time, run time for retrieve, 'YYYYMMDDHHMISS,YYYYMMDDHHMISS,...'
#' @param dataCode, like "NAFP_FOR_FTM_HIGH_EC_GLB", "NAFP_FOR_FTM_HIGH_EC_ASI", and so on.
#' @param fcstEle, forecast element (single), default is 2m temperature "TEF0"
#' @param fcstLevel, forecast level (single)
#' @param minVT, start forecast hour
#' @param maxVT, end forecast hour
#' @param latLons, latitude/longitude, multiple points are supported with separated by ",".
#'
#' @return data.frame of point forecast.
#' @export
#'
#' @examples
#'   dataV <- cimiss_model_point_by_time_and_level("20170120000000")
#'
cimiss_model_point_by_time_and_level <- function(time,
                                                 dataCode="NAFP_FOR_FTM_HIGH_EC_GLB",
                                                 fcstEle="TEF0", fcstLevel=0,
                                                 minVT=0, maxVT=240,
                                                 latLons='39.8/116.4667') {
  # retrieve parameters
  params = list()
  params[["dataCode"]] <- dataCode
  params[["time"]] <- time
  params[["fcstEle"]] <- fcstEle
  params[["minVT"]] <- as.character(minVT)
  params[["maxVT"]] <- as.character(maxVT)
  params[["fcstLevel"]] <- as.character(fcstLevel)
  params[["latLons"]] <- latLons
  interfaceId = "getNafpEleAtPointByTimeAndLevelAndValidtimeRange"

  # retrieve data
  result <- get_http_result(interfaceId, params)
  if (is.null(result)) return(NULL)
  
  # extract data
  data = result[['DS']]
  
  # return data
  return(data)
}


#' Retrieve grid data from CIMISS service.
#'
#' @description
#'     Retrieve grid data from CIMISS service with MUSIC java libray
#'
#' @param time, model run time, like "2016081712"
#' @param minLat, minimum latitude
#' @param minLon, minimum longitude
#' @param maxLat, maximum latitude
#' @param maxLon, maximum longitude
#' @param dataCode, MUSIC data code, default is "NAFP_FOR_FTM_HIGH_EC_GLB", which is for
#'                  ECMWF deterministic high resolution model global surface forecast.
#' @param fcstLevel, vertical level, default is 0.
#' @param fcstEle, forecast element, default is 2m temperature "TEF0"
#' @param validTime, forecast hour, default is 0
#'
#' @return list(x,y,z,level,time,fhour)
#' @export
#'
#' @examples
#'   data <- cimiss_model_grid_in_rect("2016081712", 15, 60, 55, 136)
#'
#'
cimiss_model_grid_in_rect <- function(time, minLat, minLon, maxLat, maxLon,
                                      dataCode="NAFP_FOR_FTM_HIGH_EC_GLB",
                                      fcstLevel=0, fcstEle="TEF0", validTime=0, outList=FALSE) {
  # retrieve parameters
  params <- list()
  params[['dataCode']] <- dataCode
  params[['fcstEle']] <- fcstEle
  params[['time']] <- paste0(time,"0000")
  params[['minLat']] <- as.character(minLat)
  params[['minLon']] <- as.character(minLon)
  params[['maxLat']] <- as.character(maxLat)
  params[['maxLon']] <- as.character(maxLon)
  params[['fcstLevel']] <- as.character(fcstLevel)
  params[['validTime']] <- as.character(validTime)
  interfaceId <- "getNafpEleGridInRectByTimeAndLevelAndValidtime"

  # retrieve data
  result <- get_http_result(interfaceId, params)
  if (is.null(result)) return(NULL)

  # extract data
  data <- t(result$DS)
  lon <- result$startLon + (1:result$lonCount) * result$lonStep
  lat <- result$startLat + (1:result$latCount) * result$latStep
  
  # convert latitude to increase order
  if (lat[1] > lat[2]) {
    lat  <- rev(lat)
    data <- data[,ncol(data):1]
  }
  
  # get time information
  initTime <- as.POSIXct(time, format= "%Y%m%d%H", tz="GMT")
  fhour <- validTime
  validTime <- initTime + fhour*3600
  
  # return
  if (outList) {
    # construct list
    re <- list(gridData=raster::raster(list(x=lon, y=lat, z=data)),
               level=fcstLevel,
               initTime=initTime,
               validTime=validTime,
               fhour=fhour)
  } else {
    re <- data.table::data.table(lon=rep(lon, length(lat)),
                                 lat=rep(lat, each=length(lon)),
                                 lev=fcstLevel, validTime=validTime,
                                 initTime=initTime, fhour=fhour,
                                 var=as.vector(data))
  }
  
  return(re)
}

