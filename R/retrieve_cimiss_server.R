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


#' Convert observation data.frame to correct data type.
#'
#' @param obsData : data frame of observations.
#'
#' @return data frame of observations.
#' @export
#'
cimiss_obs_convert_type <- function(obsData){
  for (name in colnames(obsData)){
    if (startsWith(name, "Station")) next
    if (name %in% c("Province", "Country", "City", "Cnty", "Town",
                    "DATA_ID", "REP_CORR_ID", "Admin_Code_CHN")) next
    if (name == "Datetime") {
      obsData[[name]] <- lubridate::parse_date_time(obsData[[name]], "%Y%m%d%H%M%S")
      next
    }
    obsData[[name]] <- as.numeric(obsData[[name]])
  }
  return(obsData)
}


#' Get the observation latest time.
#'
#' @description
#'     Retrieve data latest times by cimiss music REST API.
#'     Refer to http://10.20.76.55/cimissapiweb/index_index.action
#'
#' @param dataCode : dataset code, like "SURF_CHN_MUL_HOR", "SURF_CHN_MUL_HOR_N", and so on.
#' @param latestTime : latestTime > 0, like 2 is return the latest time in 2 hours.
#'
#' @return the latest time, like '20200216020000'
#' @export
#'
#' @examples
#'   data <- cimiss_get_model_latest_time('SURF_CHN_MUL_HOR_N', latestTime=12)
#'
cimiss_get_obs_latest_time <- function(dataCode="SURF_CHN_MUL_HOR", latestTime=6){
  
  # retrieve parameters
  params = list()
  params[["dataCode"]] <- trimws(dataCode)
  params[["latestTime"]] <- as.character(latestTime)
  
  # Interface, refer to
  # http://10.20.76.55/cimissapiweb/apicustomapiclassdefine_list.action?ids=getSurfEleByTimeRange&apiclass=SURF_API
  interfaceId = "getSurfLatestTime"
  
  # retrieve data
  result <- get_http_result(interfaceId, lapply(params, trimws))
  if (is.null(result)) return(NULL)
  
  # extract data
  time = result[['DS']]$Datetime
  time = strptime(time, format="%Y%m%d%H%M%S")
  
  # return data
  return(time)
}


#' Retrieve station records from CIMISS by times.
#'
#' @description
#'     Retrieve data by cimiss music REST API.
#'     Refer to http://10.20.76.55/cimissapiweb/index_index.action
#'
#' @param times : time for retrieve, 'YYYYMMDDHHMISS,YYYYMMDDHHMISS,...'
#' @param dataCode : dataset code, like "SURF_CHN_MUL_HOR", "SURF_CHN_MUL_HOR_N", and so on.
#' @param transType: transform the return data frame's column type to datetime, numeric.
#' @param staLevels: station levels, seperated by ',', "011,012,013" is National Reference Climate Station,
#'                   Basic weather station, General weather station, ...
#' @param eleValueRanges: elements value ranges, seperated by ';'
#'                        range, (a,) is >a, [a,) is >=a, (,a) is <a, (,a] is <=a,
#'                               (a,b) is >a & <b, [a,b) is >=a & <b, (a,b] is >a & <=b,
#'                               [a,b] is >=a & <=b
#'                        list, a,b,c;
#'                        e.g., "VIS:(,1000);RHU:(70,)", "Q_PRE_1h:0,3,4" is PRE quantity is credible.
#' @param orderby: elements order, seperated by ',', like  
#'                 "TEM:asc" is ascending order temperature,
#'                 "TEM:asc,SUM_PRE_1h:desc" is ascending order temperature first and descending PRE_1h.
#' @param limitCnt: the number of maximum returned records
#' @param distinct: return unique records, default is false.
#' @param elements : elements for retrieve, 'ele1,ele2,...'
#'
#' @return data frame of observations.
#' @export
#' @examples \dontrun{
#'   r1 <- cimiss_obs_by_time('20181108000000')
#' }
#'
cimiss_obs_by_time <- function(times,
                               dataCode="SURF_CHN_MUL_HOR_N",
                               staLevels=NULL, eleValueRanges=NULL, orderby=NULL, 
                               limitCnt=NULL, distinct=FALSE, transType=TRUE,
                               elements="Station_Id_C,Station_Id_d,lat,lon,Datetime,TEM"){
  
  # retrieve parameters
  params = list()
  params[["dataCode"]] <- trimws(dataCode)
  params[["times"]] <- times
  if(!is.null(staLevels)) params[["staLevels"]] <- staLevels
  if(!is.null(eleValueRanges)) params[["eleValueRanges"]] <- eleValueRanges
  if(is.null(orderby)) params[["orderby"]] <- "Datetime:ASC" else params[["orderby"]] <- orderby
  if(!is.null(limitCnt)) params[["limitCnt"]] <- as.character(limitCnt)
  if(distinct) params[["distinct"]] <- "true"
  params[["elements"]] <- elements
  
  # Interface, refer to
  # http://10.20.76.55/cimissapiweb/apicustomapiclassdefine_list.action?ids=getSurfEleByTime&apiclass=SURF_API
  interfaceId <- "getSurfEleByTime"
  
  # retrieve data
  result <- get_http_result(interfaceId, lapply(params, trimws))
  if (is.null(result)) return(NULL)
  
  # extract data
  data = result[['DS']]
  if (transType) data <- cimiss_obs_convert_type(data)
  
  # return data
  return(data)
}


#' Retrieve station records from CIMISS in region by time.
#'
#' @description
#'     Retrieve data by cimiss music REST API.
#'     Refer to http://10.20.76.55/cimissapiweb/index_index.action
#'
#' @param times : times for retrieve, 'YYYYMMDDHHMISS,YYYYMMDDHHMISS,...', seperate by ","
#' @param minLat : minimum latitude
#' @param minLon : minimum longitude
#' @param maxLat : maximum latitude
#' @param maxLon : maximum longitude
#' @param dataCode : dataset code, like "SURF_CHN_MUL_HOR", "SURF_CHN_MUL_HOR_N", and so on.
#' @param transType: transform the return data frame's column type to datetime, numeric.
#' @param staLevels: station levels, seperated by ',', "011,012,013" is National Reference Climate Station,
#'                   Basic weather station, General weather station, ...
#' @param eleValueRanges: elements value ranges, seperated by ';'
#'                        range, (a,) is >a, [a,) is >=a, (,a) is <a, (,a] is <=a,
#'                               (a,b) is >a & <b, [a,b) is >=a & <b, (a,b] is >a & <=b,
#'                               [a,b] is >=a & <=b
#'                        list, a,b,c;
#'                        e.g., "VIS:(,1000);RHU:(70,)", "Q_PRE_1h:0,3,4" is PRE quantity is credible.
#' @param orderby: elements order, seperated by ',', like  
#'                 "TEM:asc" is ascending order temperature,
#'                 "TEM:asc,SUM_PRE_1h:desc" is ascending order temperature first and descending PRE_1h.
#' @param limitCnt: the number of maximum returned records
#' @param elements : elements for retrieve, 'ele1,ele2,...'
#'
#' @return data frame of observations.
#' @export
#'
#' @examples
#'   data <- cimiss_obs_in_rect_by_time('20200206000000', 35, 110, 45, 120)
#'
cimiss_obs_in_rect_by_time <- function(times, minLat, minLon, maxLat, maxLon,
                                       dataCode="SURF_CHN_MUL_HOR_N", transType=TRUE,
                                       staLevels=NULL, eleValueRanges=NULL, orderby=NULL, limitCnt=NULL,
                                       elements="Station_Id_C,Datetime,Lat,Lon,TEM"){
  
  # retrieve parameters
  params = list()
  params[["dataCode"]] <- trimws(dataCode)
  params[["elements"]] <- elements
  params[["times"]] <- times
  params[["minLat"]] <- as.character(minLat)
  params[["minLon"]] <- as.character(minLon)
  params[["maxLat"]] <- as.character(maxLat)
  params[["maxLon"]] <- as.character(maxLon)
  if(!is.null(staLevels)) params[["staLevels"]] <- staLevels
  if(!is.null(eleValueRanges)) params[["eleValueRanges"]] <- eleValueRanges
  if(is.null(orderby)) params[["orderby"]] <- "Datetime:ASC" else params[["orderby"]] <- orderby
  if(!is.null(limitCnt)) params[["limitCnt"]] <- as.character(limitCnt)

  # Interface, refer to
  # http://10.20.76.55/cimissapiweb/apicustomapiclassdefine_list.action?ids=getSurfEleInRectByTime&apiclass=SURF_API
  interfaceId = "getSurfEleInRectByTime"
  
  # retrieve data
  result <- get_http_result(interfaceId, lapply(params, trimws))
  if (is.null(result)) return(NULL)
  
  # extract data
  data = result[['DS']]
  if (transType) data <- cimiss_obs_convert_type(data)
  
  # return data
  return(data)
}


#' Retrieve station records from CIMISS in region by time range.
#'
#' @description
#'     Retrieve data by cimiss music REST API.
#'     Refer to http://10.20.76.55/cimissapiweb/index_index.action
#'
#' @param timeRange : time range for retrieve, "[YYYYMMDDHHMISS,YYYYMMDDHHMISS]",
#'                    like, "[201509010000,20150903060000]"
#' @param minLat : minimum latitude
#' @param minLon : minimum longitude
#' @param maxLat : maximum latitude
#' @param maxLon : maximum longitude
#' @param dataCode : dataset code, like "SURF_CHN_MUL_HOR", "SURF_CHN_MUL_HOR_N", and so on.
#' @param transType: transform the return data frame's column type to datetime, numeric.
#' @param eleValueRanges: elements value ranges, seperated by ';'
#'                        range, (a,) is >a, [a,) is >=a, (,a) is <a, (,a] is <=a,
#'                               (a,b) is >a & <b, [a,b) is >=a & <b, (a,b] is >a & <=b,
#'                               [a,b] is >=a & <=b
#'                        list, a,b,c;
#'                        e.g., "VIS:(,1000);RHU:(70,)", "Q_PRE_1h:0,3,4" is PRE quantity is credible.
#' @param orderby: elements order, seperated by ',', like  
#'                 "TEM:asc" is ascending order temperature,
#'                 "TEM:asc,SUM_PRE_1h:desc" is ascending order temperature first and descending PRE_1h.
#' @param limitCnt: the number of maximum returned records
#' @param elements : elements for retrieve, 'ele1,ele2,...'
#'
#' @return data frame of observations.
#' @export
#'
#' @examples
#'     elements <- "Station_Id_C,Station_Id_d,Station_Name,Station_levl,Datetime,Lat,Lon,PRE_Time_0808"
#'     data <- cimiss_obs_in_rect_by_time_range(
#'         "[20060801000000,20060801000000]",35,110,45,120,
#'         dataCode="SURF_CHN_MUL_DAY",elements=elements)
#'
cimiss_obs_in_rect_by_time_range <- function(timeRange, minLat, minLon, maxLat, maxLon,
                                             dataCode="SURF_CHN_MUL_HOR_N", transType=TRUE,
                                             eleValueRanges=NULL, orderby=NULL, limitCnt=NULL,
                                             elements="Station_Id_C,Datetime,Lat,Lon,TEM"){
  
  # retrieve parameters
  params = list()
  params[["dataCode"]] <- trimws(dataCode)
  params[["elements"]] <- elements
  params[["timeRange"]] <- timeRange
  params[["minLat"]] <- as.character(minLat)
  params[["minLon"]] <- as.character(minLon)
  params[["maxLat"]] <- as.character(maxLat)
  params[["maxLon"]] <- as.character(maxLon)
  if(!is.null(eleValueRanges)) params[["eleValueRanges"]] <- eleValueRanges
  if(is.null(orderby)) params[["orderby"]] <- "Datetime:ASC" else params[["orderby"]] <- orderby
  if(!is.null(limitCnt)) params[["limitCnt"]] <- as.character(limitCnt)
  
  # Interface, refer to
  # http://10.20.76.55/cimissapiweb/apicustomapiclassdefine_list.action?ids=getSurfEleInRectByTimeRange&apiclass=SURF_API
  interfaceId <- "getSurfEleInRectByTimeRange"

  # retrieve data
  result <- get_http_result(interfaceId, lapply(params, trimws))
  if (is.null(result)) return(NULL)
  
  # extract data
  data = result[['DS']]
  if (transType) data <- cimiss_obs_convert_type(data)
  
  # return data
  return(data)
}


#' Retrieve station records from CIMISS in adminstration region by time.
#'
#' @description
#'     Retrieve data by cimiss music REST API.
#'     Refer to http://10.20.76.55/cimissapiweb/index_index.action
#'
#' @param times : times for retrieve, 'YYYYMMDDHHMISS,YYYYMMDDHHMISS,...', seperate by ","
#' @param adminCodes: administration(or province code), sperated by ",",
#'                    like "110000" is Beijing, "440000" is Guangdong
#' @param dataCode : dataset code, like "SURF_CHN_MUL_HOR", "SURF_CHN_MUL_HOR_N", and so on.
#' @param transType: transform the return data frame's column type to datetime, numeric.
#' @param staLevels: station levels, seperated by ',', "011,012,013" is National Reference Climate Station,
#'                   Basic weather station, General weather station, ...
#' @param eleValueRanges: elements value ranges, seperated by ';'
#'                        range, (a,) is >a, [a,) is >=a, (,a) is <a, (,a] is <=a,
#'                               (a,b) is >a & <b, [a,b) is >=a & <b, (a,b] is >a & <=b,
#'                               [a,b] is >=a & <=b
#'                        list, a,b,c;
#'                        e.g., "VIS:(,1000);RHU:(70,)", "Q_PRE_1h:0,3,4" is PRE quantity is credible.
#' @param orderby: elements order, seperated by ',', like  
#'                 "TEM:asc" is ascending order temperature,
#'                 "TEM:asc,SUM_PRE_1h:desc" is ascending order temperature first and descending PRE_1h.
#' @param limitCnt: the number of maximum returned records
#' @param elements : elements for retrieve, 'ele1,ele2,...'
#'
#' @return data frame of observations.
#' @export
#'
#' @examples
#'   data <- cimiss_obs_in_admin_by_time('20200206000000', adminCodes="110000")
#'
cimiss_obs_in_admin_by_time <- function(times, adminCodes="110000",
                                        dataCode="SURF_CHN_MUL_HOR_N", transType=TRUE,
                                        staLevels=NULL, eleValueRanges=NULL, orderby=NULL, limitCnt=NULL,
                                        elements="Station_Id_C,Datetime,Lat,Lon,TEM"){
  
  # retrieve parameters
  params = list()
  params[["dataCode"]] <- trimws(dataCode)
  params[["elements"]] <- elements
  params[["times"]] <- times
  params[["adminCodes"]] <- adminCodes
  if(!is.null(staLevels)) params[["staLevels"]] <- staLevels
  if(!is.null(eleValueRanges)) params[["eleValueRanges"]] <- eleValueRanges
  if(is.null(orderby)) params[["orderby"]] <- "Datetime:ASC" else params[["orderby"]] <- orderby
  if(!is.null(limitCnt)) params[["limitCnt"]] <- as.character(limitCnt)
  
  # Interface, refer to
  # http://10.20.76.55/cimissapiweb/apicustomapiclassdefine_list.action?ids=getNafpEleGridByTimeAndLevelAndValidtime&apiclass=NAFP_API
  interfaceId = "getSurfEleInRegionByTime"
  
  # retrieve data
  result <- get_http_result(interfaceId, lapply(params, trimws))
  if (is.null(result)) return(NULL)
  
  # extract data
  data = result[['DS']]
  if (transType) data <- cimiss_obs_convert_type(data)
  
  # return data
  return(data)
}


#' Retrieve station records from CIMISS in adminstration region by time range.
#'
#' @description
#'     Retrieve data by cimiss music REST API.
#'     Refer to http://10.20.76.55/cimissapiweb/index_index.action
#'
#' @param timeRange : time range for retrieve, "[YYYYMMDDHHMISS,YYYYMMDDHHMISS]",
#'                    like, "[201509010000,20150903060000]"
#' @param adminCodes: administration(or province code), sperated by ",",
#'                    like "110000" is Beijing, "440000" is Guangdong
#' @param dataCode : dataset code, like "SURF_CHN_MUL_HOR", "SURF_CHN_MUL_HOR_N", and so on.
#' @param transType: transform the return data frame's column type to datetime, numeric.
#' @param staLevels: station levels, seperated by ',', "011,012,013" is National Reference Climate Station,
#'                   Basic weather station, General weather station, ...
#' @param eleValueRanges: elements value ranges, seperated by ';'
#'                        range, (a,) is >a, [a,) is >=a, (,a) is <a, (,a] is <=a,
#'                               (a,b) is >a & <b, [a,b) is >=a & <b, (a,b] is >a & <=b,
#'                               [a,b] is >=a & <=b
#'                        list, a,b,c;
#'                        e.g., "VIS:(,1000);RHU:(70,)", "Q_PRE_1h:0,3,4" is PRE quantity is credible.
#' @param orderby: elements order, seperated by ',', like  
#'                 "TEM:asc" is ascending order temperature,
#'                 "TEM:asc,SUM_PRE_1h:desc" is ascending order temperature first and descending PRE_1h.
#' @param limitCnt: the number of maximum returned records
#' @param elements : elements for retrieve, 'ele1,ele2,...'
#'
#' @return data frame of observations.
#' @export
#'
#' @examples
#'   data <- cimiss_obs_in_admin_by_time_range("[202002010000,20200203060000]", adminCodes="110000")
#'
cimiss_obs_in_admin_by_time_range <- function(timeRange, adminCodes="110000",
                                              dataCode="SURF_CHN_MUL_HOR_N", transType=TRUE,
                                              staLevels=NULL, eleValueRanges=NULL, orderby=NULL, limitCnt=NULL,
                                              elements="Station_Id_C,Datetime,Lat,Lon,TEM"){
  
  # retrieve parameters
  params = list()
  params[["dataCode"]] <- trimws(dataCode)
  params[["elements"]] <- elements
  params[["times"]] <- timeRange
  params[["adminCodes"]] <- adminCodes
  if(!is.null(staLevels)) params[["staLevels"]] <- staLevels
  if(!is.null(eleValueRanges)) params[["eleValueRanges"]] <- eleValueRanges
  if(is.null(orderby)) params[["orderby"]] <- "Datetime:ASC" else params[["orderby"]] <- orderby
  if(!is.null(limitCnt)) params[["limitCnt"]] <- as.character(limitCnt)
  
  # Interface, refer to
  # http://10.20.76.55/cimissapiweb/apicustomapiclassdefine_list.action?ids=getNafpEleGridByTimeAndLevelAndValidtime&apiclass=NAFP_API
  interfaceId = "getSurfEleInRegionByTimeRange"
  
  # retrieve data
  result <- get_http_result(interfaceId, lapply(params, trimws))
  if (is.null(result)) return(NULL)
  
  # extract data
  data = result[['DS']]
  if (transType) data <- cimiss_obs_convert_type(data)
  
  # return data
  return(data)
}


#' Retrieve station records from CIMISS in basin region by time.
#'
#' @description
#'     Retrieve data by cimiss music REST API.
#'     Refer to http://10.20.76.55/cimissapiweb/index_index.action
#'
#' @param times : times for retrieve, 'YYYYMMDDHHMISS,YYYYMMDDHHMISS,...', seperate by ","
#' @param basinCodes: basin codes, sperated by ",",
#'                    like "CJLY" is Yangzi River, "sta_2480" is 2480 stations
#' @param dataCode : dataset code, like "SURF_CHN_MUL_HOR", "SURF_CHN_MUL_HOR_N", and so on.
#' @param transType: transform the return data frame's column type to datetime, numeric.
#' @param staLevels: station levels, seperated by ',', "011,012,013" is National Reference Climate Station,
#'                   Basic weather station, General weather station, ...
#' @param eleValueRanges: elements value ranges, seperated by ';'
#'                        range, (a,) is >a, [a,) is >=a, (,a) is <a, (,a] is <=a,
#'                               (a,b) is >a & <b, [a,b) is >=a & <b, (a,b] is >a & <=b,
#'                               [a,b] is >=a & <=b
#'                        list, a,b,c;
#'                        e.g., "VIS:(,1000);RHU:(70,)", "Q_PRE_1h:0,3,4" is PRE quantity is credible.
#' @param orderby: elements order, seperated by ',', like  
#'                 "TEM:asc" is ascending order temperature,
#'                 "TEM:asc,SUM_PRE_1h:desc" is ascending order temperature first and descending PRE_1h.
#' @param limitCnt: the number of maximum returned records
#' @param elements : elements for retrieve, 'ele1,ele2,...'
#'
#' @return data frame of observations.
#' @export
#'
#' @examples
#'   data <- cimiss_obs_in_basin_by_time('20200206000000', basinCodes="CJLY")
#'
cimiss_obs_in_basin_by_time <- function(times, basinCodes="CJLY",
                                        dataCode="SURF_CHN_MUL_HOR_N", transType=TRUE,
                                        staLevels=NULL, eleValueRanges=NULL, orderby=NULL, limitCnt=NULL,
                                        elements="Station_Id_C,Datetime,Lat,Lon,TEM"){
  
  # retrieve parameters
  params = list()
  params[["dataCode"]] <- trimws(dataCode)
  params[["elements"]] <- elements
  params[["times"]] <- times
  params[["basinCodes"]] <- basinCodes
  if(!is.null(staLevels)) params[["staLevels"]] <- staLevels
  if(!is.null(eleValueRanges)) params[["eleValueRanges"]] <- eleValueRanges
  if(is.null(orderby)) params[["orderby"]] <- "Datetime:ASC" else params[["orderby"]] <- orderby
  if(!is.null(limitCnt)) params[["limitCnt"]] <- as.character(limitCnt)
  
  # Interface, refer to
  # http://10.20.76.55/cimissapiweb/apicustomapiclassdefine_list.action?ids=getNafpEleGridByTimeAndLevelAndValidtime&apiclass=NAFP_API
  interfaceId = "getSurfEleInBasinByTime"
  
  # retrieve data
  result <- get_http_result(interfaceId, lapply(params, trimws))
  if (is.null(result)) return(NULL)
  
  # extract data
  data = result[['DS']]
  if (transType) data <- cimiss_obs_convert_type(data)
  
  # return data
  return(data)
}


#' Retrieve station records from CIMISS in basin region by time range.
#'
#' @description
#'     Retrieve data by cimiss music REST API.
#'     Refer to http://10.20.76.55/cimissapiweb/index_index.action
#'
#' @param timeRange : time range for retrieve, "[YYYYMMDDHHMISS,YYYYMMDDHHMISS]",
#'                    like, "[201509010000,20150903060000]"
#' @param basinCodes: basin codes, sperated by ",",
#'                    like "CJLY" is Yangzi River, "sta_2480" is 2480 stations
#' @param dataCode : dataset code, like "SURF_CHN_MUL_HOR", "SURF_CHN_MUL_HOR_N", and so on.
#' @param transType: transform the return data frame's column type to datetime, numeric.
#' @param staLevels: station levels, seperated by ',', "011,012,013" is National Reference Climate Station,
#'                   Basic weather station, General weather station, ...
#' @param eleValueRanges: elements value ranges, seperated by ';'
#'                        range, (a,) is >a, [a,) is >=a, (,a) is <a, (,a] is <=a,
#'                               (a,b) is >a & <b, [a,b) is >=a & <b, (a,b] is >a & <=b,
#'                               [a,b] is >=a & <=b
#'                        list, a,b,c;
#'                        e.g., "VIS:(,1000);RHU:(70,)", "Q_PRE_1h:0,3,4" is PRE quantity is credible.
#' @param orderby: elements order, seperated by ',', like  
#'                 "TEM:asc" is ascending order temperature,
#'                 "TEM:asc,SUM_PRE_1h:desc" is ascending order temperature first and descending PRE_1h.
#' @param limitCnt: the number of maximum returned records
#' @param elements : elements for retrieve, 'ele1,ele2,...'
#'
#' @return data frame of observations.
#' @export
#'
#' @examples
#'   data <- cimiss_obs_in_basin_by_time_range("[202002010000,20200203060000]", basinCodes="CJLY")
#'
cimiss_obs_in_basin_by_time_range <- function(timeRange, basinCodes="CJLY",
                                              dataCode="SURF_CHN_MUL_HOR_N", transType=TRUE,
                                              staLevels=NULL, eleValueRanges=NULL, orderby=NULL, limitCnt=NULL,
                                              elements="Station_Id_C,Datetime,Lat,Lon,TEM"){
  
  # retrieve parameters
  params = list()
  params[["dataCode"]] <- trimws(dataCode)
  params[["elements"]] <- elements
  params[["timeRange"]] <- timeRange
  params[["basinCodes"]] <- basinCodes
  if(!is.null(staLevels)) params[["staLevels"]] <- staLevels
  if(!is.null(eleValueRanges)) params[["eleValueRanges"]] <- eleValueRanges
  if(is.null(orderby)) params[["orderby"]] <- "Datetime:ASC" else params[["orderby"]] <- orderby
  if(!is.null(limitCnt)) params[["limitCnt"]] <- as.character(limitCnt)
  
  # Interface, refer to
  # http://10.20.76.55/cimissapiweb/apicustomapiclassdefine_list.action?ids=getNafpEleGridByTimeAndLevelAndValidtime&apiclass=NAFP_API
  interfaceId = "getSurfEleInBasinByTimeRange"
  
  # retrieve data
  result <- get_http_result(interfaceId, lapply(params, trimws))
  if (is.null(result)) return(NULL)
  
  # extract data
  data = result[['DS']]
  if (transType) data <- cimiss_obs_convert_type(data)
  
  # return data
  return(data)
}


#' Retrieve station records from CIMISS by time and station ID.
#'
#' @description
#'     Retrieve data by cimiss music REST API.
#'     Refer to http://10.20.76.55/cimissapiweb/index_index.action
#'
#' @param times : time for retrieve, 'YYYYMMDDHHMISS,YYYYMMDDHHMISS,...'
#' @param dataCode : dataset code, like "SURF_CHN_MUL_HOR", "SURF_CHN_MUL_HOR_N", and so on.
#' @param transType: transform the return data frame's column type to datetime, numeric.
#' @param staLevels: station levels, seperated by ',', "011,012,013" is National Reference Climate Station,
#'                   Basic weather station, General weather station, ...
#' @param eleValueRanges: elements value ranges, seperated by ';'
#'                        range, (a,) is >a, [a,) is >=a, (,a) is <a, (,a] is <=a,
#'                               (a,b) is >a & <b, [a,b) is >=a & <b, (a,b] is >a & <=b,
#'                               [a,b] is >=a & <=b
#'                        list, a,b,c;
#'                        e.g., "VIS:(,1000);RHU:(70,)", "Q_PRE_1h:0,3,4" is PRE quantity is credible.
#' @param orderby: elements order, seperated by ',', like  
#'                 "TEM:asc" is ascending order temperature,
#'                 "TEM:asc,SUM_PRE_1h:desc" is ascending order temperature first and descending PRE_1h.
#' @param limitCnt: the number of maximum returned records
#' @param elements : elements for retrieve, 'ele1,ele2,...'
#' @param staIds : station ids, 'xxxxx,xxxxx,...'
#'
#' @return data frame of observations.
#' @export
#' @examples \dontrun{
#'   r1 <- cimiss_obs_by_time_and_staIds('20181108000000')
#' }
#'
cimiss_obs_by_time_and_staIds <- function(times,
                                          dataCode="SURF_CHN_MUL_HOR_N", transType=TRUE,
                                          staLevels=NULL, eleValueRanges=NULL, orderby=NULL, limitCnt=NULL,
                                          elements="Station_Id_C,Datetime,TEM",
                                          staIds="54511"){
  
  # retrieve parameters
  params = list()
  params[["dataCode"]] <- trimws(dataCode)
  if(!is.null(staLevels)) params[["staLevels"]] <- staLevels
  if(!is.null(eleValueRanges)) params[["eleValueRanges"]] <- eleValueRanges
  if(is.null(orderby)) params[["orderby"]] <- "Datetime:ASC" else params[["orderby"]] <- orderby
  if(!is.null(limitCnt)) params[["limitCnt"]] <- as.character(limitCnt)
  params[["elements"]] <- elements
  params[["times"]] <- times
  params[["staIds"]] <- staIds
  
  # Interface, refer to
  # http://10.20.76.55/cimissapiweb/apicustomapiclassdefine_list.action?ids=getSurfEleByTimeAndStaID&apiclass=SURF_API
  interfaceId = "getSurfEleByTimeAndStaID"
  
  # retrieve data
  result <- get_http_result(interfaceId, lapply(params, trimws))
  if (is.null(result)) return(NULL)
  
  # extract data
  data = result[['DS']]
  if (transType) data <- cimiss_obs_convert_type(data)
  
  # return data
  return(data)
}


#' Retrieve station records from CIMISS by time and station ID.
#'
#' @description
#'     Retrieve data by cimiss music REST API.
#'     Refer to http://10.20.76.55/cimissapiweb/index_index.action
#'
#' @param timeRange : time range for retrieve, "[YYYYMMDDHHMISS,YYYYMMDDHHMISS]",
#'                    like, "[201509010000,20150903060000]"
#' @param dataCode : dataset code, like "SURF_CHN_MUL_HOR", "SURF_CHN_MUL_HOR_N", and so on.
#' @param transType: transform the return data frame's column type to datetime, numeric.
#' @param hourSeparate: hour space, [1, 24], like 6: 0,6,12,18
#' @param minSeparate: minute space, [1, 60], like 10: 0,10,20,30,40,50
#' @param eleValueRanges: elements value ranges, seperated by ';'
#'                        range, (a,) is >a, [a,) is >=a, (,a) is <a, (,a] is <=a,
#'                               (a,b) is >a & <b, [a,b) is >=a & <b, (a,b] is >a & <=b,
#'                               [a,b] is >=a & <=b
#'                        list, a,b,c;
#'                        e.g., "VIS:(,1000);RHU:(70,)", "Q_PRE_1h:0,3,4" is PRE quantity is credible.
#' @param orderby: elements order, seperated by ',', like  
#'                 "TEM:asc" is ascending order temperature,
#'                 "TEM:asc,SUM_PRE_1h:desc" is ascending order temperature first and descending PRE_1h.
#' @param limitCnt: the number of maximum returned records
#' @param elements : elements for retrieve, 'ele1,ele2,...'
#' @param staIds : station ids, 'xxxxx,xxxxx,...'
#'
#' @return data frame of observations.
#' @export
#' @examples \dontrun{
#'   r1 <- cimiss_obs_by_time_range_and_staIds("[20060801000000,20060801000000]")
#' }
#'
cimiss_obs_by_time_range_and_staIds <- function(timeRange,
                                                dataCode="SURF_CHN_MUL_HOR_N", transType=TRUE,
                                                hourSeparate=NULL, minSeparate=NULL,
                                                eleValueRanges=NULL, orderby=NULL, limitCnt=NULL,
                                                elements="Station_Id_C,Datetime,TEM",
                                                staIds="54511"){
  
  # retrieve parameters
  params = list()
  params[["dataCode"]] <- trimws(dataCode)
  params[["elements"]] <- elements
  params[["timeRange"]] <- timeRange
  if(!is.null(hourSeparate)) params[["hourSeparate"]] <- as.character(hourSeparate)
  if(!is.null(minSeparate)) params[["minSeparate"]] <- as.character(minSeparate)
  if(!is.null(eleValueRanges)) params[["eleValueRanges"]] <- eleValueRanges
  if(is.null(orderby)) params[["orderby"]] <- "Datetime:ASC" else params[["orderby"]] <- orderby
  if(!is.null(limitCnt)) params[["limitCnt"]] <- as.character(limitCnt)
  params[["staIds"]] <- staIds
  
  # Interface, refer to
  # http://10.20.76.55/cimissapiweb/apicustomapiclassdefine_list.action?ids=getSurfEleByTimeRangeAndStaID&apiclass=SURF_API
  interfaceId = "getSurfEleByTimeRangeAndStaID"
  
  # retrieve data
  result <- get_http_result(interfaceId, lapply(params, trimws))
  if (is.null(result)) return(NULL)
  
  # extract data
  data = result[['DS']]
  if (transType) data <- cimiss_obs_convert_type(data)
  
  # return data
  return(data)
}

#' Retrieve station records from CIMISS by same period.
#'
#' @description
#'     Retrieve data by cimiss music REST API.
#'     Refer to http://10.20.76.55/cimissapiweb/index_index.action
#'
#' @param dataCode : dataset code, like "SURF_CHN_MUL_HOR", "SURF_CHN_MUL_HOR_N", and so on.
#' @param transType: transform the return data frame's column type to datetime, numeric.
#' @param minYear: start year, like 2005
#' @param maxYear: end year, like 2015
#' @param minMD: start date, like "0125" is 01/25
#' @param maxMD: end date, like "0205" is 02/25
#' @param eleValueRanges: elements value ranges, seperated by ';'
#'                        range, (a,) is >a, [a,) is >=a, (,a) is <a, (,a] is <=a,
#'                               (a,b) is >a & <b, [a,b) is >=a & <b, (a,b] is >a & <=b,
#'                               [a,b] is >=a & <=b
#'                        list, a,b,c;
#'                        e.g., "VIS:(,1000);RHU:(70,)", "Q_PRE_1h:0,3,4" is PRE quantity is credible.
#' @param orderby: elements order, seperated by ',', like  
#'                 "TEM:asc" is ascending order temperature,
#'                 "TEM:asc,SUM_PRE_1h:desc" is ascending order temperature first and descending PRE_1h.
#' @param limitCnt: the number of maximum returned records
#' @param elements : elements for retrieve, 'ele1,ele2,...'
#'
#' @return data frame of observations.
#' @export
#' @examples \dontrun{
#'   r1 <- cimiss_obs_by_period(2015, 2018, "0501", "0505")
#' }
#'
cimiss_obs_by_period <- function(minYear, maxYear, minMD, maxMD, dataCode="SURF_CHN_MUL_HOR_N",
                                 transType=TRUE, eleValueRanges=NULL, orderby=NULL, limitCnt=NULL,
                                 elements="Station_Id_C,Station_Id_d,lat,lon,Datetime,TEM"){
  
  # retrieve parameters
  params = list()
  params[["dataCode"]] <- trimws(dataCode)
  params[["minYear"]] <- as.character(minYear)
  params[["maxYear"]] <- as.character(maxYear)
  params[["minMD"]] <- minMD
  params[["maxMD"]] <- maxMD
  if(!is.null(eleValueRanges)) params[["eleValueRanges"]] <- eleValueRanges
  if(is.null(orderby)) params[["orderby"]] <- "Datetime:ASC" else params[["orderby"]] <- orderby
  if(!is.null(limitCnt)) params[["limitCnt"]] <- as.character(limitCnt)
  params[["elements"]] <- elements
  
  # Interface, refer to
  # http://10.20.76.55/cimissapiweb/apicustomapiclassdefine_list.action?ids=statSurfTem&apiclass=SURF_API
  interfaceId <- "getSurfEleByInHistoryBySamePeriod"
  
  # retrieve data
  result <- get_http_result(interfaceId, lapply(params, trimws))
  if (is.null(result)) return(NULL)
  
  # extract data
  data = result[['DS']]
  if (transType) data <- cimiss_obs_convert_type(data)
  
  # return data
  return(data)
}


#' Retrieve station records from CIMISS by same period.
#'
#' @description
#'     Retrieve data by cimiss music REST API.
#'     Refer to http://10.20.76.55/cimissapiweb/index_index.action
#'
#' @param dataCode : dataset code, like "SURF_CHN_MUL_HOR", "SURF_CHN_MUL_HOR_N", and so on.
#' @param transType: transform the return data frame's column type to datetime, numeric.
#' @param minYear: start year, like "2005"
#' @param maxYear: end year, like "2015"
#' @param minMD: start date, like "0125" is 01/25
#' @param maxMD: end date, like "0205" is 02/25
#' @param eleValueRanges: elements value ranges, seperated by ';'
#'                        range, (a,) is >a, [a,) is >=a, (,a) is <a, (,a] is <=a,
#'                               (a,b) is >a & <b, [a,b) is >=a & <b, (a,b] is >a & <=b,
#'                               [a,b] is >=a & <=b
#'                        list, a,b,c;
#'                        e.g., "VIS:(,1000);RHU:(70,)", "Q_PRE_1h:0,3,4" is PRE quantity is credible.
#' @param orderby: elements order, seperated by ',', like  
#'                 "TEM:asc" is ascending order temperature,
#'                 "TEM:asc,SUM_PRE_1h:desc" is ascending order temperature first and descending PRE_1h.
#' @param limitCnt: the number of maximum returned records
#' @param elements : elements for retrieve, 'ele1,ele2,...'
#'
#' @return data frame of observations.
#' @export
#' @examples \dontrun{
#'   r1 <- cimiss_obs_by_period_and_staIds(2015, 2018, "0501", "0505", staIds="54511")
#' }
#'
cimiss_obs_by_period_and_staIds <- function(minYear, maxYear, minMD, maxMD, staIds="54511",
                                            dataCode="SURF_CHN_MUL_HOR_N", transType=TRUE,
                                            eleValueRanges=NULL, orderby=NULL, limitCnt=NULL,
                                            elements="Station_Id_C,Station_Id_d,lat,lon,Datetime,TEM"){
  
  # retrieve parameters
  params = list()
  params[["dataCode"]] <- trimws(dataCode)
  params[["minYear"]] <- as.character(minYear)
  params[["maxYear"]] <- as.character(maxYear)
  params[["minMD"]] <- minMD
  params[["maxMD"]] <- maxMD
  params[["staIds"]] <- staIds
  if(!is.null(eleValueRanges)) params[["eleValueRanges"]] <- eleValueRanges
  if(is.null(orderby)) params[["orderby"]] <- "Datetime:ASC" else params[["orderby"]] <- orderby
  if(!is.null(limitCnt)) params[["limitCnt"]] <- as.character(limitCnt)
  params[["elements"]] <- elements
  
  # Interface, refer to
  # http://10.20.76.55/cimissapiweb/apicustomapiclassdefine_list.action?ids=statSurfTem&apiclass=SURF_API
  interfaceId <- "getSurfEleInHistoryBySamePeriodAndStaID"
  
  # retrieve data
  result <- get_http_result(interfaceId, lapply(params, trimws))
  if (is.null(result)) return(NULL)
  
  # extract data
  data = result[['DS']]
  if (transType) data <- cimiss_obs_convert_type(data)
  
  # return data
  return(data)
}


#' Retrieve station records from CIMISS by same period in province.
#'
#' @description
#'     Retrieve data by cimiss music REST API.
#'     Refer to http://10.20.76.55/cimissapiweb/index_index.action
#'
#' @param dataCode : dataset code, like "SURF_CHN_MUL_HOR", "SURF_CHN_MUL_HOR_N", and so on.
#' @param transType: transform the return data frame's column type to datetime, numeric.
#' @param minYear: start year, like "2005"
#' @param maxYear: end year, like "2015"
#' @param minMD: start date, like "0125" is 01/25
#' @param maxMD: end date, like "0205" is 02/25
#' @param adminCodes: administration(or province code), sperated by ",",
#'                    like "110000" is Beijing, "440000" is Guangdong
#' @param eleValueRanges: elements value ranges, seperated by ';'
#'                        range, (a,) is >a, [a,) is >=a, (,a) is <a, (,a] is <=a,
#'                               (a,b) is >a & <b, [a,b) is >=a & <b, (a,b] is >a & <=b,
#'                               [a,b] is >=a & <=b
#'                        list, a,b,c;
#'                        e.g., "VIS:(,1000);RHU:(70,)", "Q_PRE_1h:0,3,4" is PRE quantity is credible.
#' @param orderby: elements order, seperated by ',', like  
#'                 "TEM:asc" is ascending order temperature,
#'                 "TEM:asc,SUM_PRE_1h:desc" is ascending order temperature first and descending PRE_1h.
#' @param limitCnt: the number of maximum returned records
#' @param elements : elements for retrieve, 'ele1,ele2,...'
#'
#' @return data frame of observations.
#' @export
#' @examples \dontrun{
#'   r1 <- cimiss_obs_by_period(2015, 2018, "0501", "0505", adminCodes="110000")
#' }
#'
cimiss_obs_in_admin_by_period <- function(minYear, maxYear, minMD, maxMD, adminCodes="110000",
                                          dataCode="SURF_CHN_MUL_HOR_N", transType=TRUE,
                                          eleValueRanges=NULL, orderby=NULL, limitCnt=NULL,
                                          elements="Station_Id_C,Station_Id_d,lat,lon,Datetime,TEM"){
  
  # retrieve parameters
  params = list()
  params[["dataCode"]] <- trimws(dataCode)
  params[["minYear"]] <- as.character(minYear)
  params[["maxYear"]] <- as.character(maxYear)
  params[["minMD"]] <- minMD
  params[["maxMD"]] <- maxMD
  params[["adminCodes"]] <- adminCodes
  if(!is.null(eleValueRanges)) params[["eleValueRanges"]] <- eleValueRanges
  if(is.null(orderby)) params[["orderby"]] <- "Datetime:ASC" else params[["orderby"]] <- orderby
  if(!is.null(limitCnt)) params[["limitCnt"]] <- as.character(limitCnt)
  params[["elements"]] <- elements
  
  # Interface, refer to
  # http://10.20.76.55/cimissapiweb/apicustomapiclassdefine_list.action?ids=statSurfTem&apiclass=SURF_API
  interfaceId <- "getSurfEleInHistoryBySamePeriodAndRegion"
  
  # retrieve data
  result <- get_http_result(interfaceId, lapply(params, trimws))
  if (is.null(result)) return(NULL)
  
  # extract data
  data = result[['DS']]
  if (transType) data <- cimiss_obs_convert_type(data)
  
  # return data
  return(data)
}


#' Get the model latest initial time.
#'
#' @description
#'     Retrieve data by cimiss music REST API.
#'     Refer to http://10.20.76.55/cimissapiweb/index_index.action
#'
#' @param dataCode : dataset code, like "NAFP_FOR_FTM_HIGH_EC_ANEA", "NAFP_FOR_FTM_HIGH_EC_ASI", and so on.
#' @param latestTime : latestTime, >0, like 2 is return the latest data in 2 hours.
#'
#' @return data frame of observations.
#' @export
#'
#' @examples
#'   data <- cimiss_get_model_latest_time('NAFP_FOR_FTM_HIGH_EC_ANEA', latestTime=12)
#'
cimiss_get_model_latest_time <- function(dataCode="NAFP_FOR_FTM_HIGH_EC_ANEA", latestTime=24){
  
  # retrieve parameters
  params = list()
  params[["dataCode"]] <- trimws(dataCode)
  params[["latestTime"]] <- as.character(latestTime)
  
  # Interface, refer to
  # http://10.20.76.55/cimissapiweb/apicustomapiclassdefine_list.action?ids=getNafpEleGridByTimeAndLevelAndValidtime&apiclass=NAFP_API
  interfaceId = "getNafpLatestTime"
  
  # retrieve data
  result <- get_http_result(interfaceId, lapply(params, trimws))
  if (is.null(result)) return(NULL)
  
  # extract data
  time = result[['DS']]$Datetime
  time = strptime(time, format="%Y%m%d%H%M%S")
  
  # return data
  return(time)
}


#' Retrieve grid data from CIMISS service.
#'
#' @description
#'     Retrieve data by cimiss music REST API.
#'     Refer to http://10.20.76.55/cimissapiweb/index_index.action
#'
#' @param time, model run time, like "2016081712"
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
cimiss_model_grid <- function(time, dataCode="NAFP_FOR_FTM_HIGH_EC_GLB",
                              fcstLevel=0, fcstEle="TEF0", validTime=0, outList=FALSE) {
  # retrieve parameters
  params <- list()
  params[['dataCode']] <- trimws(dataCode)
  params[['fcstEle']] <- fcstEle
  params[['time']] <- paste0(time,"0000")
  params[['fcstLevel']] <- as.character(fcstLevel)
  params[['validTime']] <- as.character(validTime)
  
  # Interface, refer to
  # http://10.20.76.55/cimissapiweb/apicustomapiclassdefine_list.action?ids=getNafpEleGridByTimeAndLevelAndValidtime&apiclass=NAFP_API
  interfaceId <- "getNafpEleGridByTimeAndLevelAndValidtime"
  
  # retrieve data
  result <- get_http_result(interfaceId, lapply(params, trimws))
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


#' Retrieve grid data from CIMISS service.
#'
#' @description
#'     Retrieve data by cimiss music REST API.
#'     Refer to http://10.20.76.55/cimissapiweb/index_index.action
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
  params[['dataCode']] <- trimws(dataCode)
  params[['fcstEle']] <- fcstEle
  params[['time']] <- paste0(time,"0000")
  params[['minLat']] <- as.character(minLat)
  params[['minLon']] <- as.character(minLon)
  params[['maxLat']] <- as.character(maxLat)
  params[['maxLon']] <- as.character(maxLon)
  params[['fcstLevel']] <- as.character(fcstLevel)
  params[['validTime']] <- as.character(validTime)
  
  # Interface, refer to
  # http://10.20.76.55/cimissapiweb/apicustomapiclassdefine_list.action?ids=getNafpEleGridInRectByTimeAndLevelAndValidtime&apiclass=NAFP_API
  interfaceId <- "getNafpEleGridInRectByTimeAndLevelAndValidtime"

  # retrieve data
  result <- get_http_result(interfaceId, lapply(params, trimws))
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


#' retrieve model point forecast data from CIMISS.
#' 
#' @description
#'     Retrieve data by cimiss music REST API.
#'     Refer to http://10.20.76.55/cimissapiweb/index_index.action
#'
#' @param time, run time for retrieve, 'YYYYMMDDHHMISS,YYYYMMDDHHMISS,...'
#' @param dataCode, like "NAFP_FOR_FTM_HIGH_EC_GLB", "NAFP_FOR_FTM_HIGH_EC_ASI", and so on.
#' @param fcstEle, forecast element (single), default is 2m temperature "TEF0"
#' @param fcstLevel, forecast level (single)
#' @param minVT, start forecast hour
#' @param maxVT, end forecast hour
#' @param staIds : station ids, 'xxxxx,xxxxx,...'
#'
#' @return data.frame of point forecast.
#' @export
#'
#' @examples
#'   dataV <- cimiss_model_staIds_by_time_and_level("20170120000000")
#'
cimiss_model_staIds_by_time_and_level <- function(time,
                                                 dataCode="NAFP_FOR_FTM_HIGH_EC_GLB",
                                                 fcstEle="TEF0", fcstLevel=0,
                                                 minVT=0, maxVT=240,
                                                 staIds='54511') {
  # retrieve parameters
  params = list()
  params[["dataCode"]] <- trimws(dataCode)
  params[["time"]] <- time
  params[["fcstEle"]] <- fcstEle
  params[["minVT"]] <- as.character(minVT)
  params[["maxVT"]] <- as.character(maxVT)
  params[["fcstLevel"]] <- as.character(fcstLevel)
  params[["latLons"]] <- latLons
  
  # Interface, refer to
  # http://10.20.76.55/cimissapiweb/apicustomapiclassdefine_list.action?ids=getNafpEleAtPointByTimeAndLevelAndValidtimeRange&apiclass=NAFP_API
  interfaceId = "getNafpEleByTimeAndLevelAndValidtimeRangeAndStaID"
  
  # retrieve data
  result <- get_http_result(interfaceId, lapply(params, trimws))
  if (is.null(result)) return(NULL)
  
  # extract data
  data = result[['DS']]
  
  # return data
  return(data)
}
