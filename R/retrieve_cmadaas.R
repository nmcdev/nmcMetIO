#' Get the http result from CMADaaS REST api service
#'
#' @description
#' 需要在"/用户名/.nmcdev/config.ini"配置文件中设置CMADaaS数据库地址和账户信息:  
#' 
#' * [CMADaaS]  
#' * DNS = xx.xx.xx.xx  
#' * PORT = xx  
#' * USER_ID = test  
#' * PASSWORD = 123456  
#' * serviceNodeId = NMIC_MUSIC_CMADAAS  
#' 
#' @param interface_id, MUSIC interface id.
#' @param params, dictionary for MUSIC parameters.    
#'               可以直接指定'serviceNodeId', 'userId'和'pwd'三个参数,
#'               若没有, 则从 .nmcdev 配置文件中读取.
#' @param urlOnly, if urlOnly = True, return URL string.
#'
#' @return
#' @export
#'
#' @examples
#'   params <- list('serviceNodeId'='NMIC_MUSIC_CMADAAS', 'userId'='test',
#'                  'pwd'='123456', 'dataCode'='SURF_CHN_MUL_HOR_N',
#'                  'elements'='Datetime,Station_Id_d,Lat,Lon,PRE_24h',
#'                  'times'='20200910000000', 'limitCnt'='10')
#'   data <- get_rest_result('getSurfEleByTime', params)
#'   
get_rest_result <- function(interface_id, params, urlOnly=FALSE){
  
  # get configure information
  config = get_config_from_rcfile()
  if (is.null(config)) {
    return(NULL)
  }
  
  # get MUSIC server DNS and PORT
  dns  <- config$CMADaaS$DNS
  port <- config$CMADaaS$PORT
  
  # construct complete parameters
  signParams <- params
  
  # get MUSIC user information
  if (is.null(signParams[['serviceNodeId']])) {
    signParams[['serviceNodeId']] <- config$CMADaaS$serviceNodeId
  }
  if (is.null(signParams[['userId']])) {
    signParams[['userId']] <- config$CMADaaS$USER_ID
  }
  if (is.null(signParams[['pwd']])) {
    signParams[['pwd']] <- config$CMADaaS$PASSWORD
  }
  
  # data interface Id and out data format
  signParams[['interfaceId']] <- stringr::str_trim(interface_id)
  signParams[['dataFormat']] <- 'json'
  
  # add time stamp and nonce code
  signParams[['timestamp']] <- sprintf("%1.0f", as.numeric(Sys.time(), units="secs")*1000)
  signParams[['nonce']] <- uuid::UUIDgenerate()
  
  # construct sign string with hashlib md5 code
  signStr <- ""
  for (key in sort(names(signParams))){
    signStr <- paste0(signStr, key, "=", stringr::str_trim(signParams[[key]]), "&")
  }
  signStr <- stringr::str_sub(signStr, end=-2)
  signStr <- paste0(signStr, '&sign=', toupper(digest::digest(signStr, algo="md5", serialize=FALSE)))
  
  # remove password parameters
  signStr <- stringr::str_remove(signStr, paste0('&pwd=', stringr::str_trim(signParams[['pwd']])))
  
  # construct URL
  urlStr <- paste0('http://', dns, ':', port, '/music-ws/api?', signStr)
  if (urlOnly) {
    return(urlStr)
  }
  
  # read data
  data <- jsonlite::fromJSON(urlStr)
  if (data$returnCode != 0) {
    return(NULL)
  }
  return(data)
}



#' Retrieve model grid data from CMADaSS service.
#' 
#' @description 
#'     Retrieve model grid data from CMADaaS service.
#'     refer to: http://10.20.76.55/cimissapiweb/apidataclassdefine_list.action
#'
#' @param time: model initial or run time, like "2016081712" (format "%Y%m%d%H").
#' @param dataCode: MUSIC data code.
#'          NAFP_CRA40_FTM_6HOR_ANA(default): 中国40年再分析资料
#'          NAFP_ECMF_C1D_GLB_FOR: 欧洲中心数值预报产品-高分辨率C1D-全球
#'          NAFP_ECMF_FTM_HIGH_ANEA_FOR: 欧洲中心数值预报产品-高分辨率C1D-亚洲地区
#'          ......
#' @param fcstEle: forecast element, like temperature "TEM"
#' @param limit: region limit, [minLat, minLon, maxLat, maxLon]
#' @param fcstLevel: vertical level, like 500
#' @param levelType: forecast level type, 表示Grib数据中的层次类型, 可在云平台上查询.
#' @param validTime: forecast hour, like 0 or 6 
#' @param outList: if TRUE, return list type.
#'
#' @return data.table data.
#' @export
#'
#' @examples
#'   data <- cmadass_model_grid("2020041900")
#' 
cmadass_model_grid <- function(time, dataCode="NAFP_CRA40_FTM_6HOR_ANA", fcstEle="GPH", limit=NULL,
                               fcstLevel=500, levelType="-", validTime=0, outList=FALSE) {
  # retrieve parameters
  params <- list()
  params[['dataCode']] <- dataCode
  params[['fcstEle']] <- fcstEle
  params[['time']] <- paste0(time,"0000")
  params[['fcstLevel']] <- as.character(fcstLevel)
  params[['levelType']] <- as.character(levelType)
  params[['validTime']] <- as.character(validTime)
  
  # Interface
  if (is.null(limit)) {
    interfaceId <- "getNafpEleGridByTimeAndLevelAndValidtime"
  } else {
    params[['minLat']] <- as.character(limit[0])
    params[['minLon']] <- as.character(limit[1])
    params[['maxLat']] <- as.character(limit[2])
    params[['maxLon']] <- as.character(limit[3])
    interfaceId <- "getNafpEleGridInRectByTimeAndLevelAndValidtime"
  }
  
  
  # retrieve data
  result <- get_rest_result(interfaceId, lapply(params, trimws))
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
  initTime <- as.POSIXct(time, format="%Y%m%d%H", tz="GMT")
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


