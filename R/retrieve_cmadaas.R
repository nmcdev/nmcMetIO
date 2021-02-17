#' Get the http result from CAMDaaS REST api service
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
  if (is.null(config[['serviceNodeId']])) {
    signParams[['serviceNodeId']] <- config$CMADaaS$serviceNodeId
  }
  if (is.null(config[['serviceNodeId']])) {
    signParams[['userId']] <- config$CMADaaS$userId
  }
  if (is.null(config[['serviceNodeId']])) {
    signParams[['pwd']] <- config$CMADaaS$pwd
  }
  
  # data interface Id and out data format
  signParams <- stringr::str_trim(interface_id)
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
  urlStr <- paste0('http://', dns, ':', port, '/music-ws/api?', sign_str)
  if (urlOnly) {
    return(urlStr)
  }
  
  # read data
  data <- jsonlite::fromJSON(url)
  if (data$returnCode != 0) {
    return(NULL)
  }
  return(data)
}


