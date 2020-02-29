#' Retrieve data contents from micaps cassandra service.
#'
#' @description
#'   Retrieve data contents from micaps cassandra service.
#'
#' @param directory : the directory on the micaps service
#' @param filename : the data filename, if not given, will use the lastest model run.
#'
#' @return raw, bytearray (convert with readBin)
#' @export
#'
gds_get_content <- function(directory, filename){
  
  # read GDS DataBlock.proto (This file provided by Wang Ruotong)
  RProtoBuf::readProtoFiles(system.file("extdata/DataBlock.proto", package="nmcMetIO"))
  
  # retrieve the data content
  url = gds_concate_url("getData", directory, filename)
  data = RCurl::getURLContent(url, binary=TRUE)
  msg <- RProtoBuf::read(DataBlock.ByteArrayResult, data)
  if (msg$errorCode !=0){
    return(NULL)
  }
  return(msg$byteArray)
}

#' Get the latest filename in the data directory on micaps cassandra service.
#'
#' @param directory : the directory on the micaps service
#' @param filter : the filename filter pattern, when filename=NULL, this will be used to
#'                 find the specified file.
#'
#' @return filename, string.
#' @export
#'
gds_get_latest_filename <- function(directory, filter="*.024"){
  # read GDS DataBlock.proto (This file provided by Wang Ruotong)
  RProtoBuf::readProtoFiles(system.file("extdata/DataBlock.proto", package="nmcMetIO"))
  
  # get the latest filename
  url = gds_concate_url("getLatestDataName", directory, "", filter=filter)
  data = RCurl::getURLContent(url, binary=TRUE)
  msg = RProtoBuf::read(DataBlock.StringResult, data)
  if (msg$errorCode != 0){
    return(NULL)
  }
  return(msg$name)
}


#' concate gds service url
#'
#' @param requestType :request type.
#' @param directory : the directory on the micaps service
#' @param fileName : the data filename, if not given, will use the lastest model run.
#' @param filter : the filename filter pattern, when filename=NULL, this will be used to
#'                 find the specified file.
#'
#' @return data message.
#' @export
#'
gds_concate_url <- function(requestType, directory, filename, filter=NULL){
  config <- get_config_from_rcfile()
  
  # construct url
  url = paste0("http://", config[['MICAPS']][['GDS_IP']], ":",
               config[['MICAPS']][['GDS_PORT']], "/DataService?")
  url = paste(url, "requestType=", requestType, sep="")
  url = paste(url, "&directory=", directory, sep="")
  url = paste(url, "&fileName=", filename, sep="")
  if(!is.null(filter)){
    url = paste(url, "&filter=", filter, sep="") 
  }
  return(url)
}
