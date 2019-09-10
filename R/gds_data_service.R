#' Retrieve data contents from micaps cassandra service.
#'
#' @description
#'   Retrieve data contents from micaps cassandra service.
#'
#' @param directory : the directory on the micaps service
#' @param filename : the data filename, if not given, will use the lastest model run.
#' @param filter : the filename filter pattern, when filename=NULL, this will be used to
#'                 find the specified file.
#'
#' @return raw, bytearray (convert with readBin)
#' @export
#'
gds_get_content <- function(directory, filename=NULL, filter="*.024"){
  
  # read GDS DataBlock.proto
  RProtoBuf::readProtoFiles(system.file("extdata/DataBlock.proto", package="nmcMetIO"))
  
  # get the latest filename
  if (is.null(filename)){
    url = gds_concate_url("getLatestDataName", directory, "", filter)
    data = RCurl::getURLContent(url, binary=TRUE)
    msg = RProtoBuf::read(DataBlock.StringResult, data)
    if (msg$errorCode != 0){
      return(NULL)
    }
    filename = msg$name
  }
  
  # retrieve the data content
  url = gds_concate_url("getData", directory, filename, filter)
  data = RCurl::getURLContent(url, binary=TRUE)
  msg <- RProtoBuf::read(DataBlock.ByteArrayResult, data)
  if (msg$errorCode !=0){
    return(NULL)
  }
  return(msg$byteArray)
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
gds_concate_url <- function(requestType, directory, filename, filter){
  config <- get_config_from_rcfile()
  
  # construct url
  url = paste0("http://", config[['MICAPS']][['GDS_IP']], ":",
               config[['MICAPS']][['GDS_PROT']], "/DataService?")
  url = paste(url, "requestType=", requestType, sep="")
  url = paste(url, "&directory=", directory, sep="")
  url = paste(url, "&fileName=", filename, sep="")
  url = paste(url, "&filter=", filter, sep="")
  return(url)
}
