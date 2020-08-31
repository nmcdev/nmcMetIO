#' Get configure information from config_met_io.ini file.
#'
#' @return list, configure parameters.
#' @export
#'
get_config_from_rcfile <- function(){
  # construct configure file
  if(Sys.info()[['sysname']] == "Windows"){
    config.dir <- file.path(Sys.getenv("USERPROFILE"), ".nmcdev")
  }else{
    config.dir <- file.path(path.expand("~"), ".nmcdev")
  }
  config.file <- file.path(config.dir, "config.ini")
  
  # check configure file
  if(!file.exists(config.file)){
    warning(paste0(config.file, " does not exist."), call. = FALSE)
    return(NULL)
  }
  
  # read configurations
  config <- configr::read.config(file=config.file)
  config$Directory <- config.dir
  return(config)
}


#' Get the cache file pathname.
#'
#' @param subDir : sub directory string.
#' @param filename :  cache filename
#' @param name : cache name, like "MICAPS_DATA"
#' @param cacheClear : if True, clear old cache folder
#'
#' @return
#' @export
#'
get_cache_file <- function(subDir, filename, name=NULL, cacheClear=TRUE){
  config <- get_config_from_rcfile()
  
  # get cache file directory
  if(is.null(config[["MICAPS"]][["CACHE_DIR"]])){
    cacheDir <- file.path(config[['Directory']], "cache")
  }else{
    cacheDir <- file.path(config[["MICAPS"]][["CACHE_DIR"]], "cache")
  }
  
  # add cache name, if neccessary
  if(!is.null(name)){
    cacheDir <- file.path(cacheDir, name)
  }
  
  # Use the week number of year as subdir
  cacheSubDir1 <- file.path(cacheDir, format(Sys.time(), "%Y%U"))
  cacheSubDir2 <- file.path(cacheSubDir1, subDir)
  if (!dir.exists(cacheSubDir2)){
    dir.create(cacheSubDir2, recursive=TRUE)
  }
  
  # clear old cache folder
  if(cacheClear){
    for(dir in list.dirs(cacheDir, recursive = FALSE)) {
      if(dir == cacheSubDir1) {
        next
      }
      unlink(dir, recursive = TRUE)
    }
  }
  
  # return cache file pathname
  cacheFile <- file.path(cacheSubDir2, paste0(filename, '.Rds'))
  return(cacheFile)
}

