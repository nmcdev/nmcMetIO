#' Get configure information from config_met_io.ini file.
#'
#' @return list, configure parameters.
#' @export
#'
get_config_from_rcfile <- function(){
  # construct configure file
  if(Sys.info()[['sysname']] == "Windows"){
    config.file <- file.path(Sys.getenv("USERPROFILE"), ".nmcdev", "config.ini")
  }else{
    config.file <- file.path(path.expand("~"), ".nmcdev", "config.ini")
  }
  
  # check configure file
  if(!file.exists(config.file)){
    warning(paste0(config.file, " does not exist."), call. = FALSE)
    return(NULL)
  }
  
  # read configurations
  config <- configr::read.config(file=config.file)
  return(config)
}
