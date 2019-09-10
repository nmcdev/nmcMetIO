#' Get configure information from config_met_io.ini file.
#'
#' @return list, configure parameters.
#' @export
#'
get_config_from_rcfile <- function(){
  # construct configure file
  config.file <- file.path(path.expand('~'), "config_met_io.ini")
  
  # check configure file
  if(!file.exists(config.file)){
    warning(paste0("config_met_io.ini does not exist in ", path.expand('~')), call. = FALSE)
    return(NULL)
  }
  
  # read configurations
  config <- configr::read.config(file=config.file)
  return(config)
}