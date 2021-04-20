#' Get the time range string for MUSIC API.
#'
#' @param startTimeStr : start time string, UTC, "YYYYMMDDHH"
#' @param num : the time range number.
#' @param units : the time units to be used, like "day", "hour", "second", ...
#'
#' @return character.
#' @export
#'
#' @examples
#'   startTimeStr <- "2020021400"
#'   get_music_time_range_str(startTimeStr)
#'   
get_music_time_range_str <- function(startTimeStr, num=36, units="hour"){
  startTime <- strptime(startTimeStr, format="%Y%m%d%H", tz="GMT")
  endTime <- startTime + lubridate::period(num=num, units="hour")
  startTimeStr <- strftime(startTime, "%Y%m%d%H%M%S")
  endTimeStr <- strftime(endTime, "%Y%m%d%H%M%S")
  timeRange <- paste0("[",startTimeStr,",",endTimeStr,"]")
  return(timeRange)
}
