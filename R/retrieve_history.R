#' Download day history observations, one file for each month.
#'
#' @param dateRange : date range, c('YYYYMM','YYYYMM')
#' @param elements : elements for retrieve, 'ele1,ele2,...'
#' @param outDir : data output directory
#'
#' @return None.
#' @export
#'
#' @examples
#'     retrieve_history_surf_daily_obs(c('201001','201512'))
#'
#'
retrieve_history_surf_daily_obs <- function(dateRange,
                                            elements=NULL,
                                            staLevels=NULL,
                                            outFile="day_rain_obs",
                                            outDir="."){
  # check the elements
  if (is.null(elements)) {
    elements <- paste("Station_Id_C,Station_Name,Station_Id_d,Station_levl,",
                      "Datetime,Lat,Lon,PRE_Time_0808", sep="")
  }
  
  # construct month date series
  startTimes <- seq(lubridate::ymd(paste(dateRange[1],'01',sep="")),
                    lubridate::ymd(paste(dateRange[2],'01',sep="")), by='1 months')
  endTimes <- startTimes
  lubridate::day(endTimes) <- lubridate::days_in_month(startTimes)
  startTimes <- format(startTimes, "%Y%m%d000000")
  endTimes <- format(endTimes, "%Y%m%d000000")
  
  # Because of the CIMISS data mount limit,
  # so loop every year to download the data.
  for (i in 1:length(startTimes)) {
    # construct output file name
    outfile1 = file.path(outDir,paste(outFile,"_",substr(startTimes[i],1,6),".rds",sep=""))
    if (file.exists(outfile1)) next
    
    # construct time range string
    timeRange <- paste("[",startTimes[i],",",endTimes[i],"]",sep="")
    
    # retrieve observations from CIMISS server
    obsData <- cimiss_obs_by_time_range(timeRange, dataCode="SURF_CHN_MUL_DAY",
                                        elements=elements, staLevels=staLevels)
    
    # save observation data to file
    saveRDS(obsData, file=outfile1)
  }
}

