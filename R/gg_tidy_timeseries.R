tidy_timeseries <- function(data,
                            startdate = NULL,
                            enddate = NULL,
                            style = NULL,
                            aqiStyle = NULL,
                            monitorID = NULL,
                            title = "") {
  
  if (!is.null(monitorID)) {
    data <- filter(data, monitorID == monitorID)
  }
  
  ggplot_pm25Timeseries(data) +
    geom_pm25Points() +
    pwfsl_scales(data, 
                 startdate,
                 enddate) + 
    stat_nowcast() +
    aqiStackedBar() +
    aqiLines()
  
}