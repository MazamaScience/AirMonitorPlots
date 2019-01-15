tidy_timeseries <- function(data,
                            startdate = NULL,
                            enddate = NULL,
                            style = NULL,
                            aqiStyle = NULL,
                            monitorIDs = NULL,
                            title = NULL) {
  
  if (!is.null(monitorIDs)) {
    data <- filter(data, monitorID %in% monitorIDs)
  } 
  
  if (length(unique(data$monitorID)) > 1) {
    mapping <- aes(color = monitorID)
    if (is.null(title)) title <- ""
  } else {
    mapping <- aes(color = NULL)
    if(is.null(title)) title <- unique(data$siteName)
  }
  
  ggplot_pm25Timeseries(data) +
    pwfsl_scales(data, 
                 startdate,
                 enddate) + 
    geom_pm25Points(mapping) +
    stat_nowcast(mapping) +
    aqiStackedBar() +
    aqiLines() +
    scale_color_brewer(palette = "Dark2") +
    ggtitle(title)+
    legend_pm25Timeseries()
  
  
}
