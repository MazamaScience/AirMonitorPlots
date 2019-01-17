tidy_timeseries <- function(data,
                            startdate = NULL,
                            enddate = NULL,
                            style = NULL,
                            aqiStyle = NULL,
                            monitorIDs = NULL,
                            title = NULL,
                            timeseries.legend = TRUE,
                            pm25LegendLabel = "Hourly PM2.5 Values",
                            nowcastLegendLabel = "NowCast") {
  
  if (!is.null(monitorIDs)) {
    data <- filter(data, monitorID %in% monitorIDs)
  } 
  
  if (length(unique(data$monitorID)) > 1) {
    mapping <- aes(color = monitorID)
    if (is.null(title)) title <- ""
  } else {
    mapping <- NULL
    if(is.null(title)) title <- unique(data$siteName)
  }
  
  ggplot_pm25Timeseries(data) +
    pwfsl_scales(data, 
                 startdate,
                 enddate) + 
    geom_pm25Points(mapping, 
                    legend.label = pm25LegendLabel, 
                    timeseries.legend = TRUE) +
    stat_nowcast(mapping, 
                 legend.label = nowcastLegendLabel,
                 timeseries.legend = TRUE) +
    aqiStackedBar() +
    aqiLines() +
    scale_color_brewer(palette = "Dark2") +
    ggtitle(title)+
    legend_pm25Timeseries(legend.labels = c(pm25LegendLabel, nowcastLegendLabel))
  
  
}
