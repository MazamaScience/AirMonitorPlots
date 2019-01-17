#' @title Create timeseries plot for one or more monitors
#'
#' @description
#' This function assembles various layers to create a production-ready
#' timeseries plot for one or more monitors. 
#'
#' @param ws_tidy dataframe of monitor data, created from a \code{\link{ws_monitor}}
#' object using \code{monitor_toTidy()}. 
#' @param startdate Desired start date (integer or character in ymd format or POSIXct)
#' @param enddate Desired end date (integer or character in ymd format or POSIXct)
#' @param style Plot style. Not currently supported.
#' @param aqiStyle AQI style to add AQI color bars, lines, and labels. 
#' Not currently supported.
#' @param monitorIDs vector of monitorIDs to include in the plot. If 
#' more than one, different monitors will be plotted in different colors
#' and timeseries.legend must be false.
#' @param title Plot title. If NULL, a suitable title will be constructed.
#' @param timeseries.legend Logical indicating whether to include a legend.
#' @return A **ggplot** object
#'
#' @import ggplot2
#' @export
#' 

tidy_timeseries <- function(ws_tidy,
                            startdate = NULL,
                            enddate = NULL,
                            style = NULL,
                            aqiStyle = NULL,
                            monitorIDs = NULL,
                            title = NULL,
                            legend = TRUE) {
  
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
  
  
  pm25LegendLabel = "Hourly PM2.5 Values"
  nowcastLegendLabel = "NowCast"
  
  ggplot_pm25Timeseries(data) +
    pwfsl_scales(data, 
                 startdate,
                 enddate) + 
    geom_pm25Points(mapping, 
                    legend.label = pm25LegendLabel, 
                    timeseries.legend = legend) +
    stat_nowcast(mapping, 
                 legend.label = nowcastLegendLabel,
                 timeseries.legend = legend) +
    aqiStackedBar() +
    aqiLines() +
    scale_color_brewer(palette = "Dark2") +
    ggtitle(title) +
    legend_pm25Timeseries(legend.labels = c(pm25LegendLabel, nowcastLegendLabel))
  
  
}
