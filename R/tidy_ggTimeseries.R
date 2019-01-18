#' @title Create timeseries plot for one or more monitors
#'
#' @description
#' This function assembles various layers to create a production-ready
#' timeseries plot for one or more monitors. 
#'
#' @param ws_tidy dataframe of monitor data, created from a \code{ws_monitor}
#' object using \code{monitor_toTidy()}. 
#' @param startdate Desired start date (integer or character in ymd format or POSIXct)
#' @param enddate Desired end date (integer or character in ymd format or POSIXct)
#' @param style Plot style. Not currently supported.
#' @param aqiStyle AQI style to add AQI color bars, lines, and labels. 
#' Not currently supported.
#' @param monitorIDs vector of monitorIDs to include in the plot. If 
#' more than one, different monitors will be plotted in different colors.
#' @param title Plot title. If NULL, a suitable title will be constructed.
#' @return A **ggplot** object
#'
#' @import ggplot2
#' @importFrom rlang .data
#' 
#' @export
#' 
#' @examples
#' ws_monitor <- airnow_loadLatest()
#' ws_tidy <- monitor_toTidy(ws_monitor)
#' tidy_ggTimeseries(ws_tidy, monitorIDs = "410432002_01")

tidy_ggTimeseries <- function(ws_tidy,
                              startdate = NULL,
                              enddate = NULL,
                              style = NULL,
                              aqiStyle = NULL,
                              monitorIDs = NULL,
                              title = NULL) {
  
  data <- ws_tidy
  
  if (!is.null(monitorIDs)) {
    data <- filter(.data = data, .data$monitorID %in% monitorIDs)
  } 
  
  if ( length(unique(data$monitorID)) > 1) {
    mapping <- aes_(color = ~monitorID)
    if (is.null(title)) title <- ""
  } else {
    mapping <- NULL
    if(is.null(title)) title <- unique(data$siteName)
  }
  
  
  pm25LegendLabel = "Hourly PM2.5 Values"
  nowcastLegendLabel = "NowCast"
  
  ggplot_pm25Timeseries(data,
                        startdate = startdate,
                        enddate = enddate) +
    geom_pm25Points(mapping) +
    stat_nowcast(mapping) +
    custom_aqiStackedBar() +
    custom_aqiLines() +
    scale_color_brewer(palette = "Dark2") +
    ggtitle(title) +
    custom_legend(labels = c("Hourly PM2.5 Values", "NowCast"),
                  aesthetics = list(color = c(1,1),
                                    size = c(1.5, 0.5),
                                    linetype = c(NA, 1),
                                    shape = c(16, NA),
                                    alpha = c(0.3, 1)),
                  theme_args = list(legend.position = "top"))
  
  
}
