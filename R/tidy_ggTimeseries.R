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
#' @param style Plot style. \code{small} or \code{large}. \code{style = small} is 
#' appropriate for plots 450x450px or smaller; \code{style = large} is appropriate
#' for plots larger than 450x450px. 
#' @param aqiStyle AQI style to add AQI color bars, lines, and labels. 
#' Not currently supported.
#' @param monitorIDs vector of monitorIDs to include in the plot. If 
#' more than one, different monitors will be plotted in different colors.
#' @param title Plot title. If NULL, a suitable title will be constructed.
#' @param ... Arguments passed onto \code{\link{ggplot_pm25Timeseries}}.
#' @return A **ggplot** object
#'
#' @import PWFSLSmoke
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
                              style = "small",
                              aqiStyle = NULL,
                              monitorIDs = NULL,
                              title = NULL,
                              ...) {
  
  
  # Parameter Validation
  if (!monitor_isTidy(ws_tidy)) 
    stop("ws_tidy must be a ws_tidy object")
  if (!style %in% c("small", "large"))
    stop("Invalid style. Choose from 'small' or 'large'.")

  
  if (any(!monitorIDs %in% unique(ws_tidy$monitorID))) {
    invalidIDs <- monitorIDs[which(!monitorIDs %in% unique(ws_tidy$monitorID))]
    stop(paste0("Invalid monitorIDs. MonitorIDs not present in data: ", paste0(invalidIDs, collapse = ", ")))
  }
  
  if ( !is.null(startdate) && parseDatetime(startdate) > range(ws_tidy$datetime)[2] ) {
    stop("startdate is outside of data date range")
  } 
  if ( !is.null(enddate) && parseDatetime(enddate) < range(ws_tidy$datetime)[1] ) {
    stop("enddate is outside of data date range")
  }
  
  
  
  # Prepare data
  
  if (!is.null(monitorIDs)) {
    ws_tidy <- dplyr::filter(.data = ws_tidy, .data$monitorID %in% monitorIDs)
  } 
  
  pm25LegendLabel = "Hourly PM2.5 Values"
  nowcastLegendLabel = "NowCast"
  
  if ( length(unique(ws_tidy$monitorID)) > 1) {
    mapping_pm25 <- mapping_nowcast <- aes_(color = ~monitorID)
    if (is.null(title)) title <- ""
  } else {
    mapping_pm25 <- aes(color = !!pm25LegendLabel)
    mapping_nowcast <- aes(color = !!nowcastLegendLabel)
    if(is.null(title)) title <- paste0("Hourly PM2.5 Values and NowCast\n", 
                                       "Site: ", unique(ws_tidy$siteName))
  }
  
  # Styling args
  if (style == "large") {
    pointsize <- 2
    linesize <- 0.8
    date_labels <- "%b %d"
    minor_break_width <- NULL
    base_size = 15
  } else if (style == "small") {
    pointsize <- 2
    linesize <- 0.8
    date_labels <- "%b\n%d"
    minor_break_width <- "6 hours"
    base_size = 11
  }
  
  # Prepare the plot
  
  plot <- ggplot_pm25Timeseries(ws_tidy,
                                startdate = startdate,
                                enddate = enddate,
                                includeFullEnddate = FALSE,
                                date_labels = date_labels,
                                minor_break_width = minor_break_width, 
                                base_size = base_size,
                                ...) +
    custom_aqiLines(size = 1, alpha = .8) +
    geom_pm25Points(mapping_pm25, size = pointsize) +
    stat_nowcast(mapping_nowcast, size = linesize) +
    custom_aqiStackedBar() +
    ggtitle(title) 
  
  
  if ( length(unique(ws_tidy$monitorID)) == 1 ) {
    # Add legend
    values <- c(1,1)
    names(values) <- c(pm25LegendLabel, nowcastLegendLabel)
    scale <- scale_color_manual(name = "", values = values, labels = names(values))
    guide <- guides(color = guide_legend(title = "", 
                                         override.aes = list(color = c(1,1),
                                              size = c(pointsize, linesize),
                                              linetype = c(NA, 1),
                                              shape = c(16, NA),
                                              alpha = c(0.3, 1))))
    plot <- plot + scale + guide 
  } else {
    plot <- plot + scale_color_brewer(palette = "Dark2") 
  }
  
  plot + theme_timeseriesPlot_pwfsl(size = style)
  
}
