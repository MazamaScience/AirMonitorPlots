#' @title Instantiate a pm25 timeseries ggplot
#'
#' @description
#' Create a plot using ggplot with default mappings and styling. Layers can then 
#' be added to this plot using \code{ggplot2} syntax. 
#'
#' @inheritParams custom_pm25DiurnalScales
#' @param ws_data Default dataset to use when adding layers. Must be either a \code{ws_monitor} 
#' object or \code{ws_tidy} object. 
#' @param startdate Desired startdate for data to include, in a format that can be 
#' parsed with \link{parseDatetime}.
#' @param enddate Desired enddate for data to include, in a format that can be parsed 
#' with \link{parseDatetime}.
#' @param timezone Timezone to use to set hours of the day
#' @param shadedNight add nighttime shading based on of middle day in selected period
#' @param mapping Default mapping for the plot
#' @param ... Additional arguments passed on to \code{\link{custom_pm25DiurnalScales}}.
#'
#' @import ggplot2
#' @importFrom rlang .data
#' @export
#' @examples
#' ws_monitor <- PWFSLSmoke::Carmel_Valley
#' ggplot_pm25Diurnal(ws_monitor) +
#'   coord_polar() +
#'   geom_pm25Points() +
#'   custom_aqiStackedBar(width = 1, alpha = .3) 
#' 
#' ggplot_pm25Diurnal(ws_monitor, 
#'                    startdate = 20160801, 
#'                    enddate = 20160810) + 
#'   stat_boxplot(aes(group = hour)) 
#'   




ggplot_pm25Diurnal <- function(ws_data,
                               startdate = NULL,
                               enddate = NULL,
                               timezone = NULL,
                               ylim = NULL, 
                               shadedNight=TRUE,
                               mapping = aes_(x = ~hour, y = ~pm25),
                               ...) {
  
  # Sanity checks
  if ( monitor_isMonitor(ws_data) ) {
    ws_tidy <- monitor_toTidy(ws_data)
  } else if ( monitor_isTidy(ws_data) ) {
    ws_tidy <- ws_data
  } else {
    stop("ws_data must be either a ws_monitor object or ws_tidy object.")
  }
  
  if ( !is.null(startdate) & !is.null(enddate) ) {
    daterange <- range(ws_tidy$datetime)
    if ( parseDatetime(startdate) > daterange[2] ) {
      stop("startdate is outside of data date range")
    } 
    if ( parseDatetime(enddate) < daterange[1] ) {
      stop("enddate is outside of data date range")
    }
  }
  
  # Get timezone
  if ( is.null(timezone) ) {
    if (length(unique(ws_tidy$timezone)) > 1) {
      timezone <- "UTC"
      xlab <- "Time of Day (UTC)"
    } else {
      timezone <- ws_tidy$timezone[1]
      xlab <- "Time of Day (Local)"
    }
  } else if ( is.null(xlab) ) {
    xlab <- paste0("Time of Day (", timezone, ")")
  }
  
  # Subset based on startdate and enddate
  if (!is.null(startdate)) {
    s <- parseDatetime(startdate, timezone = timezone)
    ws_tidy <- dplyr::filter(ws_tidy, .data$datetime >= lubridate::floor_date(s, unit = "day"))
  }
  if (!is.null(enddate)) {
    e <- parseDatetime(enddate, timezone = timezone)
    ws_tidy <- dplyr::filter(ws_tidy, .data$datetime <= lubridate::ceiling_date(e, unit = "day"))
  }
  
  
  # Add column for 'hour'
  ws_tidy$hour <- as.numeric(strftime(ws_tidy$datetime, "%H", tz = timezone))
  ws_tidy$day  <- strftime(ws_tidy$datetime, "%Y%m%d", tz = timezone)
  

  
  plot <- ggplot(ws_tidy, mapping) +
    theme_timeseriesPlot_pwfsl() +
    custom_pm25DiurnalScales(ws_tidy,
                             xlab = xlab,
                             ylim = ylim,
                             ...)
  
  # Calculate day/night shading 
  if (shadedNight) {
    # Get the sunrise/sunset information
    ti <- timeInfo(ws_tidy$datetime, longitude=ws_tidy$longitude[1], latitude=ws_tidy$latitude[1], timezone=timezone)
    
    # Extract the middle row
    ti <- ti[round(nrow(ti)/2),]
    
    # Get sunrise and sunset in units of hours
    sunrise <- lubridate::hour(ti$sunrise) + lubridate::minute(ti$sunrise)/60
    sunset <- lubridate::hour(ti$sunset) + lubridate::minute(ti$sunset)/60
    
    # Add shaded night
    scales <- layer_scales(plot)
    
    morning <- annotate("rect", 
                        xmin = scales$x$limits[1], 
                        xmax = sunrise, 
                        ymin = scales$y$limits[1], 
                        ymax = scales$y$limits[2],
                        fill = "black",
                        alpha = 0.1)
    night <-   annotate("rect",
                        xmin = sunset,
                        xmax = scales$x$limits[2],
                        ymin = scales$y$limits[1],
                        ymax = scales$y$limits[2],
                        fill = "black",
                        alpha = 0.1)
    plot <- plot + morning + night
  }
  
  plot
  
}

