#' @title Create a timeseries plot for one or more monitors
#'
#' @description
#' Create a timeseries plot with points or lines showing PM2.5 data over a 
#' period for one or more monitors in a \emph{mts_monitor} object.
#' 
#' @param mts_monitor \emph{mts_monitor} object.
#' @param startdate Desired start date (integer or character in Ymd format 
#'        or \code{POSIXct}).
#' @param enddate Desired end date (integer or character in Ymd format
#'        or \code{POSIXct}).
#' @param colorPalette Palette function to convert monitor values into colors.
#' @param dataStyle Style for displaly of PM2.5 values. One of \code{pwfsl}.
#' @param nowcastStyle Style for displaly of Nowcast values. One of \code{pwfsl}.
#' @param ylimStyle Style of y-axis limits. One of \code{auto|pwfsl}.
#' @param aqiStyle AQI style to add AQI color bars, lines and labels.
#' @param dateFormat Format for x-axis dates. Used for \code{date_labels}
#' argument to \code{scale_x_datetime}.
#' @param title Optional title.
#'
#' @return A `ggplot` plot object with a daily bar plot for a single monitor.
#'
#' @importFrom rlang .data
#' @export
#' @examples
#' mts_monitor <- AirMonitor::Carmel_Valley
#' startdate <- "2016-07-25"
#' enddate <- "2016-08-03"
#' timeseriesPlotBase(mts_monitor, startdate, enddate,
#'                    title = "Carmel Valley\n2016")


timeseriesPlotBase <- function(mts_monitor,
                               startdate = NULL,
                               enddate = NULL,
                               colorPalette = aqiPalette("aqi"),
                               dataStyle = "pwfsl",
                               nowcastStyle = "pwfsl",
                               ylimStyle = "auto",
                               aqiStyle = NULL,
                               dateFormat = "%b %d",
                               title = "") {
  
  # For debugging --------------------------------------------------------------
  
  if (FALSE) {
    
    # Carmel Valley
    mts_monitor <- AirMonitor::Carmel_Valley
    startdate <- "2016-07-25"
    enddate <- "2016-08-03"
    colorPalette <- aqiPalette("aqi")
    dataStyle <- "pwfsl"
    nowcastStyle <- "pwfsl"
    ylimStyle <- "pwfsl"
    aqiStyle <- "bars_lines"
    dateFormat <- "%b %d"
    title <- "Carmel Valley\n2016"
    
  }
  
  # Validate arguments ---------------------------------------------------------
  
  if ( !monitor_isValid(mts_monitor) ) {
    stop("Required parameter 'mts_monitor' is not a valid mts_monitor object.")
  } else if ( monitor_isEmpty(mts_monitor) ) {
    stop("Required parameter 'mts_monitor' is empty.")
  }
  
  if ( is.null(startdate) && is.null(enddate) ) {
    stop("Required parameters 'startdate' and 'enddate' must be defined.")
  }
  
  if ( startdate == enddate ) {
    stop("'startdate' and 'enddate' cannot be equal.")
  }
  
  # Time limits ----------------------------------------------------------------
  
  # TODO:  Handle multilpe timezones
  
  # TODO:  The "pwfsl" style should be for single monitors only
  
  timezone <- mts_monitor$meta$timezone[1]
  
  # handle various startdates
  if ( !is.null(startdate) ) {
    if ( is.numeric(startdate) || is.character(startdate) ) {
      startdate <- lubridate::ymd(startdate, tz = timezone)
    } else if ( lubridate::is.POSIXct(startdate) ) {
      startdate <- lubridate::force_tz(startdate, tzone = timezone)
    } else if ( !is.null(startdate) ) {
      stop(paste0(
        "Required parameter 'startdate' must be integer or character",
        " in Ymd format or of class POSIXct."))
    }
  }
  
  # handle various enddates
  if ( !is.null(enddate) ) {
    if ( is.numeric(enddate) || is.character(enddate) ) {
      enddate <- lubridate::ymd(enddate, tz = timezone)
    } else if ( lubridate::is.POSIXct(enddate) ) {
      enddate <- lubridate::force_tz(enddate, tzone = timezone)
    } else if ( !is.null(enddate) ) {
      stop(paste0(
        "Required parameter 'enddate' must be integer or character",
        " in Ymd format or of class POSIXct."))
    }
  }
  
  # We will include the complete 'enddate' day
  dayCount <- as.integer(difftime(enddate, startdate, units = "days")) + 1
  
  # Choose date_breaks and minor_breaks
  s <- startdate
  e <- enddate + lubridate::ddays(1) # full 24 hours of enddate
  if ( dayCount >= 0 && dayCount <= 9 ) {
    breaks <- seq(s, e, by = "1 day")
    minor_breaks <- seq(s, e, by = "3 hours")
  } else if ( dayCount <= 21 ) {
    breaks <- seq(s, e, by = "3 days")
    minor_breaks <- seq(s, e, by = "6 hours")
  } else if ( dayCount <= 60 ) {
    breaks <- seq(s, e, by = "1 week")
    minor_breaks <- seq(s, e, by = "1 day")
  } else if ( dayCount <= 120 ) {
    breaks <- seq(s, e, by = "2 weeks")
    minor_breaks <- seq(s, e, by = "1 day")
  } else {
    breaks <- seq(s, e, by = "1 month")
    minor_breaks <- seq(s, e, by = "1 week")
  }
  
  # Timeseries data ------------------------------------------------------------
  
  # Create nowcast before subsetting because we need hours from the previous day
  mon_nowcast <- monitor_nowcast(mts_monitor, includeShortTerm = TRUE) %>%
    monitor_subset(tlim = c(startdate, enddate + lubridate::dhours(23)),
                   timezone = timezone)

  tidyNowcast <- reshape2::melt(mon_nowcast$data, id.vars = "datetime")
  
  # Subset based on startdate and enddate
  mon <- monitor_subset(mts_monitor,
                        tlim = c(startdate, enddate + lubridate::dhours(23)),
                        timezone = timezone)
  
  tidyData <- reshape2::melt(mon$data, id.vars = "datetime")
  
  # Default data style
  pm25Alpha <- 1.0
  pm25Color <- "black"
  pm25Shape <- "circle"
  
  if ( dataStyle == "pwfsl" ) {
    pm25Alpha <- 0.3
    pm25Color <- "black"
    pm25Shape <- "circle"
  }
  
  # Default Nowcast style
  nowcastAlpha <- 1.0
  nowcastColor <- "black"
  nowcastSize <- 0.5
  
  if ( dataStyle == "pwfsl" ) {
    nowcastAlpha <- 1.0
    nowcastColor <- "black"
    nowcastShape <- 0.5
  }
  
  # Axis limits ----------------------------------------------------------------
  
  if ( ylimStyle == "pwfsl" ) {
    # Well defined y-axis limits for visual stability
    ylo <- 0
    ymax <- max( tidyData$value, na.rm = TRUE )
    if ( ymax <= 50 ) {
      yhi <- 50
    } else if ( ymax <= 100 ) {
      yhi <- 100
    } else if ( ymax <= 200 ) {
      yhi <- 200
    } else if ( ymax <= 400 ) {
      yhi <- 400
    } else if ( ymax <= 600 )  {
      yhi <- 600
    } else if ( ymax <= 1000 )  {
      yhi <- 1000
    } else if ( ymax <= 1500 )  {
      yhi <- 1500
    } else {
      yhi <- 1.05 * ymax
    }
  } else {
    # Standard y-axis limits
    ylo <- 0
    yhi <- max(1.00*tidyData$value, na.rm = TRUE)
  }
  
  # NOTE:  X-axis must be extended to fit the complete last day.
  # NOTE:  Then a little bit more for style.
  xRangeSecs <- as.numeric(difftime(enddate, startdate, timezone, units = "secs"))
  marginSecs <- 0.02 * xRangeSecs
  xlo <- startdate - lubridate::dseconds(marginSecs)
  xhi <- enddate + lubridate::ddays(1) + lubridate::dseconds(marginSecs)
  
  # AQI annotation styling
  if ( !is.null(aqiStyle) ) {
    aqiLineSize <- 0.5     # horizontal lines
    aqiBarWidth <- 0.01    # stacked bars
    if ( stringr::str_detect(aqiStyle, "bars") ) {
      # Set bar width and move xlo to accommodate barWidth and some extra space
      widthSecs <- aqiBarWidth * xRangeSecs
      xlo <- xlo - (3 * lubridate::dseconds(widthSecs))
    }
  }
  
  # Create plot ----------------------------------------------------------------
  
  base_family <- ""
  base_size <- 11 # DELETEME
  half_line <- base_size/2 # DELEMTE
  
  ggplotBase <- ggplot()
  
  if ( !is.null(aqiStyle) ) {
    ggplotBase <- aqiAnnotation(
      ggplotBase, 
      xlo, 
      xhi, 
      ylo, 
      yhi, 
      aqiStyle, 
      aqiBarWidth,
      aqiLineSize
    )
  }
  
  ggplotBase <- ggplotBase + 
    
    geom_point(
      data = tidyData, 
      aes(
        .data$datetime, 
        .data$value
      ),
      shape = pm25Shape,
      alpha = pm25Alpha,
      col = pm25Color,
      show.legend = FALSE
    )
  
  if ( nowcastStyle != "" ) {
    
    ggplotBase <- ggplotBase + 
      
      geom_line(
        data = tidyNowcast, 
        aes(
          .data$datetime, 
          .data$value
        ),
        size = nowcastSize,
        alpha = nowcastAlpha,
        col = nowcastColor,
        show.legend = FALSE
      )
    
  }
  
  ggplotBase <- ggplotBase + 
    
    # Add x- and y-axes
    scale_x_datetime(
      timezone = timezone,
      limits = c(xlo,xhi),
      expand = c(0,0.05),
      breaks = breaks,
      minor_breaks = minor_breaks,
      date_labels = "%b %d"
    ) +
    
    # Y limits with no extra space below zero
    scale_y_continuous(
      limits = c(ylo,yhi),
      expand = c(0.05,0)
    ) +
    ylab("PM2.5 (\u00b5g/m3)") +
    
    # Title
    ggtitle(title)
  
  # Return ---------------------------------------------------------------------
  
  return(ggplotBase)
  
}





