#' @title Create a timeseries plot for one or more monitors
#'
#' @description
#' Create a timeseries plot with points or lines showing PM2.5 data over a 
#' period for one or more monitors in a \eph{ws_monitor} object.
#' 
#' @param ws_monitor \emph{ws_monitor} object.
#' @param startdate Desired start date (integer or character in Ymd format 
#'        or \code{POSIXct}).
#' @param enddate Desired end date (integer or character in Ymd format
#'        or \code{POSIXct}).
#' @param colorPalette Palette function to convert monitor values into colors.
#' @param ylimStyle Style of y-axis limits. One of \code{auto|pwfsl}.
#' @param showAQIStackedBars Logical specifying whether to show stacked AQI
#' color bars on the left.
#' @param showAQILines Logical specifying whether to show AQI color lines.
#' @param showAQILegend Logical specifying whether to show an AQI legend.
#' @param dateFormat Format for x-axis dates. Used for \code{date_labels}
#' argument to \code{scale_x_datetime}.
#' @param title Optional title.
#'
#' @return A `ggplot` plot object with a daily bar plot for a single monitor.
#'
#' @importFrom rlang .data
#' @export
#' @examples
#' ws_monitor <- PWFSLSmoke::Carmel_Valley
#' startdate <- "2016-08-05"
#' enddate <- "2016-08-19"
#' timeseriesPlotBase(ws_monitor, startdate, enddate)

timeseriesPlotBase <- function(ws_monitor,
                             startdate = NULL,
                             enddate = NULL,
                             colorPalette = aqiPalette("aqi"),
                             ylimStyle = "auto",
                             showAQIStackedBars = FALSE,
                             showAQILines = FALSE,
                             showAQILegend = FALSE,
                             dateFormat = "%b %d",
                             title = "") {
  
  # For debugging --------------------------------------------------------------
  
  if (FALSE) {
    
    # Carmel Valley
    ws_monitor <- PWFSLSmoke::Carmel_Valley
    startdate <- "2016-07-01"
    enddate <- "2016-08-28"
    colorPalette <- aqiPalette("aqi")
    ylimStyle <- "pwfsl"
    showAQIStackedBars <- TRUE
    showAQILines <- TRUE
    showAQILegend <- FALSE
    title <- ""
    
  }
  
  # Validate arguments ---------------------------------------------------------
  
  if ( !monitor_isMonitor(ws_monitor) ) {
    stop("Required parameter 'ws_monitor' is not a valid ws_monitor object.")
  } else if ( monitor_isEmpty(ws_monitor) ) {
    stop("Required parameter 'ws_monitor' is empty.")
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
  
  timezone <- ws_monitor$meta$timezone[1]

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
  
  # Choose date_breaks
  if ( dayCount >= 0 && dayCount <= 9 ) {
    date_breaks = "1 days"
  } else if ( dayCount <= 21 ) {
    date_breaks = "3 days"
  } else if ( dayCount <= 60 ) {
    date_breaks = "1 weeks"
  } else if ( dayCount <= 120 ) {
    date_breaks = "2 weeks"
  } else {
    date_breaks = "1 months"
  }
  
  # Subset based on startdate and enddate
  mon <- monitor_subset(ws_monitor,
                        tlim = c(startdate, enddate + lubridate::dhours(23)),
                        timezone = timezone)
  
  # Timeseries data ------------------------------------------------------------
  
  dailyData <- mon$data
  tidyData <- reshape2::melt(mon$data, id.vars = "datetime")
  
  
  
  
  # Everything on the same plot
  ggplot(tidyData, 
         aes(
           datetime, 
           value
           )
         ) + 
    geom_point(
      shape = "square",
      alpha = 0.1,
      col = 'black',
      show.legend = FALSE
    )
  
  # # Separate plots
  # ggplot(tidyData, aes(datetime,value)) + 
  #   geom_point() + 
  #   stat_smooth() +
  #   facet_wrap(~variable)
  # 

  
  
  
  
  
  # Color
  dailyData$color = colorPalette(dailyData$pm25)
  
  # Axis limits ----------------------------------------------------------------
  
  if ( ylimStyle == "pwfsl" ) {
    # Well defined y-axis limits for visual stability
    ylo <- 0
    ymax <- max( dailyData$pm25, na.rm = TRUE )
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
    yhi <- max(1.05*dailyData$pm25, na.rm = TRUE)
  }

  # NOTE:  X-axis must be extended to fit the first and last bars.
  # NOTE:  Then a little bit more for style.
  xRangeSecs <- as.numeric(difftime(enddate, startdate, timezone, units = "secs"))
  marginSecs <- 0.02 * xRangeSecs
  xlo <- startdate - lubridate::ddays(0.5) - lubridate::dseconds(marginSecs)
  xhi <- enddate + lubridate::ddays(0.5) + lubridate::dseconds(marginSecs)

  # AQI Stacked bars -----------------------------------------------------------
  
  if ( showAQIStackedBars ) {
    
    # Get bar width
    width <- 0.01 * xRangeSecs
    right <- xlo - lubridate::dseconds(marginSecs) 
    xlo <- right - lubridate::dseconds(width)

    # Create data
    aqiStackedBarsData <- data.frame(
      xmin = rep(xlo, 6),
      xmax = rep(right, 6),
      ymin = c(ylo, AQI$breaks_24[2:6]),
      ymax = c(AQI$breaks_24[2:6], 1e6)
    )
    # Last bar must top out at yhi
    aqiStackedBarsData <- aqiStackedBarsData %>%
      dplyr::filter(.data$ymin < yhi)
    barCount <- nrow(aqiStackedBarsData)
    aqiStackedBarsData$ymax[barCount] <- yhi
    aqiStackedBarsColors <- AQI$colors[1:barCount]
    
  }
  
  if ( showAQILines ) {
    
    # Create data
    aqiStackedLinesData <- data.frame(
      x = rep(xlo, 5),
      xend = rep(xhi, 5),
      y = c(AQI$breaks_24[2:6]),
      yend = c(AQI$breaks_24[2:6])
    )
    aqiLinesColors <- AQI$colors[2:6]
    
  }
  
  
  # Create plot ----------------------------------------------------------------
  
  base_family <- ""
  base_size <- 11 # DELETEME
  half_line <- base_size/2 # DELEMTE
  
  timeseriesPlotBase <- ggplot()
  
  if ( showAQIStackedBars ) {
    
    timeseriesPlotBase <- timeseriesPlotBase + 
      
      geom_rect(
        data = aqiStackedBarsData,
        aes(
          xmin = .data$xmin,
          xmax = .data$xmax,
          ymin = .data$ymin,
          ymax = .data$ymax
        ),
        fill = aqiStackedBarsColors
      )
    
  }
  
  if ( showAQILines ) {
    
    timeseriesPlotBase <- timeseriesPlotBase + 
      
      geom_segment(
        data = aqiStackedLinesData,
        aes(
          x = .data$x,
          xend = .data$xend,
          y = .data$y,
          yend = .data$yend
        ),
        color = aqiLinesColors
      )  
    
  }
  
  timeseriesPlotBase <- timeseriesPlotBase + 
    
    # Add daily statistic bars
    geom_bar(
      data = dailyData,
      # See https://www.aj2duncan.com/blog/missing-data-ggplot2-barplots/
      position = position_dodge(preserve = 'single'), # don't drop missing values
      aes(
        x = .data$datetime,
        y = .data$pm25#,
        #fill = .data$color
      ),
      fill = dailyData$color,
      color = borderColor,
      stat = "identity"
    ) +
    
    # Add x- and y-axes
    scale_x_datetime(
      limits = c(xlo,xhi),
      expand = c(0,0),
      date_breaks = date_breaks, 
      date_labels = dateFormat
    ) +
    
    # Y limits with no extra space below zero
    scale_y_continuous(
      limits = c(ylo,yhi),
      expand = c(0,0)
    ) +
    ylab("PM2.5 (\u00b5g/m3)") +
    
    # Title
    ggtitle(title)
  
  return(timeseriesPlotBase)
  
}





