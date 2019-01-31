#' @title Create a daily barplot timeseries for one or more monitors
#'
#' @description
#' This function assembles various layers to create a production-ready
#' daily barplot for one or more monitors. 
#'
#' @param ws_tidy dataframe of monitor data, created from a \code{ws_monitor}
#' object using \code{monitor_toTidy()}. 
#' @param startdate Desired start date (integer or character in ymd format or POSIXct)
#' @param enddate Desired end date (integer or character in ymd format or POSIXct)
#' @param monitorIDs vector of monitorIDs to include in the plot. If 
#' more than one, different monitors will be plotted in different colors.
#' @param style String indicating plotting style. Either \code{"large"} or \code{"small"}.
#' \code{style = "large"} is suitable for plots larger than 450x450px, and \code{"small"}
#' is suitable for plots 450x450px or smaller. 
#' @param title Plot title. If NULL, a suitable title will be constructed.
#' @param timezone Timezone for x-axis scale. If NULL and only one timezone present
#' in the data, the data timezone will be used. If NULL and multiple timezones 
#' present, the default is UTC. 
#' @param today Logical indicating whether to include a shaded "current NowCast" bar 
#' for Today. Ignored if data is not current. 
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
#' ws_monitor <- PWFSLSmoke::Carmel_Valley
#' ws_tidy <- monitor_toTidy(ws_monitor)
#' tidy_ggDailyBarplot(ws_tidy)

tidy_ggDailyBarplot <- function(ws_tidy,
                                startdate = NULL,
                                enddate = NULL,
                                monitorIDs = NULL,
                                style = "small", 
                                title = NULL,
                                timezone = NULL,
                                today = TRUE,
                                ...) {
  
  
  # Sanity checks
  if (!monitor_isTidy(ws_tidy)) {
    stop("ws_tidy must be ws_tidy object")
  }
  
  if (any(!monitorIDs %in% unique(ws_tidy$monitorID))) {
    invalidIDs <- monitorIDs[which(!monitorIDs %in% unique(ws_tidy$monitorID))]
    stop(paste0("MonitorIDs not present in data: ", paste0(invalidIDs, collapse = ", ")))
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
  
  if (!is.null(timezone)) {
    if (!timezone %in% OlsonNames()) {
      stop("Invalid Timezone")
    }
  }
  
  
  # Subset Data
  if (!is.null(monitorIDs)) {
    ws_tidy <- dplyr::filter(.data = ws_tidy, .data$monitorID %in% monitorIDs)
  } 
  
  
  if ( length(unique(ws_tidy$monitorID)) > 1) {
    if (is.null(title)) title <- paste0("Daily Average PM2.5 for ", 
                                        length(unique(ws_tidy$monitorID)), 
                                        " monitors")
  } else {
    if(is.null(title)) title <- paste0("Daily Average PM2.5\n",
                                       "Site: ", unique(ws_tidy$siteName))
  }
  
  # Get timezone
  if ( length(unique(ws_tidy$timezone)) > 1 ) {
    timezone <- "UTC"
    xlab <- "Time (UTC)"
  } else {
    timezone <- unique(ws_tidy$timezone)
    xlab <- "Local Time"
  }
  
  # Set default startdate and enddate
  if (is.null(startdate)) {
    startdate <- min(ws_tidy$datetime)
  } 
  if (is.null(enddate)) {
    enddate <- max(ws_tidy$datetime)
  }
  
  # Parse startdate and enddate
  startdate <- lubridate::floor_date(parseDatetime(startdate, timezone = timezone), "day")
  enddate <- min(c(
    lubridate::floor_date(lubridate::now(timezone), "day") + lubridate::dhours(23),
    lubridate::floor_date(parseDatetime(enddate, timezone = timezone), "day") + lubridate::dhours(23)
  ))
  
  # Custom style formatting
  if (style == "large") {
    nowcastTextSize <- 4.5
    nowcastText <- "Current\nNowCast"
    date_format <- "%b %d"
    base_size <- 15
  } else if (style == "small") {
    nowcastTextSize <- 4
    nowcastText <- "Now-\nCast"
    date_format <- "%b\n%d"
    base_size <- 11
  }
  
  
  
  # Set "today"
  if ( !enddate == lubridate::ceiling_date(lubridate::now(timezone), "day") - lubridate::dhours(1) ) {
    today <- FALSE
  }
  
  
  # Create "current nowcast" bar
  if (today) {
    
    now <- lubridate::now()
    lastValidIndex <- dplyr::last(which(!is.na(ws_tidy$pm25)))
    
    if ( now - ws_tidy$datetime[lastValidIndex] > lubridate::dhours(5) ) {
      currentNowcast <- 0
    } else {
      nowcast <- .nowcast(ws_tidy$pm25)
      currentNowcast <- nowcast[lastValidIndex]
    }
    
    center <- lubridate::floor_date(now, "day") + lubridate::dhours(12)
    left <- center - 0.8/2*86400
    right <- center + 0.8/2*86400
    
    color <- AQI$colors[.bincode(currentNowcast, AQI$breaks_24)]
    
    rect <- annotate("rect", 
                     xmin = left, 
                     xmax = right,
                     ymin = 0,
                     ymax = currentNowcast,
                     fill = color,
                     color = "gray60",
                     alpha = .8)
    rect2 <- annotate("rect", 
                      xmin = left, 
                      xmax = right,
                      ymin = 0,
                      ymax = currentNowcast,
                      fill = "gray60",
                      color = "gray60",
                      alpha = .3)
    text <- annotate("text", 
                     y = 0,
                     x = center,
                     label = nowcastText,
                     vjust = 1.5,
                     color = "gray40",
                     size =  nowcastTextSize )
    
    
    nowcastBar <- list(
      rect,
      rect2,
      text,
      coord_cartesian(clip = "off") # Turn off clipping so labels can be added outside of plot region
    )
    
  } else {
    
    nowcastBar <- list()
    
  }
  
  
  # Create the plot
  ggplot_pm25Timeseries(ws_tidy,
                                startdate = startdate,
                                enddate = enddate,
                                timezone = timezone,
                                date_labels = date_format,
                                tick_location = "midday",
                                today_label = !today,
                                base_size = base_size,
                                ...) +
    custom_aqiLines(size = 1, alpha = .8) +
    stat_dailyAQILevel(timezone = timezone,
                       adjustylim = TRUE, 
                       color = "black") +
    custom_aqiStackedBar(width = .015) +
    ## Format/theme tweaks
    # Remove padding on y scale
    scale_y_continuous(expand = c(0,0)) +
    theme(axis.line.x.bottom = element_blank(), # remove line on x-axis
          panel.border = element_blank(), # remove box around plot
          panel.grid = element_blank(), # remove background grid lines
          axis.ticks.x.bottom = element_blank()) + #remove x-axis ticks + 
    ggtitle(title) +
    xlab(xlab) +
    nowcastBar +
    theme_dailyBarplot_pwfsl(size = style)
  
}
