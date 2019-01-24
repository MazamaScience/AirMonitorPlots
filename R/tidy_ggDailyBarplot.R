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
#' @param title Plot title. If NULL, a suitable title will be constructed.
#' @param timezone Timezone for x-axis scale. If NULL and only one timezone present
#' in the data, the data timezone will be used. If NULL and multiple timezones 
#' present, the default is UTC. 
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
                              style = "large", 
                              title = NULL,
                              timezone = NULL,
                              today = TRUE) {
  
  
  # Sanity checks
  if (!monitor_isTidy(ws_tidy)) {
    stop("ws_tidy must be ws_tidy objec")
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
  
  # Custom style formatting
  date_format <- "%b %d"
  if (style == "large") {
    nowcastTextSize <- 4.5
    custom_theme <- theme(axis.title.x.bottom = element_blank(),
                          plot.margin = margin(
                            unit(25, "pt"),    # Top
                            unit(10, "pt"),    # Right
                            unit(15, "pt"),    # Bottom
                            unit(10, "pt")     # Left
                          ),
                          axis.text = element_text(size = 12),
                          axis.title.y = element_text(size = 18),
                          plot.title = element_text(size = 20))
  }
  
  
  
  # Custom formatting for when today = TRUE
  if (!lubridate::as_date(parseDatetime(enddate), timezone) == lubridate::today(timezone)) {
    today <- FALSE
  }
  if (today) {
    labels_dt <- seq(lubridate::floor_date(parseDatetime(startdate, timezone), "day"),
                  lubridate::floor_date(parseDatetime(enddate, timezone), "day"),
                  by = "day")
    labels <- strftime(labels_dt, date_format)
    labels[length(labels)] <- ""
    date_format <- waiver()
  } else {
    labels <- waiver()
  }
  
  # Create the plot
  plot <- ggplot_pm25Timeseries(ws_tidy,
                        startdate = startdate,
                        enddate = enddate,
                        timezone = timezone,
                        labels = labels,
                        date_labels = date_format,
                        tick_location = "midday") +
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
    xlab(xlab)
  
  if (today) {
    plot <- plot + custom_currentNowcast(ws_tidy, 
                                         timezone = timezone,
                                         text_size = nowcastTextSize)
  }
  plot + custom_theme
}
