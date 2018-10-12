#' @title Create a Clock plot for a single monitor
#'
#' @description
#' Create a "clock plot" showing PM2.5 data for a single day for the given 
#' monitors. A colored bar curves around in a clockwise manner with 12/4 of the
#' bar colored for each hour of the local time day.
#' 
#' @param ws_monitor \emph{ws_monitor} object.
#' @param monitorID Monitor ID of interest.
#' @param startdate Desired start date (integer or character representing YYYYMMDD)
#' @param style Plot style one of \code{icon|fan}.
#'
#' @return A **ggplot** plot of the given monitors and data.
#'
#' @export

clockPlot <- function(ws_monitor,
                      monitorIDs = NULL,
                      startdate = NULL,
                      style = 'icon') {
  
  # Validate data -------------------------------------------------------------
  
  if (!monitor_isMonitor(ws_monitor)) {
    stop("This function can currently only take in a `ws_monitor` object")
  }

  # validHourlyDataTypes <- c("nowcast", "raw", "none")
  # if (!(hourlyDataType %in% validHourlyDataTypes)) {
  #   stop(
  #     paste0(
  #       hourlyDataType, " is not a valid hourlyDataType. \n",
  #       "Please choose from: ", paste0(validHourlyDataTypes, collapse = ", ")
  #     )
  #   )
  #   
  # }
  
  # Set up data ---------------------------------------------------------------
  
  # Subset based on monitorID

  mon <- ws_monitor %>%
    monitor_subset(monitorIDs = monitorID)

  # Subset based on startdate

  timezone <- mon$meta$timezone[1]

  if ( is.numeric(startdate) || is.character(startdate) ) {
    startdate <- lubridate::ymd(startdate, tz = timezone)
  } else if ( lubridate::is.POSIXct(startdate) ) {
    startdate <- lubridate::force_tz(startdate, tzone = timezone)
  } else if ( !is.null(startdate) ) {
    stop(paste0(
      "Argument 'startdate' must be a numeric/charcter vector of the form yyyymmdd",
      "or of class POSIXct."))
  }
  enddate <- startdate - lubridate::dhours(23)
  
  mon <- monitor_subset(mon, tlim=c(startdate,enddate))
  
  # ## Transform data
  # 
  # # Calculate daily data (or none)
  # # TODO: Add ability to include only hourly values (no daily)
  # includeDaily <- TRUE
  # if (includeDaily) {
  #   
  #   dailyData <- monData %>%
  #     monitor_dailyStatistic() %>%
  #     monitor_subset(tlim = tlim) %>%
  #     monitor_toTidy() %>%
  #     mutate(
  #       aqiCategory = cut(
  #         .data$pm25,
  #         AQI$breaks_24,
  #         include.lowest = TRUE,
  #         labels = AQI$names))
  #   
  # } else {
  #   
  #   dailyData <- NULL
  #   
  # }
  # 
  # # Calculate the appropriate hourly values (or none)
  # if (hourlyDataType != "none") {
  #   
  #   if (hourlyDataType == "nowcast") {
  #     
  #     hourlyData <- monData %>%
  #       monitor_nowcast(includeShortTerm = TRUE) %>%
  #       monitor_subset(tlim = tlim) %>%
  #       monitor_toTidy() %>%
  #       mutate(
  #         aqiCategory = cut(
  #           .data$pm25,
  #           AQI$breaks_24,
  #           include.lowest = TRUE,
  #           labels = AQI$names))
  #     
  #     # hourlyDataType == "raw"
  #   } else {
  #     
  #     hourlyData <- monData %>%
  #       monitor_subset(tlim = tlim) %>%
  #       monitor_toTidy() %>%
  #       mutate(
  #         aqiCategory = cut(
  #           .data$pm25,
  #           AQI$breaks_24,
  #           include.lowest = TRUE,
  #           labels = AQI$names))
  #   }
  #   
  # } else {
  #   
  #   hourlyData <- NULL
  # }
  
  
  # Set up labels -------------------------------------------------------------
  
  # if (is.null(title)) {
  #   
  #   if (includeDaily) {
  #     dailyPart <- "Daily (AQI)"
  #   } else {
  #     dailyPart <- NULL
  #   }
  #   
  #   if (hourlyDataType == "nowcast") {
  #     hourlyPart <- "Hourly (NowCast)"
  #   } else if (hourlyDataType == "raw") {
  #     hourlyPart <- "Hourly (raw)"
  #   } else {
  #     hourlyPart <- NULL
  #   }
  #   
  #   if (includeDaily && hourlyDataType != "none") {
  #     titlePart <- paste(dailyPart, hourlyPart, sep = " and ")
  #   } else {
  #     titlePart <- paste0(dailyPart, hourlyPart)
  #   }
  #   
  #   title <- bquote(.(titlePart) ~ PM[2.5] ~ "Levels")
  #   
  # }
  # 
  # if (is.null(xLabel)) {
  #   yearPart <- paste(
  #     unique(lubridate::year(dailyData$datetime)),
  #     collapse = ", ")
  #   
  #   xLabel <- paste0("Date, midnight to midnight (", yearPart, ")")
  # }
  # 
  # if (is.null(yLabel)) {
  #   yLabel <- expression(paste("PM"[2.5] * " (", mu, "g/m"^3 * ")"))
  # }
  # 
  # if (includeLink) {
  #   caption <-
  #     "Learn more about AQI at: airnow.gov/index.cfm?action=aqibasics.aqi"
  # } else {
  #   caption <- NULL
  # }
  
  # Define scales -------------------------------------------------------------
  
  # ## color scale
  # 
  # aqiNames <- AQI$names
  # aqiActions <- AQI$actions
  # 
  # if (palette == "epa_aqi") {
  #   aqiColors <- AQI$colors
  # } else {
  #   message("Color scale not recognized: ", palette,
  #           ". Defaulting to EPA AQI colors.")
  #   aqiColors <- AQI$colors
  # }
  # 
  # ## time scale
  # 
  # minDate <- lubridate::as_datetime(min(dailyData$datetime))
  # maxDate <- lubridate::as_datetime(max(dailyData$datetime))
  # 
  # timeSpan <- lubridate::as.interval(maxDate - minDate, minDate)
  # numDays <- timeSpan %/% lubridate::days(1)
  # 
  # if (numDays <= 14) {
  #   timeScale <- "day"
  # } else if (numDays <= 30) {
  #   timeScale <- "2 days"
  # } else if (numDays <= 150) {
  #   timeScale <- "week"
  # } else {
  #   timeScale <- "month"
  # }
  # 
  # ## datetime vector for date labels
  # 
  # if (is.null(hourlyData)) {
  #   datetimeValues <- dailyData$datetime
  # } else {
  #   datetimeValues <- hourlyData$datetime
  # }
  # 
  # datetimeValues <- datetimeValues %>% lubridate::with_tz(tzone = timezone)
  # 
  # datetimeLabelsMajor <- unique(
  #   lubridate::floor_date(
  #     datetimeValues,
  #     unit = timeScale)
  # ) + lubridate::dhours(12)
  # 
  # datetimeLabelsMinor <- unique(
  #   lubridate::floor_date(
  #     datetimeValues,
  #     unit = "day")
  # )
  
  # Plot data -----------------------------------------------------------------
  
  # # TODO: create new ggplot stat object to handle daily data computation
  # # TODO: add ability to create plot with either raw hourly data or nowcast
  # dailyHourlyPlot <-
  #   ggplot(dailyData,
  #          aes_(x = ~ datetime, y = ~ pm25,
  #               fill = ~ aqiCategory)) +
  #   geom_col(data = hourlyData,
  #            aes_(color = ~ aqiCategory), # needed for legend
  #            width = 3600 * .45,
  #            size = 0) +
  #   geom_col(data = dailyData,
  #            aes_(x = ~ datetime + lubridate::dhours(12)),
  #            width = 86400,
  #            alpha = 0.3,
  #            color = "grey20",
  #            size = .1) +
  #   facet_wrap(~ siteName, ncol = columns) +
  #   
  #   # TODO: combine AQI text into single scale
  #   # TODO: make legend scale with plot size
  #   scale_fill_manual(
  #     name = "Daily Air Quality Index (24hr AQI)",
  #     values = aqiColors,
  #     labels = aqiNames,
  #     drop = FALSE,
  #     guide = guide_legend(
  #       order = 1,
  #       reverse = TRUE,
  #       override.aes = list(alpha = 1, color = NA))) +
  #   scale_color_manual(
  #     name = "Hourly NowCast (actions to protect yourself)",
  #     values = aqiColors,
  #     labels = aqiActions,
  #     drop = FALSE,
  #     guide = guide_legend(
  #       order = 2,
  #       reverse = TRUE,
  #       override.aes = list(color = NA, fill = NA))) +
  #   
  #   scale_x_datetime(
  #     breaks = datetimeLabelsMajor,
  #     minor_breaks = datetimeLabelsMinor,
  #     date_labels = "%b %d",
  #     expand = c(0, 0)) +
  #   
  #   # TODO: make labels a parameter
  #   # TODO: make labels scale with plot size
  #   labs(
  #     title = title,
  #     x = xLabel,
  #     y = yLabel,
  #     caption = caption) +
  #   
  #   theme_mazamaBar(base_size = 12)
  # 
  # if (includeThirdCol) {
  #   dailyHourlyPlot <- dailyHourlyPlot +
  #     geom_point(data = dailyData,
  #                aes_(x = ~ datetime + lubridate::dhours(12),
  #                     shape = ~ aqiCategory),
  #                alpha = 0) +
  #     scale_shape_manual(
  #       values = c(6:11),
  #       name = "Standard Advice",
  #       labels = c(
  #         "Tempus vitae molestie Convallis curabitur vestibulum",
  #         "Justo rutrum A ut arcu felis eget litora libero",
  #         "ligula morbi vestibulum eu tellus vel consectetur",
  #         "congue. Ultricies et commodo amet accumsan nunc eget",
  #         "mattis tincidunt nonummy donec viverra sed nec iaculis",
  #         "consectetuer lectus dictum. Velit iaculis a"
  #       ),
  #       drop = FALSE,
  #       na.translate = FALSE) +
  #     theme(legend.text = element_text(size = rel(.5)))
  # }
  # 
  # return(dailyHourlyPlot)
  
  data <- monitor$data
  names(data) <- c("datetime", "pm25")
  
  dailyMean <- round(mean(data$pm25), digits = 0)
  
  # Define AQI palette
  pal <- leaflet::colorBin(palette = AQI$colors, bins = AQI$breaks_24, na.color = "#bbbbbb")
  
  # Define the start, end, and color of each period
  data$fraction = (1 / 24)
  data$ymax = cumsum(data$fraction)
  data$ymin = c(0, head(data$ymax, n = -1))
  data$hue = pal(data$pm25)
  
  # For bottom gap between the start and end of the day
  gap <- 0.1
  thetaOffset <- pi + (2 * pi) * (1 - (1 / (1 + gap))) / 2
  
  x <- c(0)
  y <- c(0)
  h <- c(pal(dailyMean))
  face = data.frame(x, y)
  
  clockPlot <- ggplot(data) +
    geom_point(data = face, size = 170, color = "black", aes(x = x, y = y)) +
    geom_rect(aes(fill = hue, ymin = ymin, ymax = ymax, xmin = 1.5, xmax = 4)) +
    coord_polar(theta = 'y', direction = 1, start = thetaOffset) +
    xlim(0, 4.5) +
    ylim(0, 1 + gap) +
    theme(panel.grid = element_blank()) +
    theme(axis.title = element_blank()) +
    theme(axis.text = element_blank()) +
    theme(axis.ticks = element_blank()) +
    theme(panel.background = element_rect(fill = "transparent", colour = NA)) +
    theme(plot.background = element_rect(fill = "transparent", colour = NA)) +
    scale_fill_identity(guide = "legend", breaks = data$hue) +
    theme(legend.position = "none")
  
  return(clockPlot)

}
