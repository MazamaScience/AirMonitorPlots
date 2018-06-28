#' @title Create a Tarnay plot for many monitors
#'
#' @description
#' Create a timeseries barplot showing PM2.5 data for the given monitors. The
#' overall plot is faceted by monitor, and each facet has two sets of columns:
#' one for daily levels, and one for hourly levels.
#'
#' @param monitors Monitor ID(s) to create plot for.
#' @param data Data used to create plot (NOTE: currently must be a `ws_monitor`
#'   object).
#' @param columns Number of columns the faceted plot should have (default 1).
#' @param title The title of the plot. Defaults to specifying the types of
#'   data present in the plot.
#' @param xLabel The x-axis label of the plot. Defaults to years present in
#'   data.
#' @param yLabel The y-axis label of the plot. Defaults to PM2.5.
#' @param includeLink Option to include a link to an AQI explainer at the bottom
#'   of the plot (default `TRUE`).
#' @param hourlyType The type of hourly data to include in the plot. The options
#'   include "nowcast" (hourly nowcast values), "raw" (raw hourly values), or
#'   "none" (no hourly data at all) (default "nowcast").
#' @param colorScale The ordered color pallete used to represent each AQI
#'   category. Currently defaults to (and only accepts) "epa_aqi".
#'
#' @return A **ggplot** plot of the given monitors and data.
#'
#' @import ggplot2
#' @export
#'
#' @examples
#'
createTarnayPlot <- function(monitors,
                             data,
                             columns = 1,
                             title = NULL,
                             xLabel = NULL,
                             yLabel = NULL,
                             includeLink = TRUE,
                             hourlyType = "nowcast",
                             colorScale = "epa_aqi",
                             includeThirdCol = FALSE) {

  # Validate data -------------------------------------------------------------

  # TODO: make function work with tidy monitor data
  ##      Need to implement a `monitor_dailyStatistic()` function for tidy
  ##      monitor data
  if (!monitor_isMonitor(data)) {
    stop("This function can currently only take in a `ws_monitor` object")
  }

  availableHourlyTypes <- c("nowcast", "raw", "none")
  if (!(hourlyType %in% availableHourlyTypes)) {
    stop(
      paste0(
        hourlyType, " is not a posible hourly data type. \n",
        "Please choose from: ", paste0(availableHourlyTypes, collapse = ", ")
      )
    )

  }

  # Set up data ---------------------------------------------------------------

  monData <- data %>%
    monitor_subset(monitorIDs = monitors)

  # Calculate daily data (or none)
  # TODO: Add ability to include only hourly values (no daily)
  includeDaily <- TRUE
  if (includeDaily) {

    dailyData <- monData %>%
      monitor_dailyStatistic() %>%
      monitor_toTidy() %>%
      mutate(
        aqiCategory = cut(
          .data$pm25,
          AQI$breaks_24,
          include.lowest = TRUE,
          labels = AQI$names))

  } else {

    dailyData <- NULL
  }

  # Calculate the appropriate hourly values (or none)
  if (hourlyType != "none") {

    if (hourlyType == "nowcast") {

      hourlyData <- monData %>%
        monitor_nowcast() %>%
        monitor_toTidy() %>%
        mutate(
          aqiCategory = cut(
            .data$pm25,
            AQI$breaks_24,
            include.lowest = TRUE,
            labels = AQI$names))

    # hourlyType == "raw"
    } else {

      hourlyData <- monData %>%
        monitor_toTidy() %>%
        mutate(
          aqiCategory = cut(
            .data$pm25,
            AQI$breaks_24,
            include.lowest = TRUE,
            labels = AQI$names))
    }

  } else {

    hourlyData <- NULL
  }


  # Set up labels -------------------------------------------------------------

  if (is.null(title)) {

    if (includeDaily) {
      dailyPart <- "Daily (AQI)"
    } else {
      dailyPart <- NULL
    }

    if (hourlyType == "nowcast") {
      hourlyPart <- "Hourly (NowCast)"
    } else if (hourlyType == "raw") {
      hourlyPart <- "Hourly (raw)"
    } else {
      hourlyPart <- NULL
    }

    if (includeDaily && hourlyType != "none") {
      titlePart <- paste(dailyPart, hourlyPart, sep = " and ")
    } else {
      titlePart <- paste0(dailyPart, hourlyPart)
    }

    title <- bquote(.(titlePart) ~ PM[2.5] ~ "Levels")

  }

  if (is.null(xLabel)) {
    yearPart <- paste(
      unique(lubridate::year(dailyData$datetime)),
      collapse = ", ")

    xLabel <- paste0("Date, midnight to midnight (", yearPart, ")")
  }

  if (is.null(yLabel)) {
    yLabel <- expression(paste("PM"[2.5] * " (", mu, "g/m"^3 * ")"))
  }

  if (includeLink) {
    caption <-
      "Learn more about AQI at: airnow.gov/index.cfm?action=aqibasics.aqi"
  } else {
    caption <- NULL
  }

  # Define scales -------------------------------------------------------------

  aqiNames <- AQI$names
  aqiActions <- AQI$actions

  if (colorScale == "epa_aqi") {
    aqiColors <- AQI$colors
  } else {
    message("Color scale not recognized: ", colorScale,
            ". Defaulting to EPA AQI colors.")
    aqiColors <- AQI$colors
  }

  # Plot data -----------------------------------------------------------------

  # TODO: create new ggplot stat object to handle daily data computation
  # TODO: add ability to create plot with either raw hourly data or nowcast
  tarnayPlot <-
    ggplot(dailyData,
      aes_(x = ~ datetime, y = ~ pm25,
        fill = ~ aqiCategory)) +
    geom_col(data = hourlyData,
      aes_(color = ~ aqiCategory), # needed for legend
      width = 3600 * .45,
      size = 0) +
    geom_col(data = dailyData,
      aes_(x = ~ datetime + lubridate::dhours(12)),
      width = 86400,
      alpha = 0.3,
      color = "black",
      size = .2) +
    facet_wrap(~ siteName, ncol = columns) +

    # TODO: combine AQI text into single scale
    # TODO: make legend scale with plot size
    scale_fill_manual(
      name = "Daily Air Quality Index (24hr AQI)",
      values = aqiColors,
      labels = aqiNames,
      drop = FALSE,
      guide = guide_legend(
        order = 1,
        override.aes = list(alpha = 1, color = NA))) +
    scale_color_manual(
      name = "Hourly NowCast (actions to protect yourself)",
      values = aqiColors,
      labels = aqiActions,
      drop = FALSE,
      guide = guide_legend(
        order = 2,
        override.aes = list(color = NA, fill = NA))) +

    scale_x_datetime(
      breaks = unique(
        lubridate::floor_date(
          dailyData$datetime,
          unit = "day")
        ) + lubridate::dhours(12),
      minor_breaks = unique(
        lubridate::floor_date(
          dailyData$datetime,
          unit = "day")
        ),
      date_labels = "%b %d",
      expand = c(0, 0)) +

    # TODO: make labels a parameter
    # TODO: make labels scale with plot size
    labs(
      title = title,
      x = xLabel,
      y = yLabel,
      caption = caption) +

    theme_mazamaBar(base_size = 12)

  if (includeThirdCol) {
    tarnayPlot <- tarnayPlot +
      geom_point(data = dailyData,
                 aes_(x = ~ datetime + lubridate::dhours(12),
                      shape = ~ aqiCategory),
                 alpha = 0) +
      scale_shape_manual(
        values = c(6:11),
        name = "Standard Advice",
        labels = c(
          "Tempus vitae molestie Convallis curabitur vestibulum",
          "Justo rutrum A ut arcu felis eget litora libero",
          "ligula morbi vestibulum eu tellus vel consectetur",
          "congue. Ultricies et commodo amet accumsan nunc eget",
          "mattis tincidunt nonummy donec viverra sed nec iaculis",
          "consectetuer lectus dictum. Velit iaculis a"
        ),
        drop = FALSE,
        na.translate = FALSE) +
      theme(legend.text = element_text(size = rel(.5)))
  }

  return(tarnayPlot)
}
