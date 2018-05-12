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
                             yLabel = NULL) {


  # TODO: make function work with tidy monitor data
  ##      Need to implement a `monitor_dailyStatistic()` function for tidy
  ##      monitor data
  if (!monitor_isMonitor(data)) {
    stop("This function can currently only take in a `ws_monitor` object")
  }

  # Set up data

  monData <- data %>%
    monitor_subset(monitorIDs = monitors)

  hourlyData <- monData %>%
    monitor_toTidy() %>%
    mutate(
      aqiCategory = cut(
        .data$pm25,
        AQI$breaks_24,
        include.lowest = TRUE,
        labels = AQI$names))

  dailyData <- monData %>%
    monitor_dailyStatistic() %>%
    monitor_toTidy() %>%
    mutate(
      aqiCategory = cut(
        .data$pm25,
        AQI$breaks_24,
        include.lowest = TRUE,
        labels = AQI$names))

  # Set up parameters
  if (is.null(title)) {
    title <- expression(paste("Daily and Hourly ", "PM"[2.5], " Levels"))
  }

  if (is.null(xLabel)) {
    xLabel <- paste(
      unique(lubridate::year(dailyData$datetime)),
      collapse = ", ")
  }

  if (is.null(yLabel)) {
    yLabel <- expression(paste("PM"[2.5] * " (", mu, "g/m"^3 * ")"))
  }

  # define scales

  aqiNames <- AQI$names
  aqiActions <- AQI$actions
  aqiColors <- AQI$colors

  # plot data

  # TODO: create new ggplot stat object to handle daily data computation
  # TODO: add ability to create plot with either raw hourly data or nowcast
  tarnayPlot <-
    ggplot(dailyData,
      aes_(x = ~ datetime, y = ~ pm25,
        fill = ~ aqiCategory)) +
    geom_col( # used to align axes
      width = 86400,
      alpha = 0) +
    geom_col(data = hourlyData,
      aes_(color = ~ aqiCategory), # needed for legend
      width = 3600 * .45,
      size = 0) +
    geom_col(data = dailyData,
      width = 86400,
      alpha = 0.3,
      color = "black",
      size = .2) +
    facet_wrap(~ siteName, ncol = columns) +

    # TODO: combine AQI text into single scale
    # TODO: make legend scale with plot size
    scale_fill_manual(
      name = "AQI Category",
      values = aqiColors,
      labels = aqiNames,
      drop = FALSE,
      guide = guide_legend(
        order = 1,
        override.aes = list(alpha = 1, color = NA))) +
    scale_color_manual(
      name = "Recommended Actions",
      values = aqiColors,
      labels = aqiActions,
      drop = FALSE,
      guide = guide_legend(
        order = 2,
        override.aes = list(color = NA, fill = NA))) +
    scale_x_datetime(
      date_breaks = "1 day",
      date_labels = '%b %d',
      expand = c(0, 0)) +

    # TODO: make labels a parameter
    # TODO: make labels scale with plot size
    labs(
      title = title,
      x = xLabel,
      y = yLabel) +

    # TODO: Create theme object that can be used across the package
    theme_minimal() +
    theme(
      strip.background = element_rect(fill = "#E0E0E0"),

      panel.background = element_rect(color = "black"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_line(linetype = 3, color = 'gray70'),
      panel.grid.major.y = element_line(linetype = 3, color = 'gray40'),
      panel.grid.minor.y = element_blank(),

      legend.position = "top",
      legend.justification = c(1, 1),
      legend.direction = "vertical",
      legend.box.background = element_rect(color = "black"),

      plot.title = element_text(size = 24),
      axis.title = element_text(size = 18),
      axis.text.x = element_text(size = 12, angle = 30, hjust = 1),
      axis.text.y = element_text(size = 12),
      strip.text = element_text(size = 14),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    )

  tarnayPlot
}
