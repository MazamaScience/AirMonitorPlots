############################
# Create a summary barplot #
############################

library(PWFSLSmoke)
library(PWFSLSmokePlots)
library(ggplot2)
library(ggthemes)

# load data

data <- airnow_loadLatest()
santaMariaMonitors <- c("060831008_01",
                        "060792006_01",
                        "060832004_01",
                        "060798002_01",
                        "060792004_01")

summaryBarplot <- function(monitors, data) {

  data <- data %>%
    monitor_subset(monitorIDs = monitors)

  # make data tidy

  hourlyData <- data %>%
    wsMonToTidy() %>%
    mutate(
      aqiCategory = cut(
        pm25,
        AQI$breaks_24,
        include.lowest = T,
        labels = AQI$names))

  dailyData <- data %>%
    monitor_dailyStatistic() %>%
    wsMonToTidy() %>%
    mutate(
      aqiCategory = cut(
        pm25,
        AQI$breaks_24,
        include.lowest = T,
        labels = AQI$names))

  # define scales

  aqiNames <- AQI$names

  aqiActions <- c(
    'None.',
    'Unusually sensitive individuals should consider limiting prolonged or heavy exertion.',
    'People within Sensitive Groups should reduce prolonged or heavy outdoor exertion.',
    'People within Sensitive Groups should avoid all physical outdoor activity.',
    'Everyone should avoid prolonged or heavy exertion.',
    'Everyone should avoid any outdoor activity.'
  )

  aqiColors <- AQI$colors

  # plot data

  summaryPlot <-
    ggplot(dailyData,
      aes(x = datetime, y = pm25,
        fill = aqiCategory)) +
    # used to align axes
    geom_col(data = dailyData,
      width = 86400,
      alpha = 0) +
    geom_col(data = hourlyData,
      aes(color = aqiCategory),
      width = 3600 * .45,
      size = 0) +
    geom_col(data = dailyData,
      width = 86400,
      alpha = 0.3,
      color = "black",
      size = .2) +
    facet_wrap(~ siteName, ncol = 1) +
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
    labs(
      title = expression(paste("Daily and Hourly ", "PM"[2.5], " Levels")),
      x = "Date (midnight to midnight)",
      y = expression(paste("PM"[2.5] * " (", mu, "g/m"^3 * ")"))) +
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

  summaryPlot
}
