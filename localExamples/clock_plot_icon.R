library(PWFSLSmokePlots)

makeClock <- function(monitors, monitorID, date) {
  endDate <- date + 24 * 60 * 60 - 1
  
  monitor <- monitor_subset(monitors, monitorIDs = c(monitorID), 
                            tlim = c(date, endDate))
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
  
  clock <- ggplot(data) +
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
  
  ggsave(paste0(monitorID, "_clock.png"), clock, bg = "transparent", dpi = 100, width = 6.5, height = 6.5)
}
