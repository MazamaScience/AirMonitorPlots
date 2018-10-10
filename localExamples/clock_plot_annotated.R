library(PWFSLSmokePlots)

makeAnnotatedClock <- function(monitors, monitorID, date) {
  endDate <- date + 24 * 60 * 60 - 1
  
  monitor <- monitor_subset(monitors, monitorIDs = c(monitorID), 
                            tlim = c(date, endDate))
  data <- monitor$data
  names(data) <- c("datetime", "pm25")
  dailyMean <- round(mean(data$pm25), digits = 0)
  
  # Define AQI palette
  pal <- leaflet::colorBin(palette = AQI$colors, 
                           bins = AQI$breaks_24, 
                           na.color = "#bbbbbb")
  
  # Define the start, end, and color of each hour
  data$fraction = (1 / 24)
  data$ymax = cumsum(data$fraction)
  data$ymin = c(0, head(data$ymax, n = -1))
  data$hue = pal(data$pm25)
  
  # For bottom gap between the start and end of the day
  gap <- 0.1
  thetaOffset <- pi + (2 * pi) * (1 - (1 / (1 + gap))) / 2
  
  
  sunriseTime <- as.POSIXct("2017-08-30 6:25:00")
  sunsetTime <- as.POSIXct("2017-08-30 19:52:00")
  
  sunriseTOD <- difftime(sunriseTime, date, units = "hours")
  sunsetTOD <- difftime(sunsetTime, date, units = "hours")
  
  sunrisePercent <- sunriseTOD / 24
  sunsetPercent <- sunsetTOD / 24
  
  sunriseDisplay <- paste0("Sunrise\n", lubridate::hour(sunriseTime), ":", lubridate::minute(sunriseTime))
  sunsetDisplay <- paste0("Sunset\n", lubridate::hour(sunsetTime), ":", lubridate::minute(sunsetTime))
  
  x <- c(0)
  y <- c(0)
  h <- c(pal(dailyMean))
  face = data.frame(x, y)
  
  clock <- ggplot(data) +
    geom_point(data = face, size = 53, color = "black", aes(x = x, y = y)) +
    geom_rect(aes(fill = hue, ymin = ymin, ymax = ymax, xmin = 2.5, xmax = 4)) +
    annotate("segment", x = 0, y = sunrisePercent, xend = 4.5, yend = sunrisePercent, color = "black", size = 1.6) +
    annotate("text", x = 5.5, y = sunrisePercent, label = sunriseDisplay, color = "slategray", size = 4) +
    annotate("segment", x = 0, y = sunsetPercent, xend = 4.5, yend = sunsetPercent, color = "black", size = 1.6) +
    annotate("text", x = 5.5, y = sunsetPercent, label = sunsetDisplay, color = "slategray", size = 4) +
    geom_point(data = face, size = 50, color = pal(dailyMean), aes(x = x, y = y)) +
    annotate("text", x = 0, y = .5, label = dailyMean, color = "black", size = 16) +
    annotate("text", x = 5, y = .5, label = paste0(date, ", ", monitor$meta$siteName), color = "black", size = 5) +
    coord_polar(theta = 'y', direction = 1, start = thetaOffset) +
    xlim(0, 6) +
    ylim(0, 1 + gap) +
    theme(panel.grid = element_blank(), axis.title = element_blank(), axis.text = element_blank(), 
          axis.ticks = element_blank(), legend.position = "none") +
    scale_fill_identity(guide = "legend", breaks = data$hue)
  
  ggsave(paste0(monitorID, "_clock.png"), clock, bg = "transparent", dpi = 100, width = 6.5, height = 6.5)
  
  clock
}
