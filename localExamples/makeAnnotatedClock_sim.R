library(PWFSLSmokePlots)

makeAnnotatedClock_sim <- function(monitors, monitorID, date) {
  endDate <- date + 24 * 60 * 60 - 1
  
  monitor <- monitor_subset(monitors, monitorIDs = c(monitorID), 
                            tlim = c(date, endDate))
  data <- monitor$data
  names(data) <- c("datetime", "pm25")
  dailyMean <- round(mean(data$pm25, na.rm = TRUE), digits = 0)
  
  ti <- timeInfo(date, 
                 monitor$meta$longitude,
                 monitor$meta$latitude,
                 monitor$meta$timezone)
  
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
  gap <- 0.5
  thetaOffset <- pi + (2 * pi) * (1 - (1 / (1 + gap))) / 2
  
  
  # 1 - (1 / (gap + 1)) = 0.33333
  
  # 0.5 - 
  
  # gap=0, angle = pi/2
  
  # gap=0.5, angle = 
  
  
  # 2pi /  what is the equivilent of pi/2 when the range is shrunk to 2/3 its normal size
  
  
  # gap=1, angle = 0

  x <- c(0)
  y <- c(0)
  h <- c(pal(dailyMean))
  face = data.frame(x, y)
  
  clock <- ggplot(data) +
    # annotate("segment", x = 0, y = 0.8333, xend = 5, yend = 0.8333, color = "gray50", size = 1.5) +
    annotate("segment", x = 0, y = 0.5, xend = 5, yend = 0.5, color = "gray50", size = 1.5) +

    geom_rect(aes(fill = hue, ymin = ymin, ymax = ymax, xmin = 4, xmax = 4.5)) +
    geom_point(data = face, size = 110, color = pal(dailyMean), aes(x = x, y = y)) +
    annotate("text", x = 0, y = .5, label = dailyMean, color = "black", size = 16) +
    annotate("text", x = 5, y = .5, label = paste0(date, ", ", monitor$meta$siteName), color = "black", size = 5) +
    coord_polar(theta = 'y', direction = 1, start = thetaOffset) +
    xlim(0, 6) +
    ylim(0, 1 + gap) +
    theme(panel.grid = element_blank(), axis.title = element_blank(), axis.text = element_blank(), 
          axis.ticks = element_blank(), legend.position = "none") +
    theme(panel.background = element_rect(fill = "white", color = NA)) +
    theme(plot.background = element_rect(fill = "white", color = NA)) +
    scale_fill_identity(guide = "legend", breaks = data$hue)
  clock
}

if (FALSE) {
  airnow <- airnow_load()
  day <- as.POSIXct("2017-08-30")
  id <- "410290203_01"
  p <- makeAnnotatedClock_sim(airnow, id, day)
  p
}
