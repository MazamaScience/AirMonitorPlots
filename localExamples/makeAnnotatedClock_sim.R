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
  gap <- 0.2
  thetaOffset <- pi + (2 * pi) * (1 - (1 / (1 + gap))) / 2

  x <- c(0)
  y <- c(0)
  h <- c(pal(dailyMean))
  face = data.frame(x, y)
  
  print(1 / (1 + gap))
  
  clock <- ggplot(data) +
    #annotate("segment", x = 0, y = 0.25 * (1 / (1 + gap)), xend = 5, yend = 0.25 * (1 / (1 + gap)), color = "gray50", size = 1.5) +
    #annotate("segment", x = 0, y = (1 + gap) * 0.75 * (1 / (1 + gap)), xend = 5, yend = (1 + gap) * 0.75 * (1 / (1 + gap)), color = "gray50", size = 1.5) +
   # annotate("segment", x = 0, y = 1, xend = 5, yend = 1, color = "gray50", size = 1.5) +
    #annotate("segment", x = 0, y = 0.5, xend = 5, yend = 0.5, color = "gray50", size = 1.5) +
    #annotate("segment", x = 0, y = 1, xend = 5, yend = 1, color = "gray50", size = 1.5) +
    
    annotate("segment", x = 0, y = 0.8333, xend = 5, yend = 0.8333, color = "gray50", size = 1.5) +
    annotate("segment", x = 0, y = 0.25 * 0.8333, xend = 5, yend = 0.25 * 0.8333, color = "gray50", size = 1.5) +
    
    #annotate("segment", x = 0, y = (0.25) * (1 / (1 + gap)), xend = 5, yend = (0.25) * (1 / (1 + gap)), color = "gray50", size = 1.5) +
    #annotate("segment", x = 0, y = 0.125, xend = 5, yend = 0.125, color = "gray50", size = 1.5) +
    
    
    geom_rect(aes(fill = hue, ymin = ymin, ymax = ymax, xmin = 4, xmax = 4.5)) +
    geom_point(data = face, size = 110, color = pal(dailyMean), aes(x = x, y = y)) +
    annotate("text", x = 0, y = .5, label = dailyMean, color = "black", size = 16) +
    annotate("text", x = 5, y = .5, label = paste0(date, ", ", monitor$meta$siteName), color = "black", size = 5) +
    coord_polar(theta = 'y', direction = 1, start = thetaOffset) +
    xlim(0, 6) +
    ylim(0, 1 + gap) +
    theme(panel.grid = element_blank(), axis.title = element_blank(), axis.text = element_blank(), 
          axis.ticks = element_blank(), legend.position = "none") +
    theme(panel.background = element_rect(fill = "white", colour = NA)) +
    theme(plot.background = element_rect(fill = "white", colour = NA)) +
    scale_fill_identity(guide = "legend", breaks = data$hue)
  
  clock
}
