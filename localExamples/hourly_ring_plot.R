library(PWFSLSmokePlots)

startDate <- as.POSIXct("2017-08-20")
format(startDate, tz = "GMT", usetz = TRUE)
endDate <- startDate + 24 * 60 * 60 - 1

#monitors <- airnow_load()
monitor <- monitor_subset(monitors, monitorIDs = c('410290203_01'), 
                          tlim = c(startDate, endDate))
data <- monitor$data
names(data) <- c("datetime", "pm25")

avg <- round(mean(data$pm25), digits = 0)

# Time average every four hours to break the day up into 6 periods
periodAvgs <- colMeans(matrix(data$pm25, nrow = 1), na.rm = TRUE)
data <- data.frame(periodAvgs)
names(data) <- c("pm25")

# Define AQI palette
pal <- leaflet::colorBin(palette = AQI$colors, bins = AQI$breaks_24, na.color = "#bbbbbb")

# Define the start, end, and color of each period
data$fraction = (1 / 24)
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n = -1))
data$hue = pal(data$pm25)

# For bottom gap between the start and end of the day
gap <- 0.1
thetaOffset <- pi + (2 * pi) * (1 - (1 / (1 + gap))) / 2# * (2 - (1 / (1 + gap)))

x <- c(0)
y <- c(0)
h <- c(pal(avg))
face = data.frame(x, y)

clock <- ggplot(data) +
  annotate("segment", x = 0, y = 0.25 * 1 / (1 + gap), xend = 4.5, yend = 0.25 * 1 / (1 + gap), color = "slategray", size = 1.5) +
  annotate("segment", x = 0, y = 1 - 0.25 * 1 / (1 + gap), xend = 4.5, yend = 1 - 0.25 * 1 / (1 + gap), color = "slategray", size = 1.5) +
  geom_rect(aes(fill = hue, ymin = ymin, ymax = ymax, xmin = 2.5, xmax = 4)) +
  geom_point(data = face, size = 65, color = pal(avg), aes(x = x, y = y)) +
  annotate("text", x = 0, y = .5, label = avg, color = "black", size = 26) +
  coord_polar(theta = 'y', direction = 1, start = thetaOffset) +
  xlim(0, 5) +
  ylim(0, 1 + gap) +
  theme(panel.grid = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  scale_fill_identity(guide = "legend", breaks = data$hue) +
  theme(legend.position = "none")
clock