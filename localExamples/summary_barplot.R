############################
# Create a summary barplot #
############################

library(PWFSLSmoke)
library(PWFSLSmokePlots)
library(ggplot2)
library(ggthemes)

# load data

# yakimaMonitors <- c("530770016_01", "530770009_01",
#                     "530770015_01", "530770005_01")
yakimaMonitors <- c("530770009_01")
recentData <- airnow_loadLatest() %>%
  monitor_subset(monitorIDs = yakimaMonitors)

recentDaily <- recentData %>%
  monitor_dailyStatistic()

# make data tidy

hourlyData <- wsMonToTidy(recentData) %>%
  mutate(aqiCategory = cut(pm25,
                           AQI$breaks_24,
                           include.lowest = T,
                           labels = AQI$names))

dailyData <- wsMonToTidy(recentDaily) %>%
  mutate(aqiCategory = cut(pm25,
                           AQI$breaks_24,
                           include.lowest = T,
                           labels = AQI$names))

# define scales

colorScale <- AQI$colors
names(colorScale) <- AQI$names

# plot data

summaryPlot <-
  ggplot(data = dailyData,
         aes(x = datetime,
             y = pm25,
             fill = aqiCategory)) +
  geom_col(data = dailyData,
           width = 86400,
           alpha = .4,
           color = "#2b2b2b",
           size = .2) +
  geom_col(data = hourlyData,
           width = 3600 * .75) +
  facet_wrap(~ siteName,
             ncol = 2) +
  scale_fill_manual(values = colorScale,
                    breaks = rev(names(colorScale)), # low values on the bottom
                    drop = FALSE) +
  scale_x_datetime(date_breaks = "1 day",
                   #date_minor_breaks = "12 hours",
                   date_labels = '%b %d',
                   expand = c(0, 0)) +
  labs(title = expression(paste("Daily and Hourly ", "PM"[2.5], " Levels")),
       x = "Date",
       y = expression(paste("PM"[2.5] * " (", mu, "g/m"^3 * ")")),
       fill = "AQI Category") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "#E0E0E0"),
        panel.background = element_rect(color = "black"),
        panel.grid.major = element_line(linetype = 1, color = 'gray75'),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_line(linetype = 2, color = 'gray65'),
        axis.text.x = element_text(angle = 45, hjust = 1))

summaryPlot
