############################
# Create a summary barplot #
############################

library(PWFSLSmoke)
library(PWFSLSmokePlots)
library(ggplot2)

# load data

yakimaMonitors <- c("530770016_01", "530770009_01", "530770015_01", "530770005_01")
recentData <- airnow_loadLatest() %>%
  monitor_subset(monitorIDs = yakimaMonitors)

recentDaily <- recentData %>%
  monitor_dailyStatistic()

# make data tidy

hourlyData <- wsMonToTidy(recentData)
dailyData <- wsMonToTidy(recentDaily)

# define scales



# plot data

summary_plot <-
  ggplot(data = dailyData,
         aes(x = datetime,
             y = pm25,
             fill = pm25)) +
  geom_bar(stat = 'identity') +
  geom_bar(data = hourlyData,
           stat = 'identity',
           alpha = .5) +
  facet_wrap(~ monitorID,
             ncol = 2)

summary_plot
