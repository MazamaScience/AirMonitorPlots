# Reproduce plots for Weaverville, CA just outside Redding:
# https://tools.airfire.org/monitor-plot/v4/plot?databaseversion=4.0&webserviceapi=4.0&plottype=timeseries&monitorid=061050002_01
# https://tools.airfire.org/monitor-plot/v4/plot?databaseversion=4.0&webserviceapi=4.0&plottype=dailybarplot&monitorid=061050002_01

library(AirMonitor)
library(AirMonitorPlots) # version >= 0.8.0

# Load data
airnow <- airnow_loadLatest()
airnow %>% monitor_filter(stateCode == "CA") %>% monitor_leaflet()

airsis <- airsis_loadLatest()
airsis %>% monitor_filter(stateCode == "CA") %>% monitor_leaflet()

# wrcc <- wrcc_loadLatest()
# wrcc %>% monitor_filter(stateCode == "CA") %>% monitor_leaflet()

monitor <- airnow
id <- "3cf38c450135e594_840061050002"

# ----- Timeseries -------------------------------------------------------------

startdate <- lubridate::floor_date(lubridate::now() - lubridate::ddays(6), "day")
enddate <- lubridate::now()

# * large -----
png("timeseries.png", width = 700, height = 700, units = "px")

monitor_ggTimeseries(
  monitor,
  startdate = startdate,
  enddate = enddate,
  id = id,
  style = "large"
)

dev.off()

# * small -----
png("smalltimeseries.png", width = 450, height = 450, units = "px")

monitor_ggTimeseries(
  monitor,
  startdate = startdate,
  enddate = enddate,
  id = id,
  style = "small"
)

dev.off()

# NOTE:  On 2022-08-12, both plots look good.

# ----- Barplot ----------------------------------------------------------------

# * large -----
png("barplot.png", width = 700, height = 700, units = "px")

monitor_ggDailyBarplot(
  monitor,
  startdate = startdate,
  enddate = enddate,
  id = id,
  style = "large"
)

dev.off()

# * large w/ brand -----
png("barplot_brand.png", width = 700, height = 700, units = "px")

monitor_ggDailyBarplot(
  monitor,
  startdate = startdate,
  enddate = enddate,
  id = id,
  style = "large"
) %>%
  brandPlot(size = 0.2)

dev.off()

# * small w/ brand -----
png("smallbarplot.png", width = 450, height = 450, units = "px")

monitor_ggDailyBarplot(
  monitor,
  startdate = startdate,
  enddate = enddate,
  id = id,
  style = "small"
) %>%
  brandPlot(brandName = "USFS", location = "topleft", size = 0.1) ###%>%
###brandPlot(brandName = "AirFire", location = "topright", size = 0.1)

dev.off()

# -----DailyByHour -------------------------------------------------------------

# * large -----
png("dailybyhour.png", width = 700, height = 700, units = "px")

monitor_ggDailyByHour(
  monitor,
  startdate = startdate,
  enddate = enddate,
  id = id,
  style  = "large"
)

dev.off()

# * small -----
png("smalldailybyhour.png", width = 450, height = 450, units = "px")

monitor_ggDailyByHour(
  monitor,
  startdate = startdate,
  enddate = enddate,
  id = id,
  style = "small"
)

dev.off()


