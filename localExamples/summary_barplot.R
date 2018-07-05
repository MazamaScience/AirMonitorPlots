############################
# Create a summary barplot #
############################

library(PWFSLSmokePlots)

# Get monitors

monitors <- c(
  #"060798002_01",
  #"060832004_01",
  #"060792004_01",
  "060792006_01",
  "060831008_01"
)

# load data

data_latest <- airnow_loadLatest()
data_2017 <- airnow_load()
data_2017_sub <- monitor_subset(
  data_2017,
  tlim = c(20171201, 20171207),
  timezone = "America/Los_Angeles"
)
data_2017_sub_utc <- monitor_subset(
  data_2017,
  tlim = c(20171201, 20171207),
  timezone = "UTC"
)

# create plots

p1 <- createTarnayPlot(monitors = monitors, data = data_2017, tlim = c(20171201, 20171207))
p2 <- createTarnayPlot(monitors = monitors, data = data_2017_sub)
p2 <- createTarnayPlot(monitors = monitors, data = data_2017_sub_utc)
p3 <- createTarnayPlot(monitors = monitors, data = data_latest)

