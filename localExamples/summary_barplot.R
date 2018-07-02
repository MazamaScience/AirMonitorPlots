############################
# Create a summary barplot #
############################

library(PWFSLSmokePlots)

# load data

data_latest <- airnow_loadLatest()
data_2017 <- airnow_load()

data <- data_latest
#data <- monitor_subset(data_2017, tlim = c(20170101, 20171231))

monitors <- c(
  "060831008_01",
  "060792006_01",
  "060832004_01",
  "060798002_01",
  "060792004_01"
  )

# create plot

p <- createTarnayPlot(monitors = monitors, data = data)
p
