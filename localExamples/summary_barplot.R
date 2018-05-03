############################
# Create a summary barplot #
############################

library(PWFSLSmokePlots)

# load data

data <- airnow_loadLatest()
santaMariaMonitors <- c(
  "060831008_01",
  "060792006_01",
  "060832004_01",
  "060798002_01",
  "060792004_01"
  )

# create plot

tplot <- createTarnayPlot(monitors = santaMariaMonitors, data = data)
tplot
