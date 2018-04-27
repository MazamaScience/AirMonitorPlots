############################
# Create a summary barplot #
############################

library(PWFSLSmoke)
library(PWFSLSmokePlots)
library(ggplot2)
library(ggthemes)

# load data

data <- airnow_loadLatest()
santaMariaMonitors <- c("060831008_01",
                        "060792006_01",
                        "060832004_01",
                        "060798002_01",
                        "060792004_01")

tplot <- createTarnayPlot(monitors = santaMariaMonitors, data = data)
tplot
