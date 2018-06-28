############################
# Create a summary barplot #
############################

library(PWFSLSmokePlots)

# load data

data <- airnow_loadLatest()

monitors <- c(
  "060831008_01",
  "060792006_01",
  "060832004_01",
  "060798002_01",
  "060792004_01"
  )

# create plot

p <- createTarnayPlot(monitors = monitors, data = data, includeThirdCol = TRUE)
p

# save some plots

ggsave("~/Downloads/thirdcol_sq8_dpi100_sc100.png", plot = p,
       dpi = 100,
       width = 8, height = 8,
       scale = 1)
