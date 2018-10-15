library(leaflet)

airnow <- airnow_load(2018, 8)
# monitorIDs <- c('530470009_01',
#                 '530470010_01',
#                 '530090013_01',
#                 '530530029_01',
#                 '530770009_01',
#                 '530570015_01',
#                 '530070007_01',
#                 '410290203_01',
#                 '060431001_01',
#                 '060530002_01')

monitors <- monitor_subset(airnow, stateCodes = 'WA', tlim=c(20180815,20180901))

monitorIDs <- monitors$meta$monitorID[1:5]

for (monitorID in monitorIDs) {
  
  filename <- paste0(monitorID, "_clockIcon.png")
  png(filename, 48, 48, bg="transparent")
  print( clockPlotBase(monitors, monitorID, "2018-08-20", style="icon") )
  dev.off()

  # clock <- clockPlotBase(monitors, monitorID, "2018-08-20", style="icon")
  # ggplot2::ggsave(filename, clock, bg = "transparent", dpi = 100, width = 6.5, height = 6.5)
  
  
}

clockIcon <- icons(
  iconUrl = file.path(getwd(), paste0(monitors$meta$monitorID, "_clockIcon.png")),
  iconWidth = 48, iconHeight = 48,
  iconAnchorX = 0, iconAnchorY = 0
)

m <- leaflet(monitors$meta) %>%
  addTiles() %>%
  addMarkers(lng = ~longitude,
             lat = ~latitude,
             icon = clockIcon)


