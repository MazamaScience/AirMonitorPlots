library(leaflet)

airnow <- airnow_load(2018, 8)

monitors <- monitor_subset(airnow, stateCodes = 'WA', tlim=c(20180815,20180901))

monitorIDs <- monitors$meta$monitorID

goodMonitorsMask <- rep(TRUE, length(monitorIDs))

i <- 1
for (monitorID in monitorIDs) {
  
  result <- try({
    filename <- paste0(monitorID, "_clockIcon.png")
    png(filename, 48, 48, bg="transparent")
    print( clockPlotBase(monitors, monitorID, "2018-08-25",
                         gapFraction = 1/16, style = "icon") )
    dev.off()
  }, silent = TRUE)
  
  if ( "try-error" %in% class(result) ) {
    goodMonitorsMask[i] <- FALSE
  }
  
  i <- i + 1
}

goodMonitors <- monitor_subset(monitors, monitorIDs=monitorIDs[goodMonitorsMask])

clockIcon <- icons(
  iconUrl = file.path(getwd(), paste0(goodMonitors$meta$monitorID, "_clockIcon.png")),
  iconWidth = 48, iconHeight = 48,
  iconAnchorX = 0, iconAnchorY = 0
)

m <- leaflet(goodMonitors$meta) %>%
  addTiles() %>%
  addMarkers(lng = ~longitude,
             lat = ~latitude,
             icon = clockIcon)


