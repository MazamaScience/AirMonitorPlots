library(leaflet)
library(PWFSLSmokePlots)

airnow <- airnow_load(2018, 8)

monitors <- monitor_subset(airnow, stateCodes = 'WA', tlim=c(20180815,20180901))

monitorIDs <- monitors$meta$monitorID

goodMonitorsMask <- rep(TRUE, length(monitorIDs))

i <- 1
for (monitorID in monitorIDs) {
  
  filename <- paste0(monitorID, "_clockIcon.png")
  png(filename, 48, 48, bg="transparent")
  result <- try({
    print( clockPlot(monitors, 
                     monitorID, 
                     startdate = "2018-08-25",
                     enddate = "2018-08-27",
                     style = "icon") )
  }, silent = TRUE)
  dev.off()
  
  if ( "try-error" %in% class(result) ) {
    goodMonitorsMask[i] <- FALSE
  }
  
  i <- i + 1
}

goodMonitors <- monitor_subset(monitors, monitorIDs=monitorIDs[goodMonitorsMask])

clockIcon <- icons(
  iconUrl = file.path(getwd(), paste0(goodMonitors$meta$monitorID, "_clockIcon.png")),
  iconWidth = 64, iconHeight = 64,
  iconAnchorX = 0, iconAnchorY = 0
)

m <- leaflet(goodMonitors$meta) %>%
  addTiles() %>%
  addMarkers(lng = ~longitude,
             lat = ~latitude,
             icon = clockIcon)


