library(leaflet)
library(AirMonitorPlots)

airnow <- airnow_loadAnnual(2018)

monitors <- monitor_subset(airnow, stateCodes = 'WA', tlim=c(20180815,20180901))

deviceDeploymentIDs <- monitors$meta$deviceDeploymentID

goodMonitorsMask <- rep(TRUE, length(deviceDeploymentIDs))

i <- 1
for (deviceDeploymentID in deviceDeploymentIDs) {

  filename <- paste0(deviceDeploymentID, "_clockIcon.png")
  png(filename, 64, 64, bg="transparent")
  result <- try({
    print( clockPlot(monitors,
                     deviceDeploymentID,
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

goodMonitors <- monitor_subset(monitors, deviceDeploymentIDs=deviceDeploymentIDs[goodMonitorsMask])

clockIcon <- icons(
  iconUrl = file.path(getwd(), paste0(goodMonitors$meta$deviceDeploymentID, "_clockIcon.png")),
  iconWidth = 64, iconHeight = 64,
  iconAnchorX = 0, iconAnchorY = 0
)

m <- leaflet(goodMonitors$meta) %>%
  addTiles() %>%
  addMarkers(lng = ~longitude,
             lat = ~latitude,
             icon = clockIcon)


