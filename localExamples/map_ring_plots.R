library(leaflet)

date <- as.POSIXct("2017-08-30")
format(startDate, tz = "GMT", usetz = TRUE)

#airnow <- airnow_load()
monitorIDs <- c('530470009_01',
                '530470010_01',
                '530090013_01')

monitors <- monitor_subset(airnow, monitorIDs = monitorIDs)
for (id in monitorIDs) {
  makeClock(monitors, id, date)
}

clockIcon <- icons(
  iconUrl = paste0("/Users/tate/", monitors$meta$monitorID, "_clock.png"),
  iconWidth = 48, iconHeight = 48,
  iconAnchorX = 0, iconAnchorY = 0
)

m <- leaflet(monitors$meta) %>%
  addTiles() %>%
  addMarkers(lng = ~longitude,
             lat = ~latitude,
             icon = clockIcon)
m