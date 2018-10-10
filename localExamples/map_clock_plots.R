library(leaflet)

date <- as.POSIXct("2017-08-30")
format(date, tz = "GMT", usetz = TRUE)

airnow <- PWFSLSmoke::airnow_load()
monitorIDs <- c('530470009_01',
                '530470010_01',
                '530090013_01',
                '530530029_01',
                '530770009_01',
                '530570015_01',
                '530070007_01',
                '410290203_01',
                '060431001_01',
                '060530002_01')

monitors <- monitor_subset(airnow, monitorIDs = monitorIDs)
for (id in monitorIDs) {
  makeClock(monitors, id, date)
}

clockIcon <- icons(
  iconUrl = paste0("/Users/tate/", monitors$meta$monitorID, "_clock.png"),
  iconWidth = 32, iconHeight = 32,
  iconAnchorX = 0, iconAnchorY = 0
)

m <- leaflet(monitors$meta) %>%
  addTiles() %>%
  addMarkers(lng = ~longitude,
             lat = ~latitude,
             icon = clockIcon)
m