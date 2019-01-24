dataList <- createDataList(infoList, DATA_DIR)
# Get language dependent plot labels
textListScript = paste('R/createTextList_',infoList$language, '.R', sep="")
source(textListScript)
textList <- createTextList(dataList, infoList)
# Create plot
png(pngPath, width=infoList$plotwidth, height=infoList$plotheight, units="px")
if ( infoList$plottype == "dailybarplot" ) {
  dailyBarplot(dataList, infoList, textList)
} else if ( infoList$plottype == "dailybyhour" ) {
  dailyByHour(dataList, infoList, textList)
} else if ( infoList$plottype == "timeseries" ) {
  timeseries(dataList, infoList, textList)
} else if ( infoList$plottype == "locationmap" ) {
  locationMap(dataList, infoList, textList)
} else if ( infoList$plottype == "esrilocationmap" ) {
  esriLocationMap(dataList, infoList, textList)
} else {
  stop("invalid plotType", call. = FALSE)
}
dev.off()

timeseries <- function(dataList, infoList, textList) {
  ws_monitor <- dataList$ws_monitor
  monitorID <- infoList$monitorid
  lookbackDays <- infoList$lookbackdays
  size <- infoList$size
  
  enddate <- lubridate::now()
  startdate <- lubridate::now() - lubridate::ddays(lookbackDays)
  
  style <- ifelse(size <= 500, "small", "large")
  
  monitor_ggTimeseries(ws_monitor,
                       startdate = startdate,
                       enddate = enddate,
                       style = style,
                       monitorIDs = monitorID)
  
}

dailyBarplot <- function(dataList, infoList, textList) {
  ws_monitor <- dataList$ws_monitor
  monitorID <- infoList$monitorid
  lookbackDays <- infoList$lookbackdays
  size <- infoList$size
  
  enddate <- lubridate::now()
  startdate <- lubridate::now() - lubridate::ddays(lookbackDays)
  
  style <- ifelse(size <= 500, "small", "large")
  
  monitor_ggDailyBarplot(ws_monitor,
                         startdate = startdate, 
                         enddate = enddate,
                         style = style,
                         monitorIDs = monitorID)
}