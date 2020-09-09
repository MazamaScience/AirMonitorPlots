# Function in monitoring-plot-service

# dailyByHour <- function(
#   dataList = NULL,
#   infoList = NULL,
#   textList = NULL
# ) {
#
#   logger.debug("----- dailyByHour() -----")
#
#
#   # ----- Validate parameters --------------------------------------------------
#
#   MazamaCoreUtils::stopIfNull(dataList, "Required parameter 'dataList' is missing.")
#   MazamaCoreUtils::stopIfNull(infoList, "Required parameter 'infoList' is missing.")
#   MazamaCoreUtils::stopIfNull(textList, "Required parameter 'textList' is missing.")
#
#
#   # ----- Draw the plot --------------------------------------------------------
#
#   plot <- AirMonitorPlots::monitor_ggDailyByHour(
#     ws_monitor = dataList$ws_monitor,
#     startdate  = infoList$startdate,
#     enddate    = infoList$enddate,
#     style      = ifelse(infoList$size <= 500, "small", "large")
#   )
#
#   return(plot)
#
# }

library(PWFSLSmoke)

ws_monitor <-
  monitor_loadLatest() %>%
  monitor_subset(monitorIDs = "060431001_01")

# ----- With NULL input, this shows data that is one day behind

AirMonitorPlots::monitor_ggDailyByHour(
  ws_monitor = ws_monitor,
  startdate = NULL,
  enddate = NULL,
  style = "large"
)

# ----- With enddate just before midnight UTC, data is one day behind

enddate <- MazamaCoreUtils::parseDatetime("2020-09-08 22:00:00", timezone = "UTC")
startdate <- enddate - lubridate::ddays(10)

strftime(enddate, "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles")

AirMonitorPlots::monitor_ggDailyByHour(
  ws_monitor = ws_monitor,
  startdate = startdate,
  enddate = enddate,
  style = "large"
)


# ----- With enddate just after midnight UTC, data is still one day behind

enddate <- MazamaCoreUtils::parseDatetime("2020-09-09 02:00:00", timezone = "UTC")
startdate <- enddate - lubridate::ddays(10)

strftime(enddate, "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles")

AirMonitorPlots::monitor_ggDailyByHour(
  ws_monitor = ws_monitor,
  startdate = startdate,
  enddate = enddate,
  style = "large"
)


