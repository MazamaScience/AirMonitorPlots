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

# ----- With enddate just before the most recent midnight UTC

enddate <-
  lubridate::now(tzone = "America/Los_Angeles") %>%
  lubridate::floor_date(unit = "day") - lubridate::dhours(1)
startdate <- enddate - lubridate::ddays(10)

strftime(enddate, "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles")

AirMonitorPlots::monitor_ggDailyByHour(
  ws_monitor = ws_monitor,
  startdate = startdate,
  enddate = enddate,
  style = "large"
)


# ----- With enddate just after the most recent midnight UTC

enddate <-
  lubridate::now(tzone = "America/Los_Angeles") %>%
  lubridate::floor_date(unit = "day") + lubridate::dhours(1)
startdate <- enddate - lubridate::ddays(10)

strftime(enddate, "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles")

AirMonitorPlots::monitor_ggDailyByHour(
  ws_monitor = ws_monitor,
  startdate = startdate,
  enddate = enddate,
  style = "large"
)


