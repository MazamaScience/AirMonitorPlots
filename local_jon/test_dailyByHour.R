library(AirSensor2)
library(AirMonitorPlots)

load("/Users/jonathancallahan/Projects/MazamaScience/ASIC-2026/R_workshop/Chicago.rda")


monitor_ggDailyHourlyBarplot(
    monitor = Chicago,
    startdate = NULL,
    enddate = NULL,
    id = NULL,
    columns = 1,
    title = NULL,
    timezone = NULL,
    xLabel = NULL,
    yLabel = NULL,
    hourlyDataType = "raw",
    palette = "epa_aqi",
    includeLegend = FALSE
)


# ===== DEBUGGING ==============================================================

monitor <- Chicago
startdate <- NULL
enddate <- NULL
id <- NULL
columns <- 1
title <- NULL
timezone <- NULL
xLabel <- NULL
yLabel <- NULL
hourlyDataType <- c("nowcast", "raw", "none")
palette <- "epa_aqi"
includeLegend <- TRUE

