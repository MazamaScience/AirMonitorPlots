# convenience function to daily and hourly data

get_daily_hourly_data <- function(monitors, data) {

  monData <- data %>%
    monitor_subset(deviceDeploymentIDs = monitors)

  # daily
  dailyData <- monData %>%
    monitor_dailyStatistic() %>%
    monitor_toTidy() %>%
    mutate(
      aqiCategory = cut(
        .data$pm25,
        AQI$breaks_24,
        include.lowest = TRUE,
        labels = AQI$names)) %>%
    select(datetime, deviceDeploymentID, pm25, timezone, siteName, aqiCategory)


  # nowcast with short term
  hourlyData_ist <- monData %>%
    monitor_nowcast(includeShortTerm = TRUE) %>%
    monitor_toTidy() %>%
    mutate(
      aqiCategory = cut(
        .data$pm25,
        AQI$breaks_24,
        include.lowest = TRUE,
        labels = AQI$names),
      datetime_tz = lubridate::with_tz(datetime, "America/Los_Angeles")) %>%
    select(datetime, datetime_tz, deviceDeploymentID, pm25, timezone, siteName, aqiCategory)

  # nowcast no short term
  hourlyData_nst <- monData %>%
    monitor_nowcast(includeShortTerm = FALSE) %>%
    monitor_toTidy() %>%
    mutate(
      aqiCategory = cut(
        .data$pm25,
        AQI$breaks_24,
        include.lowest = TRUE,
        labels = AQI$names),
      datetime_tz = lubridate::with_tz(datetime, "America/Los_Angeles")) %>%
    select(datetime, datetime_tz, deviceDeploymentID, pm25, timezone, siteName, aqiCategory)

  # raw hourly
  hourlyData_raw <- monData %>%
    monitor_toTidy() %>%
    mutate(
      aqiCategory = cut(
        .data$pm25,
        AQI$breaks_24,
        include.lowest = TRUE,
        labels = AQI$names),
      datetime_tz = lubridate::with_tz(datetime, "America/Los_Angeles")) %>%
    select(datetime, datetime_tz, deviceDeploymentID, pm25, timezone, siteName, aqiCategory)


  data_list <- list(
    "hourly_raw" = hourlyData_raw,
    "hourly_nc_ist" = hourlyData_ist,
    "hourly_nc_nst" = hourlyData_nst,
    "daily" = dailyData
  )

  data_list
}
