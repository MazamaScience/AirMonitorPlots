test_that("Validates parameters", {

  mts_tidy <- monitor_toTidy(AirMonitor::Carmel_Valley)

  expect_error(ggplot_pm25Diurnal("mts_tidy"))
  expect_error(ggplot_pm25Diurnal(mts_tidy, startdate = 20200101))
  expect_error(ggplot_pm25Diurnal(mts_tidy, enddate = 11111111111111))
  expect_error(ggplot_pm25Diurnal(mts_tidy, shadedNight = "true"))
  expect_error(ggplot_pm25Diurnal(mts_tidy, base_size = "eleven"))

})

test_that("works with mts_monitor and mts_tidy objects", {

  mts_monitor <- AirMonitor::Carmel_Valley
  mts_tidy <- monitor_toTidy(mts_monitor)

  expect_s3_class(ggplot_pm25Diurnal(mts_monitor), "ggplot")
  expect_s3_class(ggplot_pm25Diurnal(mts_tidy), "ggplot")

  expect_error(ggplot_pm25Diurnal("mts_data"))

})

test_that("Handles timezones correctly", {

  mts_tidy <- monitor_toTidy(AirMonitor::Carmel_Valley)

  # Default timezone
  timezone <- mts_tidy$timezone[1]
  p <- ggplot_pm25Diurnal(mts_tidy)
  hour0 <- p$data[p$data$hour == 0, ]$datetime[1]
  expect_equal(lubridate::hour(lubridate::with_tz(hour0, timezone)), 0)

  # Custom timezone
  timezone <- "America/New_York"
  p <- ggplot_pm25Diurnal(mts_tidy, timezone = timezone)
  hour0 <- p$data[p$data$hour == 0, ]$datetime[1]
  expect_equal(lubridate::hour(lubridate::with_tz(hour0, timezone)), 0)

})
