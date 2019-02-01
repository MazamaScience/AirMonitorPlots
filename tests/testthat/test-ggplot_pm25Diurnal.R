context("ggplot_pm25Diurnal")

test_that("Validates parameters", {
  
  expect_error(ggplot_pm25Diurnal("ws_tidy"), "ws_data must be either a ws_monitor object or ws_tidy object")
  expect_error(ggplot_pm25Diurnal(Carmel_Valley, startdate = 20200101), "startdate is outside of data date range")
  expect_error(ggplot_pm25Diurnal(Carmel_Valley, enddate = 11111111111111), "enddate is outside of data date range")
  expect_error(ggplot_pm25Diurnal(Carmel_Valley, shadedNight = "true"), "shadedNight must be logical")
  expect_error(ggplot_pm25Diurnal(Carmel_Valley, base_size = "eleven"), "base_size must be numeric")
  
}) 

test_that("works with ws_monitor and ws_tidy objects", {
  
  expect_is(ggplot_pm25Diurnal(PWFSLSmoke::Carmel_Valley), "ggplot")
  expect_is(ggplot_pm25Diurnal(monitor_toTidy(Carmel_Valley)), "ggplot")
  expect_error(ggplot_pm25Diurnal("ws_data"), "ws_data must be either a ws_monitor object or ws_tidy object")
  
})

test_that("Handles timezones correctly", {
  
  # Default timezone
  timezone <- Carmel_Valley$meta$timezone
  p <- ggplot_pm25Diurnal(Carmel_Valley)
  hour0 <- p$data[p$data$hour == 0,]$datetime[1]
  expect_equal(lubridate::hour(lubridate::with_tz(hour0, timezone)), 0)
  
  # Custom timezone
  timezone <- "America/New_York"
  p <- ggplot_pm25Diurnal(Carmel_Valley, timezone = timezone)
  hour0 <- p$data[p$data$hour == 0,]$datetime[1]
  expect_equal(lubridate::hour(lubridate::with_tz(hour0, timezone)), 0)
  
})