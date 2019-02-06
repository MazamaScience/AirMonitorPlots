context("ggplot_pm25Diurnal")

test_that("Validates parameters", {
  
  ws_monitor <- PWFSLSmoke::Carmel_Valley
  
  expect_error(ggplot_pm25Diurnal("ws_tidy"), "ws_data must be either a ws_monitor object or ws_tidy object")
  expect_error(ggplot_pm25Diurnal(ws_monitor, startdate = 20200101), "startdate is outside of data date range")
  expect_error(ggplot_pm25Diurnal(ws_monitor, enddate = 11111111111111), "enddate is outside of data date range")
  expect_error(ggplot_pm25Diurnal(ws_monitor, shadedNight = "true"), "shadedNight must be logical")
  expect_error(ggplot_pm25Diurnal(ws_monitor, base_size = "eleven"), "base_size must be numeric")
  
}) 

test_that("works with ws_monitor and ws_tidy objects", {
  
  ws_monitor <- PWFSLSmoke::Carmel_Valley
  
  expect_is(ggplot_pm25Diurnal(ws_monitor), "ggplot")
  expect_is(ggplot_pm25Diurnal(monitor_toTidy(ws_monitor)), "ggplot")
  expect_error(ggplot_pm25Diurnal("ws_data"), "ws_data must be either a ws_monitor object or ws_tidy object")
  
})

test_that("Handles timezones correctly", {
  
  ws_monitor <- PWFSLSmoke::Carmel_Valley
  
  # Default timezone
  timezone <- ws_monitor$meta$timezone
  p <- ggplot_pm25Diurnal(ws_monitor)
  hour0 <- p$data[p$data$hour == 0,]$datetime[1]
  expect_equal(lubridate::hour(lubridate::with_tz(hour0, timezone)), 0)
  
  # Custom timezone
  timezone <- "America/New_York"
  p <- ggplot_pm25Diurnal(ws_monitor, timezone = timezone)
  hour0 <- p$data[p$data$hour == 0,]$datetime[1]
  expect_equal(lubridate::hour(lubridate::with_tz(hour0, timezone)), 0)
  
})