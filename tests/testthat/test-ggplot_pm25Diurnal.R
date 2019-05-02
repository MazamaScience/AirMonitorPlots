test_that("Validates parameters", {

  ws_tidy <- monitor_toTidy(PWFSLSmoke::Carmel_Valley)

  expect_error(ggplot_pm25Diurnal("ws_tidy"))
  expect_error(ggplot_pm25Diurnal(ws_tidy, startdate = 20200101))
  expect_error(ggplot_pm25Diurnal(ws_tidy, enddate = 11111111111111))
  expect_error(ggplot_pm25Diurnal(ws_tidy, shadedNight = "true"))
  expect_error(ggplot_pm25Diurnal(ws_tidy, base_size = "eleven"))

})

test_that("works with ws_monitor and ws_tidy objects", {

  ws_monitor <- PWFSLSmoke::Carmel_Valley
  ws_tidy <- monitor_toTidy(ws_monitor)

  expect_s3_class(ggplot_pm25Diurnal(ws_monitor), "ggplot")
  expect_s3_class(ggplot_pm25Diurnal(ws_tidy), "ggplot")

  expect_error(ggplot_pm25Diurnal("ws_data"))

})

test_that("Handles timezones correctly", {

  ws_tidy <- monitor_toTidy(PWFSLSmoke::Carmel_Valley)

  # Default timezone
  timezone <- ws_tidy$timezone[1]
  p <- ggplot_pm25Diurnal(ws_tidy)
  hour0 <- p$data[p$data$hour == 0, ]$datetime[1]
  expect_equal(lubridate::hour(lubridate::with_tz(hour0, timezone)), 0)

  # Custom timezone
  timezone <- "America/New_York"
  p <- ggplot_pm25Diurnal(ws_tidy, timezone = timezone)
  hour0 <- p$data[p$data$hour == 0, ]$datetime[1]
  expect_equal(lubridate::hour(lubridate::with_tz(hour0, timezone)), 0)

})
