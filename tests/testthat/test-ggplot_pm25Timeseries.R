test_that("parameters are validated", {

  mts_tidy <- monitor_toTidy(AirMonitor::Carmel_Valley)

  expect_error(ggplot_pm25Timeseries("mts_tidy"))
  expect_error(ggplot_pm25Timeseries(mts_tidy, startdate = 20200101))
  expect_error(ggplot_pm25Timeseries(mts_tidy, enddate = 11111111111111))
  expect_error(ggplot_pm25Timeseries(mts_tidy, base_size = "eleven"))

})

test_that("works with mts_monitor and mts_tidy objects", {

  mts_monitor <- AirMonitor::Carmel_Valley
  mts_tidy <- monitor_toTidy(mts_monitor)

  expect_s3_class(ggplot_pm25Timeseries(mts_monitor), "ggplot")
  expect_s3_class(ggplot_pm25Timeseries(mts_tidy), "ggplot")

})
