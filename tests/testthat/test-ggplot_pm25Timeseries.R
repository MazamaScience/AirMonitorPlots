context("ggplot_pm25Timeseries")

test_that("Validates parameters", {
  
  expect_error(ggplot_pm25Timeseries("ws_tidy"), "ws_data must be either a ws_monitor object or ws_tidy object")
  expect_error(ggplot_pm25Timeseries(Carmel_Valley, startdate = 20200101), "startdate is outside of data date range")
  expect_error(ggplot_pm25Timeseries(Carmel_Valley, enddate = 11111111111111), "enddate is outside of data date range")
  expect_error(ggplot_pm25Timeseries(Carmel_Valley, base_size = "eleven"), "base_size must be numeric")
  
}) 

test_that("works with ws_monitor and ws_tidy objects", {
  
  expect_is(ggplot_pm25Timeseries(PWFSLSmoke::Carmel_Valley), "ggplot")
  expect_is(ggplot_pm25Timeseries(monitor_toTidy(Carmel_Valley)), "ggplot")
  expect_error(ggplot_pm25Timeseries("ws_data"), "ws_data must be either a ws_monitor object or ws_tidy object")
  
})
