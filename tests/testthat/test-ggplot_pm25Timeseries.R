context("ggplot_pm25Timeseries")

test_that("parameters are validated", {
  
  ws_monitor <- PWFSLSmoke::Carmel_Valley
  
  expect_error(ggplot_pm25Timeseries("ws_tidy"), "ws_data must be either a ws_monitor object or ws_tidy object")
  expect_error(ggplot_pm25Timeseries(ws_monitor, startdate = 20200101), "startdate is outside of data date range")
  expect_error(ggplot_pm25Timeseries(ws_monitor, enddate = 11111111111111), "enddate is outside of data date range")
  expect_error(ggplot_pm25Timeseries(ws_monitor, base_size = "eleven"), "base_size must be numeric")
  
}) 

test_that("works with ws_monitor and ws_tidy objects", {
  
  ws_monitor <- PWFSLSmoke::Carmel_Valley
  
  expect_is(ggplot_pm25Timeseries(ws_monitor), "ggplot")
  expect_is(ggplot_pm25Timeseries(monitor_toTidy(ws_monitor)), "ggplot")
  expect_error(ggplot_pm25Timeseries("ws_data"), "ws_data must be either a ws_monitor object or ws_tidy object")
  
})
