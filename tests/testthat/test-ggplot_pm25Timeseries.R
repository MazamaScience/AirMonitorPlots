test_that("parameters are validated", {

  ws_tidy <- monitor_toTidy(PWFSLSmoke::Carmel_Valley)

  expect_error(ggplot_pm25Timeseries("ws_tidy"))
  expect_error(ggplot_pm25Timeseries(ws_tidy, startdate = 20200101))
  expect_error(ggplot_pm25Timeseries(ws_tidy, enddate = 11111111111111))
  expect_error(ggplot_pm25Timeseries(ws_tidy, base_size = "eleven"))

})

test_that("works with ws_monitor and ws_tidy objects", {

  ws_monitor <- PWFSLSmoke::Carmel_Valley
  ws_tidy <- monitor_toTidy(ws_monitor)

  expect_s3_class(ggplot_pm25Timeseries(ws_monitor), "ggplot")
  expect_s3_class(ggplot_pm25Timeseries(ws_tidy), "ggplot")

})
