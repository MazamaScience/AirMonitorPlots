test_that("parameters are validated", {

  ws_cv <- PWFSLSmoke::Carmel_Valley

  expect_error(monitor_ggClockPlot("ws_cv"))
  expect_error(monitor_ggClockPlot(ws_cv, timezone = "invalid"))
  expect_error(monitor_ggClockPlot(ws_cv, monitorID = "invalid"))

})

test_that("return has the class 'ggplot'", {

  ws_cv <- PWFSLSmoke::Carmel_Valley

  expect_s3_class(monitor_ggClockPlot(ws_cv), "ggplot")

})

