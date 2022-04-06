test_that("parameters are validated", {

  mts_cv <- AirMonitor::Carmel_Valley

  expect_error(monitor_ggClockPlot("mts_cv"))
  expect_error(monitor_ggClockPlot(mts_cv, timezone = "invalid"))
  expect_error(monitor_ggClockPlot(mts_cv, deviceDeploymentID = "invalid"))

})

test_that("return has the class 'ggplot'", {

  mts_cv <- AirMonitor::Carmel_Valley

  expect_s3_class(monitor_ggClockPlot(mts_cv), "ggplot")

})

