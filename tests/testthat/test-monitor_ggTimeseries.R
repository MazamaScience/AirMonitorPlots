test_that("parameters are validated", {

  mts_cv <- AirMonitor::Carmel_Valley

  expect_error(monitor_ggTimeseries("mts_cv"))
  expect_error(monitor_ggTimeseries(mts_cv, startdate = 20200101))
  expect_error(monitor_ggTimeseries(mts_cv, enddate = 11111111111111))
  expect_error(monitor_ggTimeseries(mts_cv, style = "invalid"))
  expect_error(monitor_ggTimeseries(mts_cv, deviceDeploymentIDs = "invalid"))

})

test_that("return has the class 'ggplot'", {

  mts_cv <- AirMonitor::Carmel_Valley

  expect_s3_class(monitor_ggTimeseries(mts_cv), "ggplot")

})
