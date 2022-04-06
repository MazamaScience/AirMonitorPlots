test_that("parameters are validated", {

  mts_cv <- AirMonitor::Carmel_Valley

  expect_error(monitor_ggDailyByHour("mts_cv"))
  expect_error(monitor_ggDailyByHour(mts_cv, deviceDeploymentID = "invalid"))
  expect_error(monitor_ggDailyByHour(mts_cv, style = "invalid"))
  expect_error(monitor_ggDailyByHour(mts_cv, timezone = "invalid"))

})

test_that("return has the class 'ggplot'", {

  mts_cv <- AirMonitor::Carmel_Valley

  expect_s3_class(monitor_ggDailyByHour(mts_cv), "ggplot")

})
