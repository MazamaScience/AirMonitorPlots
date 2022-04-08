test_that("parameters are validated", {

  mts_cv <- AirMonitor::Carmel_Valley

  expect_error(monitor_ggDailyBarplot("mts_cv"))
  expect_error(monitor_ggDailyBarplot(mts_cv, deviceDeploymentIDs = "invalid"))
  expect_error(monitor_ggDailyBarplot(mts_cv, style = "invalid"))
  expect_error(monitor_ggDailyBarplot(mts_cv, timezone = "invalid"))
  expect_error(monitor_ggDailyBarplot(mts_cv, today = "true"))

})

test_that("return has the class 'ggplot'", {

  mts_cv <- AirMonitor::Carmel_Valley

  expect_s3_class(monitor_ggDailyBarplot(mts_cv), "ggplot")

})
