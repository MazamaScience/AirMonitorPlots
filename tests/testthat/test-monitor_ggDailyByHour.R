test_that("parameters are validated", {

  ws_cv <- PWFSLSmoke::Carmel_Valley

  expect_error(monitor_ggDailyByHour("ws_cv"))
  expect_error(monitor_ggDailyByHour(ws_cv, startdate = 20200101))
  expect_error(monitor_ggDailyByHour(ws_cv, enddate = 18080808))
  expect_error(monitor_ggDailyByHour(ws_cv, monitorID = "invalid"))
  expect_error(monitor_ggDailyByHour(ws_cv, style = "invalid"))
  expect_error(monitor_ggDailyByHour(ws_cv, timezone = "invalid"))

})

test_that("return has the class 'ggplot'", {

  ws_cv <- PWFSLSmoke::Carmel_Valley

  expect_s3_class(monitor_ggDailyByHour(ws_cv), "ggplot")

})
