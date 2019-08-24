test_that("parameters are validated", {

  ws_cv <- PWFSLSmoke::Carmel_Valley

  expect_error(monitor_ggDailyBarplot("ws_cv"))
  expect_error(monitor_ggDailyBarplot(ws_cv, monitorIDs = "invalid"))
  expect_error(monitor_ggDailyBarplot(ws_cv, style = "invalid"))
  expect_error(monitor_ggDailyBarplot(ws_cv, timezone = "invalid"))
  expect_error(monitor_ggDailyBarplot(ws_cv, today = "true"))

})

test_that("return has the class 'ggplot'", {

  ws_cv <- PWFSLSmoke::Carmel_Valley

  expect_s3_class(monitor_ggDailyBarplot(ws_cv), "ggplot")

})
