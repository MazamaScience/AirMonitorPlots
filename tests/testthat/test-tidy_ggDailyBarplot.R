context("tidy_ggDailyBarplot")

test_that("parameters are validated", {
  
  ws_tidy <- monitor_toTidy(PWFSLSmoke::Carmel_Valley)

  expect_error(tidy_ggDailyBarplot("ws_tidy"), "ws_tidy must be a ws_tidy object")
  expect_error(tidy_ggDailyBarplot(ws_tidy, monitorIDs = "invalid"), "monitorIDs not present in data")
  expect_error(tidy_ggDailyBarplot(ws_tidy, style = "invalid"), "Invalid style")
  expect_error(tidy_ggDailyBarplot(ws_tidy, timezone = "invalid"), "Invalid timezone")
  expect_error(tidy_ggDailyBarplot(ws_tidy, today = "true"), "today must be logical")
  expect_error(tidy_ggDailyBarplot(ws_tidy, startdate = 22000101), "startdate is outside of data date range")
  expect_error(tidy_ggDailyBarplot(ws_tidy, enddate = 18010101), "enddate is outside of data date range")
    
})