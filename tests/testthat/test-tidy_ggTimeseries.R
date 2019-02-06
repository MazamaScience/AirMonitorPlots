context("tidy_ggTimeseries")

test_that("parameters are validated", {
  
  ws_tidy <- monitor_toTidy(PWFSLSmoke::Carmel_Valley)
  
  expect_error(tidy_ggTimeseries("ws_tidy"), "ws_tidy must be a ws_tidy object")
  expect_error(tidy_ggTimeseries(ws_tidy, startdate = 20200101), "startdate is outside of data date range")
  expect_error(tidy_ggTimeseries(ws_tidy, enddate = 11111111111111), "enddate is outside of data date range")
  expect_error(tidy_ggTimeseries(ws_tidy, style = "invalid"), "Invalid style")
  expect_error(tidy_ggTimeseries(ws_tidy, monitorIDs = "invalid"), "Invalid monitorIDs")
  
})