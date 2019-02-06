context("tidy_ggDailyByHour")

test_that("parameters are validated", {
  
  ws_tidy <- monitor_toTidy(PWFSLSmoke::Carmel_Valley)
  
  expect_error(tidy_ggDailyByHour("ws_tidy"), "ws_tidy must be a ws_tidy object")
  expect_error(tidy_ggDailyByHour(ws_tidy, startdate = 20200101), "startdate is outside of data date range")
  expect_error(tidy_ggDailyByHour(ws_tidy, enddate = 18080808), "enddate is outside of data date range")
  expect_error(tidy_ggDailyByHour(ws_tidy, monitorID = "invalid"), "Invalid monitorID")
  expect_error(tidy_ggDailyByHour(ws_tidy, style = "invalid"), "Invalid style")
  expect_error(tidy_ggDailyByHour(ws_tidy, timezone = "invalid"), "Invalid timezone")
  
  expect_error(tidy_ggDailyByHour(monitor_toTidy(PWFSLSmoke::Northwest_Megafires)), "monitorID")
  
})