context("tidy_ggClockPlot")

test_that("Parameters are validated", {
  
  ws_tidy <- monitor_toTidy(PWFSLSmoke::Carmel_Valley)
  
  expect_error(tidy_ggClockPlot(ws_tidy, timezone = "invalid"), "Invalid timezone")
  expect_error(tidy_ggClockPlot("ws_tidy"), "ws_tidy must be a ws_tidy object")
  expect_error(tidy_ggClockPlot(ws_tidy, monitorID = "invalid"), "monitorID not present in data")
  
})

test_that("Generates correct output",{
  ws_nm <- monitor_toTidy(PWFSLSmoke::Northwest_Megafires)
  ws_cv <- monitor_toTidy(PWFSLSmoke::Carmel_Valley)
  
  
  expect_is(tidy_ggClockPlot(ws_cv), "ggplot")
  expect_is(tidy_ggClockPlot(ws_cv, startdate = 20160630, enddate = 20160705), "ggplot")
  expect_is(tidy_ggClockPlot(ws_nm, monitorID = "410050004_01"), "ggplot")
  expect_is(tidy_ggClockPlot(ws_nm, monitorID = "410050004_01", timezone = "America/Los_Angeles"), "ggplot")
  
})

