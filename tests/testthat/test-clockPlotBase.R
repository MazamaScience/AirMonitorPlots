context("clockPlotBase")

test_that("arguments are validated", {
  
  # Must provide a ws_monitor
  expect_error(clockPlotBase())
  
  # Cannot accept more than one monitor
  multiple_monitors <- PWFSLSmoke::loadDaily()
  expect_error(clockPlotBase(multiple_monitors))
  
  # Invalid monitor object
  expect_error(clockPlotBase("Mystery Monitor"))
  
  ws_monitor <- PWFSLSmoke::Carmel_Valley
  
  # Needs a start or end date
  expect_error(clockPlotBase(ws_monitor))
  
  # Start/end dates must be numeric or character type in Ymd format, or a POSIXct object
  expect_error(clockPlotBase(ws_monitor, startdate = TRUE))
  expect_error(clockPlotBase(ws_monitor, enddate = FALSE))
  expect_error(clockPlotBase(ws_monitor, startdate = FALSE, enddate = TRUE))
  
  # Invalid color palette
  expect_error(clockPlotBase(ws_monitor, startdate = "2016-08-09", colorPalette = "Not a palette"))
  
})
