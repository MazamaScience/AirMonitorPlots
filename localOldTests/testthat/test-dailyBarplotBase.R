context("dailyBarplotBase")

test_that("arguments are validated", {
  
  # Must provide a ws_monitor
  expect_error(dailyBarplotBase())
  
  # Cannot accept more than one monitor
  multiple_monitors <- PWFSLSmoke::loadDaily()
  expect_error(dailyBarplotBase(multiple_monitors))
  
  # Invalid monitor object
  expect_error(dailyBarplotBase("Mystery Monitor"))
  
  ws_monitor <- PWFSLSmoke::Carmel_Valley
  
  # Needs a start or end date
  expect_error(dailyBarplotBase(ws_monitor))
  
  # Start/end dates must be numeric or character type in Ymd format, or a POSIXct object
  expect_error(dailyBarplotBase(ws_monitor, startdate = TRUE))
  expect_error(dailyBarplotBase(ws_monitor, enddate = FALSE))
  expect_error(dailyBarplotBase(ws_monitor, startdate = FALSE, enddate = TRUE))
  
  # Invalid color palette
  expect_error(dailyBarplotBase(ws_monitor, startdate = "2016-08-09", colorPalette = "Not a palette"))
  
})
