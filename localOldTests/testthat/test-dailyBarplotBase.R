context("dailyBarplotBase")

test_that("arguments are validated", {
  
  # Must provide a mts_monitor
  expect_error(dailyBarplotBase())
  
  # Cannot accept more than one monitor
  multiple_monitors <- AirMonitor::loadDaily()
  expect_error(dailyBarplotBase(multiple_monitors))
  
  # Invalid monitor object
  expect_error(dailyBarplotBase("Mystery Monitor"))
  
  mts_monitor <- AirMonitor::Carmel_Valley
  
  # Needs a start or end date
  expect_error(dailyBarplotBase(mts_monitor))
  
  # Start/end dates must be numeric or character type in Ymd format, or a POSIXct object
  expect_error(dailyBarplotBase(mts_monitor, startdate = TRUE))
  expect_error(dailyBarplotBase(mts_monitor, enddate = FALSE))
  expect_error(dailyBarplotBase(mts_monitor, startdate = FALSE, enddate = TRUE))
  
  # Invalid color palette
  expect_error(dailyBarplotBase(mts_monitor, startdate = "2016-08-09", colorPalette = "Not a palette"))
  
})
