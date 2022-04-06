context("dailyBarplot")

test_that("arguments are validated", {

  # Must provide a mts_monitor
  expect_error(dailyBarplot())
  
  # Cannot accept more than one monitor
  multiple_monitors <- AirMonitor::Northwest_Megafires
  expect_error(dailyBarplot(multiple_monitors))
  
  # Invalid monitor object
  expect_error(dailyBarplot("Mystery Monitor"))
  
  # Invalid style
  mts_monitor <- AirMonitor::Carmel_Valley
  expect_error(dailyBarplot(mts_monitor, style = "invalid_style"))

})
