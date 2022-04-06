context("clockPlot")

test_that("arguments are validated", {
  
  # Must provide a mts_monitor
  expect_error(clockPlot())
  
  # Cannot accept more than one monitor
  multiple_monitors <- AirMonitor::loadDaily()
  expect_error(clockPlot(multiple_monitors))
  
  # Invalid monitor object
  expect_error(clockPlot("Mystery Monitor"))
  
  # Invalid style
  mts_monitor <- AirMonitor::Carmel_Valley
  expect_error(clockPlot(mts_monitor, style = "invalid_style"))
  expect_error(clockPlot(mts_monitor, style = "fffulllfffannnaaavggg"))
  
  # Invalid center color
  expect_error(clockPlot(mts_monitor, style = "base", centerColor = "invalid color"))
  
})
