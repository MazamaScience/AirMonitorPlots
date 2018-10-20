context("clockPlot")

test_that("arguments are validated", {
  
  # Must provide a ws_monitor
  expect_error(clockPlot())
  
  # Cannot accept more than one monitor
  multiple_monitors <- PWFSLSmoke::loadDaily()
  expect_error(clockPlot(multiple_monitors))
  
  # Invalid monitor object
  expect_error(clockPlot("Mystery Monitor"))
  
  # Invalid style
  ws_monitor <- PWFSLSmoke::Carmel_Valley
  expect_error(clockPlot(ws_monitor, style = "invalid_style"))
  expect_error(clockPlot(ws_monitor, style = "fffulllfffannnaaavggg"))
  
  # Invalid center color
  expect_error(clockPlot(ws_monitor, style = "base", centerColor = "invalid color"))
})
