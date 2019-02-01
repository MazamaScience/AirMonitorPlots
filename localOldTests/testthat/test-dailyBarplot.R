context("dailyBarplot")

test_that("arguments are validated", {

  # Must provide a ws_monitor
  expect_error(dailyBarplot())
  
  # Cannot accept more than one monitor
  multiple_monitors <- PWFSLSmoke::Northwest_Megafires
  expect_error(dailyBarplot(multiple_monitors))
  
  # Invalid monitor object
  expect_error(dailyBarplot("Mystery Monitor"))
  
  # Invalid style
  ws_monitor <- PWFSLSmoke::Carmel_Valley
  expect_error(dailyBarplot(ws_monitor, style = "invalid_style"))

})
