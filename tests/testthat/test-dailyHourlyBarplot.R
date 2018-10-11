context("dailyHourlyBarplot")

test_that("arguments are validated", {
  expect_error(dailyHourlyBarplot("abcd"))
  ws_monitor <- PWFSLSmoke::Carmel_Valley
  expect_error(dailyHourlyBarplot(ws_monitor, hourlyDataType="abcd"))
})
