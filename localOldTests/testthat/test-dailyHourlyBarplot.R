context("dailyHourlyBarplot")

test_that("arguments are validated", {
  expect_error(dailyHourlyBarplot("abcd"))
  mts_monitor <- AirMonitor::Carmel_Valley
  expect_error(dailyHourlyBarplot(mts_monitor, hourlyDataType="abcd"))
})
