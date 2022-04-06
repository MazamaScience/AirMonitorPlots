test_that("arguments are validated", {

  expect_error(custom_aqiStackedBar(width = "four"))

})

test_that("annotations can be added to a plot", {

  p <- ggplot_pm25Timeseries(AirMonitor::Carmel_Valley)

  expect_s3_class(p + custom_aqiLines(), "ggplot")
  expect_s3_class(p + custom_aqiStackedBar(), "ggplot")

})
