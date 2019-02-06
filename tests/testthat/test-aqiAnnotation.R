context("AQI annotation")

test_that("arguments are validated", {
  
  expect_error(custom_aqiStackedBar(width = "four"))
  
})

test_that("annotations can be added to a plot", {
  
  p <- ggplot_pm25Timeseries(PWFSLSmoke::Carmel_Valley)
  
  expect_is(p + custom_aqiLines(), "ggplot")
  expect_is(p + custom_aqiStackedBar(), "ggplot")
  
})