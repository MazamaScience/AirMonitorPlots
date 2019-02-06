context("brandPlot")

test_that("arguments are validated", {
  
  p <- monitor_ggTimeseries(PWFSLSmoke::Carmel_Valley)
  
  # Must provide a plot to brand
  expect_error(brandPlot())
  
  # Correct parameter classes
  expect_error(brandPlot("plot"), "'plot' must be a ggplot object")
  expect_error(brandPlot(p, size = "four"), "size must be a number")
  
  # Valid strings 
  expect_error(brandPlot(p, brandStyle = "invalid_style"), "Invalid brandStyle")
  expect_error(brandPlot(p, brandName = "invalid_name"), "Invalid brandName")
  expect_error(brandPlot(p, brandFilePath = "invalid_file_path"), "Invalid brandFilePath")
  expect_error(brandPlot(p, location = "invalid_location"), "Invalid location")
  
})
