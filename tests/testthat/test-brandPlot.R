test_that("arguments are validated", {

  p <- monitor_ggTimeseries(PWFSLSmoke::Carmel_Valley)

  # Must provide a plot to brand
  expect_error(brandPlot())

  # Correct parameter classes
  expect_error(brandPlot("plot"))
  expect_error(brandPlot(p, size = "four"))

  # Valid strings
  expect_error(brandPlot(p, brandStyle = "invalid_style"))
  expect_error(brandPlot(p, brandName = "invalid_name"))
  expect_error(brandPlot(p, brandFilePath = "invalid_file_path"))
  expect_error(brandPlot(p, location = "invalid_location"))

})
