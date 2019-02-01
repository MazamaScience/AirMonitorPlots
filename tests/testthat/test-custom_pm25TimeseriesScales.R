context("custom_pm25TimeseriesScales")

test_that("Handles timezones correctly", {
  # Default timezone
  timezone <- Carmel_Valley$meta$timezone
  p <- ggplot() + custom_pm25TimeseriesScales(Carmel_Valley)
  scales <- layer_scales(p)
  expect_equal(attr(scales$x$breaks, "tzone"), timezone)
  
  # Custom timezone
  timezone <- "America/New_York"
  p <- ggplot() + custom_pm25TimeseriesScales(Carmel_Valley, timezone = timezone)
  scales <- layer_scales(p)
  expect_equal(attr(scales$x$breaks, "tzone"), timezone)
  
  # Multiple timezones should default to UTC
  p <- p <- ggplot() + custom_pm25TimeseriesScales(Northwest_Megafires)
  scales <- layer_scales(p)
  expect_equal(attr(scales$x$breaks, "tzone"), "UTC")
})