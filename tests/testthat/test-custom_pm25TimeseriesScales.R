test_that("timezones are handled correctly", {

  mts_monitor <- AirMonitor::Carmel_Valley

  # Default timezone
  timezone <- mts_monitor$meta$timezone
  p <- ggplot() + custom_pm25TimeseriesScales(mts_monitor)
  scales <- layer_scales(p)
  expect_equal(attr(scales$x$breaks, "tzone"), timezone)

  # Custom timezone
  timezone <- "America/New_York"
  p <- ggplot() + custom_pm25TimeseriesScales(mts_monitor, timezone = timezone)
  scales <- layer_scales(p)
  expect_equal(attr(scales$x$breaks, "tzone"), timezone)

  # Multiple timezones should default to UTC
  p <- p <- ggplot() + custom_pm25TimeseriesScales(AirMonitor::NW_Megafires)
  scales <- layer_scales(p)
  expect_equal(attr(scales$x$breaks, "tzone"), "UTC")

})
