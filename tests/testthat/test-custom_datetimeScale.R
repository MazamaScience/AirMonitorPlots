test_that("parameters are validated", {

  # startdate and enddate must be specified
  expect_error(custom_datetimeScale())
  expect_error(custom_datetimeScale(startdate = 20170101))

  # Valid parameters
  expect_error(custom_datetimeScale(startdate = 20170101, enddate = 20170101, timezone = "fake"))
  expect_error(custom_datetimeScale(startdate = 20170101, enddate = 20170101, expand = "zero"))
  expect_error(custom_datetimeScale(startdate = 20170101, enddate = 20170101, tick_location = "invalid"))
  expect_error(custom_datetimeScale(startdate = 20170101, enddate = 20170101, includeFullEnddate = "true"))
  expect_error(custom_datetimeScale(startdate = 20170101, enddate = 20170101, today_label = "false"))

})
