context("custom_datetimeScale")

test_that("parameters are validated", {
  
  # startdate and enddate must be specified
  expect_error(custom_datetimeScale(), "startdate must be specified")
  expect_error(custom_datetimeScale(startdate = 20170101), "enddate must be specified")
  
  # Valid parameters
  expect_error(custom_datetimeScale(startdate = 20170101, enddate = 20170101, timezone = "fake"),
               "Invalid timezone")
  expect_error(custom_datetimeScale(startdate = 20170101, enddate = 20170101, expand = "zero"),
               "Invalid 'expand'")
  expect_error(custom_datetimeScale(startdate = 20170101, enddate = 20170101, tick_location = "invalid"),
               "Invalid tick_location")
  expect_error(custom_datetimeScale(startdate = 20170101, enddate = 20170101, includeFullEnddate = "true"),
               "includeFullEnddate must be logical")
  expect_error(custom_datetimeScale(startdate = 20170101, enddate = 20170101, today_label = "false"),
               "today_label must be logical")
  
})