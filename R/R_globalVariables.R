# -----------------------------------------------------------------------------
# globalVariables.R
#
# This file declares symbols that are used unquoted inside tidy evaluation
# contexts (e.g., ggplot2 aes(), dplyr verbs). Declaring them here prevents
# "no visible binding for global variable" notes during R CMD check.
# -----------------------------------------------------------------------------

utils::globalVariables(c(
  # Common time-series fields
  "datetime", "hour", "pm25", "value", "siteName", "timezone",

  # AQI and category fields
  "aqi", "aqiCategory", "aqiLabel",

  # Spatial fields
  "longitude", "latitude", "elevation", "stateCode", "countryCode",

  # Plotting helpers
  "label", "variable", "mean", "median", "hour"
))
