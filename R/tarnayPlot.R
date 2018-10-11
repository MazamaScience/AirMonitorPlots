#' @title Create a Daily-Hourly plot for many monitors
#'
#' @description
#' Create a time series barplot showing PM2.5 data for the given monitors. The
#' overall plot is faceted by monitor, and each facet has two sets of columns:
#' one for daily levels, and one for hourly levels.
#'
#' @param monitors Monitor ID(s) to create plot for.
#' @param data Data used to create plot (NOTE: currently must be a `ws_monitor`
#'   object).
#' @param tlim Time range to subset the data by. Can either be a
#'   character/numeric vector in form of 'yyyymmdd', or a POSIXct object.
#'   Defaults to `NULL` (no subsetting).
#' @param columns Number of columns the faceted plot should have (default 1).
#' @param title The title of the plot. Defaults to specifying the types of
#'   data present in the plot.
#' @param xLabel The x-axis label of the plot. Defaults to years present in
#'   data.
#' @param yLabel The y-axis label of the plot. Defaults to PM2.5.
#' @param includeLink Option to include a link to an AQI help page at the bottom
#'   of the plot (default `TRUE`).
#' @param hourlyType The type of hourly data to include in the plot. The options
#'   include "nowcast" (hourly nowcast values), "raw" (raw hourly values), or
#'   "none" (no hourly data at all) (default "nowcast").
#' @param colorScale The ordered color palette used to represent each AQI
#'   category. Currently defaults to (and only accepts) "epa_aqi".
#' @param includeThirdCol Option to include a third column in the legend.
#'   Currently in testing (default `False`).
#'
#' @return A **ggplot** plot of the given monitors and data.
#'
#' @import PWFSLSmoke
#' @importFrom dplyr mutate
#' @export
#'

createTarnayPlot <- function(monitors,
                             data,
                             tlim = NULL,
                             columns = 1,
                             title = NULL,
                             xLabel = NULL,
                             yLabel = NULL,
                             includeLink = TRUE,
                             hourlyType = "nowcast",
                             colorScale = "epa_aqi",
                             includeThirdCol = FALSE) {

  .Deprecated("dailyHourlyBarplot")
  
  dailyHourlyBarplot(data,
                     monitors,
                     tlim,
                     columns,
                     title,
                     xLabel,
                     yLabel,
                     includeLink,
                     hourlyType,
                     colorScale,
                     includeThirdCol)

}
