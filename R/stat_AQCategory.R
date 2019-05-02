#' @title Add air quality categories to a plot
#'
#' @description
#' This function calculates the air quality categories for the data and colors
#' the data by AQ cateogry when it is added to a plot. The default is to add
#' them as bars.
#'
#' @param mapping Set of aesthetic mappings created by \code{aes()}. If
#'   specified and \code{inherit.aes = TRUE} (the default), it is combined with
#'   the default mapping at the top level of the plot. You must supply
#'   \code{mapping} if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options:
#'   if \code{NULL}, the default, the data is inherited from the plot data. A
#'   \code{data.frame} or other object, will override the plot data. A
#'   \code{function} will be called witha  single argument, the plot data. The
#'   return value must be a \code{data.frame}, and will be used as the layer
#'   data.
#' @param mv4Colors If \code{TRUE}, use the colors used in the monitoring v4
#'   site. Otherwise, use the "official" AQI colors.
#' @param nowcast If \code{TRUE}, y values will be transformed using
#'   \code{\link{stat_nowcast}}.
#' @param geom The geometic object to display the data
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function.
#' @param na.rm remove NA values from data
#' @param show.legend logical indicating whether this layer should be included
#'   in legends.
#' @param inherit.aes if \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and the aesthetics and shouldn't inherit behaviour from
#'   the default plot specificatino, eg \code{borders()}.
#' @param ... additional arguments passed on to \code{layer()}, such as
#'   aesthetics.
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' ggplot_pm25Timeseries(PWFSLSmoke::Carmel_Valley,
#'                       startdate = 20160801,
#'                       enddate = 20160815) +
#'   stat_AQCategory()
#'
#' ggplot_pm25Timeseries(PWFSLSmoke::Carmel_Valley,
#'                       startdate = 20160801,
#'                       enddate = 20160805) +
#'   geom_line() +
#'   stat_AQCategory(geom = "point",
#'                   size = 2,
#'                   shape = 21,
#'                   color = 1)
stat_AQCategory <- function(
  mapping = NULL,
  data = NULL,
  mv4Colors = FALSE,
  nowcast = TRUE,
  geom = "bar",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {

  ## NOTE:
  #  `if(test) yes else no` is more efficient than `ifelse(test, yes, no)` when
  #  test is of length 1.

  if (nowcast) version <- "pm" else version <- "identity"

  stat_nowcast(
    mapping = mapping,
    data = data,
    mv4Colors = mv4Colors,
    geom = geom,
    position = position,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    aqiColors = TRUE,
    version = version,
    ...
  )

}
