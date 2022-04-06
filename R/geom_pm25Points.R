#' @title Add pm25 points to a plot
#'
#' @description
#' This is a wrapper around \code{\link[ggplot2]{geom_point}} with default
#' aesthetics.
#'
#' @inheritParams ggplot2::geom_point
#'
#' @export
#' @import ggplot2
#'
#' @examples
#' library(AirMonitorPlots)
#'
#' ggplot_pm25Timeseries(AirMonitor::Carmel_Valley) + geom_pm25Points()
#'

geom_pm25Points <- function(
  mapping = NULL,
  data = NULL,
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  stat = "identity",
  ...
) {

  ## References:
  #  * Construct a manual legend for a complicated plot
  #    https://stackoverflow.com/q/17148679/
  #  * http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/

  list(
    layer(
      stat = stat,
      data = data,
      mapping = mapping,
      geom = GeomPm25Points,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  )

}


GeomPm25Points <- ggproto(
  "GeomPm25Points",
  GeomPoint,
  # do some styling
  required_aes = c("x", "y"),
  default_aes = aes(
    alpha = 0.3,
    colour = "black",
    shape = 19,
    size = 1.5,
    fill = NA,
    stroke = 0.5
  )
)
