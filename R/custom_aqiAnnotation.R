#' @title Add AQI stacked bars to a plot
#'
#' @description
#' Adds AQI stacked bars to a plot. 
#'
#' @param width Width of bars as a fraction of plot width.
#' @param position Position adjustment, either as a string, or the result of 
#' a call to a position adjustment function
#' @param ... additional arguments passed on to layer, such as alpha. 
#'
#' @return A `ggplot` plot object with AQI annotations.
#' 
#' @import ggplot2
#' @export
#' @examples 
#' p <- ggplot_pm25Timeseries(Carmel_Valley)
#' p + custom_aqiLines() + custom_aqiStackedBar()
#' 

custom_aqiStackedBar <- function(width = 0.02,
                                 position = "identity",
                                 ...) {
  
  # Validate parameters
  if (!is.numeric(width)) stop("width must be a number")
  
  list(
    layer(
      stat = StatAqiBar, geom = GeomRect, position = position, 
      data = NULL, mapping = NULL, show.legend = FALSE, 
      inherit.aes = TRUE,
      params = list(width = width, ...)
    ),
    theme(
      axis.line.y = element_blank()
    )
  )
}

StatAqiBar <- ggproto(
  "StatAqiBar",
  Stat,
  compute_group = function(data, scales, params, width) {
    
    # Get the plot dimensions
    xrange <- scales$x$get_limits()
    yrange <- scales$y$get_limits()
    
    # Set left and right for bars
    left <- xrange[1]
    right <- xrange[1] + width * (xrange[2]-xrange[1])
    
    # Create data
    # GeomRect uses xmin, xmax, ymin, ymax
    aqiStackedBarsData <- data.frame(
      xmin = rep(left, 6),
      xmax = rep(right, 6),
      ymin = c(yrange[1], AQI$breaks_24[2:6]),
      ymax =c(AQI$breaks_24[2:6], 1e6)
    )
    
    # Last bar must top out at yrange[2]
    aqiStackedBarsData <- aqiStackedBarsData %>%
      dplyr::filter(.data$ymin < yrange[2])
    barCount <- nrow(aqiStackedBarsData)
    aqiStackedBarsData$ymax[barCount] <- yrange[2]
    aqiStackedBarsData$fill <- AQI$colors[1:barCount]
    
    return(aqiStackedBarsData)
  }
)

#' @title Add AQI lines to a plot
#'
#' @description
#' Adds AQI lines to a plot
#'
#' @param ... Arguments passed on to layer, such as aesthetic properties like
#' size or alpha. 
#'
#' @return A `ggplot` plot object with AQI annotations.
#' 
#' @import ggplot2
#' @export

custom_aqiLines <- function(...) {
  
  list(
    layer(
      stat = StatAqiLines, data = NULL, mapping = NULL, geom = GeomSegment,
      position = "identity", show.legend = NA, inherit.aes = TRUE,
      params = list(na.rm = TRUE, color = AQI$colors[2:6], ...)
    )
  )
}



StatAqiLines <- ggproto(
  "StatAqiLines",
  Stat,
  compute_group = function(data, scales, params) {
    
    # Get the plot dimensions
    xrange <- scales$x$get_limits()
    yrange <- scales$y$get_limits()
    
    
    aqiLinesData <- data.frame(
      x = rep(xrange[1], 5),
      xend = rep(xrange[2], 5),
      y = c(AQI$breaks_24[2:6]),
      yend = c(AQI$breaks_24[2:6])
    )
    return(aqiLinesData)
  })


