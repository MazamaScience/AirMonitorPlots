#' @title Add pm25 points to a plot
#'
#' @description
#' Add pm25 points to a ggplot. This is a wrapper around \code{geom_point} with
#' default aesthetics. The main difference is the timeseries.legend option which,
#' when true, will set mappings so that a legend can be drawn for these points using
#' \link{legend_pm25Timeseries}. 
#'
#' @inheritParams ggplot2::geom_point
#' @param timeseries.legend Logical indicating whether to set mappings so that
#' a legend can later be added using \link{legend_pm25Timeseries}.
#' @param legend.label Label for pm25Points part of the legend. Must match 
#' legend.labels argument in \link{legend_pm25Timeseries}.
#'
#' @import ggplot2
#' @export
#' 


geom_pm25Points <- function(mapping = NULL, 
                            data = NULL,                            
                            position = "identity", 
                            na.rm = FALSE, 
                            show.legend = NA, 
                            inherit.aes = TRUE, 
                            stat = "identity", 
                            timeseries.legend = FALSE,
                            legend.label = "PM2.5",
                            ...) {
  
  if ( timeseries.legend ) {
    if (!is.null(mapping)) {
      stop("timeseries legend can only be created when mapping is NULL.")
    }
    ## Map aesthetics to a variable (legend.label)
    if (is.null(mapping)) {
      mapping <- aes(colour = !!legend.label) 
    }
  } 
  
  
  
  
  
  list(
    layer(
      stat = stat, data = data, mapping = mapping, geom = GeomPm25Points,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
    
    
    # http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/
    # https://stackoverflow.com/questions/17148679/construct-a-manual-legend-for-a-complicated-plot
  )
  
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomPm25Points <- ggproto("GeomPm25Points", GeomPoint,
                          # do some styling
                          required_aes = c("x", "y"),
                          default_aes = aes(alpha = 0.3,
                                            colour = "black",
                                            shape = 19,
                                            size = 1.5,
                                            fill = NA,
                                            stroke = 0.5)
)


ggplot_pm25Timeseries <- function(data) {
  ggplot(data, aes(x = datetime, y = pm25)) +
    theme_timeseriesPlot_pwfsl()
}