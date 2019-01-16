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
    # if (is.null(mapping)) {
      mapping <- aes(colour = !!legend.label)
      
    # } else {
    #   if (is.null(mapping$shape)) {
    #     mapping$shape <- legend.label
    #     shape_scale <- scale_shape_manual(name = legend.title, values = shape, guide = "legend")
    #   } else {
    #     shape_scale <- NULL
    #   }
    #   if (is.null(mapping$colour)) {
    #     mapping$colour <- legend.label
    #     color_scale <- scale_color_manual(name = legend.title, values = color, guide = "legend")
    #   } else {
    #     color_scale <- NULL
    #   } 
    #   if (is.null(mapping$size)) {
    #     mapping$size <- legend.label
    #     size_scale <- scale_size_manual(name = legend.title, values = size, guide = "legend")
    #   } else {
    #     size_scale <- NULL
    #   }
    #   if (is.null(mapping$alpha)) {
    #     mapping$alpha <- legend.label
    #     alpha_scale <- scale_alpha_manual(name = legend.title, values = alpha, guide = "legend")
    #   } else {
    #     alpha_scale <- NULL
    #   }
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