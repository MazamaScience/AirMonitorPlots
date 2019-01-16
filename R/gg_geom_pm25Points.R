geom_pm25Points <- function(mapping = NULL, 
                            data = NULL,                            
                            position = "identity", 
                            na.rm = FALSE, 
                            show.legend = FALSE, 
                            inherit.aes = TRUE, 
                            stat = "identity", 
                            color = "black",
                            size = 1,
                            shape = 19,
                            alpha = 0.3,
                            legend.title = "",
                            legend.label = "PM2.5",
                            ...) {
  
    ## Map aesthetics to a variable (legend.label)
    if (is.null(mapping)) {
      mapping <- aes(colour = !!legend.label,
                     shape = !!legend.label,
                     size = !!legend.label,
                     alpha = !!legend.label)
    } else {
      mapping$shape <- ifelse(is.null(mapping$shape), legend.label, mapping$shape)
      mapping$colour <- ifelse(is.null(mapping$colour), legend.label, mapping$colour)
      mapping$size <- ifelse(is.null(mapping$size), legend.label, mapping$size)
      mapping$alpha <- ifelse(is.null(mapping$size), legend.label, mapping$size)
    }

    
  
  
  list(
    layer(
      stat = stat, data = data, mapping = mapping, geom = GeomPm25Points,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm,
                    ...)
    ),
      scale_color_manual(name = legend.title, values = color, guide = "legend"),
      scale_shape_manual(name = legend.title, values = shape, guide = "legend"),
      scale_alpha_manual(name = legend.title, values = alpha, guide = "legend"),
      scale_size_manual (name = legend.title, values = size, guide = "legend")

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