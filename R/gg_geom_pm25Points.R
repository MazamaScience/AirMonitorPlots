geom_pm25Points <- function(mapping = NULL, 
                            data = NULL,                            
                            position = "identity", 
                            na.rm = FALSE, 
                            show.legend = NA, 
                            inherit.aes = TRUE, 
                            stat = "identity", 
                            color = "black",
                            legend.title = "",
                            legend.label = "PM2.5",
                            ...) {
  
  # Add mapping to color so legend can be added later if desired
  print(mapping)
  if (is.null(mapping)) {
    mapping <- aes(shape = !!legend.label, linetype = !!legend.label)
    # Set default shape and linetype
    pm25_shape <- 19
    pm25_linetype <- NA
  } else {
    pm25_shape <- mapping$shape
    pm25_linetype <- mapping$linetype
    mapping$shape <- legend.label
    mapping$linetype <- legend.label
  }

  
  list(
    layer(
      stat = stat, data = data, mapping = mapping, geom = GeomPm25Points,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    ),
    scale_shape_manual(name = legend.title, 
                       values = c(legend.label = pm25_shape), 
                       guide = "legend"),
    scale_linetype_manual(name = legend.title, 
                          values = c(legend.label = pm25_linetype), 
                          guide = "legend")
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