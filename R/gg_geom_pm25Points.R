geom_pm25Points <- function(mapping = NULL, 
                            data = NULL,                            
                            position = "identity", 
                            na.rm = FALSE, 
                            show.legend = NA, 
                            inherit.aes = TRUE, 
                            stat = "identity", 
                            ...) {
  
  layer(
    stat = stat, data = data, mapping = mapping, geom = GeomPm25Points,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  ) 
}

GeomPm25Points <- ggproto("GeomPm25Points", GeomPoint,
                          # do some styling
                          default_aes = aes(alpha = 0.3,
                                            colour = "black",
                                            shape = 19,
                                            size = 1.5,
                                            fill = NA,
                                            stroke = 0.5)
)


ggplot_pm25Timeseries <- function(data) {
  ggplot(data, aes(x = datetime, y = pm25)) +
    theme_timeseriesPlot_pwfsl() +
    ylab("PM2.5 (\u00b5g/m3)")
}