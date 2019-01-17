aqiStackedBar <- function(width = 0.02,
                          position = "identity", 
                          inherit.aes = TRUE, 
                          ...) {
  
  list(
    layer(
      stat = AqiBar, geom = GeomRect, position = position, 
      data = NULL, mapping = NULL, show.legend = FALSE, 
      inherit.aes = inherit.aes,
      params = list(width = width, ...)
    ),
    theme(
      axis.line.y = element_blank()
    )
  )
}

AqiBar <- ggproto("AqiBar", Stat,
                  
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

aqiLines <- function(mapping = NULL, 
                     data = NULL,                            
                     position = "identity", 
                     na.rm = FALSE, 
                     show.legend = NA, 
                     inherit.aes = TRUE, 
                     stat = "identity", 
                     ...) {
  
  list(
    layer(
      stat = AqiLines, data = data, mapping = mapping, geom = GeomSegment,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, color = AQI$colors[2:6], ...)
    )
  )
}

AqiLines <- ggproto("AqiLines", Stat,
                    
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
                    

