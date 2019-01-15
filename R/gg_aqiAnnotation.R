aqiStackedBar <- function(mapping = NULL, 
                           data = NULL,                            
                           position = "identity", 
                           na.rm = FALSE, 
                           show.legend = NA, 
                           inherit.aes = TRUE, 
                           stat = "identity", 
                           ...) {
  
  list(
    layer(
      stat = AqiBar, data = data, mapping = mapping, geom = GeomRect,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    ),
    theme(
      axis.line.y = element_blank()
    )
  )
}

AqiBar <- ggproto("AqiBar", Stat,
                  
                  compute_group = function(data, scales, params) {
                    
                    xrange <- range(data$x, na.rm = TRUE)
                    yrange <- range(data$y, na.rm = TRUE)
                    xlo <- xrange[1]
                    xhi <- xrange[1] + .01 * (xrange[2]-xrange[1])
                    
                    # Create data
                    aqiStackedBarsData <- data.frame(
                      xmin = rep(xlo, 6),
                      xmax = rep((xhi), 6),
                      ymin = c(ylo, AQI$breaks_24[2:6]),
                      ymax =c(AQI$breaks_24[2:6], 1e6)
                    )
                    
                    # Last bar must top out at yrange[2]
                    aqiStackedBarsData <- aqiStackedBarsData %>%
                      dplyr::filter(.data$ymin < yrange[2])
                    barCount <- nrow(aqiStackedBarsData)
                    aqiStackedBarsData$ymax[barCount] <- yrange[2]
                    aqiStackedBarsData$fill <- AQI$colors[1:barCount]
                    
                    
                    print(aqiStackedBarsData)
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
                      
                      xrange <- range(data$x, na.rm = TRUE)
                      yrange <- range(data$y, na.rm = TRUE)
                      xlo <- xrange[1]
                      xhi <- xrange[1] + .01 * (xrange[2]-xrange[1])
                      
                      aqiLinesData <- data.frame(
                        x = rep(xlo, 5),
                        xend = rep(xrange[2], 5),
                        y = c(AQI$breaks_24[2:6]),
                        yend = c(AQI$breaks_24[2:6])
                      )
                      return(aqiLinesData)
                    })
                    

