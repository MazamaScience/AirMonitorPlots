#' @title Add AQI annotations an existing ggplot
#'
#' @description
#' Adds AQI annotations to an existing \code{ggplot}. The \code{style} parameter
#' must contain one ore more of the following options separated by underscores:
#' 
#' \itemize{
#' \item{\code{bars} -- stacked color bars}
#' \item{\code{lines} -- colored lines}
#' \item{\code{labels} -- AQI labels}
#' }
#'
#' The returned object may be further amended with `ggplot` elements.
#' 
#' @param gg \code{ggplot} object.
#' @param xlo Left edge of the plot as a \code{POSIXct} value.
#' @param xhi Right edge of the plot as a \code{POSIXct} value.
#' @param ylo Bottom edge of the plot as a \code{numeric} value.
#' @param yhi Top edge of the plot as a \code{numeric} value.
#' @param style Annotation style as described below.
#' @param barWidth Width of stacked bars as a percentage of the X axis.
#' @param lineSize Size of horizontal lines.
#'
#' @return A `ggplot` plot object with AQI annotations.
#'
#' @importFrom rlang .data
#' @export

aqiAnnotation <- function(gg,
                          xlo,
                          xhi,
                          ylo,
                          yhi,
                          style = "bars_lines",
                          barWidth = 0.01,
                          lineSize = 0.5) {
  
  # For debugging --------------------------------------------------------------
  

  # Validate arguments ---------------------------------------------------------

  # Return immediately if 'style' is empty
  if ( is.null(style) || style == "" ) {
    return(gg)
  }
  
  if ( !lubridate::is.POSIXct(xlo) ) {
    stop("Argument 'xlo' is not of class 'POSIXct'")
  }
  
  if ( !lubridate::is.POSIXct(xhi) ) {
    stop("Argument 'xhi' is not of class 'POSIXct'")
  }
  
  # Accept any variation of style options with no required order
  validStyleOptions <- c("bars", "lines")
  styleOptions <- unlist(stringr::str_split(style, "_"))

  for ( option in styleOptions ) {
    if ( !option %in% validStyleOptions ) {
      stop(
        paste0(
          "Invalid style option: \"", option, "\". ",
          "The following 'style' argument options are supported: \"", 
          paste0(validStyleOptions, collapse = "|"), "\""
        )
      )
    }
  }
  
  # Prepare data ---------------------------------------------------------------

  if ( "bars" %in% styleOptions ) {
    
    # Get bar width
    barWidthSecs <- barWidth * as.numeric(difftime(xhi, xlo, units = "secs"))
    barLeft <- xlo
    barRight <- xlo + lubridate::dseconds(barWidthSecs)
    
    # Create data
    aqiStackedBarsData <- data.frame(
      xmin = rep(xlo, 6),
      xmax = rep((xlo + barWidthSecs), 6),
      ymin = c(ylo, AQI$breaks_24[2:6]),
      ymax = c(AQI$breaks_24[2:6], 1e6)
    )
    # Last bar must top out at yhi
    aqiStackedBarsData <- aqiStackedBarsData %>%
      dplyr::filter(.data$ymin < yhi)
    barCount <- nrow(aqiStackedBarsData)
    aqiStackedBarsData$ymax[barCount] <- yhi
    aqiStackedBarsColors <- AQI$colors[1:barCount]
    
  }
  
  if ( "lines" %in% styleOptions ) {
    
    # Create data
    aqiStackedLinesData <- data.frame(
      x = rep(xlo, 5),
      xend = rep(xhi, 5),
      y = c(AQI$breaks_24[2:6]),
      yend = c(AQI$breaks_24[2:6])
    )
    aqiLinesColors <- AQI$colors[2:6]
    
  }
  
  
  # Amend plot -----------------------------------------------------------------
  
  ggPlotBase <- gg
  
  if ( "bars" %in% styleOptions ) {
    
    ggPlotBase <- ggPlotBase + 
      
      geom_rect(
        data = aqiStackedBarsData,
        aes(
          xmin = .data$xmin,
          xmax = .data$xmax,
          ymin = .data$ymin,
          ymax = .data$ymax
        ),
        fill = aqiStackedBarsColors
      )
    
  }
  
  if ( "lines" %in% styleOptions ) {
    
    ggPlotBase <- ggPlotBase + 
      
      geom_segment(
        data = aqiStackedLinesData,
        aes(
          x = .data$x,
          xend = .data$xend,
          y = .data$y,
          yend = .data$yend
        ),
        color = aqiLinesColors,
        size = lineSize
      )  
    
  }

  # Return ---------------------------------------------------------------------
  
  return(ggPlotBase)
  
}





