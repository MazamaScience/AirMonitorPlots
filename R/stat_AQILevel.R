#' @title Add AQI colors to a plot
#'
#' @description
#' This function calculates the AQI PM25 categories for the data, and colors the data
#' by AQI cateogry when it is added to a plot. The default is to add them as bars. 
#' 
#' @param mapping Set of aesthetic mappings created by \code{aes()}. If specified and 
#' \code{inherit.aes = TRUE} (the default), it is combined with the default mapping
#' at the top level of the plot. You must supply \code{mapping} if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options: 
#' if \code{NULL}, the default, the data is inherited from the plot data. 
#' A \code{data.frame} or other object, will override the plot data. 
#' A \code{function} will be called witha  single argument, the plot data.
#' The return value must be a \code{data.frame}, and will be used as the layer data.
#' @param mv4Colors If \code{TRUE}, use the colors used in the monitoring v4 site. Otherwise,
#' use the "official" AQI colors. 
#' @param geom The geometic object to display the data
#' @param position Position adjustment, either as a string, or the result of a call to a
#' position adjustment function. 
#' @param na.rm remove NA values from data
#' @param show.legend logical indicating whether this layer should be included in legends.
#' @param inherit.aes if \code{FALSE}, overrides the default aesthetics, rather than combining with them. 
#' This is most useful for helper functions that define both data and the aesthetics and
#' shouldn't inherit behaviour from the default plot specificatino, eg \code{borders()}.
#' @param ... additional arguments passed on to \code{layer()}, such as aesthetics. 
#'
#' @import ggplot2
#' @export
#' 
#' @examples 
#' ws_monitor <- airsis_loadLatest()
#' ggplot_pm25Timeseries(ws_monitor) +
#'   stat_AQILevel() + 
#'   stat_AQILevel(geom = "point", mv4Colors = TRUE)


stat_AQILevel <- function(mapping = NULL, data = NULL, mv4Colors = FALSE,
                          geom = "bar", position = "identity", na.rm = FALSE, 
                          show.legend = NA, inherit.aes = TRUE, 
                          ...) {
  
  list(
    layer(
      stat = StatAQILevel, data = data, mapping = mapping, geom = geom, 
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(mv4Colors = mv4Colors, ...)
    )
  )
}


StatAQILevel <- ggproto("StatAQILevel", Stat,
                        compute_group = function(data, 
                                                 scales, 
                                                 params,
                                                 mv4Colors) {
                          
                          # Add column for AQI level
                          data$aqi <- .bincode(data$y, AQI$breaks_24, include.lowest = TRUE)
                          if (!"colour" %in% names(data)) {
                            if (mv4Colors) {
                              data$colour <- AQI$mv4Colors[data$aqi]
                            } else {
                              data$colour <- AQI$colors[data$aqi] 
                            }
                          }
                          if (!"fill" %in% names(data)) {
                            if (mv4Colors) {
                              data$fill <- AQI$mv4Colors[data$aqi]
                            } else {
                              data$fill <- AQI$colors[data$aqi]
                            }
                            
                          }
                          return(data)
                        },
                        required_aes = c("x", "y")
)

