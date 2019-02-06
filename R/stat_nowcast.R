#' @title Add NowCast values to a plot
#'
#' @description
#' This function calculates the NowCast version of the data, and adds it to a plot.
#' The default is to add a NowCast line. 
#'
#' @param mapping Set of aesthetic mappings created by \code{aes()}. If specified and 
#' \code{inherit.aes = TRUE} (the default), it is combined with the default mapping
#' at the top level of the plot. You must supply \code{mapping} if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options: 
#' if \code{NULL}, the default, the data is inherited from the plot data. 
#' A \code{data.frame} or other object, will override the plot data. 
#' A \code{function} will be called with a  single argument, the plot data.
#' The return value must be a \code{data.frame}, and will be used as the layer data.
#' @param version character identity specifying the type of nowcast algorithm to be used. 
#' For details see \link{monitor_nowcast}. 
#' @param includeShortTerm calculate preliminary NowCast values starting with the 2nd hour.
#' @param geom The geometic object to display the data
#' @param aqiColors if \code{TRUE}, AQI colors will be displayed. 
#' @param mv4Colors if \code{TRUE}, AQI colors from the monitoring V4 site will be used. 
#' Ignored if \code{aqiColors} is \code{FALSE}. 
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
#' ggplot_pm25Timeseries(PWFSLSmoke::Carmel_Valley,
#'                       startdate = 20160801, 
#'                       enddate = 20160810) + 
#'   geom_pm25Points() +
#'   stat_nowcast()


stat_nowcast <- function(
  mapping = NULL, 
  data = NULL, 
  version='pm',
  includeShortTerm=FALSE, 
  geom = "path",
  aqiColors = FALSE, 
  mv4Colors = FALSE,
  position = "identity", 
  na.rm = FALSE, 
  show.legend = NA, 
  inherit.aes = TRUE, 
  ...) {
  
  layer(
    stat = StatNowcast, 
    data = data, 
    mapping = mapping, 
    geom = geom, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(includeShortTerm = includeShortTerm, 
                  version = version, 
                  mv4Colors = mv4Colors, 
                  aqiColors = aqiColors, 
                  ...)
  )
  
}


StatNowcast <- ggproto(
  "StatNowcast", 
  StatIdentity,
  extra_params =  c("includeShortTerm", "version", "mv4Colors", "aqiColors", "na.rm"),
  # BEGIN setup_data function
  setup_data = function(data, params) {
    
    if (!params$version == "identity") {
      data$y <- .nowcast(data$y, params$version, params$includeShortTerm)
    }
    
    if ( params$aqiColors ) {
      
      # Add column for AQI level
      data$aqi <- .bincode(data$y, AQI$breaks_24, include.lowest = TRUE)
      if (!"colour" %in% names(data)) {
        if (params$mv4Colors) {
          data$colour <- AQI$mv4Colors[data$aqi]
        } else {
          data$colour <- AQI$colors[data$aqi] 
        }
      }
      
      if (!"fill" %in% names(data)) {
        if ( params$mv4Colors ) {
          data$fill <- AQI$mv4Colors[data$aqi]
        } else {
          data$fill <- AQI$colors[data$aqi]
        }
      }
      
    }
    
    return(data)
  },
  # END setup_data function
  required_aes = c("x", "y")
  
)

