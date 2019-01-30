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
#' @param timezone timezone for day start and end for averaging. If \code{NULL}, uses the timezone
#' used by the x-axis datetime scale. If the x-axis datetime scale has no timezone, it defaults to UTC. 
#' @param minHours Minimum number oof valid data hours required to calculate each daily statistic
#' @param width bar width in units of days. 
#' @param adjustylim if \code{TRUE}, the ylim of the plot will automatically be adjusted for the 
#' range of the daily means. 
#' @param missingDataBar if \code{TRUE}, a transparent gray bar will be plotted where data is missing.
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
#'   stat_AQILevel(color = NA, width = 3000) +
#'   stat_dailyAQILevel(alpha = .5, missingDataBar = FALSE, width = 1, size = 1) +
#'   facet_wrap(~monitorID)
#'   
#' ws_monitor <- airnow_loadLatest() 
#' ws_monitor <- monitor_subset(ws_monitor, monitorID = "160590004_01")
#' ggplot_pm25Timeseries(ws_monitor) +
#'   stat_dailyAQILevel()


stat_dailyAQILevel <- function(mapping = NULL, data = NULL, mv4Colors = FALSE,  
                               timezone = NULL, minHours = 18, width = .8,
                               adjustylim = FALSE, missingDataBar = TRUE, 
                               geom = "bar", position = "identity", na.rm = FALSE, 
                               show.legend = NA, inherit.aes = TRUE,
                               ...) {
  
  width <- 86400 * width 
  list(
    layer(
      stat = StatDailyAQILevel, data = data, mapping = mapping, geom = geom, 
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(mv4Colors = mv4Colors, timezone = timezone, minHours = minHours, na.rm = na.rm, 
                    adjustylim = adjustylim, width = width, missingDataBar = missingDataBar, ...)
    )
  )
}


StatDailyAQILevel <- ggproto("StatDailyAQILevel", Stat,
                        compute_group = function(data, 
                                                 scales, 
                                                 params,
                                                 mv4Colors,
                                                 timezone,
                                                 minHours,
                                                 na.rm,
                                                 adjustylim,
                                                 missingDataBar) {
                          
                          # Get timezone
                          if (is.null(timezone)) {
                            timezone <- ifelse (!is.null(attr(scales$x$breaks, "tzone")), attr(scales$x$breaks, "tzone"), "UTC")
                          }
                          
                          # Get date from numeric to posixct
                          df <- data
                          df$datetime <- as.POSIXct(data$x, tz = timezone, origin = "1970-01-01")
                          
                          # Get Daily Mean
                          dailyMeans <- df %>% 
                            mutate(date = strftime(.data$datetime, "%Y%m%d", tz = timezone)) %>%
                            group_by(date) %>% 
                            summarise(dailyMean = mean(.data$y), count = sum(!is.na(.data$y)) )
                          
                          
                          
                          dailyMeans$dailyMean <- ifelse(dailyMeans$count < minHours, NA, dailyMeans$dailyMean)
                          dailyMeans$datetime <- as.numeric(as.POSIXct(strptime(dailyMeans$date, "%Y%m%d", tz = timezone)) + lubridate::dhours(12))
                          
                          
                          data <- select(dailyMeans, 
                                         x = .data$datetime,
                                         y = .data$dailyMean)
                          
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
                          if ( adjustylim ) {
                            ymax <- max(data$y, na.rm = TRUE)
                            if ( ymax <= 50 ) {
                              yhi <- 50
                            } else if ( ymax <= 100 ) {
                              yhi <- 100
                            } else if ( ymax <= 200 ) {
                              yhi <- 200
                            } else if ( ymax <= 400 ) {
                              yhi <- 400
                            } else if ( ymax <= 600 )  {
                              yhi <- 600
                            } else if ( ymax <= 1000 )  {
                              yhi <- 1000
                            } else if ( ymax <= 1500 )  {
                              yhi <- 1500
                            } else {
                              yhi <- 1.05 * ymax
                            }
                            scales$y$limits <- c(0, yhi)
                          }
                          # Add missing data bars
                          if (missingDataBar) {
                            # Extend data to full extent
                            max_x <- max(data$x)
                            while(max_x < scales$x$get_limits()[2]) {
                              max_x <- max_x + 86400
                              data <- rbind(data, tibble(x = max_x,
                                                         y = NA,
                                                         aqi = NA,
                                                         colour = NA,
                                                         fill = NA))
                            }
                            min_x <- min(data$x) 
                            while(min_x > scales$x$get_limits()[2]) {
                              min_x <- min_x - 86400
                              data <- rbind(data, tibble(x = min_x,
                                                         y = NA,
                                                         aqi = NA,
                                                         colour = NA,
                                                         fill = NA))
                            }
                            
                            # Add gray bars
                            for (missingRow in which(is.na(data$y)) ) {
                              data[missingRow, "y"] <- scales$y$get_limits()[2]
                            }
                          }
                          
                          # Make sure there is no mean for today
                          date <- strftime(as.POSIXct(data$x, origin = "1970-01-01"), "%Y%m%d")
                          if ( strftime(lubridate::now(timezone), "%Y%m%d") %in% date ) {
                            data$y[which(date == strftime(lubridate::now(timezone), "%Y%m%d"))] <- NA
                            data$fill[which(date == strftime(lubridate::now(timezone), "%Y%m%d"))] <- NA
                          }
                          return(data)
                        },
                        required_aes = c("x", "y"),
                        finish_layer = function(self, data, params) {
                          # remove outline from missing data bars
                          data$colour <- ifelse(is.na(data$aqi), NA, data$colour)
                          data$fill <- ifelse(is.na(data$aqi), "#7F7F7F", data$fill)
                          data$alpha <- ifelse(is.na(data$aqi), 0.3, data$alpha)
                          data
                        }
)

