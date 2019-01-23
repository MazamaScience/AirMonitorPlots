#' @title Add NowCast values to a plot
#'
#' @description
#' This function calculates the NowCast values for the data, and adds  it to a plot.
#' The default is to add a NowCast line. 
#'
#' @param mapping Set of aesthetic mappings created by \code{aes()}. If specified and 
#' \code{inherit.aes = TRUE} (the default), it is combined with the default mapping
#' at the top level of the plot. You must supply \code{mapping} if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options: 
#' if \code{NULL}, the default, the data is inherited from the plot data. 
#' A \code{data.frame} or other object, will override the plot data. 
#' A \code{function} will be called witha  single argument, the plot data.
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


stat_nowcast <- function(mapping = NULL, data = NULL, version='pm',
                         includeShortTerm=FALSE, geom = "path",
                         aqiColors = FALSE, mv4Colors = FALSE,
                         position = "identity", na.rm = FALSE, show.legend = NA, 
                         inherit.aes = TRUE, 
                         ...) {
  
  layer(
    stat = StatNowcast, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(includeShortTerm = includeShortTerm, version = version, 
                  mv4Colors = mv4Colors, aqiColors = aqiColors, ...)
  )
  
}


StatNowcast <- ggproto("StatNowcast", Stat,
                       compute_group = function(data, 
                                                scales, 
                                                params, 
                                                version, 
                                                includeShortTerm,
                                                mv4Colors,
                                                aqiColors) {
                         # Set parameters based on version
                         if (version =='pm') {
                           numHrs <- 12
                           weightFactorMin <- 0.5
                           digits <- 1
                         } else if (version =='pmAsian') {
                           numHrs <- 3
                           weightFactorMin <- 0.1
                           digits <- 1
                         } else if (version == 'ozone') {
                           numHrs <- 8
                           weightFactorMin <- NA  # negative values adjusted up to 0 in .weightFactor()
                           digits <- 3  # NOTE: digits=3 assumes Ozone values given in ppm; update to 0 if values given in ppb
                         } else if (version == 'identity') {
                           NULL
                         }
                         
                         
                         # Apply nowcast to each monitor in dataframe
                         # NOTE:  We need as.data.frame for when there is only a single column of data.
                         # NOTE:  We truncate, rather than round, per the following:
                         # NOTE:  https://forum.airnowtech.org/t/the-nowcast-for-ozone-and-pm/172
                         if (!version == "identity") {
                           data$y <- .nowcast(data$y, numHrs, weightFactorMin, includeShortTerm)
                         }
                         
                         if ( aqiColors ) {
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
                         }
                         
                         return(data)
                         
                       },
                     
                     required_aes = c("x", "y")
)

# ----- Helper Functions ------------------------------------------------------

.nowcast <- function(x, numHrs, weightFactorMin, includeShortTerm) {
  
  if ( includeShortTerm ) {
    firstHr <- 2
  } else {
    firstHr <- numHrs
  }
  
  # Start at the end end of the data (most recent hour) and work backwards
  # The oldest hour for which we can calculate nowcast is numHrs, unless includeShortTerm=TRUE
  # in which case we can go back to the 2nd hour.
  for ( i in length(x):firstHr ) {
    
    # Apply nowcast algorithm to numHrs data points in order with more recent first
    concByHour <- x[i:max(1, i-numHrs+1)]
    
    if ( sum( is.na(concByHour[1:3]) ) >= 2 ) {
      
      # If two or more of the most recent 3 hours are missing, no valid Nowcast will be reported
      
      x[i] <- NA
      
    } else if ( is.na(concByHour[1]) ) {
      
      # If the current hour is missing, no valid Nowcast will be reported
      
      # NOTE:  This conflicts with the algorithm as described here:
      # NOTE:    https://www3.epa.gov/airnow/ani/pm25_aqi_reporting_nowcast_overview.pdf
      # NOTE:
      # NOTE:  But experience shows that NowCast replacements for missing values for missing
      # NOTE:  PM2.5 values are very problematic.
      # NOTE:
      # NOTE:  The Wikipedia page: https://en.wikipedia.org/wiki/NowCast_(air_quality_index)
      # NOTE:  has the following statement without citation:
      # NOTE:    "Because the most recent hours of data are weighted so heavily in the NowCast when
      # NOTE:    PM levels are changing, EPA does not report the NowCast when data is missing for c1 or c2."
      # NOTE:
      # NOTE:  We take a compromise approach and only invalidate NowCast when data is missing for c1.
      
      x[i] <- NA
      
    } else {
      
      # Calculate the weight factor according to the type of air quality data
      weightFactor <- .weightFactor(concByHour, weightFactorMin)
      
      # NOTE:  We need to create vectors so that we can sum at the end with na.rm=TRUE
      
      weightedConcs <- rep(as.numeric(NA),numHrs)
      weightFactors <- rep(as.numeric(NA),numHrs)
      
      # Loop over hours to get individual elements
      for (j in 1:numHrs) {
        if ( !is.na( concByHour[j]) ) {
          weightedConcs[j] <- concByHour[j] * weightFactor^(j-1)
          weightFactors[j] <- weightFactor^(j-1)
        }
      }
      
      x[i] <- sum(weightedConcs, na.rm=TRUE) / sum(weightFactors, na.rm=TRUE)
      
    }
  }
  
  # Set missing values when there are not enough preceding hours
  x[1:(firstHr-1)] <- NA
  
  return(x)
}

# Calculate the weight factor ('w' in the nowcast formula)
#  concByHour: vector of hourly concentration values
#  weightFactorMin (optional): wight factor minimum
# Assumes concByHour has at least one valid value to calculate min & max. In fact, .nowcast won't even call 
# this function if more than one of the three most recent hours is invalid.
.weightFactor <- function(concByHour, weightFactorMin) {
  
  min <- min(concByHour, na.rm=TRUE)
  max <- max(concByHour, na.rm=TRUE)
  
  # Calculate weight factor
  # NOTE:  https://forum.airnowtech.org/t/the-nowcast-for-ozone-and-pm/172 says that there is "no minimum
  # NOTE:    weight factor" for ozone; however, we limit the value to zero since otherwise it would be possible
  # NOTE:    to get negative weights, even as large as -Inf (i.e. if min<0 & max=0).
  # NOTE:  Otherwise, we don't worry about negatives, per the following:
  # NOTE:    https://forum.airnowtech.org/t/how-does-airnow-handle-negative-hourly-concentrations/143
  weightFactor <- 1-(max-min)/max
  weightFactor <- min(weightFactor, 1, na.rm=TRUE)
  weightFactor <- max(weightFactor, weightFactorMin, 0, na.rm=TRUE)
  
  return(weightFactor)
  
}
