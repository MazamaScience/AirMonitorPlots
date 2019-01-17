#' @title Add pm25Timeseries legend to pm25Timeseries plot
#'
#' @description
#' Add a default legend to a timeseries plot. geom_pm25Points and stat_nowcast are
#' designed to work with this function, to create a tidy_timeseries plot. 
#'
#' @param legend.labels Labels for the legend. Must match names for color aesthetics. 
#' @param legend.title Title for the legend
#' @param color Vector of colors. This also affects the objects which the legend 
#' represents.
#' @param linetype Vector of linetypes for display.
#' @param shape Vector of shapes for display.
#' 
#' @import ggplot2
#' @export
#' 

legend_pm25Timeseries <- function(legend.labels = c("PM2.5", "NowCast"),
                                  legend.title = "",
                                  color = c("black", "black"),
                                  linetype = c(NA, 1),
                                  shape = c(16, NA)) {
  
  values <- color
  names(values) <- legend.labels
  
  list(
    # Add some more margin space above the plot
    theme(
      legend.position = "top"
    ),
    # Map to correct values with scale_color_manual
    scale_color_manual(name = legend.title, values = values),
    # Customize legend display
    guides(colour = guide_legend(override.aes = list(linetype = linetype,
                                     shape = shape)))
    
  )
 
  
  # Add annotation for legend
  # Using a bunch of 'grid' stuff...
  
}  
