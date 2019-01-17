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
