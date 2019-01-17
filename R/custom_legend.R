#' @title Add a custom legend to a plot
#'
#' @description
#' Add a default legend to a plot. 
#'
#' @param title Legend title.
#' @param labels Labels for legend elements
#' @param aesthetics List of aesthetic for legend elements. Each 
#' list item should have the name of the aesthetic (like \code{linetype} or \code{color}), 
#' and the value should be a vector the same length as \code{labels}.
#' @param theme_args List of arguments to be passed on to \code{theme}, such as 
#' \code{legend.position}. 
#' @param ... Additional arguments passed on to \code{guide_legend()}.
#' 
#' @import ggplot2
#' @export
#' 
#' @example 
#' ws_monitor <- airnow_loadLatest()
#' ws_tidy <- monitor_toTidy(ws_monitor)
#' tidyMonitor <- dplyr::filter(ws_tidy, monitorID == "410432002_01")
#' ggplot_pm25Timeseries(tidyMonitor) +
#'   geom_pm25Points() + 
#'   stat_nowcast() +
#'   gg_legend(title = "",
#'             labels = c("PM2.5", "NowCast"),
#'             aesthetics = list(color = c(1,1),
#'                               alpha = c(.3, 1),
#'                               linetype = c(NA, 1),
#'                               shape = c(16, NA)),
#'             theme_args = list(legend.position = "top")
#'   )



custom_legend <- function(title = "",
                          labels = NULL,
                          aesthetics = NULL,
                          theme_args = list(
                            legend.position = "right",
                            legend.direction = "vertical"
                          ),
                          ...) {
  
  # Create mapping to force legend
  # NOTE:  ggplot2 only adds a legend for aesthetics that have been mapped to 
  # NOTE:  values, so here we map aesthetics to values, the 'labels'. Then, 
  # NOTE:  add them to the plot but later specify their color to be 'transparent' 
  # NOTE:  so they don't show up. Finally, we add the legend, adding the custom 
  # NOTE:  aesthetics. 
  mappings <- list()
  for (legend_item in labels) {
    mapping <- aes(color = !!legend_item)
    mappings[[legend_item]] <- 
      list(
        stat_identity(mapping = mapping, geom = "line"),
        stat_identity(mapping = mapping, geom = "point")
      )
  }
  
  # Map to invisible
  values <- rep("transparent", length(labels))
  names(values) <- labels
  scale <- scale_color_manual(name = title, 
                              values = values)
  
  # The legend, with desired aesthetics displayed
  guide <- guides(color = guide_legend(
    title = title,
    override.aes = aesthetics,
    ...
  ))
  
  # Add theme arguments
  custom_theme <- do.call(theme, theme_args)
  
  # Now add it all to the plot 
  list(
    mappings,
    scale,
    guide,
    custom_theme
  )
  
}