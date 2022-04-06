#' @title Theme for "monitoring site" daily barplots
#'
#' @description
#' Applies a theme to a **ggplot** plot object.
#' This theme is intended for use with the `dailyBarplot()` function and
#' generates plots suitable for the PWFSL monitoring site. It is suited to
#' display of 1-4 weeks of data.
#'
#' @param base_size Base font size.
#' @param base_family Base font family.
#'
#' @return A **ggplot** theme
#'
#' @import ggplot2
#' @export

theme_dailyBarplot_airfire <- function(base_size = 11,
                                     base_family = "") {
  
  theme_classic(
    base_size = base_size,
    base_family = base_family
  ) + 
    
    theme(
      
      # All text is black
      text = element_text(color = "black"),
      
      # A little white space around the edges
      plot.margin = margin(
        unit(4.0 * base_size, "pt"),    # Top
        unit(2.0 * base_size, "pt"),    # Right
        unit(2.0 * base_size, "pt"),    # Bottom
        unit(2.0 * base_size, "pt")     # Left
      ),
      
      # Y-axis
      #axis.line.y = element_blank(),
      axis.title.y = element_text(
        size = 1.0 * base_size,
        margin = margin(r = 1.0 * base_size)
      ),
      #axis.ticks.y = element_blank(),
      axis.text.y = element_text(
        size = 1.0 * base_size,
        margin = margin(r = 0.5 * base_size)
      ),
      
      # X-axis
      axis.line.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_text(
        size = 1.0 * base_size,
        margin = margin(t = 1.0 * base_size)
      ),
      
      # Title
      plot.title = element_text(
        color = "black",
        size = 1.5 * base_size,
        hjust = 0.5
      )
      
    )
  
}
