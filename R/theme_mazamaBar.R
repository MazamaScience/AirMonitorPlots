#' @title Theme tailored for use with bar plots in the style of Mazama Science
#'
#' @description
#' Applies a theme to a **ggplot** plot object. This is a minimal theme tailored
#' for faceted bar plots.
#'
#' @param base_size Base font size.
#' @param base_family Base font family.
#'
#' @return A **ggplot** theme
#'
#' @import ggplot2
#' @export
#'
#' @examples
#'
theme_mazamaBar <- function(base_size = 11, base_family = "") {

  half_line <- base_size/2

  # TODO: Create theme object that can be used across the package
  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = base_family
  ) +

    ggplot2::theme(
      strip.background = element_rect(fill = "#E0E0E0"),

      panel.border = element_rect(color = "grey20", fill = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_line(linetype = 3, color = 'gray70'),
      panel.grid.major.y = element_line(linetype = 3, color = 'gray40'),
      panel.grid.minor.y = element_blank(),

      axis.ticks.y = element_line(colour = "grey20"),
      axis.ticks.length = unit(half_line/4, "pt"),

      legend.position = "top",
      legend.justification = c(1, 1),
      legend.direction = "vertical",
      legend.box.background = element_rect(color = "black"),
      legend.spacing = unit(0.1, "cm"),

      plot.title = element_text(size = rel(2)),
      axis.title = element_text(size = rel(1.4)),
      axis.title.x = element_text(size = rel(.9)),
      axis.title.y = element_text(size = rel(.9)),
      axis.text.x = element_text(size = rel(1), angle = 30, hjust = 1),
      axis.text.y = element_text(size = rel(1)),
      strip.text = element_text(size = rel(1)),
      legend.title = element_text(size = rel(1)),
      legend.text = element_text(size = rel(.7))
    )

}
