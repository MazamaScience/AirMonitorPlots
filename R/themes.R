#' @title Theme for PWFSL plots
#'
#' @description
#' Applies the package standard theme to a \emph{ggplot} plot object.
#'
#' @param base_size Base font size.
#' @param base_family Base font family.
#'
#' @return A \emph{ggplot} theme.
#'
#' @import ggplot2
#' @export
theme_pwfsl <- function(
  base_size = 11,
  base_family = ""
) {

  theme_classic(
    base_size = base_size,
    base_family = base_family
  ) +

  theme(

    # All text is black
    text = element_text(color = "black"),

    # A little white space around the edges
    plot.margin = margin(
      unit(2.0 * base_size, "pt"),    # Top
      unit(1.0 * base_size, "pt"),    # Right
      unit(1.5 * base_size, "pt"),    # Bottom
      unit(1.0 * base_size, "pt")     # Left
    ),

    # Axes
    axis.title = element_text(
      size = 1.2 * base_size
    ),
    axis.text = element_text(
      size = 1.0 * base_size
    ),
    # Y-axis
    ###axis.line.y = element_blank(),
    axis.title.y = element_text(
      margin = margin(r = 1.0 * base_size)
    ),
    ###axis.ticks.y = element_blank(),
    axis.text.y = element_text(
      margin = margin(r = 0.5 * base_size)
    ),

    # X-axis
    ###axis.line.x = element_blank(),
    axis.title.x = element_text(
      margin = margin(t = 1.0 * base_size)
    ),
    ###axis.ticks.x = element_blank(),
    axis.text.x = element_text(
      margin = margin(t = 1.0 * base_size)
    ),

    # Legend
    legend.text = element_text(
      size = 1.0 * base_size,
      face = "italic",
      margin = margin(r = 50)
    ),

    # Box outline and grid lines
    panel.border = element_rect(fill = NA),

    panel.grid.major = element_line(
      linetype = "dotted",
      size = 0.3,
      colour = "grey"
    ),
    panel.grid.minor.x = element_line(
      linetype = "dotted",
      size = 0.1,
      colour = "grey"
    ),
    panel.grid.minor.y = element_blank(),

    # Title
    plot.title = element_text(
      color = "black",
      size = 1.5 * base_size,
      hjust = 0.5,
      vjust = 5,
      face = "bold"
    )
  )
}

#' @keywords internal
#'
#' @title Custom styling for plots based on size
#'
#' @description
#' Applies a theme to a \emph{ggplot} plot object. This theme is intended for
#' use when generating plots for the PWFSL monitoring site.
#'
#' @param size \code{small} or \code{large}. \code{style = small} is appropriate
#'   for plots 450x450px or smaller; \code{style = large} is appropriate for
#'   plots larger than 450x450px.
#'
#' @return A \emph{ggplot} theme.
#'
#' @import ggplot2
#' @export
theme_custom_size <- function(size = "large") {
  if (size == "large") {
    theme()
  } else {
    theme(
      plot.title = element_text(margin = margin(0, 0, 0, 0)),
      axis.title = element_text(margin = margin()),
      axis.text = element_text(margin = margin(0, 0, 0, 0)),
      legend.margin = margin(0, 0, 0, 0),
      axis.title.x.bottom = element_blank()
    )
  }
}


#' @title Theme for PWFSL timeseries plots for use in the monitoring site
#'
#' @description
#' Applies a theme to a \emph{ggplot} plot object. This theme is intended for
#' use with the \code{\link{monitor_ggTimeseries}} function and generates plots
#' suitable for the PWFSL monitoring site. It is suited to display of 1-4 weeks
#' of data.
#'
#' @param size \code{small} or \code{large}. \code{style = small} is appropriate
#'   for plots 450x450px or smaller; \code{style = large} is appropriate for
#'   plots larger than 450x450px.
#'
#' @return A \emph{ggplot} theme.
#'
#' @import ggplot2
#' @export
theme_timeseriesPlot_pwfsl <- function(size = "large") {

  theme(
    legend.position = "top",
    legend.text.align = 1,
    panel.grid.major = element_line(
      size = 0.5,
      linetype = 2
    ),
    panel.grid.minor.x = element_line(
      size = 0.5,
      linetype = 3,
      color = "gray90"
    )
  ) +
  theme_custom_size(size = size)

}


#' @title Theme for PWFSL daily barplot for use in the monitoring site
#'
#' @description
#' Applies a theme to a \emph{ggplot} plot object. This theme is intended for
#' use with the \code{\link{monitor_ggDailyBarplot}} function and generates
#' plots suitable for the PWFSL monitoring site. It is suited to display of 1-4
#' weeks of data.
#'
#' @param size \code{small} or \code{large}. \code{style = small} is appropriate
#'   for plots 450x450px or smaller; \code{style = large} is appropriate for
#'   plots larger than 450x450px.
#'
#' @return A \emph{ggplot} theme.
#'
#' @import ggplot2
#' @export
theme_dailyBarplot_pwfsl <- function(size = "large") {

  theme(
    axis.title.x.bottom = element_blank(),
    axis.line.x.bottom = element_blank(), # remove line on x-axis
    panel.border = element_blank(),       # remove box around plot
    panel.grid = element_blank(),         # remove background grid lines
    axis.ticks.x.bottom = element_blank() # remove x-axis ticks
  ) +
  theme_custom_size(size = size)

}


#' @title Theme for PWFSL dailyByHour plot for use in the monitoring site
#'
#' @description
#' Applies a theme to a \emph{ggplot} plot object. This theme is intended for
#' use with the \code{\link{monitor_ggDailyByHour}} function and generates plots
#' suitable for the PWFSL monitoring site.
#'
#' @param size \code{small} or \code{large}. \code{style = small} is appropriate
#'   for plots 450x450px or smaller; \code{style = large} is appropriate for
#'   plots larger than 450x450px.
#'
#' @return A \emph{ggplot} theme
#'
#' @import ggplot2
#' @export
theme_dailyByHour_pwfsl <- function(size = "large") {

  theme(
    legend.key.size = unit(1, "cm"),
    legend.position = "top",
    legend.text.align = 1
  ) +
  theme_custom_size(size = size)

}


#' @title Theme for PWFSL clock plot
#'
#' @description
#' Applies a theme to a \emph{ggplot} plot object. This theme is intended for
#' use with the \code{\link{monitor_ggClockPlot}} function and generates plots
#' suitable for the PWFSL monitoring site.
#'
#' @param size \code{small} or \code{large}. \code{style = small} is appropriate
#'   for plots 450x450px or smaller; \code{style = large} is appropriate for
#'   plots larger than 450x450px.
#'
#' @return A \emph{ggplot} theme.
#'
#' @import ggplot2
#' @export

theme_clockPlot_pwfsl <- function(size = "large") {

  theme(
    axis.title = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.text.y =  element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.length = unit(0.5, "cm"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(linetype = 1)
  ) +
  theme_custom_size(size = size)

}


#' @title Theme for PWFSL Daily Hourly Barplot
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

theme_dailyHourlyBarplot_pwfsl <- function(
  base_size = 11,
  base_family = ""
) {

  half_line <- base_size / 2

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

      axis.ticks = element_line(colour = "grey20"),
      axis.ticks.length = unit(half_line / 4, "pt"),

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
      strip.text = element_text(size = rel(.9)),
      legend.title = element_text(size = rel(1)),
      legend.text = element_text(size = rel(.7))
    )

}
