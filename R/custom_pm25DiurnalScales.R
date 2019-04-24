#' @title PWFSL PM2.5 diurnal scales
#'
#' @description
#' Add PWFSL-style x-axis and y-axis scales suitable for a plot showing PM2.5
#' data as a funciton of hour of the day.
#'
#' @param data pm25 timeseries data. Should match the default dataset of the
#'   plot.
#' @param ylim custom y-axis limits. This function will apply a default limit
#'   depending on the data.
#' @param xlab Custom x-axis label. If \code{NULL} a default xlab will be
#'   generated.
#' @param ylab Custam y-axis label.
#' @param yexp Vector of range expansion constants used to add some padding
#'   around the data on the y-axis, to ensure that they are placed some distance
#'   away from the axes.
#' @param xexp Vector of range expansion constants used to add some padding
#'   around the data on the x-axis, to ensure that they are placed some distance
#'   away from the axes.
#' @param offsetBreaks if \code{TRUE}, x-axis ticks and guides are offset by
#'   0.5.
#'
#' @importFrom rlang .data
#' @import ggplot2
#' @export
custom_pm25DiurnalScales <- function(
  data = NULL,
  ylim = NULL,
  xlab = NULL,
  ylab = "PM2.5 (\u00b5g/m3)",
  yexp = c(0.05, 0.05),
  xexp = c(0.05, 0.05),
  offsetBreaks = FALSE
) {


  # Validate parameters --------------------------------------------------------

  if (monitor_isMonitor(data)) {
    data <- monitor_toTidy(data)
  } else if (monitor_isTidy(data)) {
    data <- data
  } else {
    stop("data must be either a ws_monitor object or ws_tidy object.")
  }


  # Calculate axis limits ----------------------------------------------------

  # Default to well defined y-axis limits for visual stability
  if (is.null(ylim)) {
    ylo <- 0
    ymax <- max(data$pm25, na.rm = TRUE)

    yhi <- dplyr::case_when(
      ymax <= 50   ~ 50,
      ymax <= 100  ~ 100,
      ymax <= 200  ~ 200,
      ymax <= 400  ~ 400,
      ymax <= 600  ~ 600,
      ymax <= 1000 ~ 1000,
      ymax <= 1500 ~ 1500,
      TRUE         ~ 1.05 * ymax
    )

  } else {
    # Standard y-axis limits
    ylo <- ylim[1]
    yhi <- ylim[2]
  }

  xmin <- 0 - (23 * xexp[1])
  xmax <- 23 + (23 * xexp[2])


  # Calculate breaks -----------------------------------------------------------

  ## NOTE:
  #  `ifelse` is not used, because the condition `offsetBreaks` is length 1,
  #  which means the output of `ifelse` would also be a 1 element vector.

  if (offsetBreaks) {
    breaks <- seq(-0.5, 22.5, by = 3)
  } else {
    breaks <- seq(0, 22, by = 3)
  }

  if (offsetBreaks) {
    minor_breaks <- seq(-0.5, 22.5, by = 1)
  } else {
    minor_breaks <- seq(0, 22, by = 1)
  }


  # Add scales -----------------------------------------------------------------

  list(
    scale_x_continuous(
      breaks = breaks,
      minor_breaks = seq(0, 23, by = 1),
      labels = c("midnight", "3am", "6am", "9am", "Noon", "3pm", "6pm", "9pm"),
      limits = c(xmin, xmax),
      expand = c(0, 0)
    ),
    scale_y_continuous(
      limits = c(ylo - (yexp[1] * yhi), yhi + (yexp[2] * yhi)),
      expand = c(0, 0)
    ),
    ylab(ylab),
    xlab(xlab
    )
  )

}
