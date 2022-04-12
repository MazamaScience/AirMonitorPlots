#' @export
#' @importFrom rlang .data
#'
#' @title Calendar plot
#'
#' @description Annual calendar view of a daily reading using a selected input.
#'
#' @param monitor A \emph{mts_monitor} object.
#' @param id deviceDeploymentID to include in the plot. This can be NULL if
#'   \code{monitor} only has one unique deviceDeploymentID.
#' @param ncol Number of columns in the plot.
#' @param title Optional title.
#' @param discrete Logical specifying use of a discrete color scale.
#' @param breaks Color scale break points.
#' @param break_labels Scale breaks names.
#' @param aspect_ratio Plot aspect ratio.
#' @param legend_title Optional title used in the legend.
#' @param stat Statistic used for daily aggregation (default: "mean").
#'
#' @return ggobject
#'
monitor_ggCalendar <- function(
  monitor = NULL,
  id = NULL,
  ncol = 4,
  title = NULL,
  discrete = TRUE,
  breaks = NULL,
  break_labels = NULL,
  aspect_ratio = 1,
  legend_title = NULL,
  stat = "mean"
) {

  # ----- Validate parameters ------------------------------------------------

  if ( !AirMonitor::monitor_isValid(monitor) )
    stop("Parameter 'monitor' is not a valid 'mts_monitor' object.")

  if ( AirMonitor::monitor_isEmpty(monitor) )
    stop("Parameter 'monitor' has no data")

  # Use first monitor if undefined
  if ( is.null(id) ) {
    warning("Undefined deviceDeploymentID: Using first monitor")
    id <- monitor$meta$deviceDeploymentID[1]
  }

  # ----- Define the data used -----------------------------------------------

  monitor <-
    monitor %>%
    AirMonitor::monitor_select(id) %>%
    AirMonitor::monitor_dailyStatistic(get(stat))

  # Always specify local timezones!
  timezone <- monitor$meta$timezone

  # Create data frame
  df <- monitor$data

  # Fill missing dates # CHECK IF LUBRIDATE CAN BE USED
  df <-
    tidyr::complete(
      data = df,
      datetime = seq(
        from = as.POSIXct(paste0(strftime( df$datetime,
                                           format = "%Y",
                                           tz = timezone )[2], "-01-01"),
                          tz = timezone),
        to = as.POSIXct(paste0(strftime( df$datetime,
                                         format = "%Y",
                                         tz = timezone )[2], "-12-31"),
                        tz = timezone ),
        by = "1 day"
      )
    )


  # ----- Prepare plot data --------------------------------------------------

  # Rename the data column to "pm25"
  names(df)[2] <- "pm25"

  # Create calendar plot handler data frame
  df$datetime   <- zoo::as.Date(df$datetime, tz = timezone)  # format date
  df$day        <- as.numeric(strftime(df$datetime, format = "%d", tz = timezone))
  df$yearmonth  <- zoo::as.yearmon(df$datetime, tz = timezone)
  df$yearmonthf <- factor(df$yearmonth)
  df$week       <- as.numeric(strftime(df$datetime, format = "%W", tz = timezone))
  df$year       <- as.numeric(strftime(df$datetime, format = "%Y", tz = timezone))
  df$month      <- as.numeric(strftime(df$datetime, format = "%m", tz = timezone))
  df$monthf     <- months.Date(df$datetime, abbreviate = TRUE)
  df$weekdayf   <- weekdays.Date(df$datetime, abbreviate = TRUE)
  df$weekday    <- as.numeric(strftime(df$datetime, format = "%d",tz = timezone))
  df$monthweek  <- as.numeric(NA) # placeholder
  df$weekd      <- ordered(df$weekdayf,
                           levels = c( "Mon", "Tue", "Wed",
                                       "Thu", "Fri", "Sat", "Sun" ))

  # Compute week number for each month
  df <- plyr::ddply( .data = df,
                     .variables = plyr::.(.data$yearmonthf),
                     .fun = transform,
                     monthweek = 1 + .data$week - min(.data$week) )

  # Capture only whats needed
  df <- df[, c( "year", "yearmonthf","monthf",
                "week", "monthweek", "weekdayf",
                "weekd", "day", "pm25" )]

  # ----- Set plot defaults --------------------------------------------------

  if ( is.null(legend_title) ) {
    legend_title <- "PM2.5 (\u03bcg / m\u00b3)"
  }
  if ( is.null(title) ) {
    # Create the title
    title <- paste0(unique(range(df$year)), ": ", monitor$meta$deviceDeploymentID)
  }

  # Determine fill type
  if ( discrete ) { # Discrete color scale
    if (is.null(breaks) ) {
      breaks <- c(0, 12, 35, 55, 75, 1000)
      labels <- c("0-12", "12-35", "35-55", "55-75", ">75")
    }
    if ( length(breaks) - length(labels) != 1 ) {
      warning("Break-labels and breaks length must differ by 1.")
      labels <- NULL
    }
    scale_fill <- ggplot2::scale_fill_discrete(na.value = "white")
    fill <- cut( df$pm25,
                 breaks = breaks,
                 labels = labels )
  } else { # Continuous color scale
    scale_fill <- ggplot2::scale_fill_continuous(na.value = "white")
    fill = df$pm25
  }

  # ----- Create plot --------------------------------------------------------

  gg <-
    ggplot2::ggplot(
      df,
      ggplot2::aes(
        stats::reorder(.data$monthweek, dplyr::desc(.data$monthweek) ),
        .data$weekd,
        fill = fill
      )
    ) +
    ggplot2::geom_tile(color = "grey88", size = 0.5) +
    ggplot2::facet_wrap( drop = TRUE,
                         ncol = ncol,
                         dir = "h",
                         factor(monthf, levels = month.abb) ~ . ) +
    ggplot2::labs( title = title,
                   fill = legend_title ) +
    ggplot2::geom_text( ggplot2::aes(label = .data$day),
                        size = 3,
                        fontface = "bold" ) +
    ggplot2::theme_classic() +
    ggplot2::theme( axis.title.y = ggplot2::element_blank(),
                    axis.text.y = ggplot2::element_blank(),
                    axis.text.x = ggplot2::element_text(size = 7),
                    axis.ticks.y = ggplot2::element_blank(),
                    axis.title.x = ggplot2::element_blank(),
                    axis.line.y = ggplot2::element_blank(),
                    legend.position = "bottom",
                    aspect.ratio = aspect_ratio,
                    legend.text = ggplot2::element_text(size = "8") ) +

    ggplot2::coord_flip() +
    scale_fill # Add the determined scale

  # ----- Return ------------------------------------------------------------

  return(gg)

}

# ===== Debugging ==========================================================

if (FALSE) {

  monitor <- AirMonitor::NW_Megafires
  id <- monitor$meta$deviceDeploymentID[1]
  ncol = 4
  discrete = TRUE
  aspect_ratio = 1

}

