#' @export
#' @import ggplot2
#' @importFrom rlang .data
#'
#' @title Create a Daily-Hourly plot for many monitors
#'
#' @description
#' Create a time series barplot showing PM2.5 data for the given monitors. The
#' overall plot is faceted by monitor, and each facet has two sets of columns:
#' one for daily levels, and one for hourly levels.
#'
#' The full range of data in \code{monitor} will be used unless both
#' \code{startdate} and \code{enddate} are specified.
#'
#' The timezone specified or, if \code{timezone = NULL}, that of the first
#' monitor encountered will be used for all time axes.
#'
#' @param monitor A \emph{mts_monitor} object.
#' @param startdate Desired start date (integer or character in ymd format or
#'   POSIXct).
#' @param enddate Desired end date (integer or character in ymd format or
#'   POSIXct).
#' @param id Optional vector of deviceDeploymentIDs used to filter the data.
#' @param columns Number of columns the faceted plot should have.
#' @param title The title of the plot. Defaults to specifying the types of
#'   data present in the plot.
#' @param timezone Olson timezone name for x-axis scale and date parsing. If
#'   NULL the timezone of the specified monitor will be used.
#' @param xLabel The x-axis label of the plot. Defaults to years present in
#'   data.
#' @param yLabel The y-axis label of the plot. Defaults to PM2.5.
#' @param hourlyDataType The type of hourly data to include in the plot. The
#'   options include "nowcast" (hourly nowcast values), "raw" (raw hourly values),
#'   or "none" (no hourly data at all).
#' @param hourlyBarWidth Fractional width of hourly bars relative to one hour.
#'   Defaults to 0.45. Values near 1 produce wide bars with little spacing,
#'   while smaller values produce narrower bars with more separation.
#' @param palette The ordered color palette used to represent each AQI
#'   category. Currently defaults to (and only accepts) "epa_aqi".
#' @param includeLegend Option to include a legend..
#'
#' @return A \strong{ggplot} plot of the given monitors and data.
#'
#' @examples
#' \dontrun{
#' library(AirMonitor)
#' library(AirMonitorPlots)
#'
#' # Fail gracefully if any resources are not available
#' try({
#'
#' now <- lubridate::now('America/Los_Angeles')
#' start <- now - lubridate::ddays(6)
#'
#' SF <-
#'   monitor_loadLatest() %>%
#'   monitor_filterByDistance(
#'     longitude = -122.42,
#'     latitude = 37.77,
#'     radius = 20000,
#'     count = 4
#'   ) %>%
#'   monitor_filterDate(
#'     startdate = start,
#'     enddate = now
#'   )
#'
#' monitor_ggDailyHourlyBarplot(SF)
#'
#' }, silent = FALSE)
#' }

monitor_ggDailyHourlyBarplot <- function(
    monitor,
    startdate = NULL,
    enddate = NULL,
    id = NULL,
    columns = 1,
    title = NULL,
    timezone = NULL,
    xLabel = NULL,
    yLabel = NULL,
    hourlyDataType = c("nowcast", "raw", "none"),
    hourlyBarWidth = 0.6,
    palette = "epa_aqi",
    includeLegend = FALSE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(monitor)

  # TODO: make function work with tidy monitor data
  #      Need to implement a `monitor_dailyStatistic()` function for tidy
  #      monitor data
  if ( !AirMonitor::monitor_isValid(monitor) ) {
    stop("This function can currently only take in a 'mts_monitor' object")
  }

  # Convert monitor to tidy structure
  mts_tidy <- monitor_toTidy(monitor)

  # If no ids are provided, use all deviceDeploymentIDs in monitor$meta
  if ( is.null(id) ) {
    id <- unique(monitor$meta$deviceDeploymentID)
    if ( length(id) > 30 ) {
      stop(
        paste0(
          "This monitor contains ", length(id), " deviceDeploymentIDs.\n\n",
          "Please use the 'id' argument to select 30 or fewer time series."
        )
      )
    }
  }

  # Check deviceDeploymentIDs
  if ( any(!id %in% unique(mts_tidy$deviceDeploymentID)) ) {
    invalidIDs <- id[which(!id %in% unique(mts_tidy$deviceDeploymentID))]
    stop(paste0(
      "Invalid ids specified. 'monitor' does not contain deviceDeploymentIDs: ",
      paste0(invalidIDs, collapse = ", ")
    ))
  }

  # Check hourlyDataType
  hourlyDataType <- match.arg(hourlyDataType)

  # Check hourlyBarWidth
  if ( !is.numeric(hourlyBarWidth) || length(hourlyBarWidth) != 1 || is.na(hourlyBarWidth) ) {
    stop("'hourlyBarWidth' must be a single numeric value.")
  }

  if ( hourlyBarWidth <= 0 || hourlyBarWidth > 1 ) {
    stop("'hourlyBarWidth' must be > 0 and <= 1.")
  }

  # Check timezone
  if ( !is.null(timezone) ) {
    if ( !timezone %in% OlsonNames() ) {
      stop("Invalid timezone")
    }
  } else {
    timezone <- unique(monitor$meta$timezone[1])
  }


  # ----- Set up data ----------------------------------------------------------

  # Get data from monitors

  singleMonitor <-
    monitor %>%
    AirMonitor::monitor_select(id)

  # Get time limits

  # Use full time range if startdate or enddate is missing
  if ( is.null(startdate) || is.null(enddate) ) {
    timeRange <- range(monitor$data$datetime)
    startdate <- timeRange[1]
    enddate <- timeRange[2]
  }

  dateRange <- MazamaCoreUtils::dateRange(
    startdate = startdate,
    enddate = enddate,
    timezone = timezone,
    unit = "day",
    ceilingEnd = TRUE
  )

  # ----- Transform data -------------------------------------------------------

  aqiActions <- AirMonitor::US_AQI$actions_eng
  aqiColors <- AirMonitor::US_AQI$colors_EPA
  aqiNames  <- AirMonitor::US_AQI$names_eng
  names(aqiColors) <- aqiNames
  aqiLevels <- aqiNames


  # Calculate daily data (or none)
  # TODO: Add ability to include only hourly values (no daily)
  includeDaily <- TRUE
  if (includeDaily) {

    dailyData <-
      singleMonitor %>%
      AirMonitor::monitor_dailyStatistic() %>%
      AirMonitor::monitor_filterDate(startdate, enddate, ceilingEnd = TRUE) %>%
      monitor_toTidy() %>%
      dplyr::mutate(
        aqiCategory = factor(
          cut(
            round(.data$pm25),
            AirMonitor::US_AQI$breaks_PM2.5_2024,
            include.lowest = TRUE,
            labels = aqiLevels
          ),
          levels = aqiLevels,
          ordered = TRUE
        )
      )

  } else {

    dailyData <- NULL

  }

  # Calculate the appropriate hourly values (or none)
  if ( hourlyDataType != "none" ) {

    if ( hourlyDataType == "nowcast" ) {

      hourlyData <-
        singleMonitor %>%
        AirMonitor::monitor_nowcast(includeShortTerm = TRUE) %>%
        AirMonitor::monitor_filterDate(startdate, enddate, ceilingEnd = TRUE) %>%
        monitor_toTidy() %>%
        dplyr::mutate(
          aqiCategory = factor(
            cut(
              round(.data$pm25),
              AirMonitor::US_AQI$breaks_PM2.5_2024,
              include.lowest = TRUE,
              labels = aqiLevels
            ),
            levels = aqiLevels,
            ordered = TRUE
          )
        )

    } else {

      # hourlyDataType == "raw"
      hourlyData <-
        singleMonitor %>%
        AirMonitor::monitor_filterDate(startdate, enddate, ceilingEnd = TRUE) %>%
        monitor_toTidy() %>%
        dplyr::mutate(
          aqiCategory = factor(
            cut(
              round(.data$pm25),
              AirMonitor::US_AQI$breaks_PM2.5_2024,
              include.lowest = TRUE,
              labels = aqiLevels
            ),
            levels = aqiLevels,
            ordered = TRUE
          )
        )
    }

  } else {

    hourlyData <- NULL

  }

  # ----- Set up labels --------------------------------------------------------

  if ( is.null(title) ) {

    if ( includeDaily ) {
      dailyPart <- "Daily (AQI)"
    } else {
      dailyPart <- NULL
    }

    if ( hourlyDataType == "nowcast" ) {
      hourlyPart <- "Hourly (NowCast)"
    } else if ( hourlyDataType == "raw" ) {
      hourlyPart <- "Hourly (raw)"
    } else {
      hourlyPart <- NULL
    }

    if ( includeDaily && hourlyDataType != "none" ) {
      titlePart <- paste(dailyPart, hourlyPart, sep = " and ")
    } else {
      titlePart <- paste0(dailyPart, hourlyPart)
    }

    title <- bquote(.(titlePart) ~ PM[2.5] ~ "Levels")

  }

  if ( is.null(xLabel) ) {
    yearPart <- paste(
      unique(lubridate::year(dailyData$datetime)),
      collapse = ", ")

    xLabel <- paste0("Date, midnight to midnight (", yearPart, ")")
  }

  if ( is.null(yLabel) ) {
    yLabel <- expression(paste("PM"[2.5] * " (", mu, "g/m"^3 * ")"))
  }

  # ----- Define time axis -----------------------------------------------------

  minDate <- lubridate::as_datetime(min(dailyData$datetime))
  maxDate <- lubridate::as_datetime(max(dailyData$datetime))

  timeSpan <- lubridate::as.interval(maxDate - minDate, minDate)
  numDays <- timeSpan %/% lubridate::days(1)

  if (numDays <= 14) {
    timeScale <- "day"
  } else if (numDays <= 30) {
    timeScale <- "2 days"
  } else if (numDays <= 150) {
    timeScale <- "week"
  } else {
    timeScale <- "month"
  }

  # datetime vector for date labels

  if ( is.null(hourlyData) ) {
    datetimeValues <- dailyData$datetime
  } else {
    datetimeValues <- hourlyData$datetime
  }

  datetimeValues <- datetimeValues %>% lubridate::with_tz(tzone = timezone)

  datetimeLabelsMajor <- unique(
    lubridate::floor_date(
      datetimeValues,
      unit = timeScale
    )
  ) + lubridate::dhours(12)

  datetimeLabelsMinor <- unique(
    lubridate::floor_date(
      datetimeValues,
      unit = "day"
    )
  )

  # ----- Plot data ------------------------------------------------------------

  gg <-
    ggplot2::ggplot(
      dailyData,
      ggplot2::aes_(x = ~ datetime, y = ~ pm25)
    ) +
    ggplot2::geom_col(
      data = hourlyData,
      ggplot2::aes_(fill = ~aqiCategory, color = ~ aqiCategory),
      width = 3600 * hourlyBarWidth,
      linewidth = 0,
      na.rm = TRUE,
      show.legend = includeLegend
    ) +
    ggplot2::geom_col(
      data = dailyData,
      ggplot2::aes_(x = ~ datetime + lubridate::dhours(12), fill = ~ aqiCategory),
      width = 86400,
      alpha = 0.3,
      color = "grey20",
      linewidth = .2,
      na.rm = TRUE,
      show.legend = includeLegend
    ) +

    ggplot2::scale_x_datetime(
      breaks = datetimeLabelsMajor,
      minor_breaks = datetimeLabelsMinor,
      date_labels = "%b %d",
      expand = c(0, 0)
    ) +

    ggplot2::labs(
      title = title,
      x = xLabel,
      y = yLabel
    ) +

    ggplot2::facet_wrap(~ locationName, ncol = columns) +

    theme_dailyHourlyBarplot_airfire(base_size = 12) +

    ggplot2::scale_fill_manual(
      name   = "Daily Air Quality Index (24hr AQI)",
      values = aqiColors,
      breaks = aqiLevels,          # all factor levels
      labels = aqiNames,
      drop   = FALSE,              # include unused factor levels
      na.translate = FALSE,        # omit NA from legend
      guide  = ggplot2::guide_legend(order = 1, reverse = TRUE)
    ) +
    ggplot2::scale_color_manual(
      name = "Hourly NowCast (actions to protect yourself)",
      values = aqiColors,
      breaks = aqiLevels,          # all factor levels
      labels = aqiActions,
      drop   = FALSE,              # include unused factor levels
      na.translate = FALSE,        # omit NA from legend
      guide = ggplot2::guide_legend(
        order = 2,
        reverse = TRUE,
        override.aes = list(color = NA, fill = NA) # blank
      )
    )


  return(gg)

}
