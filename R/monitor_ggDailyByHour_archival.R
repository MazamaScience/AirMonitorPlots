#' @title Create an archival diurnal plot for one or more monitors
#'
#' @description
#' This function assembles various layers to create a production-ready archival
#' diurnal plot for a single monitor.
#'
#' The full range of data in \code{monitor} will be used unless both
#' \code{startdate} and \code{enddate} are specified.
#'
#' @inheritParams ggplot_pm25Diurnal
#' @param monitor A \emph{mts_monitor} object.
#' @param id deviceDeploymentID to include in the plot. This can be NULL if
#'   \code{monitor} only has one unique deviceDeploymentID.
#' @param style String indicating plotting style. Either \code{"large"} or
#'   \code{"small"}. \code{style = "large"} is suitable for plots larger than
#'   450x450px, and \code{"small"} is suitable for plots 450x450px or smaller.
#' @param title Plot title. If NULL, a suitable title will be constructed.
#' @param timezone Olson timezone name for x-axis scale and date parsing. If
#'   NULL the timezone of the specified monitor will be used.
#' @param ... Extra arguments passed to \code{ggplot_pm25Diurnal()}.
#'
#' @return A \emph{ggplot} object.
#'
#' @import ggplot2
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' library(AirMonitorPlots)
#'
#' AirMonitor::Carmel_Valley %>%
#'   AirMonitor::monitor_trimDate() %>%
#'   monitor_ggDailyByHour_archival()
#'
#' \dontrun{
#' # Fail gracefully if any resources are not available
#' try({
#' monitor <- AirMonitor::airnow_loadLatest()
#' monitor_ggDailyByHour_archival(monitor, id = "51b9bcb4eaac7c9d_530330030")
#' }, silent = FALSE)
#' }

monitor_ggDailyByHour_archival <- function(
  monitor,
  startdate = NULL,
  enddate = NULL,
  id = NULL,
  style = c("large", "small"),
  title = NULL,
  timezone = NULL,
  ...
) {

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(monitor)

  if ( !AirMonitor::monitor_isValid(monitor) )
    stop("Parameter 'monitor' is not a valid mts_monitor object.")

  if ( AirMonitor::monitor_isEmpty(monitor) )
    stop("Parameter 'monitor' contains no data.")

  # Check deviceDeploymentID
  if ( is.null(id) ) {

    if ( nrow(monitor$meta) > 1 ) {
      stop("Parameter 'id' is required if 'monitor' has multiple monitors.")
    } else {
      id <- monitor$meta$deviceDeploymentID
    }

  } else {

    if ( length(id) > 1 ) {
      stop("Parameter 'id' must contain a single deviceDeploymentID.")
    } else if ( !id %in% monitor$meta$deviceDeploymentID ) {
      stop(sprintf("deviceDeploymentID '%s' is not found in 'monitor'.", id))
    }

  }

  # Check style
  style <- match.arg(style)

  if ( !is.null(timezone) ) {
    if ( !timezone %in% OlsonNames() ) {
      stop(sprintf("Invalid timezone = '%s'. See ?OlsonNames.", timezone))
    }
  }

  # ----- Subset monitor -------------------------------------------------------

  singleMonitor <-
    monitor %>%
    AirMonitor::monitor_select(id)

  # Get timezone
  if ( is.null(timezone) )
    timezone <- singleMonitor$meta$timezone

  timeRange <- range(singleMonitor$data$datetime)

  # Create POSIXct startdate and enddate
  if ( is.null(startdate) || is.null(enddate) ) {
    startdate <- timeRange[1]
    enddate <- timeRange[2]
  } else {
    startdate <- MazamaCoreUtils::parseDatetime(startdate, timezone = timezone)
    enddate <- MazamaCoreUtils::parseDatetime(enddate, timezone = timezone)
  }

  if ( (startdate < timeRange[1] && enddate < timeRange[1]) ||
       (startdate > timeRange[2] && enddate > timeRange[2]) ) {
    stop("Both 'startdate' and 'enddate' are outside the 'monitor' time range")
  }

  dateRange <- MazamaCoreUtils::dateRange(
    startdate = startdate,
    enddate = enddate,
    timezone = timezone,
    unit = "hour",
    ceilingEnd = TRUE
  )

  singleMonitor <-
    singleMonitor %>%
    AirMonitor::monitor_filterDate(startdate, enddate, ceilingEnd = TRUE)

  # ----- Create "tidy" version ------------------------------------------------

  # NOTE:  Prefixing 'timezone' with '!!' tells dplyr to use the local variable
  # NOTE:  'timezone' instead of the mts_tidy$timezone column.

  # Convert monitor to tidy structure with 'hour', 'datestamp' and 'nowcast
  mts_tidy <-
    monitor_toTidy(singleMonitor) %>%
    dplyr::mutate(
      hour = as.numeric(strftime(.data$datetime, "%H", tz = !!timezone)),
      datestamp = strftime(.data$datetime, "%Y%m%d", tz = !!timezone),
      nowcast = .nowcast(.data$pm25)
    )

  # ----- Style ----------------------------------------------------------------

  # Get title
  if ( is.null(title) ) {
    title <- paste0("NowCast by Time of Day\n",
                    "Site: ", unique(mts_tidy$locationName),
                    " (", unique(mts_tidy$deviceDeploymentID), ")")
  }

  # Create start and end date labels
  startdateLabel <- strftime(
    x = MazamaCoreUtils::parseDatetime(startdate, timezone = timezone),
    tz = timezone,
    format = "%Y/%m/%d"
  )
  enddateLabel <- strftime(
    x = MazamaCoreUtils::parseDatetime(enddate, timezone = timezone),
    tz = timezone,
    format = "%Y/%m/%d"
  )

  # Get subtitle
  subtitle <- paste0("Hourly values and averages: ",
                     startdateLabel, " to ", enddateLabel)

  # Get labels for legend
  now_datestamp <-
    lubridate::now(tzone = timezone) %>%
    strftime("%Y%m%d", tz = timezone)
  end_datestamp <- strftime(enddate, "%Y%m%d", tz = timezone)

  meanText <- paste0(as.integer(difftime(dateRange[2], dateRange[1], units = "days")), " Day Mean")

  if (style == "large") {
    meanSize <- 8
    base_size <- 15
  } else if (style == "small") {
    meanSize <- 5
    base_size <- 11
  }

  # ----- Create plot ----------------------------------------------------------

  gg <-
    ggplot_pm25Diurnal(
      mts_tidy,
      startdate = dateRange[1],
      enddate = dateRange[2],
      mapping = aes_(x = ~ hour, y = ~ nowcast),
      base_size = base_size,
      ...
    ) +
    custom_aqiLines() +
    custom_aqiStackedBar() +
    # Mean bars
    stat_meanByHour(output = "AQIColors") +
    # Data points
    stat_nowcast(geom = "pm25Points")

  gg <- gg +
    # Title
    ggtitle(title, subtitle) +
    # Theme
    theme_dailyByHour_airfire(size = style)

  return(gg)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  # BUG ==> ggplot errors when plotting ddata right after UTC midnight
  # Most likely issue is today/yesterday tibbles with zero rows. We should
  # check for this before these separate, custom points

  monitor <- airnow_loadLatest()
  id <- "060431001_01"
  style <- "large"
  title <- NULL
  timezone <- NULL


  startdate <- NULL
  enddate <- NULL

  enddate <-
    lubridate::now(tzone = "America/Los_Angeles") %>%
    lubridate::floor_date(unit = "day") - lubridate::dhours(1)
  startdate <- enddate - lubridate::ddays(10)

  enddate <-
    lubridate::now(tzone = "America/Los_Angeles") %>%
    lubridate::floor_date(unit = "day") - lubridate::ddays(3) - lubridate::dhours(1)
  startdate <- enddate - lubridate::ddays(4)

  enddate <-
    lubridate::now(tzone = "America/Los_Angeles") %>%
    lubridate::floor_date(unit = "day") + lubridate::dhours(1)
  startdate <- enddate - lubridate::ddays(10)

  enddate <-
    lubridate::now(tzone = "UTC") %>%
    lubridate::floor_date(unit = "day")
  startdate <- enddate - lubridate::ddays(6)


  monitor_ggDailyByHour(
    monitor = monitor,
    startdate = startdate,
    enddate = enddate,
    id = id,
    style = style,
    title = title,
    timezone = timezone
  )

}
