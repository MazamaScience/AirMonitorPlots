#' @export
#' @importFrom rlang .data
#'
#' @title Trim a mts_monitor object to full days
#'
#' @param mts_monitor mts_monitor \emph{mts_monitor} object.
#' @param timezone Olson timezone used to interpret dates.
#'
#' @description Trims the date range of a \emph{mts_monitor} object to local time date
#' boundaries which are \emph{within} the range of data. This has the effect
#' of removing partial-day data records at the start and end of the timeseries
#' and is useful when calculating full-day statistics.
#'
#' Day boundaries are calculated using the specified \code{timezone} or, if
#' \code{NULL}, from \code{mts_monitor$meta$timezone}.
#'
#' @return A subset of the given \emph{mts_monitor} object.
#'
#' @examples
#' library(AirMonitorPlots)
#'
#' UTC_week <- AirMonitor::monitor_subset(
#'   AirMonitor::Carmel_Valley,
#'   tlim = c(20160801, 20160808),
#'   timezone = "UTC"
#' )
#'
#' # UTC day boundaries
#' head(UTC_week$data)
#'
#' # Trim to local time day boundaries
#' local_week <- monitor_trimDate(UTC_week)
#' head(local_week$data)
#'

monitor_trimDate <- function(
  mts_monitor = NULL,
  timezone = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(mts_monitor)

  if ( !monitor_isValid(mts_monitor) )
    stop("Parameter 'mts_monitor' is not a valid 'mts_monitor' object.")

  if ( monitor_isEmpty(mts_monitor) )
    stop("Parameter 'mts_monitor' has no data.")

  if ( is.null(timezone) )
    timezone <- mts_monitor$meta$timezone

  if ( !timezone %in% OlsonNames() )
    stop(sprintf("timezone '%s' is not a valid Olson timezone", timezone))

  # ----- Get the start and end times ------------------------------------------

  timeRange <- range(mts_monitor$data$datetime)

  # NOTE:  The dateRange() is used to restrict the time range to days that have
  # NOTE:  complete data.
  # NOTE:
  # NOTE:  floor/ceiling the start date depending on whether you are already
  # NOTE:  at the date boundary

  hour <-
    MazamaCoreUtils::parseDatetime(timeRange[1], timezone = timezone) %>%
    lubridate::hour() # hour resolution is good enough to count as an entire day

  if ( hour == 0 ) {
    ceilingStart = FALSE
  } else {
    ceilingStart = TRUE
  }

  dateRange <-
    MazamaCoreUtils::dateRange(
      startdate = timeRange[1],
      enddate = timeRange[2],
      timezone = timezone,
      unit = "sec",
      ceilingStart = ceilingStart, # date boundary *after* the start
      ceilingEnd = FALSE           # date boundary *before* the end
    )

  # ----- Subset the "mts_monitor" object ---------------------------------------

  data <-
    mts_monitor$data %>%
    dplyr::filter(.data$datetime >= dateRange[1]) %>%
    dplyr::filter(.data$datetime < dateRange[2])

  mts_monitor$data <- data

  # ----- Return ---------------------------------------------------------------

  return(mts_monitor)

}
