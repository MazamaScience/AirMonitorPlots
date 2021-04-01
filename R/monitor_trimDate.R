#' @export
#' @importFrom rlang .data
#'
#' @title Trim a ws_monitor object to full days
#'
#' @param ws_monitor ws_monitor \emph{ws_monitor} object.
#' @param timezone Olson timezone used to interpret dates.
#'
#' @description Trims the date range of a \emph{ws_monitor} object to local time date
#' boundaries which are \emph{within} the range of data. This has the effect
#' of removing partial-day data records at the start and end of the timeseries
#' and is useful when calculating full-day statistics.
#'
#' Day boundaries are calculated using the specified \code{timezone} or, if
#' \code{NULL}, from \code{ws_monitor$meta$timezone}.
#'
#' @return A subset of the given \emph{ws_monitor} object.
#'
#' @examples
#' library(AirMonitorPlots)
#'
#' UTC_week <- PWFSLSmoke::monitor_subset(
#'   PWFSLSmoke::Carmel_Valley,
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
  ws_monitor = NULL,
  timezone = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(ws_monitor)

  if ( !monitor_isMonitor(ws_monitor) )
    stop("Parameter 'ws_monitor' is not a valid 'ws_monitor' object.")

  if ( monitor_isEmpty(ws_monitor) )
    stop("Parameter 'ws_monitor' has no data.")

  if ( is.null(timezone) )
    timezone <- ws_monitor$meta$timezone

  if ( !timezone %in% OlsonNames() )
    stop(sprintf("timezone '%s' is not a valid Olson timezone", timezone))

  # ----- Get the start and end times ------------------------------------------

  timeRange <- range(ws_monitor$data$datetime)

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

  # ----- Subset the "ws_monitor" object ---------------------------------------

  data <-
    ws_monitor$data %>%
    dplyr::filter(.data$datetime >= dateRange[1]) %>%
    dplyr::filter(.data$datetime < dateRange[2])

  ws_monitor$data <- data

  # ----- Return ---------------------------------------------------------------

  return(ws_monitor)

}
