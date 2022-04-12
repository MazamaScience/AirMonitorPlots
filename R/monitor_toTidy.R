#' @title Convert mts_monitor data to a tidy format
#'
#' @description
#' Changes \emph{mts_monitor} formatted data into a
#' 'mts_tidy' format that is useful for 'tidyverse' functions. If the given data
#' is already in tidy format, it is returned as is.
#'
#' @param monitor \emph{mts_monitor} object.
#'
#' @return Tidy formatted \emph{mts_monitor} data.
#'
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' library(AirMonitorPlots)
#'
#' monitor <-
#'   AirMonitor::NW_Megafires %>%
#'   AirMonitor::monitor_select(
#'     c('450d08fb5a3e4ea0_530470009', '40ffdacb421a5ee6_530470010')
#'   )
#'
#' mts_tidy <- monitor_toTidy(monitor)
#'

monitor_toTidy <- function(
  monitor = NULL
) {

  if ( AirMonitor::monitor_isValid(monitor) ) {

    meta <- monitor$meta
    data <- monitor$data

    mts_tidy <-
      data %>%
      tidyr::gather("deviceDeploymentID", "pm25", -.data$datetime) %>%
      dplyr::inner_join(meta, by = "deviceDeploymentID") %>%
      tibble::as_tibble()

  } else if ( monitor_isTidy(monitor) ) {

    mts_tidy <- monitor

  } else {

    stop("monitor is not a 'mts_monitor' object.")

  }

  return(mts_tidy)

}
