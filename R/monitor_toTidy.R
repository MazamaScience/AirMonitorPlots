#' @title Convert mts_monitor data to a tidy format
#'
#' @description
#' Changes \emph{mts_monitor} formatted data into a
#' 'tidy' format that is useful for 'tidyverse' functions. If the given data is
#' already in a tidy format, it is returned as is.
#'
#' @param mts_monitor \emph{mts_monitor} object.
#'
#' @return Tidy formatted \emph{mts_monitor} data.
#'
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' library(AirMonitorPlots)
#'
#' mts_monitor <-
#'   AirMonitor::NW_Megafires %>%
#'   AirMonitor::monitor_select(
#'     c('450d08fb5a3e4ea0_530470009', '40ffdacb421a5ee6_530470010')
#'   )
#'
#' mts_tidy <- monitor_toTidy(mts_monitor)
#'
#' \dontrun{
#' mts_tidy2 <- monitor_toTidy(mts_tidy)
#' }
#'
monitor_toTidy <- function(
  mts_monitor = NULL
) {

  if ( AirMonitor::monitor_isValid(mts_monitor) ) {

    monMeta <- mts_monitor[["meta"]]
    monData <- mts_monitor[["data"]]

    mts_tidy <-  monData %>%
      tidyr::gather("deviceDeploymentID", "pm25", -.data$datetime) %>%
      dplyr::inner_join(monMeta, by = "deviceDeploymentID") %>%
      tibble::as_tibble()

  } else if (monitor_isTidy(mts_monitor)) {

    message("Data is already in a tidy format.")
    mts_tidy <- mts_monitor

  } else {

    stop("Data is not in a reconized format.")

  }

  return(mts_tidy)
}
