#' @title Convert `ws_monitor` data to a tidy format
#'
#' @description
#' Changes write-optomized `ws_monitor` formatted data into a read-optomized
#' 'tidy' format that is preferred for `ggplot` plotting. If the given data is
#' already in a tidy format, it is returned as is.
#'
#' @param data Data to potentially convert.
#' @return 'Tidy' formatted `ws_monitor` data.
#'
#' @export
#' @import dplyr
#' @importFrom magrittr '%>%'
#' @importFrom rlang .data
#' @import PWFSLSmoke
#'
#' @examples
#'
monitor_toTidy <- function(data = NULL) {

  if (monitor_isMonitor(data)) {
    monMeta <- tibble::as_tibble(data[["meta"]])
    monData <- tibble::as_tibble(data[["data"]])

    tidyData <-  monData %>%
      tidyr::gather("monitorID", "pm25", -.data$datetime) %>%
      dplyr::inner_join(monMeta, by = "monitorID")

  } else if (monitor_isTidy(data)) {
    message("Data is already in a tidy format")
    tidyData <- data

  } else {
    stop("Data is not in a reconized format")
  }

  return(tidyData)
}
