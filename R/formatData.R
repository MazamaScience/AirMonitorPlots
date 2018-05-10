#' @title Check if data is tidy-formatted ws_monitor data
#'
#' @description
#' Verifies that the given data can be treated as tidy-formatted "ws_monitor"
#' data. This is done by verifying that the data is a tibble data.frame object
#' with columns for information in all `ws_monitor` objects.
#'
#' @param data Data to validate.
#'
#' @return True if the data is in a recognized 'Tidy' format, otherwise False.
#'
monitor_isTidy <- function(data = NULL) {

  if (is.null(data)) {
    stop("Data parameter cannot be NULL")
  }

  requiredColumns <- c(
    "datetime", "monitorID", "pm25", "longitude", "latitude", "elevation",
    "timezone", "countryCode", "stateCode"
  )

  isTidy <- (
    all(c("tbl_df", "tbl", "data.frame") %in% class(data)) &&
    all(requiredColumns %in% colnames(data))
  )

  return(isTidy)
}


#' @title Convert `ws_monitor` Data to a 'Tidy' Format
#' @description Changes write-optomized `ws_monitor` formatted data into a
#'     read-optomized 'tidy' format.
#'
#' @param data Data to convert.
#' @return 'Tidy' formatted `ws_monitor` data.
#'
#' @export
#' @importFrom magrittr '%>%'
#' @importFrom rlang .data
#' @import dplyr
#' @import PWFSLSmoke
#'
#' @examples
#'
wsMonToTidy <- function(data = NULL) {
  if (isWSMon(data)) {
    monMeta <- tibble::as_tibble(data[["meta"]])
    monData <- tibble::as_tibble(data[["data"]])

    tidyData <-  monData %>%
      tidyr::gather("monitorID", "pm25", -.data$datetime) %>%
      dplyr::inner_join(monMeta, by = "monitorID")

  } else if (isTidy(data)) {
    message("Data is already in a tidy format")
    tidyData <- data

  } else {
    stop("Data is not in a reconized format")
  }

  tidyData
}
