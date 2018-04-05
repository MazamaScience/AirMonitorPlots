
#' @title Check if Data has `ws_monitor` Format
#' @description Verifies that the given data can be treated as a `ws_monitor`
#'     object.
#'
#' @param data Data to validate.
#' @return True if the data is of class `ws_monitor`, otherwise False.
#'
isWSMon <- function(data = NULL) {
  if (is.null(data)) {
    stop("Data parameter cannot be NULL")
  }

  "ws_monitor" %in% class(data)
}


#' @title Check if Data has a 'Tidy' Format
#' @description Verifies that the given data can be treated as 'tidy'-formatted
#'     `ws_monitor` data.
#'
#' @param data Data to validate.
#' @return True if the data is in a recognized 'Tidy' format, otherwise False.
#'
isTidy <- function(data = NULL) {
  if (is.null(data)) {
    stop("Data parameter cannot be NULL")
  }

  columns <- c(
    "datetime", "monitorID", "pm25", "longitude", "latitude",
    "elevation", "timezone", "countryCode", "stateCode", "siteName",
    "agencyName", "countyName", "msaName", "monitorType", "siteID",
    "instrumentID", "aqsID", "pwfslID", "pwfslDataIngestSource",
    "telemetryAggregator", "telemetryUnitID"
  )

  tidy <- (all(c("tbl_df", "tbl", "data.frame") %in% class(data)) &&
           all(columns %in% colnames(data)))

  tidy
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
