#' @title Check if data is tidy-formatted mts_monitor data
#'
#' @description
#' Verifies that the given data can be treated as tidy-formatted
#' data. This is done by verifying that the data is a tibble data.frame object
#' with columns for information in all \emph{mts_monitor} objects.
#'
#' @param data Data object to validate.
#'
#' @return TRUE if the data is in a recognized 'Tidy' format, otherwise FALSE
#'
#' @export
#'
#' @examples
#' library(AirMonitor)
#'
#' monitor <-
#'   NW_Megafires %>%
#'   AirMonitor::monitor_select(
#'     c('99a6ee8e126ff8cf_530470009_04', '123035bbdc2bc702_530470010_04')
#'   )
#'
#' mts_tidy <- monitor_toTidy(monitor)
#' monitor_isTidy(mts_tidy)
#'
#' \dontrun{
#' monitor_isTidy(monitor)
#' }
#'

monitor_isTidy <- function(
  data = NULL
) {

  if (is.null(data)) {
    stop("Data parameter cannot be NULL")
  }

  requiredColumns <- c(
    "datetime", "deviceDeploymentID", "pm25", "longitude", "latitude", "elevation",
    "timezone", "countryCode", "stateCode"
  )

  isTidy <- (
    all(
      c("tbl_df", "tbl", "data.frame") %in% class(data)) &&
      all(requiredColumns %in% colnames(data)
    )
  )

  return(isTidy)

}
