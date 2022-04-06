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
#' mts_monitor <-
#'   NW_megafires %>%
#'   monitor_select(c('450d08fb5a3e4ea0_530470009', '40ffdacb421a5ee6_530470010')
#' )
#'
#' mts_tidy <- monitor_toTidy(mts_monitor)
#' monitor_isTidy(mts_tidy)
#'
#' \dontrun{
#' monitor_isTidy(mts_monitor)
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
