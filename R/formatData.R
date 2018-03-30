
#' @title Convert `ws_monitor` Data to a 'Tidy' Format
#'
#' @param data Data to convert
#'
#' @return 'Tidy' formatted `ws_monitor` data
#' @export
#'
#' @examples
#'
wsMonToTidy <- function(data = NULL) {

    if (isWSMon(data)) {

        monMeta <- tibble::as_tibble(data[["meta"]])
        monData <- tibble::as_tibble(data[["data"]])

        tidyData <-  monData %>%
            tidyr::gather(monitorID, pm25, -datetime) %>%
            dplyr::inner_join(monMeta, by = "monitorID")

    } else if (isTidy(data)) {

        message("Data is already in a tidy format")
        tidyData <- data

    } else {

        stop("Data is not in a reconized format")
    }
    
    tidyData
}


#' @title Check if Data has `ws_monitor` Format
#'
#' @param data Data to interrogate
#'
#' @return True if the data is of class `ws_monitor`, otherwise False
#'
isWSMon <- function(data = NULL) {

    if (is.null(data)) {
        stop("Data parameter cannot be NULL")
    }

    class(data)[1] == "ws_monitor"
}

#' @title Check if Data has a 'Tidy' Format
#'
#' @param data Data to interrogate
#'
#' @return True if the data is in a recognized 'Tidy' format, otherwise False
#'
isTidy <- function(data = NULL) {

    if (is.null(data)) {
        stop("Data parameter cannot be NULL")
    }

    columns <- c(
        "datetime", "monitorID", "pm25", "longitude",
        "latitude", "elevation", "timezone", "countryCode",
        "stateCode",  "siteName", "agencyName", "countyName",
        "msaName", "monitorType", "siteID", "instrumentID",
        "aqsID", "pwfslID", "pwfslDataIngestSource", "telemetryAggregator",
        "telemetryUnitID"
        )
    
    tidy <- (class(data) == c("tbl_df", "tbl", "data.frame") &&
             all(columns %in% colnames(data)))
    
    tidy
}
