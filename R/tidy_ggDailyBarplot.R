# tidy_ggDailyBarplot <- function(ws_tidy,
#                                 startdate = NULL,
#                                 enddate = NULL,
#                                 style = NULL,
#                                 aqiStyle = NULL,
#                                 monitorID = NULL,
#                                 title = NULL) {
#   data <- ws_tidy
#   
#   if (!is.null(monitorIDs)) {
#     data <- filter(.data = data, .data$monitorID %in% monitorIDs)
#   } 
#   
#   if ( length(unique(data$monitorID)) > 1) {
#     mapping <- aes_(color = ~monitorID)
#     if (is.null(title)) title <- ""
#   } else {
#     mapping <- NULL
#     if(is.null(title)) title <- unique(data$siteName)
#   }
#   
#   ggplot_pm25Timeseries(data) +
#     stat_AQILevel()
#   
# }