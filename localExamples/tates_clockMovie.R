ws_monitor <- PWFSLSmoke::Carmel_Valley
mon_daily <- monitor_dailyStatistic(ws_monitor)
dates <- mon_daily$data$datetime
period <- 7 # Number of days to average in each plot

for (i in seq_along(dates)) {
  start <- i
  end <- start + period
  
  if (end <= length(dates)) {
    result <- try({
      filename <- paste0("clockPlot_", sprintf("%04d", i), ".png")
      png(filename, 256,256, bg="white")
      print( clockPlot(ws_monitor,
                       startdate = dates[start],
                       enddate = dates[end],
                       centerColor = "black",
                       style = "full_fan_avg") )
      dev.off()
    }, silent = TRUE)
  }
}

