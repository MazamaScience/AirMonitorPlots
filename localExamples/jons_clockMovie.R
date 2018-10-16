
ws_monitor <- PWFSLSmoke::Carmel_Valley
mon_daily <- monitor_dailyStatistic(ws_monitor)
dates <- mon_daily$data$datetime

for (i in seq_along(dates)) {
  
  result <- try({
    filename <- paste0("clockPlot_", sprintf("%04d", i), ".png")
    png(filename, 256,256, bg="white")
    print( clockPlot(ws_monitor, 
                     startdate = dates[i], 
                     centerColor = "black", 
                     style = "full_fan_avg") )
    dev.off()
  }, silent = TRUE)
  
}

