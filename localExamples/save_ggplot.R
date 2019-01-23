# reproduce https://tools.airfire.org/monitor-plot/v4/plot?databaseversion=4.0&webserviceapi=4.0&plottype=timeseries&monitorid=060631010_01
# and https://tools.airfire.org/monitor-plot/v4/plot?databaseversion=4.0&webserviceapi=4.0&plottype=dailybarplot&monitorid=060631010_01

library(PWFSLSmokePlots)
ws_monitor <- airnow_loadLatest()
monitorID <- "060631010_01"
startdate <- lubridate::floor_date(lubridate::now() - lubridate::ddays(6), "day")
enddate <- lubridate::now()

# Default settings

png("timeseries.png", width = 700, height = 700, units = "px")
monitor_ggTimeseries(ws_monitor,
                     startdate = startdate,
                     enddate = enddate, 
                     monitorIDs = monitorID,
                     style = "large")
dev.off()

png("smalltimeseries.png", width = 450, height = 450, units = "px")
monitor_ggTimeseries(ws_monitor,
                     startdate = startdate,
                     enddate = enddate,
                     monitorIDs = monitorID,
                     style = "small")
dev.off()

monitor_ggDailyBarplot(ws_monitor,
                       startdate = startdate,
                       enddate = enddate,
                       monitorIDs = monitorID)


b <- monitor_ggDailyBarplot(ws_monitor,
                            startdate = startdate,
                            enddate = enddate,
                            monitorIDs = monitorID)
png("barplot.png", width = 700, height = 700, units = "px")
b
dev.off()
