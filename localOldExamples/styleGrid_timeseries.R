
library(AirMonitorPlots)
library(gridExtra)

mon <- AirMonitor::Carmel_Valley
id <- mon$meta$deviceDeploymentID[1]
start1 <- "2016-06-01"
start2 <- "2016-07-01"
start3 <- "2016-08-01"
end1 <- "2016-08-08"
end2 <- "2016-08-15"
end3 <- "2016-08-25"
dbp1 <- timeseriesPlot(mon, start3, end1, "pwfsl",
                       title = "Carmel Valley\n2016")
dbp2 <- timeseriesPlot(mon, start3, end2, "pwfsl",
                       title = "Carmel Valley\n2016")
dbp3 <- timeseriesPlot(mon, start2, end2, "pwfsl",
                       title = "Carmel Valley\n2016")
dbp4 <- timeseriesPlot(mon, start1, end3, "pwfsl",
                       title = "Carmel Valley\n2016")
gridExtra::grid.arrange(dbp1, dbp2, dbp3, dbp4, nrow = 2)
