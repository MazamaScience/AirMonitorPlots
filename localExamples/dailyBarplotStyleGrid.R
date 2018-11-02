
library(PWFSLSmokePlots)
library(gridExtra)

mon <- PWFSLSmoke::Carmel_Valley
id <- mon$meta$monitorID[1]
start1 <- "2016-06-01"
start2 <- "2016-07-01"
start3 <- "2016-08-01"
end1 <- "2016-08-08"
end2 <- "2016-08-15"
end3 <- "2016-08-25"
dbp1 <- dailyBarplot(mon, id, start3, end1, "monitoring",
                     title = "Carmel Valley\n2016")
dbp2 <- dailyBarplot(mon, id, start3, end2, "monitoring",
                     title = "Carmel Valley\n2016")
dbp3 <- dailyBarplot(mon, id, start2, end2, "monitoring",
                     title = "Carmel Valley\n2016")
dbp4 <- dailyBarplot(mon, id, start1, end3, "monitoring",
                     title = "Carmel Valley\n2016")
gridExtra::grid.arrange(dbp1, dbp2, dbp3, dbp4, nrow = 2)
