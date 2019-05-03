
library(AirMonitorPlots)
library(gridExtra)

mon <- PWFSLSmoke::Carmel_Valley
id <- mon$meta$monitorID[1]
start <- "2016-08-07"
end <- "2016-08-09"
icon1 <- clockPlot(mon, id, start, end, "icon", "black", title = "icon")
icon2 <- clockPlot(mon, id, start, end, "icon_fan", "black", title = "icon_fan")
icon3 <- clockPlot(mon, id, start, end, "icon_avg", "black", title = "icon_fan_avg")
icon4 <- clockPlot(mon, id, start, end, "icon", "white", title = "icon")
icon5 <- clockPlot(mon, id, start, end, "icon_fan", "white", title = "icon_fan")
icon6 <- clockPlot(mon, id, start, end, "icon_fan_avg", "white", title = "icon_fan_avg")
full1 <- clockPlot(mon, id, start, end, "full", "black", title = "full", labelScale = 0.6)
full2 <- clockPlot(mon, id, start, end, "full_fan", "black", title = "full_fan", labelScale = 0.6)
full3 <- clockPlot(mon, id, start, end, "full_fan_avg", "black", title = "full_fan_avg", labelScale = 0.6)
full4 <- clockPlot(mon, id, start, end, "full", "white", title = "full", labelScale = 0.6)
full5 <- clockPlot(mon, id, start, end, "full_fan", "white", title = "full_fan", labelScale = 0.6)
full6 <- clockPlot(mon, id, start, end, "full_fan_avg", "white", title = "full_fan_avg", labelScale = 0.6)
grid.arrange(icon1, icon2, icon3,
             icon4, icon5, icon6,
             full1, full2, full3,
             full4, full5, full6,
             nrow = 4)
