# Request to create Daily/Hourly plots for Alaska

library(AirMonitor)
library(AirMonitorPlots)

Alaska <-
  monitor_load(20190615, 20190801) %>%
  monitor_subset(stateCodes = "AK")

# ----- Hourly dots with daily averages -- averaged together -----

Alaska_base <- ggplot_pm25Timeseries(Alaska,
                                     startdate = 20190615,
                                     enddate = 20190801)

Alaska_base +
  geom_pm25Points(shape = "square", alpha = .1) +
  stat_dailyAQCategory(alpha = .5) +
  scale_y_continuous(limits = c(0, 500)) +
  custom_aqiStackedBar(width = 0.01) +
  ggtitle("2019 Alaska Fires")

# ----- Hourly bars with daily averages -- faceted -----

Alaska_base +
  stat_AQCategory(color = NA) +
  stat_dailyAQCategory(alpha = .5, timezone = "America/Anchorage") +
  facet_grid(rows = vars(deviceDeploymentID)) +
  ggtitle("2019 Alaska Fires")

# ----- Hourly bars with daily averages -- single

AlaskaIDs <- Alaska$meta$deviceDeploymentID

# Pick out a few from the previous plot
indices <- c(3,7)
IDs <- AlaskaIDs[indices]

AK_sub <-
  Alaska %>%
  monitor_subset(deviceDeploymentIDs = IDs)

AK_sub_base <- ggplot_pm25Timeseries(AK_sub,
                                     startdate = 20190622,
                                     enddate = 20190720)
AK_sub_base +
  stat_AQCategory(color = NA) +
  stat_dailyAQCategory(alpha = .5, timezone = "America/Anchorage") +
  facet_grid(rows = vars(locationName)) +
  ylim(0, 500) +
  ggtitle("2019 Alaska Fires")

# ----- How about branding? -----

gg <-
  AK_sub_base +
  stat_AQCategory(color = NA) +
  stat_dailyAQCategory(alpha = .5, timezone = "America/Anchorage") +
  facet_grid(rows = vars(locationName)) +
  ylim(0, 500) +
  ggtitle("2019 Alaska Fires")

brandPlot(gg, brandName = "AirFire", location = "topleft", size = .15)
brandPlot(gg, brandName = "USFS", location = "topright", size = .15)



