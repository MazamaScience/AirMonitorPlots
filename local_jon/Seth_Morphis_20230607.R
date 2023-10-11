# Request to create Daily/Hourly plots for Alaska

library(AirMonitor)
library(AirMonitorPlots)

NM <-
  monitor_loadLatest() %>%
  monitor_filter(stateCode == "NM")

locationIDs <- c(
  "c504461da2f9776c_840350130025",
  "e18db9eaebcb1d5a_840350010026",
  "2a9a8a2d9fb0d52d_usfs.1072",
  "49b41e14bc699049_usfs.1073",
  "6a9d812dd5f11126_usfs.1075",
  "905003e2a2d6e260_840MMFS11074",
  "b9b5b4b14a948ae0_840MMFS11076",
  "3c55ee5db9900b2d_840MMFS11054",
  "09cecd3494af987a_840MMFS11035"
)

locationIDs_wo_Cliff <- c(
  "c504461da2f9776c_840350130025",
  "e18db9eaebcb1d5a_840350010026",
  #"2a9a8a2d9fb0d52d_usfs.1072",
  "49b41e14bc699049_usfs.1073",
  "6a9d812dd5f11126_usfs.1075",
  "905003e2a2d6e260_840MMFS11074",
  "b9b5b4b14a948ae0_840MMFS11076",
  "3c55ee5db9900b2d_840MMFS11054",
  "09cecd3494af987a_840MMFS11035"
)

SW_NM_wo_Cliff <-
  NM %>%
  monitor_select(locationIDs_wo_Cliff) %>%
  monitor_dropEmpty()

Cliff_ID <- "2a9a8a2d9fb0d52d_usfs.1072"
Cliff <- NM %>% monitor_select(Cliff_ID)

timezone <- Cliff$meta$timezone


# ----- Daily Barplots ---------------------------------------------------------

# See:  https://mazamascience.github.io/AirMonitorPlots/articles/articles/plot_gallery.html#custom-plots-1

gg <-
  ggplot_pm25Timeseries(
    SW_NM_wo_Cliff
  ) +
  ggtitle("SW New Mexico (w/o Cliff)") +
  stat_dailyAQCategory(timezone = timezone) +
  facet_grid(rows = vars(locationName)) +
  ylim(0, 20)

print(gg)

# ----- Daily/Hourly plot ------------------------------------------------------

monitor_ggDailyHourlyBarplot(NM, id = Cliff_ID)



