[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/AirMonitorPlots)](https://cran.r-project.org/package=AirMonitorPlots)
[![Downloads](https://cranlogs.r-pkg.org/badges/AirMonitorPlots)](https://cran.r-project.org/package=AirMonitorPlots)
[![DOI](https://zenodo.org/badge/123438756.svg)](https://zenodo.org/badge/latestdoi/123438756)


A dedicated Slack channel has been created for announcements, support and to 
help build a community of practice around this open source package. You may 
request an invitation to join from jonathan.callahan@dri.com.

# AirMonitorPlots R Package

```
Plot utilities for air quality monitoring data.
```

## Background

The USFS [AirFire](https://www.airfire.org) team works with air quality 
measurements associated with wildfire smoke and maintains both historical and 
real-time databases of PM2.5 monitoring data obtained from stationary monitors. 
This data is used in operational displays and for retrospective analysis. Data 
ingest and management of air quality “stationary time series” are important 
ongoing activities.

## Related Packages

The [AirMonitor](https://mazamascience.github.io/AirMonitor/) package contains 
data access functions to easily download harmonized data files as well as data 
manipulation functions that make it easy to create “recipe style” analysis 
pipelines. This combination allows analysts to work efficiently with short, 
readable R scripts. Interactive and base R plotting functions allow for visual 
review of the data.

**AirMonitorPlots** contains ggplot2 based plotting functions for advanced plots
and also provides plotting components for building up custom plots needed by 
air quality specialists.

## Installation

Install from CRAN with:

`install.packages('AirMonitorPlots')`

Install the latest version from GitHub with:

`devtools::install_github('mazamascience/AirMonitorPlots')`

------------------------------------------------------------------------

This project is supported by the [USFS AirFire](https://www.airfire.org) team.

