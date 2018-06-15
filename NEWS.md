# Updates to the PWFSLSmokeModeling R Package

```
Plot utilities for the PWFSLSmoke package.
```

## Version 0.3.1 -- Tarnay Plot refinements

Addresses the following issues regarding the "Tarnay" plot:

- Add control over hourly data calculation
- Fix bug where daily and hourly data were misaligned
- Make legend more verbose
- Make plot title reactive to the data being plotted
- Add an option to include a footnote caption linking to an AQI explainer

## Version 0.3.0 -- Tarnay Plot

Added the first plot function: the Tarnay plot. This plot is a timeseries barplot showing PM2.5 data for the given monitors. The overall plot is faceted by monitor, and each facet has two sets of columns: one for daily levels, and one for hourly levels.

## Version 0.2.1 -- Data Ingesting

* Removed `isWSMon()`
    - Functionality is now in [`PWFSLSmoke::monitor_isMonitor()`](https://github.com/MazamaScience/PWFSLSmoke/blob/master/R/monitor_isMonitor.R)
* Renamed `isTidy()` -> `monitor_isTidy()`
* Renamed `wsMonToTidy()` -> `monitor_toTidy()`


## Version 0.2.0 -- Data Ingesting

* Added `isWSMon()` function
    - Checks if data is a `ws_monitor` data object
* Added `isTidy()` function
    - Checks if data is a tidy formated object of ws_monitor data
* Added `wsMonToTidy()` function
    - Converts `ws_monitor` objects to a tidy format

## Version 0.1 -- Basic Components

* added `addPolygon()` function

