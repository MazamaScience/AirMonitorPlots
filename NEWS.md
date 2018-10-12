# Updates to the PWFSLSmokePlots R Package

```
Plot Utilities for the PWFSLSmoke Package
```

----

## Version 0.4 -- package best practices

### Version 0.4.1 

 * added `clockPlot()` function
 * added `aqiPalette()` function and unit test
 
### Version 0.4.0 

 * added unit test for `dailyHourlyBarplot()`
 * deprecated `createTarnayPlot()` in favor of `dailyHourlyBarplot()`
 * removed `~Tidy()` functions in favor of those in *PWFSLSmoke*
 * exporting "%>%" as part of the package
 * updated DESCRIPTION file 
 * wind barbs grow in length between zero and 5 knots

## Version 0.3 -- daily-hourly barplot

### Version 0.3.4 

 * added windBarbs function 

### Version 0.3.3 -- daily-hourly barplot refinements

This release tweaks the calculation of NowCast in the daily-hourly-barplot:

- add the option to subset data with `tlim` argument (allows for proper NowCast calculation)

This release also reverses the legend scale ordering for better visual consistency

### Version 0.3.2 -- daily-hourly barplot refinements

This release focuses on refining the visual aesthetics of the daily-hourly-barplot:

- Refactored the plot theme into its own function
- tweaked the theme style
- make the x-axis labeling reactive to the date range of the data
-  introduce the option to include a third column in the legend
   - currently a work in progress (is off by default)
   - contains only dummy text

### Version 0.3.1 -- daily-hourly barplot refinements

Addresses the following issues regarding the daily-hourly-barplot:

- Add control over hourly data calculation
- Fix bug where daily and hourly data were misaligned
- Make legend more verbose
- Make plot title reactive to the data being plotted
- Add an option to include a footnote caption linking to an AQI explainer

### Version 0.3.0 -- daily-hourly barplot

Added the first plot function: the "daily-hourly barplot". THis is  a timeseries barplot showing PM2.5 data for
the given monitors. The overall plot is faceted by monitor, and each facet has two sets of columns: one for
daily levels, and one for hourly levels.


## Version 0.2 -- `tidy` data

### Version 0.2.1 -- `isTidy`, `toTidy` improvements

* Removed `isWSMon()`
    - Functionality is now in [`PWFSLSmoke::monitor_isMonitor()`](https://github.com/MazamaScience/PWFSLSmoke/blob/master/R/monitor_isMonitor.R)
* Renamed `isTidy()` -> `monitor_isTidy()`
* Renamed `wsMonToTidy()` -> `monitor_toTidy()`

### Version 0.2.0 -- `isTidy`, `toTidy`

* Added `isWSMon()` function
    - Checks if data is a `ws_monitor` data object
* Added `isTidy()` function
    - Checks if data is a tidy formated object of ws_monitor data
* Added `wsMonToTidy()` function
    - Converts `ws_monitor` objects to a tidy format


## Version 0.1 -- initial package structure

* added `addPolygon()` function

