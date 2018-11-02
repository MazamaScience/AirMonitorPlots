# PWFSLSmokePlots 0.4.4

Adding standard plots used in the 
[USFS Montoring site](https://tools.airfire.org/monitoring/v4).

 * added `dailyBarplot()` and `dailyBarplotBase()`
 * added `timeseriesPlot()` and `timeseriesPlotBase()`
 * improvements to `clockPlot()` and `clockPlotBase()`
 * now importing **gridExtra**

# PWFSLSmokePlots 0.4.3

Moving all base plot related code from **PWFSLSmokePlot** to **PWFSLSmoke** so
that all functionality in the former is purely **ggplot2** based.

 * removed `addPolygon()`, `addWindBarb()` and `addWindBarbs()`
 * removed `aqiPalette()`
 * removed `monitor_getDailyMean()`

# PWFSLSmokePlots 0.4.2

 * updated for latest devtools package

# PWFSLSmokePlots 0.4.1 

 * added `monitor_getDailyMean()` function
 * added `clockPlot()` function
 * added `clockPlotBase()` function
 * added `aqiPalette()` function and unit test
 
# PWFSLSmokePlots 0.4.0 

 * added unit test for `dailyHourlyBarplot()`
 * deprecated `createTarnayPlot()` in favor of `dailyHourlyBarplot()`
 * removed `~Tidy()` functions in favor of those in **PWFSLSmoke**
 * exporting "%>%" as part of the package
 * updated `DESCRIPTION file` 
 * wind barbs grow in length between zero and 5 knots

# PWFSLSmokePlots 0.3.4 

 * added windBarbs function 

# PWFSLSmokePlots 0.3.3 -- daily-hourly barplot refinements

This release tweaks the calculation of NowCast in the daily-hourly-barplot:

 * add the option to subset data with `tlim` argument (allows for proper NowCast calculation)

This release also reverses the legend scale ordering for better visual consistency

# PWFSLSmokePlots 0.3.2 -- daily-hourly barplot refinements

This release focuses on refining the visual aesthetics of the daily-hourly-barplot:

 * Refactored the plot theme into its own function
 * tweaked the theme style
 * made the x-axis labeling reactive to the date range of the data
 * introduced the option to include a third column in the legend
(Currently contains only dummy text and off by default.)

# PWFSLSmokePlots 0.3.1 -- daily-hourly barplot refinements

Addresses the following issues regarding the daily-hourly-barplot:

 *  Add control over hourly data calculation
 *  Fix bug where daily and hourly data were misaligned
 *  Make legend more verbose
 *  Make plot title reactive to the data being plotted
 *  Add an option to include a footnote caption linking to an AQI explainer

# PWFSLSmokePlots 0.3.0 -- daily-hourly barplot

 * Added `daily-hourly barplot`(). This is  a 
timeseries barplot showing PM2.5 data for the given monitors. The overall plot 
is faceted by monitor, and each facet has two sets of columns: one for daily 
levels, and one for hourly levels.

# PWFSLSmokePlots 0.2.1

 * Removed `isWSMon()`, This functionality is now in [`PWFSLSmoke::monitor_isMonitor()`](https://github.com/MazamaScience/PWFSLSmoke/blob/master/R/monitor_isMonitor.R)
 * Renamed `isTidy()` -> `monitor_isTidy()`
 * Renamed `wsMonToTidy()` -> `monitor_toTidy()`

# PWFSLSmokePlots 0.2.0

 * Added `isWSMon()` function to test an object for the `ws_monitor` class.
 * Added `isTidy()` function to test if an object is in tidy format.
 * Added `wsMonToTidy()` function to convert `ws_monitor` objects to a tidy format.

## Version 0.1.0

 * added `addPolygon()` function

