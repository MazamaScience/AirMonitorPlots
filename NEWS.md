# AirMonitorPlots 0.9.1

Updated additional functions to use the new PM_NAAQS as the default.

# AirMonitorPlots 0.9.0

Version 0.9.0 includes updated package dependencies and changes throughout the 
code base to support the upcoming change to the PM_NAAQS See:
[PM_NAAQS update](https://www.epa.gov/system/files/documents/2024-02/pm-naaqs-air-quality-index-fact-sheet.pdf).

Updated dependencies:

* **MazamaCoreUtils** => 0.5.2
* **MazamaTimeSeries** => 0.3.0
* **AirMonitor** => 0.4.0
* **gridExtra** => removed

* Updated several functions to use the new PM2.5 NAAQS thresholds as defined in
`AirMonitor::US_AQI$breaks_PM2.5_2024`.

# AirMonitorPlots 0.8.3

Updates to work with **AirMonitor** 0.3.11.

* Minor updates to pass R CMD check.
* Minor documentation improvements.

# AirMonitorPlots 0.8.2

* Minor tweak to appearance of `monitor_ggDailyByHour()`.

# AirMonitorPlots 0.8.1

* Removed "clockPlot" functionality.
* Removed unused `aqiStyle` argument from plotting functions.
* Change `mts_monitor` argument name to `monitor` to match the **AirMonitor** package.
* Change `deviceDeploymentID` argument name to `id` to match the **AirMonitor** package.
* Various cleanup.

# AirMonitorPlots 0.8.0

_**NOTE:**  version 0.8 is not compatible with the **PWFSLSmoke** package_

Version 0.8 represents a complete refactoring of the plotting code to support
_mts_monitor_ objects found in the **AirMonitor** package. This is part of the
large shift from "v1" data archives based on a unique `montorID` to "v2" 
archives based on "known locations" and a unique `deviceDeploymentID`.

Refactored files:

* `brandPlot.R`
* `custom_aqiAnnotation.R`
* `custom_datetimeScale.R`
* `custom_pm25DiurnalScales.R`
* `custom_pm25TImeseriesScales.R`
* `geom_PM25Points.R`
* `ggplot_pm25Timeseries.R`
* `monitor_ggCalendarPlot.R`
* `monitor_ggClockPlot.R`
* `monitor_ggDailyBarplot_archival.R`
* `monitor_ggDailyBarplot.R`
* `monitor_ggDailyByHour_archival.R`
* `monitor_ggDailyByHour.R`
* `monitor_ggDailyByHourBarplot.R`
* `monitor_ggDailyTimeseries_archival.R`
* `monitor_ggTimeseries.R`
* `monitor_isTidy.R`
* `monitor_toTidy.R`
* `nowcast.R`
* `stat_AQCategory.R`
* `stat_meanByHour.R`
* `stat_nowcast.R`
* `themes.R`
* `utils-pipe.R`


# AirMonitorPlots 0.7.0

Starting the 2021 fire season at version 0.7 with minor fixes and several new 
plots.

New "archival plots are designed to create fully annotated, nice looking plots
for longer time periods than the default plots which are designed for the most
recent 10 days:

* `monitor_ggDailyBarplot_archival()`
* `monitor_ggDailyByHour_archival()`
* `monitor_ggTimeseries_archival()`

Additional helper functions:

* `monitor_trimDate()`

# AirMonitorPlots 0.5.11

* New `monitor_ggDailyHourlyBarplot()` plot function.
* New `theme_dailyHourlyBarplot_airfire()` plot theme.

# AirMonitorPlots 0.5.10

* Bug fix for `monitor_ggDailyBarplot()` to the NowCast value for "today".

# AirMonitorPlots 0.5.9

* Completely refactored `monitor_ggDailyByHour()` to get at that "today" bug.

# AirMonitorPlots 0.5.8

* Bug fix for `monitor_ggDailyByHour()` so that it doesn't ignore data from
"today".
* Increased whitespace above titles.

# AirMonitorPlots 0.5.7

* New `monitor_ggCalendar()` function.
* Updated to use *MazamaCoreUtils 0.3.10*.
* Minor cleanup/refactoring.

# AirMonitorPlots 0.5.6

* Added option for SCAQMD coloring in `stat_meanByHour()`, accessible by calling
`stat_meanByHour(output = "scaqmd")`.
* `monitor_ggDailyByHour()` now checks for available data for "yesterday" and
"today" before attempting to plot those lines.

# AirMonitorPlots 0.5.5

Use explicit time zones throughout package

# AirMonitorPlots 0.5.4

* Now using `MazamaCoreUtils::dateRange()`.
* Consistent ordering of monitor_* function arguments.
* Regularize `monitor_ggDailyBarplot()`.
* Make `monitor_ggDailyByHour()` more consistent.

# AirMonitorPlots 0.5.3

**Rename package from `PWFSLSmokePlots` to `AirMonitorPlots`.**

# AirMonitorPlots 0.5.2

Style refactoring and coalescing functions.

## Breaking Changes

All `tidy_gg*` plot functions have been removed in favor of the equivalent
`monitor_gg*` plot functions. This reflects a change in philosophy that
"tidy-formatted" data should be the default assumption within the package, and
with all `monitor_*` functions being designed to work directly with
**PWFSLSmoke** pipelines.

## Internal

Style refactoring was done to make the package more consistent with current
Mazama Science coding practices.


# AirMonitorPlots 0.5.1

Cleanup of refactored code before CRAN submission

 * renamed `_AQILevel()` functions to `_AQCategory()`
 * general improvements to documentation and code comments


# AirMonitorPlots 0.5.0

Complete refactoring of the code to utilize low level `ggplot` concepts.

  * `tidy_gg*` and `monitor_gg*` functions to create out-of-the-box plots for
    the monitoring site
  * Modularized with custom stats and geoms and `custom_` functions, making it
    easy to create customized plots.


# AirMonitorPlots 0.4.4

Adding standard plots used in the USFS Monitoring site Mv4.

 * added `dailyBarplot()` and `dailyBarplotBase()`
 * added `timeseriesPlot()` and `timeseriesPlotBase()`
 * improvements to `clockPlot()` and `clockPlotBase()`
 * now importing **gridExtra**


# AirMonitorPlots 0.4.3

Moving all base plot related code from **PWFSLSmokePlot** to **PWFSLSmoke** so
that all functionality in the former is purely **ggplot2** based.

 * removed `addPolygon()`, `addWindBarb()` and `addWindBarbs()`
 * removed `aqiPalette()`
 * removed `monitor_getDailyMean()`


# AirMonitorPlots 0.4.2

 * updated for latest **devtools** package


# AirMonitorPlots 0.4.1

 * added `monitor_getDailyMean()` function
 * added `clockPlot()` function
 * added `clockPlotBase()` function
 * added `aqiPalette()` function and unit test


# AirMonitorPlots 0.4.0

 * added unit test for `dailyHourlyBarplot()`
 * deprecated `createTarnayPlot()` in favor of `dailyHourlyBarplot()`
 * removed `~Tidy()` functions in favor of those in **PWFSLSmoke**
 * exporting "%>%" as part of the package
 * updated `DESCRIPTION file`
 * wind barbs grow in length between zero and 5 knots


# AirMonitorPlots 0.3.4

 * added windBarbs function


# AirMonitorPlots 0.3.3 -- daily-hourly barplot refinements

This release tweaks the calculation of NowCast in the daily-hourly-barplot:

 * add the option to subset data with `tlim` argument (allows for proper NowCast
   calculation)

This release also reverses the legend scale ordering for better visual
consistency


# AirMonitorPlots 0.3.2 -- daily-hourly barplot refinements

This release focuses on refining the visual aesthetics of the
daily-hourly-barplot:

 * Refactored the plot theme into its own function
 * tweaked the theme style
 * made the x-axis labeling reactive to the date range of the data
 * introduced the option to include a third column in the legend
   (Currently contains only dummy text and off by default.)


# AirMonitorPlots 0.3.1 -- daily-hourly barplot refinements

Addresses the following issues regarding the daily-hourly-barplot:

 *  Add control over hourly data calculation
 *  Fix bug where daily and hourly data were misaligned
 *  Make legend more verbose
 *  Make plot title reactive to the data being plotted
 *  Add an option to include a footnote caption linking to an AQI explainer


# AirMonitorPlots 0.3.0 -- daily-hourly barplot

 * Added `daily-hourly barplot`(). This is  a
time series barplot showing PM2.5 data for the given monitors. The overall plot
is faceted by monitor, and each facet has two sets of columns: one for daily
levels, and one for hourly levels.


# AirMonitorPlots 0.2.1

 * Removed `isWSMon()`, This functionality is now in [`PWFSLSmoke::monitor_isMonitor()`](https://github.com/MazamaScience/PWFSLSmoke/blob/master/R/monitor_isMonitor.R)
 * Renamed `isTidy()` -> `monitor_isTidy()`
 * Renamed `wsMonToTidy()` -> `monitor_toTidy()`


# AirMonitorPlots 0.2.0

 * Added `isWSMon()` function to test an object for the `ws_monitor` class.
 * Added `isTidy()` function to test if an object is in tidy format.
 * Added `wsMonToTidy()` function to convert `ws_monitor` objects to a tidy
   format.


# AirMonitorPlots 0.1.0

 * added `addPolygon()` function

