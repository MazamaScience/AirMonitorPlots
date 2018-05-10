# Updates to the PWFSLSmokeModeling R Package

```
Plot utilities for the PWFSLSmoke package.
```

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

