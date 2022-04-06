# AirMonitorPlots R Package <img src='man/figures/logo.png' align="right" height="81.5" />

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/AirMonitorPlots)](https://cran.r-project.org/package=AirMonitorPlots)
[![Downloads](http://cranlogs.r-pkg.org/badges/AirMonitorPlots)](https://cran.r-project.org/package=AirMonitorPlots)
[![Build Status](https://travis-ci.org/MazamaScience/AirMonitorPlots.svg?branch=master)](https://travis-ci.org/MazamaScience/AirMonitorPlots)
[![DOI](https://zenodo.org/badge/123438756.svg)](https://zenodo.org/badge/latestdoi/123438756)

```
Plot utilities for air quality monitoring data.
```

## Background

The USFS Pacific Wildland Fire Sciences Lab [AirFire](http://www.airfire.org) 
team works to model wildland fire emissions and has created the BlueSky Modeling
Framework. This system  integrates a wide collection of models along the smoke 
modeling chain (fire  information, fuel loadings, consumption modeling, 
emissions modeling, time rate ofemissions modeling, plume height estimations, 
and smoke trajectory and dispersion  modeling). The resulting model output has 
been integrated into many different smoke  prediction systems and scientific 
modeling efforts;

The **PWFSLSmoke** R package is being developed for PWFSL to help modelers and 
scientists more easily work with PM2.5 data from monitoring locations across 
North America.

The **AirMonitorPlots** R package is being developed to provide plotting 
componenents for building up  custom plots needed by air quality specialists.

## Installation

Users will want to install the **devtools** package to have access to latest 
versions of this package that may not yet be available on CRAN.

```
devtools::install_github('mazamascience/PWFSLSmoke', build_vignettes=TRUE)
devtools::install_github('mazamascience/AirMonitorPlots', build_vignettes=TRUE)
```

----

This project is being funded in part by the USFS Pacific Wildland Fire Sciences 
Laboratory.


