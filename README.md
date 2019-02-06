---
title: "PWFSLSmokePlots"
pagetitle: PWFSLSmokePlots
---

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/PWFSLSmokePlots)](https://cran.r-project.org/package=PWFSLSmokePlots)
[![Downloads](http://cranlogs.r-pkg.org/badges/PWFSLSmokePlots)](https://cran.r-project.org/package=PWFSLSmokePlots)
[![Build Status](https://travis-ci.org/MazamaScience/PWFSLSmokePlots.svg?branch=master)](https://travis-ci.org/MazamaScience/PWFSLSmokePlots)

# PWFSLSmokePlots R Package

```
Plot utilities for the PWFSLSmoke package.
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

The **PWFSLSmokePlots** R package is being developed to provide plotting 
componenents for building up  custom plots needed by air quality specialists.

## Installation

Users will want to install the **devtools** package to have access to latest 
versions of this package that may not yet be available on CRAN.

```
devtools::install_github('mazamascience/PWFSLSmoke', build_vignettes=TRUE)
devtools::install_github('mazamascience/PWFSLSmokePlots', build_vignettes=TRUE)
```

----

This project is being funded in part by the USFS Pacific Wildland Fire Sciences 
Laboratory.


