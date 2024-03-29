% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/custom_pm25TimeseriesScales.R
\name{custom_pm25TimeseriesScales}
\alias{custom_pm25TimeseriesScales}
\title{USFS AirFire PM2.5 timeseries scales}
\usage{
custom_pm25TimeseriesScales(
  monitor = NULL,
  startdate = NULL,
  enddate = NULL,
  ylim = NULL,
  timezone = NULL,
  xlab = NULL,
  yexp = c(0.05, 0),
  xexp = c(0, 0.05),
  ...
)
}
\arguments{
\item{monitor}{A \emph{mts_monitor} object.Should match the default dataset
of the plot.}

\item{startdate}{Desired start date (integer or character in ymd format or
POSIXct).}

\item{enddate}{Desired end date (integer or character in ymd format or
POSIXct).}

\item{ylim}{custom y-axis limits. This function will apply a default limit
depending on the data.}

\item{timezone}{Timezone for x-axis scale. If NULL and only one timezone
present in \code{monitor}, that timezone will be used. If NULL and multiple
timezones present, the default is UTC.}

\item{xlab}{Custom xlab. If \code{NULL} a default xlab will be generated.}

\item{yexp}{Vector of range expansion constants used to add some padding
around the data on the y-axis, to ensure that they are placed some distance
away from the axes.}

\item{xexp}{Vector of range expansion constants used to add some padding
around the data on the x-axis, to ensure that they are placed some distance
away from the axes.}

\item{...}{Additional arguments passed on to
\code{\link{custom_datetimeScale}}.}
}
\description{
Add USFS AirFire-style x-axis and y-axis scales suitable for a timeseries plot
showing PM2.5 data.
}
