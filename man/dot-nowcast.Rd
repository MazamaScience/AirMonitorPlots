% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nowcast.R
\name{.nowcast}
\alias{.nowcast}
\title{Calculate NowCast values}
\usage{
.nowcast(x, version = "pm", includeShortTerm = FALSE)
}
\arguments{
\item{x}{Vctor of ordered PM2.5 data in units of ug/m3.}

\item{version}{Character identity specifying the type of nowcast algorithm to
be used. For details see \code{AirMonitor::monitor_nowcast()}.}

\item{includeShortTerm}{Logical specifying whether to calculate preliminary
NowCast values starting with the 2nd hour}
}
\value{
Vector of unitless NowCast values of the same length as \code{x}.
}
\description{
This function calculates the NowCast version of the incoming data.
}
