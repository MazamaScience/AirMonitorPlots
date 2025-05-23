% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monitor_ggDailyByHour_archival.R
\name{monitor_ggDailyByHour_archival}
\alias{monitor_ggDailyByHour_archival}
\title{Create an archival diurnal plot for one or more monitors}
\usage{
monitor_ggDailyByHour_archival(
  monitor,
  startdate = NULL,
  enddate = NULL,
  id = NULL,
  style = c("large", "small"),
  title = NULL,
  timezone = NULL,
  ...
)
}
\arguments{
\item{monitor}{A \emph{mts_monitor} object.}

\item{startdate}{Desired startdate for data to include, in a format that can
be parsed with \link[MazamaCoreUtils]{parseDatetime}.}

\item{enddate}{Desired enddate for data to include, in a format that can be
parsed with \link[MazamaCoreUtils]{parseDatetime}.}

\item{id}{deviceDeploymentID to include in the plot. This can be NULL if
\code{monitor} only has one unique deviceDeploymentID.}

\item{style}{String indicating plotting style. Either \code{"large"} or
\code{"small"}. \code{style = "large"} is suitable for plots larger than
450x450px, and \code{"small"} is suitable for plots 450x450px or smaller.}

\item{title}{Plot title. If NULL, a suitable title will be constructed.}

\item{timezone}{Olson timezone name for x-axis scale and date parsing. If
NULL the timezone of the specified monitor will be used.}

\item{...}{Extra arguments passed to \code{ggplot_pm25Diurnal()}.}
}
\value{
A \emph{ggplot} object.
}
\description{
This function assembles various layers to create a production-ready archival
diurnal plot for a single monitor.

The full range of data in \code{monitor} will be used unless both
\code{startdate} and \code{enddate} are specified.
}
\examples{
library(AirMonitorPlots)

AirMonitor::Carmel_Valley \%>\%
  AirMonitor::monitor_trimDate() \%>\%
  monitor_ggDailyByHour_archival()

\dontrun{
# Fail gracefully if any resources are not available
try({
monitor <- AirMonitor::airnow_loadLatest()
monitor_ggDailyByHour_archival(monitor, id = "51b9bcb4eaac7c9d_530330030")
}, silent = FALSE)
}
}
