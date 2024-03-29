% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/custom_datetimeScale.R
\name{custom_datetimeScale}
\alias{custom_datetimeScale}
\title{Add date scale with custom styling}
\usage{
custom_datetimeScale(
  startdate = NULL,
  enddate = NULL,
  timezone = NULL,
  expand = c(0, 0.05),
  break_width = NULL,
  minor_break_width = NULL,
  date_labels = "\%b \%d",
  tick_location = c("midnight", "midday"),
  includeFullEnddate = TRUE,
  today_label = TRUE,
  ...
)
}
\arguments{
\item{startdate}{Desired start date (integer or character in ymd format or
POSIXct).}

\item{enddate}{Desired end date (integer or character in ymd format or
POSIXct).}

\item{timezone}{Timezone for label formatting.}

\item{expand}{Vector of range expansion constants used to add some padding
around the data, to ensure that they are placed some distance away from the
axes.}

\item{break_width}{Space between breaks. If \code{NULL}, suitable breaks are
calculated based on the data. See 'Details'.}

\item{minor_break_width}{Space between minor breaks. If \code{NULL}, suitable
breaks are calculated based on the data. See 'Details'.}

\item{date_labels}{date format string for formatting date labels.}

\item{tick_location}{Location of ticks ("midnight" or "midday")}

\item{includeFullEnddate}{if \code{TRUE}, the x-axis limit is pushed up to
include the full final day.}

\item{today_label}{if \code{FALSE}, no label will be generated for today.}

\item{...}{Additional arguments passed onto
\code{\link[ggplot2]{scale_x_datetime}}.}
}
\description{
Add a date scale and custom formatting for creating
consistent timeseries plots.
}
\details{
\code{break_width} and \code{minor_break_width} can be specified in several
ways.
\itemize{
  \item A number, taken to be in days
  \item An object of class \code{difftime}
  \item A character string, containing one of \code{"day"}, \code{"week"},
        \code{"month"}, \code{"quarter"} or \code{"year"}. This can
        optionally be preceded by a (positive or negative) integer and a
        space or followed by \code{"s"}. For example: \code{"3 weeks"}.
}
}
