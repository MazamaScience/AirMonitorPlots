% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monitor_ggCalendar.R
\name{monitor_ggCalendar}
\alias{monitor_ggCalendar}
\title{Calendar plot}
\usage{
monitor_ggCalendar(
  monitor = NULL,
  id = NULL,
  ncol = 4,
  title = NULL,
  discrete = TRUE,
  breaks = NULL,
  break_labels = NULL,
  aspect_ratio = 1,
  legend_title = NULL,
  stat = "mean"
)
}
\arguments{
\item{monitor}{A \emph{mts_monitor} object.}

\item{id}{deviceDeploymentID to include in the plot. This can be NULL if
\code{monitor} only has one unique deviceDeploymentID.}

\item{ncol}{Number of columns in the plot.}

\item{title}{Optional title.}

\item{discrete}{Logical specifying use of a discrete color scale.}

\item{breaks}{Color scale break points.}

\item{break_labels}{Scale breaks names.}

\item{aspect_ratio}{Plot aspect ratio.}

\item{legend_title}{Optional title used in the legend.}

\item{stat}{Statistic used for daily aggregation (default: "mean").}
}
\value{
ggobject
}
\description{
Annual calendar view of a daily reading using a selected input.
}
