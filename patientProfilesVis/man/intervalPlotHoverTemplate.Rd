% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotlyIntervalPlot.R
\name{intervalPlotHoverTemplate}
\alias{intervalPlotHoverTemplate}
\title{Create [plotly] tool tip hover template for an interval plot}
\usage{
intervalPlotHoverTemplate(
  data,
  yLab,
  timeStartVar,
  timeStartLab,
  timeEndVar,
  timeEndLab,
  colorLab = NULL,
  colorVar = NULL,
  add_vars = NULL
)
}
\arguments{
\item{data}{Data.frame with data.}

\item{yLab}{String, label for the y-axis.}

\item{timeStartVar}{String, variable of \code{data} 
with start of time interval.}

\item{timeStartLab}{String, label for \code{timeStartVar},
displayed in a message and in the plot caption.}

\item{timeEndVar}{String, variable of \code{data} 
with end of time interval.}

\item{timeEndLab}{String, label for \code{timeEndVar},
displayed in a message and in the plot caption.}

\item{colorLab}{String, label for \code{colorVar}.}

\item{colorVar}{String, variable of \code{data} with color.}

\item{add_vars}{see [subjectProfileIntervalPlot()] argument `plotly_add_vars`}
}
\value{
a character vector of html formatted strings
}
\description{
Create [plotly] tool tip hover template for an interval plot
}
