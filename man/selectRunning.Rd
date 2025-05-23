% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/selectRunning.R
\name{selectRunning}
\alias{selectRunning}
\title{Function to extract run lengths greater than a threshold}
\usage{
selectRunning(
  mydata,
  pollutant = "nox",
  criterion = ">",
  run.len = 5L,
  threshold = 500,
  type = "default",
  name = "criterion",
  result = c("yes", "no"),
  mode = c("flag", "filter"),
  ...
)
}
\arguments{
\item{mydata}{A data frame with a \code{date} field and at least one numeric
\code{pollutant} field to analyse.}

\item{pollutant}{Name of variable to process.}

\item{criterion}{Condition to select run lengths e.g. \code{">"} with select data
more than \code{threshold}.}

\item{run.len}{Run length for extracting contiguous values of \code{pollutant}
meeting the \code{criterion} in relation to the \code{threshold}.}

\item{threshold}{The threshold value for \code{pollutant} above which data should
be extracted.}

\item{type}{Used for splitting the data further. Passed to \code{\link[=cutData]{cutData()}}.}

\item{name}{The name of the column to be appended to the data frame when
\code{mode = "flag"}.}

\item{result}{A vector of length 2, defining how to label the run lengths
when \code{mode = "flag"}. The first object should be the label for the \code{TRUE}
label, and the second the \code{FALSE} label - e.g., \code{c("yes", "no")}.}

\item{mode}{Changes how the function behaves. When \code{mode = "flag"}, the
default, the function appends a column flagging where the criteria was met.
Alternatively, \code{"filter"} will filter \code{mydata} to only return rows where
the criteria was met.}

\item{...}{Additional parameters passed to \code{\link[=cutData]{cutData()}}. For use with \code{type}.}
}
\value{
A data frame
}
\description{
This is a utility function to extract runs of values above a certain
threshold. For example, for a data frame of hourly NOx values we would like
to extract all those hours where the concentration is at least 500 for
contiguous periods of 5 or more hours.
}
\details{
This function is useful, for example, for selecting pollution episodes from a
data frame where concentrations remain elevated for a certain period of time.
It may also be of more general use when analysing air pollution and
atmospheric composition data. For example, \code{\link[=selectRunning]{selectRunning()}} could be used to
extract continuous periods of rainfall --- which could be important for
particle concentrations.
}
\examples{
# extract those hours where there are at least 5 consecutive NOx
# concentrations above 500 units
mydata <- selectRunning(mydata, run.len = 5, threshold = 500)

# make a polar plot of those conditions, which shows that those
# conditions are dominated by low wind speeds, not
# in-canyon recirculation
\dontrun{
polarPlot(mydata, pollutant = "nox", type = "criterion")
}
}
\author{
David Carslaw
}
