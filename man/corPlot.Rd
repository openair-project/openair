% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corPlot.R
\name{corPlot}
\alias{corPlot}
\title{Correlation matrices with conditioning}
\usage{
corPlot(
  mydata,
  pollutants = NULL,
  type = "default",
  cluster = TRUE,
  method = "pearson",
  use = "pairwise.complete.obs",
  dendrogram = FALSE,
  lower = FALSE,
  cols = "default",
  r.thresh = 0.8,
  text.col = c("black", "black"),
  auto.text = TRUE,
  plot = TRUE,
  ...
)
}
\arguments{
\item{mydata}{A data frame which should consist of some numeric columns.}

\item{pollutants}{the names of data-series in \code{mydata} to be plotted by
\code{corPlot}. The default option \code{NULL} and the alternative
\dQuote{all} use all available valid (numeric) data.}

\item{type}{\code{type} determines how the data are split i.e. conditioned,
and then plotted. The default is will produce a single plot using the
entire data. Type can be one of the built-in types as detailed in
\code{cutData} e.g. \dQuote{season}, \dQuote{year}, \dQuote{weekday} and so
on. For example, \code{type = "season"} will produce four plots --- one for
each season.

It is also possible to choose \code{type} as another variable in the data
frame. If that variable is numeric, then the data will be split into four
quantiles (if possible) and labelled accordingly. If type is an existing
character or factor variable, then those categories/levels will be used
directly. This offers great flexibility for understanding the variation of
different variables and how they depend on one another.}

\item{cluster}{Should the data be ordered according to cluster analysis. If
\code{TRUE} hierarchical clustering is applied to the correlation matrices
using \code{hclust} to group similar variables together. With many
variables clustering can greatly assist interpretation.}

\item{method}{The correlation method to use. Can be \dQuote{pearson},
\dQuote{spearman} or \dQuote{kendall}.}

\item{use}{How to handle missing values in the \code{cor} function. The default is
"pairwise.complete.obs". Care should be taken with the choice of how to
handle missing data when considering pair-wise correlations.}

\item{dendrogram}{Should a dendrogram be plotted? When \code{TRUE} a
dendrogram is shown on the right of the plot. Note that this will only work
for \code{type = "default"}.}

\item{lower}{Should only the lower triangle be plotted?}

\item{cols}{Colours to be used for plotting. Options include
\dQuote{default}, \dQuote{increment}, \dQuote{heat}, \dQuote{spectral},
\dQuote{hue}, \dQuote{greyscale} and user defined (see \code{openColours}
for more details).}

\item{r.thresh}{Values of greater than \code{r.thresh} will be shown in bold
type. This helps to highlight high correlations.}

\item{text.col}{The colour of the text used to show the correlation values.
The first value controls the colour of negative correlations and the second
positive.}

\item{auto.text}{Either \code{TRUE} (default) or \code{FALSE}. If \code{TRUE}
titles and axis labels will automatically try and format pollutant names
and units properly e.g.  by subscripting the `2' in NO2.}

\item{plot}{Should a plot be produced? \code{FALSE} can be useful when
analysing data to extract corPlot components and plotting them in other
ways.}

\item{...}{Other graphical parameters passed onto \code{lattice:levelplot},
with common axis and title labelling options (such as \code{xlab},
\code{ylab}, \code{main}) being passed via \code{quickText} to handle
routine formatting.}
}
\value{
an \link[=openair-package]{openair} object
}
\description{
Function to to draw and visualise correlation matrices using lattice. The
primary purpose is as a tool for exploratory data analysis. Hierarchical
clustering is used to group similar variables.
}
\details{
The \code{corPlot} function plots correlation matrices. The implementation
relies heavily on that shown in Sarkar (2007), with a few extensions.

Correlation matrices are a very effective way of understating relationships
between many variables. The \code{corPlot} shows the correlation coded in
three ways: by shape (ellipses), colour and the numeric value. The ellipses
can be thought of as visual representations of scatter plot. With a perfect
positive correlation a line at 45 degrees positive slope is drawn. For zero
correlation the shape becomes a circle. See examples below.

With many different variables it can be difficult to see relationships
between variables, i.e., which variables tend to behave most like one
another. For this reason hierarchical clustering is applied to the
correlation matrices to group variables that are most similar to one another
(if \code{cluster = TRUE}).

If clustering is chosen it is also possible to add a dendrogram using the
option \code{dendrogram = TRUE}. Note that dendrogramscan only be plotted for
\code{type = "default"} i.e. when there is only a single panel. The
dendrogram can also be recovered from the plot object itself and plotted more
clearly; see examples below.

It is also possible to use the \code{openair} type option to condition the
data in many flexible ways, although this may become difficult to visualise
with too many panels.
}
\examples{
## basic corrgram plot
corPlot(mydata)
## plot by season ... and so on
corPlot(mydata, type = "season")
## recover dendrogram when cluster = TRUE and plot it
res <- corPlot(mydata)
plot(res$clust)
\dontrun{
## a more interesting are hydrocarbon measurements
hc <- importAURN(site = "my1", year = 2005, hc = TRUE)
## now it is possible to see the hydrocarbons that behave most
## similarly to one another
corPlot(hc)
}

}
\references{
Sarkar, D. (2007). Lattice Multivariate Data Visualization with
R. New York: Springer.

Friendly, M. (2002). Corrgrams : Exploratory displays for correlation
matrices. American Statistician, 2002(4), 1-16. doi:10.1198/000313002533
}
\seealso{
\code{taylor.diagram} from the \code{plotrix} package from which
some of the annotation code was used.
}
\author{
David Carslaw --- but mostly based on code contained in Sarkar (2007)
}
