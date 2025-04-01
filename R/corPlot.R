#' Correlation matrices with conditioning
#'
#' Function to to draw and visualise correlation matrices using lattice. The
#' primary purpose is as a tool for exploratory data analysis. Hierarchical
#' clustering is used to group similar variables.
#'
#' The `corPlot` function plots correlation matrices. The implementation
#' relies heavily on that shown in Sarkar (2007), with a few extensions.
#'
#' Correlation matrices are a very effective way of understating relationships
#' between many variables. The `corPlot` shows the correlation coded in
#' three ways: by shape (ellipses), colour and the numeric value. The ellipses
#' can be thought of as visual representations of scatter plot. With a perfect
#' positive correlation a line at 45 degrees positive slope is drawn. For zero
#' correlation the shape becomes a circle. See examples below.
#'
#' With many different variables it can be difficult to see relationships
#' between variables, i.e., which variables tend to behave most like one
#' another. For this reason hierarchical clustering is applied to the
#' correlation matrices to group variables that are most similar to one another
#' (if `cluster = TRUE`).
#'
#' If clustering is chosen it is also possible to add a dendrogram using the
#' option `dendrogram = TRUE`. Note that dendrogramscan only be plotted for
#' `type = "default"` i.e. when there is only a single panel. The
#' dendrogram can also be recovered from the plot object itself and plotted more
#' clearly; see examples below.
#'
#' It is also possible to use the `openair` type option to condition the
#' data in many flexible ways, although this may become difficult to visualise
#' with too many panels.
#'
#' @param mydata A data frame which should consist of some numeric columns.
#' @param pollutants the names of data-series in `mydata` to be plotted by
#'   `corPlot`. The default option `NULL` and the alternative
#'   \dQuote{all} use all available valid (numeric) data.
#' @param type `type` determines how the data are split i.e. conditioned,
#'   and then plotted. The default is will produce a single plot using the
#'   entire data. Type can be one of the built-in types as detailed in
#'   `cutData` e.g. \dQuote{season}, \dQuote{year}, \dQuote{weekday} and so
#'   on. For example, `type = "season"` will produce four plots --- one for
#'   each season.
#'
#'   It is also possible to choose `type` as another variable in the data
#'   frame. If that variable is numeric, then the data will be split into four
#'   quantiles (if possible) and labelled accordingly. If type is an existing
#'   character or factor variable, then those categories/levels will be used
#'   directly. This offers great flexibility for understanding the variation of
#'   different variables and how they depend on one another.
#' @param cluster Should the data be ordered according to cluster analysis. If
#'   `TRUE` hierarchical clustering is applied to the correlation matrices
#'   using `hclust` to group similar variables together. With many
#'   variables clustering can greatly assist interpretation.
#' @param method The correlation method to use. Can be \dQuote{pearson},
#'   \dQuote{spearman} or \dQuote{kendall}.
#' @param use How to handle missing values in the `cor` function. The default is
#'   "pairwise.complete.obs". Care should be taken with the choice of how to
#'   handle missing data when considering pair-wise correlations.
#' @param dendrogram Should a dendrogram be plotted? When `TRUE` a
#'   dendrogram is shown on the right of the plot. Note that this will only work
#'   for `type = "default"`.
#' @param lower Should only the lower triangle be plotted?
#' @param cols Colours to be used for plotting. Options include
#'   \dQuote{default}, \dQuote{increment}, \dQuote{heat}, \dQuote{spectral},
#'   \dQuote{hue}, \dQuote{greyscale} and user defined (see `openColours`
#'   for more details).
#' @param r.thresh Values of greater than `r.thresh` will be shown in bold
#'   type. This helps to highlight high correlations.
#' @param text.col The colour of the text used to show the correlation values.
#'   The first value controls the colour of negative correlations and the second
#'   positive.
#' @param auto.text Either `TRUE` (default) or `FALSE`. If `TRUE`
#'   titles and axis labels will automatically try and format pollutant names
#'   and units properly e.g.  by subscripting the `2' in NO2.
#' @param plot Should a plot be produced? `FALSE` can be useful when
#'   analysing data to extract corPlot components and plotting them in other
#'   ways.
#' @param ... Other graphical parameters passed onto `lattice:levelplot`,
#'   with common axis and title labelling options (such as `xlab`,
#'   `ylab`, `main`) being passed via `quickText` to handle
#'   routine formatting.
#' @export
#' @return an [openair][openair-package] object
#' @author David Carslaw --- but mostly based on code contained in Sarkar (2007)
#' @seealso `taylor.diagram` from the `plotrix` package from which
#'   some of the annotation code was used.
#' @references Sarkar, D. (2007). Lattice Multivariate Data Visualization with
#'   R. New York: Springer.
#'
#'   Friendly, M. (2002). Corrgrams : Exploratory displays for correlation
#'   matrices. American Statistician, 2002(4), 1-16. doi:10.1198/000313002533
#' @examples
#' ## basic corrgram plot
#' corPlot(mydata)
#' ## plot by season ... and so on
#' corPlot(mydata, type = "season")
#' ## recover dendrogram when cluster = TRUE and plot it
#' res <- corPlot(mydata)
#' plot(res$clust)
#' \dontrun{
#' ## a more interesting are hydrocarbon measurements
#' hc <- importAURN(site = "my1", year = 2005, hc = TRUE)
#' ## now it is possible to see the hydrocarbons that behave most
#' ## similarly to one another
#' corPlot(hc)
#' }
#'
corPlot <- function(
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
) {
  if (length(type) > 1) stop("Only one 'type' allowed in this function.")

  ## make sure date is present for types requiring it
  if (any(type %in% dateTypes)) {
    if (!"date" %in% names(mydata)) stop("Need a field 'date'")
  }

  ## greyscale handling
  if (length(cols) == 1 && cols == "greyscale") {
    trellis.par.set(list(strip.background = list(col = "white")))
  }

  ## set graphics
  current.strip <- trellis.par.get("strip.background")
  current.font <- trellis.par.get("fontsize")

  ## reset graphic parameters
  on.exit(trellis.par.set(
    fontsize = current.font
  ))

  ## extra.args setup
  extra.args <- list(...)

  # label controls
  extra.args$xlab <- if ("xlab" %in% names(extra.args)) {
    quickText(extra.args$xlab, auto.text)
  } else {
    quickText(NULL, auto.text)
  }
  extra.args$ylab <- if ("ylab" %in% names(extra.args)) {
    quickText(extra.args$ylab, auto.text)
  } else {
    quickText(NULL, auto.text)
  }
  extra.args$main <- if ("main" %in% names(extra.args)) {
    quickText(extra.args$main, auto.text)
  } else {
    quickText("", auto.text)
  }

  if ("fontsize" %in% names(extra.args)) {
    trellis.par.set(fontsize = list(text = extra.args$fontsize))
  }

  # layout default
  if (!"layout" %in% names(extra.args)) {
    extra.args$layout <- NULL
  }

  ## pollutant(s) handling

  # null and all cases
  if (is.null(pollutants)) {
    pollutants <- names(mydata)
  }
  if (
    is.character(pollutants) && length(pollutants) == 1 && pollutants == "all"
  ) {
    pollutants <- names(mydata)
  }

  # keep date if about
  pollutants <- if ("date" %in% names(mydata)) {
    unique(c("date", pollutants))
  } else {
    unique(c(pollutants))
  }

  mydata <- checkPrep(
    mydata,
    pollutants,
    type = type,
    remove.calm = FALSE
  )

  ## remove variables where all are NA
  mydata <- mydata[, sapply(mydata, function(x) !all(is.na(x)))]

  ## cut data depending on type
  mydata <- cutData(mydata, type, ...)

  ## proper names of labelling
  pollutants <- names(mydata[, sapply(mydata, is.numeric)])
  pol.name <- sapply(
    pollutants,
    function(x) quickText(x, auto.text)
  )

  ## number of pollutants
  npol <- length(pol.name)

  if (npol < 2) stop("Need at least two valid (numeric) fields to compare")

  prepare.cond <- function(mydata) {
    ## calculate the correlations

    thedata <- cor(
      select(mydata, where(is.numeric)),
      use = use,
      method = method
    )

    ## remove columns/rows where all are NA
    therows <- apply(thedata, 1, function(x) !all(is.na(x)))
    thecols <- apply(thedata, 2, function(x) !all(is.na(x)))
    thedata <- thedata[therows, thecols]

    ## maybe reduced number of pollutants, hence select only those present
    thepols <- pol.name[thecols]

    if (cluster) {
      ## for plotting dendogram
      clust <- hclust(dist(thedata))
      ord.dat <- order.dendrogram(as.dendrogram(hclust(dist(thedata))))
    } else {
      clust <- NULL
      ord.dat <- 1:ncol(thedata)
    }

    npol <- length(ord.dat)
    grid <- expand.grid(x = 1:npol, y = 1:npol)

    thepols <- thepols[ord.dat]

    thedata <- thedata[ord.dat, ord.dat]
    thedata <- as.vector(thedata)

    thedata <- cbind(grid, z = thedata)
    thedata <- list(
      thedata = thedata,
      pol.name = thepols,
      pol.ord = ord.dat,
      clust = clust
    )
    thedata
  }

  # main results in lists
  results.grid <- mydata %>%
    group_by(across(type)) %>%
    group_nest() %>%
    mutate(results = map(data, prepare.cond))

  # cluster model
  clust <- results.grid %>%
    mutate(clust = map(results, 4))
  clust <- clust$clust[[1]]

  ## recover by-type order

  data.order <- results.grid %>%
    mutate(out = map(results, 3))

  data.order <- lapply(data.order$out, function(x) pollutants[x])

  x2 <- unlist(lapply(1:length(data.order), function(x) {
    (rep(data.order[[x]], times = length(data.order[[x]])))
  }))
  y2 <- unlist(lapply(1:length(data.order), function(x) {
    (rep(data.order[[x]], each = length(data.order[[x]])))
  }))

  ## list of labels

  labels <- results.grid %>%
    mutate(out = map(results, 2))

  labels <- labels$out

  # vars we want
  vars <- c(type, "out")

  results.grid <- results.grid %>%
    mutate(out = map(results, 1)) %>%
    select(vars) %>%
    unnest(cols = c(out))

  div.col <- function(x) openColours(cols, x)

  ## labelleing of strips
  pol.name <- sapply(
    levels(results.grid[[type]]),
    function(x) quickText(x, auto.text)
  )
  strip <- strip.custom(factor.levels = pol.name)
  if (type == "default") strip <- FALSE

  ## special wd layout

  if (length(type) == 1 & type[1] == "wd" & is.null(extra.args$layout)) {
    ## re-order to make sensible layout
    ## starting point code as of ManKendall

    wds <- c("NW", "N", "NE", "W", "E", "SW", "S", "SE")
    results.grid[[type]] <- ordered(results.grid[[type]], levels = wds)
    wd.ok <- sapply(wds, function(x) {
      if (x %in% unique(results.grid[[type]])) FALSE else TRUE
    })

    skip <- c(wd.ok[1:4], TRUE, wd.ok[5:8])
    results.grid[[type]] <- factor(results.grid[[type]])
    extra.args$layout <- c(3, 3)
    if (!"skip" %in% names(extra.args)) {
      extra.args$skip <- skip
    }
  }

  if (!"skip" %in% names(extra.args)) {
    extra.args$skip <- FALSE
  }

  strip.dat <- strip.fun(results.grid, type, auto.text)
  strip <- strip.dat[[1]]
  strip.left <- strip.dat[[2]]
  pol.name <- strip.dat[[3]]

  ## plot dendrogram
  if (dendrogram && type == "default" && cluster) {
    legend <- list(
      right = list(
        fun = dendrogramGrob,
        args = list(
          x = as.dendrogram(clust),
          side = "right",
          size = 4
        )
      )
    )
  } else {
    legend <- NULL
  }

  temp <- paste(type, collapse = "+")
  myform <- formula(paste("z ~ x * y | ", temp, sep = ""))

  ## plot via ... handler
  levelplot.args <- list(
    x = myform,
    data = results.grid,
    at = do.breaks(c(-1.01, 1.01), 100),
    strip = strip,
    as.table = TRUE,
    aspect = 1,
    colorkey = FALSE,
    col.regions = div.col,
    legend = legend,
    par.strip.text = list(cex = 0.8),
    scales = list(
      x = list(rot = 90, labels = labels, at = 1:npol),
      y = list(labels = labels, at = 1:npol),
      relation = "free"
    ),
    text.col = text.col,
    r.thresh = r.thresh,
    label = TRUE,
    panel = function(x, y, z, ...) {
      panel.abline(v = 1:sqrt(length(z)), col = "grey95")
      panel.abline(h = 1:sqrt(length(z)), col = "grey95")
      panel.corrgram(x, y, z, lower = lower, ...)
    }
  )

  # reset for extra.args
  levelplot.args <- listUpdate(levelplot.args, extra.args)

  # plot
  plt <- do.call(levelplot, levelplot.args)

  #################
  # output
  #################

  if (plot) plot(plt)

  ## openair object

  newdata <- results.grid

  # tidy newdata for output
  rownames(newdata) <- NULL
  names(newdata)[names(newdata) == "z"] <- "cor"
  names(newdata)[names(newdata) == "x"] <- "row"
  names(newdata)[names(newdata) == "y"] <- "col"
  newdata <- cbind(x = x2, y = y2, newdata)

  # main handling
  output <-
    list(
      plot = plt,
      data = dplyr::tibble(newdata),
      call = match.call(),
      clust = clust
    )
  class(output) <- "openair"
  invisible(output)
}

panel.corrgram <- function(
  x,
  y,
  z,
  subscripts,
  at,
  level = 0.9,
  text.col,
  r.thresh = r.thresh,
  label = FALSE,
  lower = lower,
  ...
) {
  x <- as.numeric(x)[subscripts]
  y <- as.numeric(y)[subscripts]
  z <- as.numeric(z)[subscripts]

  zcol <- level.colors(z, at = at, ...)

  # just do lower triangle
  len <- length(z)
  tmp <- matrix(seq_along(z), nrow = len^(1 / 2))

  if (lower) {
    id <- which(lower.tri(tmp, diag = TRUE))
  } else {
    id <- 1:length(tmp)
  }

  for (i in seq(along = id)) {
    ell <- ellipse(
      z[id[i]],
      level = level,
      npoints = 50,
      scale = c(.2, .2),
      centre = c(x[id[i]], y[id[i]])
    )
    panel.polygon(ell, col = zcol[id[i]], border = zcol[id[i]], ...)
  }
  if (label) {
    panel.text(
      x = x[id],
      y = y[id],
      lab = 100 * round(z[id], 2),
      cex = 0.8,
      col = ifelse(z[id] < 0, text.col[1], text.col[2]),
      font = ifelse(z[id] < r.thresh, 1, 2)
    )
  }
}


## from ellipse package
ellipse <- function(
  x,
  scale = c(1, 1),
  centre = c(0, 0),
  level = 0.95,
  t = sqrt(qchisq(level, 2)),
  which = c(1, 2),
  npoints = 100,
  ...
) {
  names <- c("x", "y")
  if (is.matrix(x)) {
    xind <- which[1]
    yind <- which[2]
    r <- x[xind, yind]
    if (missing(scale)) {
      scale <- sqrt(c(x[xind, xind], x[yind, yind]))
      if (scale[1] > 0) r <- r / scale[1]
      if (scale[2] > 0) r <- r / scale[2]
    }
    if (!is.null(dimnames(x)[[1]])) {
      names <- dimnames(x)[[1]][c(xind, yind)]
    }
  } else {
    r <- x
  }
  r <- min(max(r, -1), 1) # clamp to -1..1, in case of rounding errors
  d <- acos(r)
  a <- seq(0, 2 * pi, len = npoints)
  matrix(
    c(
      t * scale[1] * cos(a + d / 2) + centre[1],
      t * scale[2] * cos(a - d / 2) + centre[2]
    ),
    npoints,
    2,
    dimnames = list(
      NULL,
      names
    )
  )
}
