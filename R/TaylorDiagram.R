
#' Taylor Diagram for model evaluation with conditioning
#'
#' Function to draw Taylor Diagrams for model evaluation. The function allows
#' conditioning by any categorical or numeric variables, which makes the
#' function very flexible.
#'
#' The Taylor Diagram is a very useful model evaluation tool. The diagram
#' provides a way of showing how three complementary model performance
#' statistics vary simultaneously. These statistics are the correlation
#' coefficient R, the standard deviation (sigma) and the (centred)
#' root-mean-square error. These three statistics can be plotted on one (2D)
#' graph because of the way they are related to one another which can be
#' represented through the Law of Cosines.
#'
#' The `openair` version of the Taylor Diagram has several enhancements
#' that increase its flexibility. In particular, the straightforward way of
#' producing conditioning plots should prove valuable under many circumstances
#' (using the `type` option). Many examples of Taylor Diagrams focus on
#' model-observation comparisons for several models using all the available
#' data. However, more insight can be gained into model performance by
#' partitioning the data in various ways e.g. by season, daylight/nighttime, day
#' of the week, by levels of a numeric variable e.g. wind speed or by land-use
#' type etc.
#'
#' To consider several pollutants on one plot, a column identifying the
#' pollutant name can be used e.g. `pollutant`. Then the Taylor Diagram can
#' be plotted as (assuming a data frame `thedata`):
#'
#' `TaylorDiagram(thedata, obs = "obs", mod = "mod", group = "model", type
#' = "pollutant")`
#'
#' which will give the model performance by pollutant in each panel.
#'
#' Note that it is important that each panel represents data with the same mean
#' observed data across different groups. Therefore `TaylorDiagram(mydata,
#' group = "model", type = "season")` is OK, whereas `TaylorDiagram(mydata,
#' group = "season", type = "model")` is not because each panel (representing a
#' model) will have four different mean values --- one for each season.
#' Generally, the option `group` is either missing (one model being
#' evaluated) or represents a column giving the model name. However, the data
#' can be normalised using the `normalise` option. Normalisation is carried
#' out on a per `group`/`type` basis making it possible to compare
#' data on different scales e.g. `TaylorDiagram(mydata, group = "season",
#' type = "model", normalise = TRUE)`. In this way it is possible to compare
#' different pollutants, sites etc. in the same panel.
#'
#' Also note that if multiple sites are present it makes sense to use `type
#' = "site"` to ensure that each panel represents an individual site with its
#' own specific standard deviation etc. If this is not the case then select a
#' single site from the data first e.g. `subset(mydata, site ==
#' "Harwell")`.
#'
#' @param mydata A data frame minimally containing a column of observations and
#'   a column of predictions.
#' @param obs A column of observations with which the predictions (`mod`)
#'   will be compared.
#' @param mod A column of model predictions. Note, `mod` can be of length 2
#'   i.e. two lots of model predictions. If two sets of predictions are are
#'   present e.g. `mod = c("base", "revised")`, then arrows are shown on
#'   the Taylor Diagram which show the change in model performance in going from
#'   the first to the second. This is useful where, for example, there is
#'   interest in comparing how one model run compares with another using
#'   different assumptions e.g. input data or model set up. See examples below.
#' @param group The `group` column is used to differentiate between
#'   different models and can be a factor or character. The total number of
#'   models compared will be equal to the number of unique values of
#'   `group`.
#'
#'   `group` can also be of length two e.g. `group = c("model",
#'   "site")`. In this case all model-site combinations will be shown but they
#'   will only be differentiated by colour/symbol by the first grouping variable
#'   ("model" in this case). In essence the plot removes the differentiation by
#'   the second grouping variable. Because there will be different values of
#'   `obs` for each group, `normalise = TRUE` should be used.
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
#'
#'   Type can be up length two e.g. `type = c("season", "weekday")` will
#'   produce a 2x2 plot split by season and day of the week. Note, when two
#'   types are provided the first forms the columns and the second the rows.
#'
#'   Note that often it will make sense to use `type = "site"` when
#'   multiple sites are available. This will ensure that each panel contains
#'   data specific to an individual site.
#' @param normalise Should the data be normalised by dividing the standard
#'   deviation of the observations? The statistics can be normalised (and
#'   non-dimensionalised) by dividing both the RMS difference and the standard
#'   deviation of the `mod` values by the standard deviation of the
#'   observations (`obs`). In this case the \dQuote{observed} point is
#'   plotted on the x-axis at unit distance from the origin. This makes it
#'   possible to plot statistics for different species (maybe with different
#'   units) on the same plot. The normalisation is done by each
#'   `group`/`type` combination.
#' @param cols Colours to be used for plotting. Useful options for categorical
#'   data are available from `RColorBrewer` colours --- see the
#'   `openair` `openColours` function for more details. Useful schemes
#'   include \dQuote{Accent}, \dQuote{Dark2}, \dQuote{Paired}, \dQuote{Pastel1},
#'   \dQuote{Pastel2}, \dQuote{Set1}, \dQuote{Set2}, \dQuote{Set3} --- but see
#'   ?`brewer.pal` for the maximum useful colours in each. For user defined
#'   the user can supply a list of colour names recognised by R (type
#'   `colours()` to see the full list). An example would be `cols =
#'   c("yellow", "green", "blue")`.
#' @param rms.col Colour for centred-RMS lines and text.
#' @param cor.col Colour for correlation coefficient lines and text.
#' @param arrow.lwd Width of arrow used when used for comparing two model
#'   outputs.
#' @param annotate Annotation shown for RMS error.
#' @param text.obs The plot annotation for observed values; default is
#'   "observed".
#' @param key Should the key be shown?
#' @param key.title Title for the key.
#' @param key.columns Number of columns to be used in the key. With many
#'   pollutants a single column can make to key too wide. The user can thus
#'   choose to use several columns by setting `columns` to be less than the
#'   number of pollutants.
#' @param key.pos Position of the key e.g. \dQuote{top}, \dQuote{bottom},
#'   \dQuote{left} and \dQuote{right}. See details in `lattice:xyplot` for
#'   more details about finer control.
#' @param strip Should a strip be shown?
#' @param auto.text Either `TRUE` (default) or `FALSE`. If `TRUE`
#'   titles and axis labels will automatically try and format pollutant names
#'   and units properly e.g.  by subscripting the `2' in NO2.
#' @param ... Other graphical parameters are passed onto `cutData` and
#'   `lattice:xyplot`. For example, `TaylorDiagram` passes the option
#'   `hemisphere = "southern"` on to `cutData` to provide southern
#'   (rather than default northern) hemisphere handling of `type =
#'   "season"`. Similarly, common graphical parameters, such as `layout`
#'   for panel arrangement and `pch` and `cex` for plot symbol type
#'   and size, are passed on to `xyplot`. Most are passed unmodified,
#'   although there are some special cases where `openair` may locally
#'   manage this process. For example, common axis and title labelling options
#'   (such as `xlab`, `ylab`, `main`) are passed via
#'   `quickText` to handle routine formatting.
#' @export
#' @return an [openair][openair-package] object. If retained, e.g., using
#'   `output <- TaylorDiagram(thedata, obs = "nox", mod = "mod")`, this
#'   output can be used to recover the data, reproduce or rework the original
#'   plot or undertake further analysis. For example, `output$data` will be
#'   a data frame consisting of the group, type, correlation coefficient (R),
#'   the standard deviation of the observations and measurements.
#' @author David Carslaw
#' @family model evaluation functions
#' @seealso `taylor.diagram` from the `plotrix` package from which
#'   some of the annotation code was used.
#' @references
#'
#' Taylor, K.E.: Summarizing multiple aspects of model performance in a single
#' diagram. J.  Geophys. Res., 106, 7183-7192, 2001 (also see PCMDI Report 55).
#'
#' @examples
#' ## in the examples below, most effort goes into making some artificial data
#' ## the function itself can be run very simply
#' \dontrun{
#' ## dummy model data for 2003
#' dat <- selectByDate(mydata, year = 2003)
#' dat <- data.frame(date = mydata$date, obs = mydata$nox, mod = mydata$nox)
#'
#' ## now make mod worse by adding bias and noise according to the month
#' ## do this for 3 different models
#' dat <- transform(dat, month = as.numeric(format(date, "%m")))
#' mod1 <- transform(dat, mod = mod + 10 * month + 10 * month * rnorm(nrow(dat)),
#' model = "model 1")
#' ## lag the results for mod1 to make the correlation coefficient worse
#' ## without affecting the sd
#' mod1 <- transform(mod1, mod = c(mod[5:length(mod)], mod[(length(mod) - 3) :
#' length(mod)]))
#'
#' ## model 2
#' mod2 <- transform(dat, mod = mod + 7 * month + 7 * month * rnorm(nrow(dat)),
#' model = "model 2")
#' ## model 3
#' mod3 <- transform(dat, mod = mod + 3 * month + 3 * month * rnorm(nrow(dat)),
#' model = "model 3")
#'
#' mod.dat <- rbind(mod1, mod2, mod3)
#'
#' ## basic Taylor plot
#'
#' TaylorDiagram(mod.dat, obs = "obs", mod = "mod", group = "model")
#'
#' ## Taylor plot by season
#' TaylorDiagram(mod.dat, obs = "obs", mod = "mod", group = "model", type = "season")
#'
#' ## now show how to evaluate model improvement (or otherwise)
#' mod1a <- transform(dat, mod = mod + 2 * month + 2 * month * rnorm(nrow(dat)),
#' model = "model 1")
#' mod2a <- transform(mod2, mod = mod * 1.3)
#' mod3a <- transform(dat, mod = mod + 10 * month + 10 * month * rnorm(nrow(dat)),
#' model = "model 3")
#' mod.dat2 <- rbind(mod1a, mod2a, mod3a)
#' mod.dat$mod2 <- mod.dat2$mod
#'
#' ## now we have a data frame with 3 models, 1 set of observations
#' ## and TWO sets of model predictions (mod and mod2)
#'
#' ## do for all models
#' TaylorDiagram(mod.dat, obs = "obs", mod = c("mod", "mod2"), group = "model")
#' }
#' \dontrun{
#' ## all models, by season
#' TaylorDiagram(mod.dat, obs = "obs", mod = c("mod", "mod2"), group = "model",
#' type = "season")
#'
#' ## consider two groups (model/month). In this case all months are shown by model
#' ## but are only differentiated by model.
#'
#' TaylorDiagram(mod.dat, obs = "obs", mod = "mod", group = c("model", "month"))
#' }
TaylorDiagram <- function(mydata, obs = "obs", mod = "mod", group = NULL, type = "default",
                          normalise = FALSE, cols = "brewer1",
                          rms.col = "darkgoldenrod", cor.col = "black", arrow.lwd = 3,
                          annotate = "centred\nRMS error", text.obs = "observed",
                          key = TRUE, key.title = group, key.columns = 1,
                          key.pos = "right", strip = TRUE, auto.text = TRUE, ...) {

  ## get rid of R check annoyances
  sd.mod <- R <- NULL

  ## greyscale handling

  ## set graphics
  current.strip <- trellis.par.get("strip.background")
  current.font <- trellis.par.get("fontsize")

  ## reset graphic parameters
  on.exit(trellis.par.set(

    fontsize = current.font
  ))

  if (length(cols) == 1 && cols == "greyscale") {
    trellis.par.set(list(strip.background = list(col = "white")))
    ## other local colours
    method.col <- "greyscale"
  } else {
    method.col <- "default"
  }


  ## extra.args setup
  extra.args <- list(...)

  ## label controls (some local xlab, ylab management in code)
  extra.args$xlab <- if ("xlab" %in% names(extra.args)) {
    quickText(extra.args$xlab, auto.text)
  } else {
    NULL
  }

  extra.args$ylab <- if ("ylab" %in% names(extra.args)) {
    quickText(extra.args$ylab, auto.text)
  } else {
    NULL
  }

  extra.args$main <- if ("main" %in% names(extra.args)) {
    quickText(extra.args$main, auto.text)
  } else {
    quickText("", auto.text)
  }

  if ("fontsize" %in% names(extra.args)) {
    trellis.par.set(fontsize = list(text = extra.args$fontsize))
  }


  if (!"layout" %in% names(extra.args)) {
    extra.args$layout <- NULL
  }


  if (!"pch" %in% names(extra.args)) {
    extra.args$pch <- 20
  }

  if (!"cex" %in% names(extra.args)) {
    extra.args$cex <- 2
  }

  ## #######################################################################################

  ## check to see if two data sets are present
  combine <- FALSE

  if (length(mod) == 2) combine <- TRUE

  if (any(type %in% dateTypes)) {
    vars <- c("date", obs, mod)
  } else {
    vars <- c(obs, mod)
  }

  ## assume two groups do not exist
  twoGrp <- FALSE

  if (!missing(group)) if (any(group %in% type)) stop("Can't have 'group' also in 'type'.")

  mydata <- cutData(mydata, type, ...)

  if (missing(group)) {
    if ((!"group" %in% type) & (!"group" %in% c(obs, mod))) {
      mydata$group <- factor("group")
      group <- "group"
      npol <- 1
    }
    ## don't overwrite a
  } else { ## means that group is there
    mydata <- cutData(mydata, group, ...)
  }

  ## if group is present, need to add that list of variables unless it is
  ## a pre-defined date-based one
  if (!missing(group)) {
    npol <- length(unique((mydata[[group[1]]])))

    ## if group is of length 2
    if (length(group) == 2L) {
      twoGrp <- TRUE
      grp1 <- group[1]
      grp2 <- group[2]

      if (missing(key.title)) key.title <- grp1
      vars <- c(vars, grp1, grp2)
      mydata$newgrp <- paste(mydata[[group[1]]], mydata[[group[2]]], sep = "-")
      group <- "newgrp"
    }

    if (group %in% dateTypes | any(type %in% dateTypes)) {
      vars <- unique(c(vars, "date", group))
    } else {
      vars <- unique(c(vars, group))
    }
  }

  ## data checks, for base and new data if necessary

  mydata <- checkPrep(mydata, vars, type)

  # check mod and obs are numbers
  mydata <- checkNum(mydata, vars = c(obs, mod))

  ## remove missing data
  mydata <- na.omit(mydata)

  legend <- NULL

  ## function to calculate stats for TD
  calcStats <- function(mydata, obs = obs, mod = mod) {
    R <- cor(mydata[[obs]], mydata[[mod]], use = "pairwise")
    sd.obs <- sd(mydata[[obs]])
    sd.mod <- sd(mydata[[mod]])
    if (normalise) {
      sd.mod <- sd.mod / sd.obs
      sd.obs <- 1
    }

    res <- data.frame(R, sd.obs, sd.mod)
    res
  }

  vars <- c(group, type)

  results <- mydata %>%
    group_by(across(vars)) %>%
    do(calcStats(., obs = obs, mod = mod[1]))

  results.new <- NULL

  if (combine) {
    results.new <- mydata %>%
      group_by(across(vars)) %>%
      do(calcStats(., obs = obs, mod = mod[2]))
  }


  ## if no group to plot, then add a dummy one to make xyplot work
  if (is.null(group)) {
    results$MyGroupVar <- factor("MyGroupVar")
    group <- "MyGroupVar"
  }

  ## set up colours
  myColors <- openColours(cols, npol)
  pch.orig <- extra.args$pch

  ## combined colours if two groups
  if (twoGrp) {
    myColors <- rep(
      openColours(cols, length(unique(mydata[[grp1]]))),
      each = length(unique(mydata[[grp2]]))
    )

    extra.args$pch <- rep(extra.args$pch, each = length(unique(mydata[[grp2]])))
  }

  ## basic function for lattice call + defaults
  temp <- paste(type, collapse = "+")

  myform <- formula(paste("R ~ sd.mod", "|", temp, sep = ""))

  scales <- list(x = list(rot = 0), y = list(rot = 0))

  pol.name <- sapply(levels(mydata[[group]]), function(x) quickText(x, auto.text))


  if (key & npol > 1 & !combine) {
    thecols <- unique(myColors)
    if (twoGrp) {
      pol.name <- levels(factor(mydata[[grp1]]))
    }

    key <- list(
      points = list(col = thecols), pch = pch.orig,
      cex = extra.args$cex, text = list(lab = pol.name, cex = 0.8),
      space = key.pos, columns = key.columns,
      title = quickText(key.title, auto.text),
      cex.title = 0.8, lines.title = 3
    )
  } else if (key & npol > 1 & combine) {
    key <- list(
      lines = list(col = myColors[1:npol]), lwd = arrow.lwd,
      text = list(lab = pol.name, cex = 0.8), space = key.pos,
      columns = key.columns,
      title = quickText(key.title, auto.text),
      cex.title = 0.8, lines.title = 3
    )
  } else {
    key <- NULL
  }


  ## special wd layout
  if (length(type) == 1 & type[1] == "wd" & is.null(extra.args$layout)) {
    ## re-order to make sensible layout
    wds <- c("NW", "N", "NE", "W", "E", "SW", "S", "SE")
    mydata$wd <- ordered(mydata$wd, levels = wds)
    ## see if wd is actually there or not
    wd.ok <- sapply(wds, function(x) {
      if (x %in% unique(mydata$wd)) FALSE else TRUE
    })
    skip <- c(wd.ok[1:4], TRUE, wd.ok[5:8])
    mydata$wd <- factor(mydata$wd) ## remove empty factor levels
    extra.args$layout <- c(3, 3)
    if (!"skip" %in% names(extra.args)) {
      extra.args$skip <- skip
    }
  }
  if (!"skip" %in% names(extra.args)) {
    extra.args$skip <- FALSE
  }


  ## proper names of labelling ####################################################

  stripName <- sapply(levels(mydata[, type[1]]), function(x) quickText(x, auto.text))
  if (strip) strip <- strip.custom(factor.levels = stripName)

  if (length(type) == 1) {
    strip.left <- FALSE
  } else { ## two conditioning variables
    stripName <- sapply(levels(mydata[, type[2]]), function(x) quickText(x, auto.text))
    strip.left <- strip.custom(factor.levels = stripName)
  }
  ## #############################################################################


  ## no strip needed for single panel
  if (length(type) == 1 & type[1] == "default") strip <- FALSE

  ## not sure how to evaluate "group" in xyplot, so change to a fixed name
  id <- which(names(results) == group)
  names(results)[id] <- "MyGroupVar"

  maxsd <- 1.2 * max(results$sd.obs, results$sd.mod)

  # xlim, ylim handling
  if (!"ylim" %in% names(extra.args)) {
    extra.args$ylim <- 1.12 * c(0, maxsd)
  }
  if (!"xlim" %in% names(extra.args)) {
    extra.args$xlim <- 1.12 * c(0, maxsd)
  }

  ## xlab, ylab local management
  if (is.null(extra.args$ylab)) {
    extra.args$ylab <- if (normalise) "standard deviation (normalised)" else "standard deviation"
  }
  if (is.null(extra.args$xlab)) {
    extra.args$xlab <- extra.args$ylab
  }


  ## plot
  xyplot.args <- list(
    x = myform, data = results, groups = results$MyGroupVar,
    aspect = 1,
    type = "n",
    as.table = TRUE,
    scales = scales,
    key = key,
    par.strip.text = list(cex = 0.8),
    strip = strip,
    strip.left = strip.left,
    panel = function(x, y, ...) {

      ## annotate each panel but don't need to do this for each grouping value
      panel.taylor.setup(
        x, y,
        results = results, maxsd = maxsd,
        cor.col = cor.col, rms.col = rms.col,
        text.obs = text.obs,
        annotate = annotate, ...
      )

      ## plot data in each panel
      panel.superpose(
        x, y,
        panel.groups = panel.taylor, ...,
        results = results, results.new = results.new,
        combine = combine, myColors = myColors,
        arrow.lwd = arrow.lwd
      )
    }
  )

  ## reset for extra.args
  xyplot.args <- listUpdate(xyplot.args, extra.args)

  ## plot
  plt <- do.call(xyplot, xyplot.args)


  if (length(type) == 1) plot(plt) else plot(useOuterStrips(plt, strip = strip, strip.left = strip.left))
  newdata <- results
  output <- list(plot = plt, data = newdata, call = match.call())
  class(output) <- "openair"


  invisible(output)
}


panel.taylor.setup <- function(x, y, subscripts, results, maxsd, cor.col, rms.col,
                               text.obs,
                               col.symbol, annotate, group.number, type, ...) {
  ## note, this assumes for each level of type there is a single measured value
  ## therefore, only the first is used  i.e. results$sd.obs[subscripts[1]]
  ## This does not matter if normalise = TRUE because all sd.obs = 1.

  ## The data frame 'results' should contain a grouping variable 'MyGroupVar',
  ## 'type' e.g. season, R (correlation coef), sd.obs and sd.mod
  xcurve <- cos(seq(0, pi / 2, by = 0.01)) * maxsd
  ycurve <- sin(seq(0, pi / 2, by = 0.01)) * maxsd
  llines(xcurve, ycurve, col = "black")

  xcurve <- cos(seq(0, pi / 2, by = 0.01)) * results$sd.obs[subscripts[1]]
  ycurve <- sin(seq(0, pi / 2, by = 0.01)) * results$sd.obs[subscripts[1]]
  llines(xcurve, ycurve, col = "black", lty = 5)

  corr.lines <- c(0.2, 0.4, 0.6, 0.8, 0.9)

  ## grid line with alpha transparency
  theCol <- t(grDevices::col2rgb(cor.col)) / 255

  for (gcl in corr.lines) llines(
      c(0, maxsd * gcl), c(0, maxsd * sqrt(1 - gcl ^ 2)),
      col = grDevices::rgb(theCol, alpha = 0.4), alpha = 0.5
    )

  bigtick <- acos(seq(0.1, 0.9, by = 0.1))
  medtick <- acos(seq(0.05, 0.95, by = 0.1))
  smltick <- acos(seq(0.91, 0.99, by = 0.01))

  lsegments(
    cos(bigtick) * maxsd, sin(bigtick) *
      maxsd, cos(bigtick) * 0.96 * maxsd, sin(bigtick) * 0.96 * maxsd,
    col = cor.col
  )

  lsegments(
    cos(medtick) * maxsd, sin(medtick) *
      maxsd, cos(medtick) * 0.98 * maxsd, sin(medtick) * 0.98 * maxsd,
    col = cor.col
  )
  lsegments(
    cos(smltick) * maxsd, sin(smltick) *
      maxsd, cos(smltick) * 0.99 * maxsd, sin(smltick) * 0.99 * maxsd,
    col = cor.col
  )

  ## arcs for standard deviations (3 by default)
  gamma <- pretty(c(0, maxsd), n = 5)
  if (gamma[length(gamma)] > maxsd) {
    gamma <- gamma[-length(gamma)]
  }
  labelpos <- seq(45, 70, length.out = length(gamma))

  ## some from plotrix
  for (gindex in 1:length(gamma)) {
    xcurve <- cos(seq(0, pi, by = 0.03)) * gamma[gindex] +
      results$sd.obs[subscripts[1]]
    endcurve <- which(xcurve < 0)
    endcurve <- ifelse(length(endcurve), min(endcurve) - 1, 105)
    ycurve <- sin(seq(0, pi, by = 0.03)) * gamma[gindex]
    maxcurve <- xcurve * xcurve + ycurve * ycurve
    startcurve <- which(maxcurve > maxsd * maxsd)
    startcurve <- ifelse(length(startcurve), max(startcurve) + 1, 0)

    llines(
      xcurve[startcurve:endcurve], ycurve[startcurve:endcurve],
      col = rms.col, lty = 5
    )
    ltext(
      xcurve[labelpos[gindex]], ycurve[labelpos[gindex]],
      gamma[gindex],
      cex = 0.7, col = rms.col, pos = 1,
      srt = 0, font = 2
    )

    ltext(
      1.1 * maxsd, 1.05 * maxsd,
      labels = annotate, cex = 0.7,
      col = rms.col, pos = 2
    )
  }

  ## angles for R key
  angles <- 180 * c(bigtick, acos(c(0.95, 0.99))) / pi

  ltext(
    cos(c(bigtick, acos(c(0.95, 0.99)))) *
      1.06 * maxsd, sin(c(bigtick, acos(c(0.95, 0.99)))) *
      1.06 * maxsd, c(seq(0.1, 0.9, by = 0.1), 0.95, 0.99),
    cex = 0.7,
    adj = 0.5, srt = angles, col = cor.col
  )

  ltext(
    0.82 * maxsd, 0.82 * maxsd, "correlation",
    srt = 315, cex = 0.7,
    col = cor.col
  )


  ## measured point and text
  lpoints(results$sd.obs[subscripts[1]], 0, pch = 20, col = "purple", cex = 1.5)
  ltext(results$sd.obs[subscripts[1]], 0, text.obs, col = "purple", cex = 0.7, pos = 3)
}


panel.taylor <- function(x, y, subscripts, results, results.new, maxsd, cor.col,
                         rms.col, combine, col.symbol, myColors, group.number,
                         type, arrow.lwd, ...) {
  R <- NULL
  sd.mod <- NULL ## avoid R NOTEs

  ## Plot actual results by type and group if given
  results <- transform(results, x = sd.mod * R, y = sd.mod * sin(acos(R)))

  if (combine) {
    results.new <- transform(results.new, x = sd.mod * R, y = sd.mod * sin(acos(R)))
    larrows(
      results$x[subscripts], results$y[subscripts],
      results.new$x[subscripts], results.new$y[subscripts],
      angle = 30, length = 0.1, col = myColors[group.number], lwd = arrow.lwd
    )
  } else {
    lpoints(
      results$x[subscripts], results$y[subscripts],
      col.symbol = myColors[group.number], ...
    )
  }
}
