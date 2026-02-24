#' Function for plotting bivariate polar plots with smoothing.
#'
#' Function for plotting pollutant concentration in polar coordinates showing
#' concentration by wind speed (or another numeric variable) and direction. Mean
#' concentrations are calculated for wind speed-direction \sQuote{bins} (e.g.
#' 0-1, 1-2 m/s,...  and 0-10, 10-20 degrees etc.).  To aid interpretation,
#' `gam` smoothing is carried out using `mgcv`.
#'
#' The bivariate polar plot is a useful diagnostic tool for quickly gaining an
#' idea of potential sources. Wind speed is one of the most useful variables to
#' use to separate source types (see references). For example, ground-level
#' concentrations resulting from buoyant plumes from chimney stacks tend to peak
#' under higher wind speed conditions. Conversely, ground-level, non-buoyant
#' plumes such as from road traffic, tend to have highest concentrations under
#' low wind speed conditions. Other sources such as from aircraft engines also
#' show differing characteristics by wind speed.
#'
#' The function has been developed to allow variables other than wind speed to
#' be plotted with wind direction in polar coordinates. The key issue is that
#' the other variable plotted against wind direction should be discriminating in
#' some way. For example, temperature can help reveal high-level sources brought
#' down to ground level in unstable atmospheric conditions, or show the effect a
#' source emission dependent on temperature e.g. biogenic isoprene.
#'
#' The plots can vary considerably depending on how much smoothing is done.  The
#' approach adopted here is based on the very flexible and capable `mgcv`
#' package that uses *Generalized Additive Models*. While methods do exist to
#' find an optimum level of smoothness, they are not necessarily useful. The
#' principal aim of `polarPlot` is as a graphical analysis rather than for
#' quantitative purposes. In this respect the smoothing aims to strike a balance
#' between revealing interesting (real) features and overly noisy data. The
#' defaults used in [polarPlot()] are based on the analysis of data from many
#' different sources. More advanced users may wish to modify the code and adopt
#' other smoothing approaches.
#'
#' Various statistics are possible to consider e.g. mean, maximum, median.
#' `statistic = "max"` is often useful for revealing sources. Pair-wise
#' statistics between two pollutants can also be calculated.
#'
#' The function can also be used to compare two pollutant species through a
#' range of pair-wise statistics (see help on `statistic`) and Grange et al.
#' (2016) (open-access publication link below).
#'
#' Wind direction is split up into 10 degree intervals and the other variable
#' (e.g. wind speed) 30 intervals. These 2D bins are then used to calculate the
#' statistics.
#'
#' These plots often show interesting features at higher wind speeds (see
#' references below). For these conditions there can be very few measurements
#' and therefore greater uncertainty in the calculation of the surface. There
#' are several ways in which this issue can be tackled. First, it is possible to
#' avoid smoothing altogether and use [polarFreq()]. Second, the effect of
#' setting a minimum number of measurements in each wind speed-direction bin can
#' be examined through `min.bin`. It is possible that a single point at high
#' wind speed conditions can strongly affect the surface prediction. Therefore,
#' setting `min.bin = 3`, for example, will remove all wind speed-direction bins
#' with fewer than 3 measurements *before* fitting the surface. Third, consider
#' setting `uncertainty = TRUE`. This option will show the predicted surface
#' together with upper and lower 95% confidence intervals, which take account of
#' the frequency of measurements.
#'
#' Variants on `polarPlot` include [polarAnnulus()] and [polarFreq()].
#'
#' @param mydata A data frame minimally containing `wd`, another variable to
#'   plot in polar coordinates (the default is a column \dQuote{ws} --- wind
#'   speed) and a pollutant. Should also contain `date` if plots by time period
#'   are required.
#'
#' @param pollutant Mandatory. A pollutant name corresponding to a variable in a
#'   data frame should be supplied e.g. `pollutant = "nox"`. There can also be
#'   more than one pollutant specified e.g. `pollutant = c("nox", "no2")`. The
#'   main use of using two or more pollutants is for model evaluation where two
#'   species would be expected to have similar concentrations. This saves the
#'   user stacking the data and it is possible to work with columns of data
#'   directly. A typical use would be `pollutant = c("obs", "mod")` to compare
#'   two columns \dQuote{obs} (the observations) and \dQuote{mod} (modelled
#'   values). When pair-wise statistics such as Pearson correlation and
#'   regression techniques are to be plotted, `pollutant` takes two elements
#'   too. For example, `pollutant = c("bc", "pm25")` where `"bc"` is a function
#'   of `"pm25"`.
#'
#' @param x Name of variable to plot against wind direction in polar
#'   coordinates, the default is wind speed, \dQuote{ws}.
#'
#' @param wd Name of wind direction field.
#'
#' @param type `type` determines how the data are split i.e. conditioned, and
#'   then plotted. The default is will produce a single plot using the entire
#'   data. Type can be one of the built-in types as detailed in `cutData` e.g.
#'   \dQuote{season}, \dQuote{year}, \dQuote{weekday} and so on. For example,
#'   `type = "season"` will produce four plots --- one for each season.
#'
#'   It is also possible to choose `type` as another variable in the data frame.
#'   If that variable is numeric, then the data will be split into four
#'   quantiles (if possible) and labelled accordingly. If type is an existing
#'   character or factor variable, then those categories/levels will be used
#'   directly. This offers great flexibility for understanding the variation of
#'   different variables and how they depend on one another.
#'
#'   Type can be up length two e.g. `type = c("season", "weekday")` will produce
#'   a 2x2 plot split by season and day of the week. Note, when two types are
#'   provided the first forms the columns and the second the rows.
#'
#' @param statistic The statistic that should be applied to each wind
#'   speed/direction bin. Because of the smoothing involved, the colour scale
#'   for some of these statistics is only to provide an indication of overall
#'   pattern and should not be interpreted in concentration units e.g. for
#'   `statistic = "weighted.mean"` where the bin mean is multiplied by the bin
#'   frequency and divided by the total frequency. In many cases using
#'   `polarFreq` will be better. Setting `statistic = "weighted.mean"` can be
#'   useful because it provides an indication of the concentration * frequency
#'   of occurrence and will highlight the wind speed/direction conditions that
#'   dominate the overall mean.Can be:
#'
#'   \itemize{ \item  \dQuote{mean} (default), \dQuote{median}, \dQuote{max}
#'   (maximum), \dQuote{frequency}. \dQuote{stdev} (standard deviation),
#'   \dQuote{weighted.mean}.
#'
#'   \item `statistic = "nwr"` Implements the Non-parametric Wind
#'   Regression approach of Henry et al. (2009) that uses kernel smoothers. The
#'   `openair` implementation is not identical because Gaussian kernels are
#'   used for both wind direction and speed. The smoothing is controlled by
#'   `ws_spread` and `wd_spread`.
#'
#'   \item `statistic = "cpf"` the conditional probability function (CPF)
#'   is plotted and a single (usually high) percentile level is supplied. The
#'   CPF is defined as CPF = my/ny, where my is the number of samples in the y
#'   bin (by default a wind direction, wind speed interval) with mixing ratios
#'   greater than the *overall* percentile concentration, and ny is the
#'   total number of samples in the same wind sector (see Ashbaugh et al.,
#'   1985). Note that percentile intervals can also be considered; see
#'   `percentile` for details.
#'
#'   \item When `statistic = "r"` or `statistic = "Pearson"`, the
#'   Pearson correlation coefficient is calculated for *two* pollutants.
#'   The calculation involves a weighted Pearson correlation coefficient, which
#'   is weighted by Gaussian kernels for wind direction an the radial variable
#'   (by default wind speed). More weight is assigned to values close to a wind
#'   speed-direction interval. Kernel weighting is used to ensure that all data
#'   are used rather than relying on the potentially small number of values in a
#'   wind speed-direction interval.
#'
#'   \item When `statistic = "Spearman"`, the Spearman correlation
#'   coefficient is calculated for *two* pollutants. The calculation
#'   involves a weighted Spearman correlation coefficient, which is weighted by
#'   Gaussian kernels for wind direction an the radial variable (by default wind
#'   speed). More weight is assigned to values close to a wind speed-direction
#'   interval. Kernel weighting is used to ensure that all data are used rather
#'   than relying on the potentially small number of values in a wind
#'   speed-direction interval.
#'
#'   \item `"robust_slope"` is another option for pair-wise statistics and
#'   `"quantile.slope"`, which uses quantile regression to estimate the
#'   slope for a particular quantile level (see also `tau` for setting the
#'   quantile level).
#'
#'   \item `"york_slope"` is another option for pair-wise statistics which
#'   uses the *York regression method* to estimate the slope. In this
#'   method the uncertainties in `x` and `y` are used in the
#'   determination of the slope. The uncertainties are provided by
#'   `x_error` and `y_error` --- see below.}
#'
#' @param limits The function does its best to choose sensible limits
#'   automatically. However, there are circumstances when the user will wish to
#'   set different ones. An example would be a series of plots showing each year
#'   of data separately. The limits are set in the form `c(lower, upper)`, so
#'   `limits = c(0, 100)` would force the plot limits to span 0-100.
#'
#' @param exclude.missing Setting this option to `TRUE` (the default) removes
#'   points from the plot that are too far from the original data. The smoothing
#'   routines will produce predictions at points where no data exist i.e. they
#'   predict. By removing the points too far from the original data produces a
#'   plot where it is clear where the original data lie. If set to `FALSE`
#'   missing data will be interpolated.
#'
#' @param uncertainty Should the uncertainty in the calculated surface be shown?
#'   If `TRUE` three plots are produced on the same scale showing the predicted
#'   surface together with the estimated lower and upper uncertainties at the
#'   95% confidence interval. Calculating the uncertainties is useful to
#'   understand whether features are real or not.  For example, at high wind
#'   speeds where there are few data there is greater uncertainty over the
#'   predicted values. The uncertainties are calculated using the GAM and
#'   weighting is done by the frequency of measurements in each wind
#'   speed-direction bin. Note that if uncertainties are calculated then the
#'   type is set to "default".
#'
#' @param percentile If `statistic = "percentile"` then `percentile` is used,
#'   expressed from 0 to 100. Note that the percentile value is calculated in
#'   the wind speed, wind direction \sQuote{bins}. For this reason it can also
#'   be useful to set `min.bin` to ensure there are a sufficient number of
#'   points available to estimate a percentile. See `quantile` for more details
#'   of how percentiles are calculated.
#'
#'   `percentile` is also used for the Conditional Probability Function (CPF)
#'   plots. `percentile` can be of length two, in which case the percentile
#'   *interval* is considered for use with CPF. For example, `percentile = c(90,
#'   100)` will plot the CPF for concentrations between the 90 and 100th
#'   percentiles. Percentile intervals can be useful for identifying specific
#'   sources. In addition, `percentile` can also be of length 3. The third value
#'   is the \sQuote{trim} value to be applied. When calculating percentile
#'   intervals many can cover very low values where there is no useful
#'   information. The trim value ensures that values greater than or equal to
#'   the trim * mean value are considered *before* the percentile intervals are
#'   calculated. The effect is to extract more detail from many source
#'   signatures. See the manual for examples. Finally, if the trim value is less
#'   than zero the percentile range is interpreted as absolute concentration
#'   values and subsetting is carried out directly.
#'
#' @param cols Colours to be used for plotting. Options include
#'   \dQuote{default}, \dQuote{increment}, \dQuote{heat}, \dQuote{jet} and
#'   `RColorBrewer` colours --- see the `openair` `openColours` function for
#'   more details. For user defined the user can supply a list of colour names
#'   recognised by R (type `colours()` to see the full list). An example would
#'   be `cols = c("yellow", "green", "blue")`. `cols` can also take the values
#'   `"viridis"`, `"magma"`, `"inferno"`, or `"plasma"` which are the viridis
#'   colour maps ported from Python's Matplotlib library.
#'
#' @param weights At the edges of the plot there may only be a few data points
#'   in each wind speed-direction interval, which could in some situations
#'   distort the plot if the concentrations are high. `weights` applies a
#'   weighting to reduce their influence. For example and by default if only a
#'   single data point exists then the weighting factor is 0.25 and for two
#'   points 0.5. To not apply any weighting and use the data as is, use `weights
#'   = c(1, 1, 1)`.
#'
#'   An alternative to down-weighting these points they can be removed
#'   altogether using `min.bin`.
#'
#' @param min.bin The minimum number of points allowed in a wind speed/wind
#'   direction bin.  The default is 1. A value of two requires at least 2 valid
#'   records in each bin an so on; bins with less than 2 valid records are set
#'   to NA. Care should be taken when using a value > 1 because of the risk of
#'   removing real data points. It is recommended to consider your data with
#'   care. Also, the `polarFreq` function can be of use in such circumstances.
#'
#' @param mis.col When `min.bin` is > 1 it can be useful to show where data are
#'   removed on the plots. This is done by shading the missing data in
#'   `mis.col`. To not highlight missing data when `min.bin` > 1 choose `mis.col
#'   = "transparent"`.
#'
#' @param alpha The alpha transparency to use for the plotting surface (a value
#'   between 0 and 1 with zero being fully transparent and 1 fully opaque).
#'   Setting a value below 1 can be useful when plotting surfaces on a map using
#'   the package `openairmaps`.
#'
#' @param upper This sets the upper limit wind speed to be used. Often there are
#'   only a relatively few data points at very high wind speeds and plotting all
#'   of them can reduce the useful information in the plot.
#'
#' @param angle.scale Sometimes the placement of the scale may interfere with an
#'   interesting feature. The user can therefore set `angle.scale` to any value
#'   between 0 and 360 degrees to mitigate such problems. For example
#'   `angle.scale = 45` will draw the scale heading in a NE direction.
#'
#' @param units The units shown on the polar axis scale.
#'
#' @param force.positive The default is `TRUE`. Sometimes if smoothing data with
#'   steep gradients it is possible for predicted values to be negative.
#'   `force.positive = TRUE` ensures that predictions remain positive. This is
#'   useful for several reasons. First, with lots of missing data more
#'   interpolation is needed and this can result in artefacts because the
#'   predictions are too far from the original data. Second, if it is known
#'   beforehand that the data are all positive, then this option carries that
#'   assumption through to the prediction. The only likely time where setting
#'   `force.positive = FALSE` would be if background concentrations were first
#'   subtracted resulting in data that is legitimately negative. For the vast
#'   majority of situations it is expected that the user will not need to alter
#'   the default option.
#'
#' @param k This is the smoothing parameter used by the `gam` function in
#'   package `mgcv`. Typically, value of around 100 (the default) seems to be
#'   suitable and will resolve important features in the plot. The most
#'   appropriate choice of `k` is problem-dependent; but extensive testing of
#'   polar plots for many different problems suggests a value of `k` of about
#'   100 is suitable. Setting `k` to higher values will not tend to affect the
#'   surface predictions by much but will add to the computation time. Lower
#'   values of `k` will increase smoothing. Sometimes with few data to plot
#'   `polarPlot` will fail. Under these circumstances it can be worth lowering
#'   the value of `k`.
#'
#' @param normalise If `TRUE` concentrations are normalised by dividing by their
#'   mean value. This is done *after* fitting the smooth surface. This option is
#'   particularly useful if one is interested in the patterns of concentrations
#'   for several pollutants on different scales e.g. NOx and CO. Often useful if
#'   more than one `pollutant` is chosen.
#'
#' @param key.header Adds additional text/labels to the scale key. For example,
#'   passing the options `key.header = "header", key.footer = "footer1"` adds
#'   addition text above and below the scale key. These arguments are passed to
#'   `drawOpenKey` via `quickText`, applying the `auto.text` argument, to handle
#'   formatting.
#'
#' @param key.footer see `key.footer`.
#'
#' @param key.position Location where the scale key is to plotted. Allowed
#'   arguments currently include `"top"`, `"right"`, `"bottom"` and `"left"`.
#'
#' @param key Fine control of the scale key via `drawOpenKey`. See `drawOpenKey`
#'   for further details.
#'
#' @param auto.text Either `TRUE` (default) or `FALSE`. If `TRUE` titles and
#'   axis labels will automatically try and format pollutant names and units
#'   properly e.g.  by subscripting the `2' in NO2.
#'
#' @param ws_spread The value of sigma used for Gaussian kernel weighting of
#'   wind speed when `statistic = "nwr"` or when correlation and regression
#'   statistics are used such as *r*. Default is `0.5`.
#'
#' @param wd_spread The value of sigma used for Gaussian kernel weighting of
#'   wind direction when `statistic = "nwr"` or when correlation and regression
#'   statistics are used such as *r*. Default is `4`.
#'
#' @param x_error The `x` error / uncertainty used when `statistic =
#'   "york_slope"`.
#'
#' @param y_error The `y` error / uncertainty used when `statistic =
#'   "york_slope"`.
#'
#' @param kernel Type of kernel used for the weighting procedure for when
#'   correlation or regression techniques are used. Only `"gaussian"` is
#'   supported but this may be enhanced in the future.
#'
#' @param formula.label When pair-wise statistics such as regression slopes are
#'   calculated and plotted, should a formula label be displayed?
#'   `formula.label` will also determine whether concentration information is
#'   printed when `statistic = "cpf"`.
#'
#' @param tau The quantile to be estimated when `statistic` is set to
#'   `"quantile.slope"`. Default is `0.5` which is equal to the median and will
#'   be ignored if `"quantile.slope"` is not used.
#'
#' @param plot Should a plot be produced? `FALSE` can be useful when analysing
#'   data to extract plot components and plotting them in other ways.
#'
#' @param ... Other graphical parameters passed onto `cutData`. For example,
#'   `polarPlot` passes the option `hemisphere = "southern"` on to `cutData` to
#'   provide southern (rather than default northern) hemisphere handling of
#'   `type = "season"`. The `main` argument is passed to `ggplot2::labs()` for
#'   the plot title.
#'
#' @import mgcv
#' @return an [openair][openair-package] object. `data` contains four set
#'   columns: `cond`, conditioning based on `type`; `u` and `v`, the
#'   translational vectors based on `ws` and `wd`; and the local `pollutant`
#'   estimate.
#' @author David Carslaw
#' @family polar directional analysis functions
#' @references
#'
#' Ashbaugh, L.L., Malm, W.C., Sadeh, W.Z., 1985. A residence time probability
#' analysis of sulfur concentrations at ground canyon national park. Atmospheric
#' Environment 19 (8), 1263-1270.
#'
#' Carslaw, D.C., Beevers, S.D, Ropkins, K and M.C. Bell (2006). Detecting and
#' quantifying aircraft and other on-airport contributions to ambient nitrogen
#' oxides in the vicinity of a large international airport.  Atmospheric
#' Environment. 40/28 pp 5424-5434.
#'
#' Carslaw, D.C., & Beevers, S.D. (2013). Characterising and understanding
#' emission sources using bivariate polar plots and k-means clustering.
#' Environmental Modelling & Software, 40, 325-329. DOI:
#' 10.1016/j.envsoft.2012.09.005.
#'
#' Henry, R.C., Chang, Y.S., Spiegelman, C.H., 2002. Locating nearby sources of
#' air pollution by nonparametric regression of atmospheric concentrations on
#' wind direction. Atmospheric Environment 36 (13), 2237-2244.
#'
#' Henry, R., Norris, G.A., Vedantham, R., Turner, J.R., 2009. Source region
#' identification using Kernel smoothing. Environ. Sci. Technol. 43 (11),
#' 4090e4097. DOI: 10.1021/es8011723.
#'
#' Uria-Tellaetxe, I. and D.C. Carslaw (2014). Source identification using a
#' conditional bivariate Probability function. Environmental Modelling &
#' Software, Vol. 59, 1-9.
#'
#' Westmoreland, E.J., N. Carslaw, D.C. Carslaw, A. Gillah and E. Bates (2007).
#' Analysis of air quality within a street canyon using statistical and
#' dispersion modelling techniques. Atmospheric Environment. Vol.  41(39), pp.
#' 9195-9205.
#'
#' Yu, K.N., Cheung, Y.P., Cheung, T., Henry, R.C., 2004. Identifying the impact
#' of large urban airports on local air quality by nonparametric regression.
#' Atmospheric Environment 38 (27), 4501-4507.
#'
#' Grange, S. K., Carslaw, D. C., & Lewis, A. C. 2016. Source apportionment
#' advances with bivariate polar plots, correlation, and regression techniques.
#' Atmospheric Environment. 145, 128-134. DOI: 10.1016/j.atmosenv.2016.09.016.
#' @examples
#'
#' # basic plot
#' polarPlot(mydata, pollutant = "nox")
#' \dontrun{
#'
#' # polarPlots by year on same scale
#' polarPlot(mydata, pollutant = "so2", type = "year", main = "polarPlot of so2")
#'
#' # set minimum number of bins to be used to see if pattern remains similar
#' polarPlot(mydata, pollutant = "nox", min.bin = 3)
#'
#' # plot by day of the week
#' polarPlot(mydata, pollutant = "pm10", type = "weekday")
#'
#' # show the 95% confidence intervals in the surface fitting
#' polarPlot(mydata, pollutant = "so2", uncertainty = TRUE)
#'
#'
#' # Pair-wise statistics
#' # Pearson correlation
#' polarPlot(mydata, pollutant = c("pm25", "pm10"), statistic = "r")
#'
#' # Robust regression slope, takes a bit of time
#' polarPlot(mydata, pollutant = c("pm25", "pm10"), statistic = "robust.slope")
#'
#' # Least squares regression works too but it is not recommended, use robust
#' # regression
#' # polarPlot(mydata, pollutant = c("pm25", "pm10"), statistic = "slope")
#' }
#'
#' @export
polarPlot <-
  function(
    mydata,
    pollutant = "nox",
    x = "ws",
    wd = "wd",
    type = "default",
    statistic = "mean",
    limits = NULL,
    exclude.missing = TRUE,
    uncertainty = FALSE,
    percentile = NA,
    cols = "default",
    weights = c(0.25, 0.5, 0.75),
    min.bin = 1,
    mis.col = "grey",
    upper = NA,
    angle.scale = 315,
    units = x,
    force.positive = TRUE,
    k = 100,
    normalise = FALSE,
    key.header = statistic,
    key.footer = pollutant,
    key.position = "right",
    key = TRUE,
    auto.text = TRUE,
    ws_spread = 1.5,
    wd_spread = 5,
    x_error = NA,
    y_error = NA,
    kernel = "gaussian",
    formula.label = TRUE,
    tau = 0.5,
    alpha = 1,
    plot = TRUE,
    ...
  ) {
    ## get rid of R check annoyances
    z <- . <- r_id <- label <- y <- NULL

    if (statistic == "percentile" & is.na(percentile[1] & statistic != "cpf")) {
      warning("percentile value missing, using 50")
      percentile <- 50
    }

    if (statistic == "cpf" & is.na(percentile[1])) {
      warning("percentile value missing, using 75")
      percentile <- 75
    }

    # Allow for period notation
    statistic <- gsub("\\.| ", "_", statistic)

    # Build vector for many checks
    correlation_stats <- c(
      "r",
      "slope",
      "intercept",
      "robust_slope",
      "robust_intercept",
      "quantile_slope",
      "quantile_intercept",
      "Pearson",
      "Spearman",
      "york_slope",
      "trend"
    )

    if (statistic %in% correlation_stats && length(pollutant) != 2) {
      stop("Correlation statistic requires two pollutants.")
    }

    # if statistic is trend, then don't force to be positive
    if (statistic == "trend") {
      force.positive <- FALSE
    }

    # names of variables for use later
    nam.x <- x
    nam.wd <- wd

    if (length(type) > 2) {
      stop("Maximum number of types is 2.")
    }

    ## can't have conditioning here
    if (uncertainty) {
      type <- "default"
    }

    if (uncertainty & length(pollutant) > 1) {
      stop("Can only have one pollutant when uncertainty = TRUE")
    }

    if (
      !statistic %in%
        c(
          "mean",
          "median",
          "frequency",
          "max",
          "stdev",
          "trend",
          "weighted_mean",
          "percentile",
          "cpf",
          "nwr",
          correlation_stats
        )
    ) {
      stop(paste0("statistic '", statistic, "' not recognised."), call. = FALSE)
    }

    if (length(weights) != 3) {
      stop("weights should be of length 3.")
    }

    if (key.header == "nwr") {
      key.header <- "NWR"
    }
    if (key.header == "weighted_mean") {
      key.header <- "weighted\nmean"
    }
    if (key.header == "percentile") {
      key.header <- c(paste(percentile, "th", sep = ""), "percentile")
    }

    if ("cpf" %in% key.header) {
      key.header <- c("CPF", "probability")
    }

    ## key handling
    if (!key) {
      key.position <- "none"
    }

    ## extra.args setup
    extra.args <- list(...)

    ## label controls
    extra.args$xlab <- if ("xlab" %in% names(extra.args)) {
      quickText(extra.args$xlab, auto.text)
    } else {
      quickText("", auto.text)
    }

    extra.args$ylab <- if ("ylab" %in% names(extra.args)) {
      quickText(extra.args$ylab, auto.text)
    } else {
      quickText("", auto.text)
    }

    extra.args$main <- if ("main" %in% names(extra.args)) {
      quickText(extra.args$main, auto.text)
    } else {
      quickText("", auto.text)
    }

    # if clustering, return lower resolution plot
    extra.args$cluster <- if ("cluster" %in% names(extra.args)) {
      TRUE
    } else {
      FALSE
    }

    ## layout default
    if (!"layout" %in% names(extra.args)) {
      extra.args$layout <- NULL
    }

    ## extract variables of interest
    vars <- c(wd, x, pollutant)

    if (statistic == "york_slope") {
      vars <- c(vars, x_error, y_error)
    }

    if (any(type %in% dateTypes)) {
      vars <- c(vars, "date")
    }

    mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)

    mydata <- na.omit(mydata)

    ## this is used later for the 'x' scale
    min.scale <- min(mydata[[x]], na.rm = TRUE)

    # check to see if a high proportion of the data is negative
    if (
      length(which(mydata[pollutant] < 0)) / nrow(mydata) > 0.1 &&
        force.positive
    ) {
      warning(">10% negative data detected, set force.positive = FALSE?")
    }

    # scale data by subtracting the min value this helps with dealing
    # with negative data on radial axis (starts from zero, always
    # positive)

    # return radial scale as attribute "radial_scale" on returned data for external plotting
    radial_scale <- range(mydata[[x]])

    mydata[[x]] <- mydata[[x]] - min(mydata[[x]], na.rm = TRUE)

    # if more than one pollutant, need to stack the data and set type =
    # "variable" this case is most relevent for model-measurement
    # compasrions where data are in columns Can also do more than one
    # pollutant and a single type that is not "default", in which case
    # pollutant becomes a conditioning variable

    # if statistic is 'r' we need the data for two pollutants in columns
    if (length(pollutant) > 1 && !statistic %in% correlation_stats) {
      if (length(type) > 1) {
        warning(paste("Only type = '", type[1], "' will be used", sep = ""))
        type <- type[1]
      }

      ## use pollutants as conditioning variables
      mydata <- gather(
        mydata,
        key = variable,
        value = value,
        pollutant,
        factor_key = TRUE
      )
      ## now set pollutant to "value"
      pollutant <- "value"

      if (type == "default") {
        type <- "variable"
      } else {
        type <- c(type, "variable")
      }
    }

    ## cutData depending on type
    mydata <- cutData(mydata, type, ...)

    ## if upper ws not set, set it to the max to display all information
    max.ws <- max(mydata[[x]], na.rm = TRUE)
    min.ws <- min(mydata[[x]], na.rm = TRUE)
    clip <- TRUE ## used for removing data where ws > upper

    if (is.na(upper)) {
      upper <- max.ws
      clip <- FALSE
    }

    # resolution deprecated, int is resolution of GAM surface predictions over int * int grid
    # 51 works well with bilinear interpolation of results

    int <- 51

    ## binning wd data properly
    ## use 10 degree binning of wd if already binned, else 5
    if (all(mydata[[wd]] %% 10 == 0, na.rm = TRUE)) {
      wd.int <- 10
    } else {
      if (toupper(statistic) == "NWR") wd.int <- 2 else wd.int <- 5 ## how to split wd
    }

    if (toupper(statistic) == "NWR") {
      ws_bins <- 40
    } else {
      ws_bins <- 30
    }

    if (statistic == "nwr") {
      k <- 200
    } # limit any smoothing

    ws.seq <- seq(min.ws, max.ws, length = ws_bins)
    wd.seq <- seq(from = wd.int, to = 360, by = wd.int) ## wind directions from wd.int to 360
    ws.wd <- expand.grid(x = ws.seq, wd = wd.seq)

    u <- with(ws.wd, x * sin(pi * wd / 180)) ## convert to polar coords
    v <- with(ws.wd, x * cos(pi * wd / 180))

    ## data to predict over
    input.data <- expand.grid(
      u = seq(-upper, upper, length = int),
      v = seq(-upper, upper, length = int)
    )

    ## Pval is only set in the CPF branch; initialise to NULL so it always exists
    Pval <- NULL

    if (statistic == "cpf") {
      ## can be interval of percentiles or a single (threshold)
      if (length(percentile) > 1) {
        statistic <- "cpfi" # CPF interval

        if (length(percentile) == 3) {
          ## in this case there is a trim value as a proprtion of the mean
          ## if this value <0 use absolute values as range
          Mean <- mean(mydata[[pollutant]], na.rm = TRUE)

          if (percentile[3] < 0) {
            Pval <- percentile[1:2]
          } else {
            Pval <- quantile(
              subset(
                mydata[[pollutant]],
                mydata[[pollutant]] >=
                  Mean *
                    percentile[3]
              ),
              probs = percentile[1:2] / 100,
              na.rm = TRUE
            )
          }
        } else {
          Pval <- quantile(
            mydata[[pollutant]],
            probs = percentile / 100,
            na.rm = TRUE
          )
        }

        sub <- paste(
          "CPF (",
          format(Pval[1], digits = 2),
          " to ",
          format(Pval[2], digits = 2),
          ")",
          sep = ""
        )
      } else {
        Pval <- quantile(
          mydata[[pollutant]],
          probs = percentile / 100,
          na.rm = TRUE
        )

        sub <- paste(
          "CPF at the ",
          percentile,
          "th percentile (=",
          format(Pval, digits = 2),
          ")",
          sep = ""
        )
        if (!formula.label) sub <- NULL # no label
      }
    } else {
      sub <- NULL
    }

    ## Thin wrapper — delegates to polar_prepare_grid() with all dependencies
    ## passed explicitly rather than captured from the enclosing scope.
    prepare.grid <- function(mydata) {
      polar_prepare_grid(
        mydata = mydata,
        wd_col = wd,
        x_col = x,
        pollutant = pollutant,
        statistic = statistic,
        correlation_stats = correlation_stats,
        Pval = Pval,
        percentile = percentile,
        ws.wd = ws.wd,
        u = u,
        v = v,
        input.data = input.data,
        ws_bins = ws_bins,
        wd.int = wd.int,
        max.ws = max.ws,
        weights = weights,
        min.bin = min.bin,
        force.positive = force.positive,
        uncertainty = uncertainty,
        exclude.missing = exclude.missing,
        upper = upper,
        k = k,
        ws_spread = ws_spread,
        wd_spread = wd_spread,
        kernel = kernel,
        tau = tau,
        x_error = x_error,
        y_error = y_error,
        cluster = extra.args$cluster
      )
    }

    ## if min.bin >1 show the missing data. Work this out by running twice:
    ## first time with no missings, second with min.bin.
    ## Plot one surface on top of the other.

    if (!missing(min.bin)) {
      tmp <- min.bin
      min.bin <- 0
      res1 <- mydata |>
        group_by(across(type)) |>
        group_modify(~ prepare.grid(.))

      min.bin <- tmp

      res <- mydata |>
        group_by(across(type)) |>
        group_modify(~ prepare.grid(.))

      res$miss <- res1$z
    } else {
      res <- mydata |>
        group_by(across(type)) |>
        group_modify(~ prepare.grid(.))
    }

    ## with CPF make sure not >1 due to surface fitting
    if (any(res$z > 1, na.rm = TRUE) & statistic %in% c("cpf", "cpfi")) {
      id <- which(res$z > 1)
      res$z[id] <- 1
    }

    ## remove wind speeds > upper to make a circle
    if (clip) {
      res$z[(res$u^2 + res$v^2)^0.5 > upper] <- NA
    }

    ## normalise by divining by mean conditioning value if needed
    if (normalise) {
      res <- mutate(res, z = z / mean(z, na.rm = TRUE))

      if (missing(key.footer)) key.footer <- "normalised\nlevel"
    }

    # correlation notation
    if (statistic %in% c("r", "Pearson", "Spearman")) {
      if (missing(key.footer)) {
        key.footer <- paste0("corr(", pollutant[1], ", ", pollutant[2], ")")
      }

      # make sure smoothing does not results in r>1 or <-1
      # sometimes happens with little data at edges
      id <- which(res$z > 1)
      if (length(id) > 0) {
        res$z[id] <- 1
      }

      id <- which(res$z < -1)
      if (length(id) > 0) res$z[id] <- -1
    }

    # if regression, set key.footer to 'm' (slope)
    if (grepl("slope|intercept", statistic) & length(pollutant == 2)) {
      key.footer <- "m"
    }

    # Labels for correlation and regression, keep lower case like other labels
    if (statistic %in% c("r", "Pearson")) {
      key.header <- expression(italic("Pearson\ncorrelation"))
    }

    if (statistic == "Spearman") {
      key.header <- expression(italic("Spearman\ncorrelation"))
    }

    if (statistic == "robust_slope") {
      key.header <- "robust\nslope"
    }

    if (statistic == "robust_intercept") {
      key.header <- "robust\nintercept"
    }

    if (statistic == "york_slope") {
      key.header <- "York regression\nslope"
    }

    if (statistic == "quantile_slope") {
      key.header <- paste0("quantile slope\n(tau: ", tau, ")")
    }

    if (statistic == "quantile_intercept") {
      key.header <- paste0("quantile intercept\n(tau: ", tau, ")")
    }

    ## auto-scaling
    nlev <- 200 ## preferred number of intervals

    ## handle missing breaks arguments

    if (any(is.null(limits)) | any(is.na(limits))) {
      # breaks <- pretty(res$z, n = nlev)
      breaks <- seq(
        min(res$z, na.rm = TRUE),
        max(res$z, na.rm = TRUE),
        length.out = nlev
      )
      labs <- pretty(breaks, 7)
      labs <- labs[labs >= min(breaks) & labs <= max(breaks)]
      at <- labs
    } else {
      ## handle user limits and clipping
      breaks <- seq(min(limits), max(limits), length.out = nlev)
      labs <- pretty(breaks, 7)
      labs <- labs[labs >= min(breaks) & labs <= max(breaks)]
      at <- labs

      ## case where user max is < data max — label only, oob_squish handles display
      if (max(limits) < max(res[["z"]], na.rm = TRUE)) {
        labs[length(labs)] <- paste(">", labs[length(labs)])
      }

      ## case where user min is > data min — label only, oob_squish handles display
      if (min(limits) > min(res[["z"]], na.rm = TRUE)) {
        labs[1] <- paste("<", labs[1])
      }
    }

    nlev2 <- length(breaks)

    col <- openColours(cols, (nlev2 - 1))

    col.scale <- breaks

    ## special handling of layout for uncertainty
    if (uncertainty & is.null(extra.args$layout)) {
      extra.args$layout <- c(3, 1)
    }

    ## scaling of 'zeroed' data
    ## note - add upper because user can set this to be different to data
    intervals <- pretty(c(mydata[[x]], upper))

    ## labels for scaling
    labels <- pretty(c(mydata[[x]], upper) + min.scale)
    ## offset the lines/labels if necessary
    intervals <- intervals + (min(labels) - min.scale)

    ## add zero in the middle if it exists
    if (min.scale != 0) {
      labels <- labels[-1]
      intervals <- intervals[-1]
    }

    # if uncertainty = TRUE, change type for plotting (3 panels)
    if (uncertainty) {
      type <- "uncertainty"
    }

    ## Pre-compute circular ring overlay data
    angles_circle <- seq(0, 2 * pi, length = 361)
    circle_df <- do.call(
      rbind,
      lapply(seq_along(intervals), function(i) {
        data.frame(
          x = intervals[i] * sin(angles_circle),
          y = intervals[i] * cos(angles_circle),
          r_id = i
        )
      })
    )

    ## Radial scale labels: attach units to the 3rd label only
    n_lbl <- length(labels)
    lbl_suffix <- c("", "", units, rep("", max(0L, n_lbl - 3L)))
    scale_labels <- trimws(paste(labels, lbl_suffix[seq_len(n_lbl)]))

    ## Format legend title (key.header may be an expression)
    if (is.expression(key.header)) {
      header_str <- tryCatch(
        as.character(key.header[[1]][[2]]),
        error = function(e) deparse(key.header)
      )
    } else {
      header_str <- paste(key.header, collapse = "\n")
    }
    footer_str <- paste(key.footer, collapse = "\n")
    title_sep <- if (key.position %in% c("top", "bottom")) " " else "\n"
    title_parts <- c(header_str, footer_str)
    title_parts <- title_parts[nzchar(trimws(title_parts))]
    legend_title <- quickText(
      paste(title_parts, collapse = title_sep),
      auto.text
    )

    ## Build ggplot2 plot
    thePlot <-
      ggplot2::ggplot(res, ggplot2::aes(x = u, y = v)) +

      ## Missing data indicator layer: shown when min.bin > 1
      {
        if ("miss" %in% names(res) && min.bin > 1) {
          ggplot2::geom_raster(
            data = res[!is.na(res$miss), ],
            mapping = ggplot2::aes(x = u, y = v),
            fill = mis.col,
            inherit.aes = FALSE
          )
        }
      } +

      ## Main smoothed surface
      ggplot2::geom_raster(ggplot2::aes(fill = z), alpha = alpha) +

      ## Circular grid rings
      ggplot2::geom_path(
        data = circle_df,
        mapping = ggplot2::aes(x = x, y = y, group = r_id),
        colour = "grey75",
        linetype = "dashed",
        linewidth = 0.4,
        inherit.aes = FALSE
      ) +

      ## N–S and E–W axis lines
      ggplot2::annotate(
        "segment",
        x = -upper,
        xend = upper,
        y = 0,
        yend = 0,
        linewidth = 0.5
      ) +
      ggplot2::annotate(
        "segment",
        x = 0,
        xend = 0,
        y = -upper,
        yend = upper,
        linewidth = 0.5
      ) +

      ## Compass direction labels
      ggplot2::annotate(
        "text",
        x = 0.07 * upper,
        y = upper * 0.95,
        label = "N",
        size = 3
      ) +
      ggplot2::annotate(
        "text",
        x = 0.07 * upper,
        y = -upper * 0.95,
        label = "S",
        size = 3
      ) +
      ggplot2::annotate(
        "text",
        x = upper * 0.95,
        y = 0.07 * upper,
        label = "E",
        size = 3
      ) +
      ggplot2::annotate(
        "text",
        x = -upper * 0.95,
        y = 0.07 * upper,
        label = "W",
        size = 3
      ) +

      ## Radial scale text labels — use geom_text (not annotate) so that
      ## multi-value data replicates correctly across facet panels
      ggplot2::geom_text(
        data = data.frame(
          x = 1.07 * intervals * sin(pi * angle.scale / 180),
          y = 1.07 * intervals * cos(pi * angle.scale / 180),
          label = scale_labels
        ),
        mapping = ggplot2::aes(x = x, y = y, label = label),
        size = 3,
        hjust = 0,
        inherit.aes = FALSE
      ) +

      ## Continuous colour scale
      ggplot2::scale_fill_gradientn(
        colours = col,
        limits = range(col.scale),
        oob = scales::oob_squish,
        na.value = NA,
        breaks = at,
        labels = labs,
        name = legend_title
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_colorbar(
          theme = ggplot2::theme(
            legend.title.position = ifelse(
              key.position %in% c("left", "right"),
              "top",
              key.position
            ),
            legend.text.position = key.position,
            legend.key.height = unit(10, "lines")
          )
        )
      ) +

      ## Key size: full height (left/right) or full width (top/bottom)
      {
        if (key.position %in% c("left", "right")) {
          ggplot2::theme(
            legend.key.height = ggplot2::unit(1, "null"),
            legend.key.spacing.y = ggplot2::unit(0, "cm")
          )
        }
      } +
      {
        if (key.position %in% c("top", "bottom")) {
          ggplot2::theme(
            legend.key.width = ggplot2::unit(1, "null"),
            legend.key.spacing.x = ggplot2::unit(0, "cm")
          )
        }
      } +

      ## Faceting
      get_facet(
        type,
        extra.args = extra.args,
        scales = "fixed",
        auto.text = auto.text
      ) +

      ## Fixed aspect ratio, allow annotations outside the plot area
      ggplot2::coord_fixed(
        xlim = c(-upper * .9, upper * .9),
        ylim = c(-upper * .9, upper * .9),
        clip = "on"
      ) +

      ## Plot labels
      ggplot2::labs(
        title = extra.args$main,
        subtitle = sub,
        caption = if (
          formula.label &
            grepl("slope|intercept", statistic) &
            length(pollutant == 2)
        ) {
          quickText(
            paste0("Formula: ", pollutant[1], " = m.", pollutant[2], " + c"),
            auto.text
          )
        } else {
          NULL
        }
      ) +

      ## Theme
      theme_openair(key.position = key.position) +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank()
      ) +
      set_extra_fontsize(extra.args)

    if (plot) {
      plot(thePlot)
    }

    newdata <- res

    # return attribute of scale used - useful for data with negative scales such as air_temp
    attr(newdata, "radial_scale") <- range(radial_scale)
    output <- list(plot = thePlot, data = newdata, call = match.call())
    class(output) <- "openair"

    # Final return
    invisible(output)
  }


## Top-level surface-fitting worker for polarPlot().
##
## In the original code this was a nested closure (prepare.grid) that implicitly
## captured ~25 variables from the enclosing polarPlot() scope.  Lifting it to a
## top-level function makes every dependency explicit, improves readability, and
## allows independent testing.
##
## Parameters mirror the variables previously captured from polarPlot():
##   wd_col / x_col  — column names for wind direction and the radial variable
##                     (renamed to avoid shadowing the local cut() results)
polar_prepare_grid <- function(
  mydata,
  wd_col,
  x_col,
  pollutant,
  statistic,
  correlation_stats,
  Pval = NULL,
  percentile = NA,
  ws.wd,
  u,
  v,
  input.data,
  ws_bins,
  wd.int,
  max.ws,
  weights,
  min.bin,
  force.positive,
  uncertainty,
  exclude.missing,
  upper,
  k,
  ws_spread,
  wd_spread,
  kernel,
  tau,
  x_error,
  y_error,
  cluster = FALSE
) {
  ## Identify which ws and wd bins the data belong to.
  ## Note: local variables 'wd' and 'x' shadow the column-name parameters.
  wd <- cut(
    wd.int * ceiling(mydata[[wd_col]] / wd.int - 0.5),
    breaks = seq(0, 360, wd.int),
    include.lowest = TRUE
  )

  x <- cut(
    mydata[[x_col]],
    breaks = seq(0, max.ws, length = ws_bins + 1),
    include.lowest = TRUE
  )

  if (!statistic %in% c(correlation_stats, "nwr", "trend")) {
    ## Simple binned statistics computed via tapply
    binned <- switch(
      statistic,
      frequency = tapply(mydata[[pollutant]], list(wd, x), function(x) {
        length(na.omit(x))
      }),
      mean = tapply(mydata[[pollutant]], list(wd, x), function(x) {
        mean(x, na.rm = TRUE)
      }),
      median = tapply(mydata[[pollutant]], list(wd, x), function(x) {
        median(x, na.rm = TRUE)
      }),
      max = tapply(mydata[[pollutant]], list(wd, x), function(x) {
        max(x, na.rm = TRUE)
      }),
      stdev = tapply(mydata[[pollutant]], list(wd, x), function(x) {
        sd(x, na.rm = TRUE)
      }),
      cpf = tapply(
        mydata[[pollutant]],
        list(wd, x),
        function(x) length(which(x > Pval)) / length(x)
      ),
      cpfi = tapply(
        mydata[[pollutant]],
        list(wd, x),
        function(x) length(which(x > Pval[1] & x <= Pval[2])) / length(x)
      ),
      weighted_mean = tapply(
        mydata[[pollutant]],
        list(wd, x),
        function(x) mean(x) * length(x) / nrow(mydata)
      ),
      percentile = tapply(mydata[[pollutant]], list(wd, x), function(x) {
        quantile(x, probs = percentile / 100, na.rm = TRUE)
      })
    )

    binned <- as.vector(t(binned))
  } else if (toupper(statistic) == "NWR") {
    ## Non-parametric Wind Regression via kernel smoothing
    binned <- rowwise(ws.wd) |>
      summarise(simple_kernel(
        across(.cols = everything()),
        mydata,
        x = x_col,
        y = wd_col,
        pollutant = pollutant,
        ws_spread = ws_spread,
        wd_spread = wd_spread,
        kernel
      ))

    binned <- binned$conc
  } else if (toupper(statistic) == "TREND") {
    ## Kernel-weighted quantile regression trend
    binned <- rowwise(ws.wd) |>
      summarise(simple_kernel_trend(
        across(.cols = everything()),
        mydata,
        x = x_col,
        y = wd_col,
        pollutant = pollutant,
        "date",
        ws_spread = ws_spread,
        wd_spread = wd_spread,
        kernel,
        tau = tau
      ))

    binned <- binned$conc
  } else {
    ## Kernel-weighted pair-wise statistics (correlation, regression, etc.)
    binned <- rowwise(ws.wd) |>
      summarise(calculate_weighted_statistics(
        across(.cols = everything()),
        mydata,
        statistic = statistic,
        x = x_col,
        y = wd_col,
        pol_1 = pollutant[1],
        pol_2 = pollutant[2],
        ws_spread = ws_spread,
        wd_spread = wd_spread,
        kernel,
        tau = tau,
        x_error = x_error,
        y_error = y_error
      ))

    binned <- binned$stat_weighted
    binned <- ifelse(binned == Inf, NA, binned)
  }

  ## Bin frequencies — used for weighting and min.bin filtering
  bin.len <- tapply(mydata[[pollutant[1]]], list(x, wd), length)
  binned.len <- as.vector(bin.len)

  ## Apply edge weights (down-weight bins with very few observations)
  W <- rep(1, times = length(binned))
  ids <- which(binned.len == 1)
  W[ids] <- W[ids] * weights[1]
  ids <- which(binned.len == 2)
  W[ids] <- W[ids] * weights[2]
  ids <- which(binned.len == 3)
  W[ids] <- W[ids] * weights[3]

  ## Remove bins with fewer than min.bin observations
  ids <- which(binned.len < min.bin)
  binned[ids] <- NA
  binned.len[ids] <- NA

  ## GAM power transform: sqrt for positive-only data, identity otherwise
  n <- if (force.positive) 0.5 else 1

  if (!uncertainty) {
    ## Standard surface fit (no confidence intervals)
    Mgam <- try(mgcv::gam(binned^n ~ s(u, v, k = k), weights = W), TRUE)

    if (!inherits(Mgam, "try-error")) {
      pred <- as.vector(predict.gam(Mgam, input.data))^(1 / n)

      if (cluster) {
        results <- interp_grid(input.data, z = pred, n = 101)
        int <- 101
      } else {
        results <- interp_grid(input.data, z = pred, n = 201)
        int <- 201
      }
    } else {
      results <- data.frame(u = u, v = v, z = binned)
      exclude.missing <- FALSE
      warning(
        call. = FALSE,
        paste(
          "Not enough data to fit surface.\nTry reducing the value of the smoothing parameter, k to less than ",
          k,
          ". \nOr use statistic = 'nwr'.",
          sep = ""
        )
      )
    }
  } else {
    ## Surface fit with 95% confidence intervals
    Mgam <- mgcv::gam(binned^n ~ s(u, v, k = k), weights = binned.len)
    pred_se <- predict.gam(Mgam, input.data, se.fit = TRUE)
    uncer <- 2 * as.vector(pred_se[[2]]) ## approx 95% CI half-width

    ## Unweighted central prediction
    Mgam <- mgcv::gam(binned^n ~ s(u, v, k = k))
    pred <- as.vector(predict.gam(Mgam, input.data))
    Lower <- (pred - uncer)^(1 / n)
    Upper <- (pred + uncer)^(1 / n)
    pred <- pred^(1 / n)

    int <- 201
    lower_uncer <- interp_grid(input.data, z = Lower, n = int) |>
      mutate(uncertainty = "lower uncertainty")
    upper_uncer <- interp_grid(input.data, z = Upper, n = int) |>
      mutate(uncertainty = "upper uncertainty")
    prediction <- interp_grid(input.data, z = pred, n = int) |>
      mutate(uncertainty = "prediction")
    results <- bind_rows(prediction, lower_uncer, upper_uncer)
  }

  ## Remove predictions that are too far from the original data
  exclude <- function(results) {
    x <- seq(-upper, upper, length = int)
    wsp <- rep(x, int)
    wdp <- rep(x, rep(int, int))

    all.data <- na.omit(data.frame(u, v, binned.len))
    ind <- with(all.data, exclude.too.far(wsp, wdp, u, v, dist = 0.05))
    results$z[ind] <- NA
    results
  }

  if (exclude.missing) {
    results <- exclude(results)
  }

  results
}


# Gaussian bivariate density function
gauss_dens <- function(x, y, mx, my, sx, sy) {
  (1 / (2 * pi * sx * sy)) *
    exp((-1 / 2) * ((x - mx)^2 / sx^2 + (y - my)^2 / sy^2))
}


# NWR kernel calculations
simple_kernel <- function(
  data,
  mydata,
  x = "ws",
  y = "wd",
  pollutant,
  ws_spread,
  wd_spread,
  kernel
) {
  # Centres
  ws1 <- data[[1]]
  wd1 <- data[[2]]

  # centred ws, wd
  ws_cent <- mydata[[x]] - ws1
  wd_cent <- mydata[[y]] - wd1
  wd_cent <- ifelse(wd_cent < -180, wd_cent + 360, wd_cent)

  weight <- gauss_dens(ws_cent, wd_cent, 0, 0, ws_spread, wd_spread)

  conc <- sum(mydata[[pollutant]] * weight) /
    sum(weight)

  return(data.frame(conc = conc))
}


# Kernel-weighted quantile regression trend
simple_kernel_trend <- function(
  data,
  mydata,
  x = "ws",
  y = "wd",
  pollutant,
  date,
  ws_spread,
  wd_spread,
  kernel,
  tau
) {
  # Centres
  ws1 <- data[[1]]
  wd1 <- data[[2]]

  # centred ws, wd
  ws_cent <- mydata[[x]] - ws1
  wd_cent <- mydata[[y]] - wd1
  wd_cent <- ifelse(wd_cent < -180, wd_cent + 360, wd_cent)

  weight <- gauss_dens(ws_cent, wd_cent, 0, 0, ws_spread, wd_spread)
  weight <- weight / max(weight)

  mydata$weight <- weight

  # quantreg is a Suggests package, so make sure it is there
  try_require("quantreg", "polarPlot")

  # don't fit all data - takes too long with no gain
  mydata <- filter(mydata, weight > 0.00001)

  # Drop dplyr's data frame for formula
  mydata <- data.frame(mydata)

  # Build model
  suppressWarnings(
    fit <- try(
      quantreg::rq(
        mydata[[pollutant[1]]] ~ mydata[[date]],
        tau = tau,
        weights = mydata[["weight"]],
        method = "fn"
      ),
      TRUE
    )
  )

  # Extract statistics
  if (!inherits(fit, "try-error")) {
    slope <- 365.25 * 24 * 3600 * fit$coefficients[2]
  } else {
    slope <- NA
  }

  return(data.frame(conc = slope))
}


## Kernel-weighted pair-wise statistics dispatcher.
##
## Computes Gaussian kernel weights centred on (ws1, wd1), filters out
## low-weight rows, then delegates to the appropriate sub-function based on
## `statistic`.  Returns a one-row data frame with columns (ws1, wd1,
## stat_weighted).
calculate_weighted_statistics <- function(
  data,
  mydata,
  statistic,
  x = "ws",
  y = "wd",
  pol_1,
  pol_2,
  ws_spread,
  wd_spread,
  kernel,
  tau,
  x_error,
  y_error
) {
  weight <- NULL

  # Centres
  ws1 <- data[[1]]
  wd1 <- data[[2]]

  # Gaussian kernel weights centred on this ws/wd point
  ws_cent <- mydata[[x]] - ws1
  wd_cent <- mydata[[y]] - wd1
  wd_cent <- ifelse(wd_cent < -180, wd_cent + 360, wd_cent)

  weight <- gauss_dens(ws_cent, wd_cent, 0, 0, ws_spread, wd_spread)
  weight <- weight / max(weight)

  mydata$weight <- weight

  # Select columns and filter low-weight rows
  vars <- c(pol_1, pol_2, "weight")
  if (!all(is.na(c(x_error, y_error)))) {
    vars <- c(vars, x_error, y_error)
  }
  thedata <- mydata[vars]
  thedata <- thedata[which(thedata$weight > 0.001), ]

  # Insufficient data — return NA
  if (nrow(thedata) < 100) {
    return(data.frame(ws1, wd1, stat_weighted = NA))
  }

  # Dispatch to the appropriate statistical sub-function
  if (statistic %in% c("r", "Pearson", "Spearman")) {
    stat_weighted <- calc_corr_stat(thedata, pol_1, pol_2, statistic)
  } else if (statistic %in% c("slope", "intercept")) {
    stat_weighted <- calc_lm_stat(thedata, pol_1, pol_2, statistic)
  } else if (statistic == "york_slope") {
    stat_weighted <- calc_york_stat(thedata, pol_1, pol_2, x_error, y_error)
  } else if (grepl("robust", statistic, ignore.case = TRUE)) {
    stat_weighted <- calc_robust_stat(thedata, pol_1, pol_2, statistic)
  } else if (grepl("quantile", statistic, ignore.case = TRUE)) {
    stat_weighted <- calc_quantile_stat(thedata, pol_1, pol_2, statistic, tau)
  } else {
    stat_weighted <- NA
  }

  data.frame(ws1, wd1, stat_weighted)
}


# Weighted Pearson or Spearman correlation
calc_corr_stat <- function(thedata, pol_1, pol_2, statistic) {
  if (statistic == "r") {
    statistic <- "Pearson"
  }
  contCorr(
    thedata[[pol_1]],
    thedata[[pol_2]],
    w = thedata$weight,
    method = statistic
  )
}


# Weighted OLS slope or intercept
calc_lm_stat <- function(thedata, pol_1, pol_2, statistic) {
  thedata <- data.frame(thedata)
  fit <- lm(
    thedata[, pol_1] ~ thedata[, pol_2],
    weights = thedata[, "weight"]
  )
  if (statistic == "slope") fit$coefficients[2] else fit$coefficients[1]
}


# York regression slope (errors-in-variables)
calc_york_stat <- function(thedata, pol_1, pol_2, x_error, y_error) {
  thedata <- as.data.frame(thedata)
  result <- try(
    YorkFit(
      thedata,
      X = names(thedata)[2],
      Y = names(thedata)[1],
      Xstd = x_error,
      Ystd = y_error,
      weight = thedata$weight
    ),
    TRUE
  )
  if (!inherits(result, "try-error")) result$Slope else NA
}


# Weighted robust (M-estimator) regression slope or intercept
calc_robust_stat <- function(thedata, pol_1, pol_2, statistic) {
  thedata <- data.frame(thedata)
  fit <- suppressWarnings(
    try(
      MASS::rlm(
        thedata[, pol_1] ~ thedata[, pol_2],
        weights = thedata[, "weight"],
        method = "M"
      ),
      TRUE
    )
  )
  if (inherits(fit, "try-error")) {
    return(NA)
  }
  if (statistic == "robust_slope") fit$coefficients[2] else fit$coefficients[1]
}


# Weighted quantile regression slope or intercept
calc_quantile_stat <- function(thedata, pol_1, pol_2, statistic, tau) {
  try_require("quantreg", "polarPlot")
  thedata <- data.frame(thedata)
  fit <- suppressWarnings(
    try(
      quantreg::rq(
        thedata[[pol_1]] ~ thedata[[pol_2]],
        tau = tau,
        weights = thedata[["weight"]],
        method = "br"
      ),
      TRUE
    )
  )
  if (inherits(fit, "try-error")) {
    return(NA)
  }
  if (statistic == "quantile_slope") {
    fit$coefficients[2]
  } else {
    fit$coefficients[1]
  }
}


# From enlightenr package
kernel_smoother <- function(x, kernel = "gaussian") {
  kernel <- tolower(kernel)
  if (kernel %in% c("gaussian", "normal")) {
    x <- (2 * pi)^-0.5 * exp(-0.5 * x^2)
  }
  if (kernel == "epanechnikov") {
    x <- 3 / 4 * (1 - x^2) * indicator_function(x)
  }
  if (kernel == "logistic") {
    x <- 1 / (exp(x) + 2 + exp(-x))
  }
  if (kernel == "cosine") {
    x <- pi / 4 * cos((pi / 2) * x) * indicator_function(x)
  }
  if (kernel == "triangular") {
    x <- (1 - abs(x)) * indicator_function(x)
  }
  if (kernel %in% c("box", "uniform")) {
    x <- 1 / 2 * indicator_function(x)
  }
  if (kernel == "tricube") {
    x <- 70 / 81 * (1 - abs(x)^3)^3 * indicator_function(x)
  }
  if (kernel == "triweight") {
    x <- 35 / 32 * (1 - x^2)^3 * indicator_function(x)
  }
  if (kernel %in% c("biweight", "quartic")) {
    x <- 15 / 16 * (1 - x^2)^2 * indicator_function(x)
  }
  x
}


indicator_function <- function(x) ifelse(abs(x) <= 1, 1, 0)


# Weighted Pearson and Spearman correlations, based on wCorr package
contCorr <- function(x, y, w, method = c("Pearson", "Spearman")) {
  if (!is.numeric(x)) {
    x <- as.numeric(x)
  }
  if (!is.numeric(y)) {
    y <- as.numeric(y)
  }
  if (!is.numeric(w)) {
    w <- as.numeric(w)
  }
  pm <- pmatch(tolower(method[[1]]), tolower(c("Pearson", "Spearman")))
  if (pm == 2) {
    x <- wrank(x, w)
    y <- wrank(y, w)
  }
  xb <- sum(w * x) / sum(w)
  yb <- sum(w * y) / sum(w)
  numerator <- sum(w * (x - xb) * (y - yb))
  denom <- sqrt(sum(w * (x - xb)^2) * sum(w * (y - yb)^2))
  numerator / denom
}


wrank <- function(x, w = rep(1, length(x))) {
  # sort by x so we can just traverse once
  ord <- order(x)
  rord <- (1:length(x))[order(ord)] # reverse order
  xp <- x[ord] # x, permuted
  wp <- w[ord] # weights, permuted
  rnk <- rep(NA, length(x)) # blank ranks vector
  # setup first itteration
  t1 <- 0 # total weight of lower ranked elements
  i <- 1 # index
  t2 <- 0 # total weight of tied elements (including self)
  n <- 0 # number of tied elements
  while (i < length(x)) {
    t2 <- t2 + wp[i] # tied weight increases by this unit
    n <- n + 1 # one additional tied unit
    if (xp[i + 1] != xp[i]) {
      # the next one is not a tie
      # find the rank of all tied elements
      rnki <- t1 + (n + 1) / (2 * n) * t2
      # push that rank to all tied units
      for (ii in 1:n) {
        rnk[i - ii + 1] <- rnki
      }
      # reset for next itteration
      t1 <- t1 + t2 # new total weight for lower values
      t2 <- 0 # new tied weight starts at 0
      n <- 0 # no tied units
    }
    i <- i + 1
  }
  # final row
  n <- n + 1 # add final tie
  t2 <- t2 + wp[i] # add final weight to tied weight
  rnki <- t1 + (n + 1) / (2 * n) * t2 # final rank
  # push that rank to all final tied units
  for (ii in 1:n) {
    rnk[i - ii + 1] <- rnki
  }
  # order by incoming index, so put in the original order
  rnk[rord]
}


YorkFit <- function(
  input_data,
  X = "X",
  Y = "Y",
  Xstd = "Xstd",
  Ystd = "Ystd",
  weight = NA,
  Ri = 0,
  eps = 1e-7
) {
  # weight can be supplied to apply to errors

  tol <- 1e-7 # need to refine

  # b0 initial guess at slope for OLR
  form <- formula(paste(Y, "~", X))
  mod <- lm(form, data = input_data)
  b0 <- mod$coefficients[2]

  X <- input_data[[X]]
  Y <- input_data[[Y]]

  Xstd <- input_data[[Xstd]]
  Ystd <- input_data[[Ystd]]

  # don't try regression if < 3 points
  if (sum(!is.na(X)) < 3 || sum(!is.na(Y)) < 3) {
    return()
  }

  # used in polar plots - Gaussian kernel weighting
  if (!all(is.na(weight))) {
    Xstd <- Xstd / weight
    Ystd <- Ystd / weight
  }

  Xw <- 1 / (Xstd^2) # X weights
  Yw <- 1 / (Ystd^2) # Y weights

  # ITERATIVE CALCULATION OF SLOPE AND INTERCEPT #

  b <- b0
  b.diff <- tol + 1

  n <- 0 # counter for debugging

  while (b.diff > tol && n < 100) {
    n <- n + 1 # counter to keep a check on convergence

    b.old <- b
    alpha.i <- sqrt(Xw * Yw)
    Wi <- (Xw * Yw) / ((b^2) * Yw + Xw - 2 * b * Ri * alpha.i)
    WiX <- Wi * X
    WiY <- Wi * Y
    sumWiX <- sum(WiX, na.rm = TRUE)
    sumWiY <- sum(WiY, na.rm = TRUE)
    sumWi <- sum(Wi, na.rm = TRUE)
    Xbar <- sumWiX / sumWi
    Ybar <- sumWiY / sumWi
    Ui <- X - Xbar
    Vi <- Y - Ybar

    Bi <- Wi * ((Ui / Yw) + (b * Vi / Xw) - (b * Ui + Vi) * Ri / alpha.i)
    wTOPint <- Bi * Wi * Vi
    wBOTint <- Bi * Wi * Ui
    sumTOP <- sum(wTOPint, na.rm = TRUE)
    sumBOT <- sum(wBOTint, na.rm = TRUE)
    b <- sumTOP / sumBOT

    # zero or problematic data
    if (anyNA(b, b.old)) {
      return(tibble(
        Intercept = NA,
        Slope = NA,
        Int_error = NA,
        Slope_error = NA,
        OLS_slope = NA
      ))
    }

    b.diff <- abs(b - b.old)
  }

  a <- Ybar - b * Xbar
  wYorkFitCoefs <- c(a, b)

  # ERROR CALCULATION #

  Xadj <- Xbar + Bi
  WiXadj <- Wi * Xadj
  sumWiXadj <- sum(WiXadj, na.rm = TRUE)
  Xadjbar <- sumWiXadj / sumWi
  Uadj <- Xadj - Xadjbar
  wErrorTerm <- Wi * Uadj * Uadj
  errorSum <- sum(wErrorTerm, na.rm = TRUE)
  b.err <- sqrt(1 / errorSum)
  a.err <- sqrt((1 / sumWi) + (Xadjbar^2) * (b.err^2))
  wYorkFitErrors <- c(a.err, b.err)

  # GOODNESS OF FIT CALCULATION #
  lgth <- length(X)
  wSint <- Wi * (Y - b * X - a)^2
  sumSint <- sum(wSint, na.rm = TRUE)
  wYorkGOF <- c(sumSint / (lgth - 2), sqrt(2 / (lgth - 2))) # GOF (should equal 1 if assumptions are valid), #standard error in GOF

  ans <- tibble(
    Intercept = a,
    Slope = b,
    Int_error = a.err,
    Slope_error = b.err,
    OLS_slope = b0
  )

  return(ans)
}


# Bi-linear interpolation on a regular grid.
# Allows surface predictions at a low resolution to be interpolated rather than
# using a high-k GAM directly.
interp.surface <- function(obj, loc) {
  # obj is a surface or image object like the list for contour, persp or image.
  # loc a matrix of 2-d locations -- new points to evaluate the surface.
  x <- obj$x
  y <- obj$y
  z <- obj$z
  nx <- length(x)
  ny <- length(y)
  # this clever idea for finding the intermediate coordinates at the new points
  # is from J-O Irisson
  lx <- approx(x, 1:nx, loc[, 1])$y
  ly <- approx(y, 1:ny, loc[, 2])$y
  lx1 <- floor(lx)
  ly1 <- floor(ly)
  # x and y distances between each new point and the closest grid point in the lower left hand corner.
  ex <- lx - lx1
  ey <- ly - ly1
  # fix up weights to handle the case when loc are equal to
  # last grid point.  These have been set to NA above.
  ex[lx1 == nx] <- 1
  ey[ly1 == ny] <- 1
  lx1[lx1 == nx] <- nx - 1
  ly1[ly1 == ny] <- ny - 1
  # bilinear interpolation finds simple weights based on the
  # the four corners of the grid box containing the new points.
  return(
    z[cbind(lx1, ly1)] *
      (1 - ex) *
      (1 - ey) +
      z[cbind(lx1 + 1, ly1)] * ex * (1 - ey) +
      z[cbind(lx1, ly1 + 1)] * (1 - ex) * ey +
      z[cbind(lx1 + 1, ly1 + 1)] * ex * ey
  )
}


# Bilinear interpolation from a coarse GAM prediction grid to a fine output grid
interp_grid <- function(input.data, x = "u", y = "v", z, n = 201) {
  # current number of points
  int <- length(unique(input.data[[x]]))

  obj <- list(
    x = seq(min(input.data[[x]]), max(input.data[[x]]), length.out = int),
    y = seq(min(input.data[[y]]), max(input.data[[y]]), length.out = int),
    z = matrix(z, nrow = int)
  )

  loc <- expand.grid(
    x = seq(min(input.data[[x]]), max(input.data[[x]]), length.out = n),
    y = seq(min(input.data[[y]]), max(input.data[[y]]), length.out = n)
  )

  res.interp <- interp.surface(obj, loc)

  out <- expand.grid(
    u = seq(min(input.data[[x]]), max(input.data[[x]]), length.out = n),
    v = seq(min(input.data[[y]]), max(input.data[[y]]), length.out = n)
  )

  data.frame(out, z = res.interp)
}
