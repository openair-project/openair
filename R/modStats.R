#' Calculate common model evaluation statistics
#'
#' Function to calculate common numerical model evaluation statistics with
#' flexible conditioning.
#'
#' This function is under development and currently provides some common model
#' evaluation statistics. These include (to be mathematically defined later):
#'
#' \itemize{
#'
#' \item \eqn{n}, the number of complete pairs of data.
#'
#' \item \eqn{FAC2}, fraction of predictions within a factor of two.
#'
#' \item \eqn{MB}, the mean bias.
#'
#' \item \eqn{MGE}, the mean gross error.
#'
#' \item \eqn{NMB}, the normalised mean bias.
#'
#' \item \eqn{NMGE}, the normalised mean gross error.
#'
#' \item \eqn{RMSE}, the root mean squared error.
#'
#' \item \eqn{r}, the Pearson correlation coefficient. Note, can also supply and
#' argument `method` e.g. `method = "spearman"`. Also returned is the
#' P value of the correlation coefficient, \eqn{P}, which may present as `0` for
#' very low values.
#'
#' \item \eqn{COE}, the *Coefficient of Efficiency* based on Legates and
#' McCabe (1999, 2012). There have been many suggestions for measuring model
#' performance over the years, but the COE is a simple formulation which is easy
#' to interpret.
#'
#' A perfect model has a COE = 1. As noted by Legates and McCabe although the
#' COE has no lower bound, a value of COE = 0.0 has a fundamental meaning. It
#' implies that the model is no more able to predict the observed values than
#' does the observed mean. Therefore, since the model can explain no more of the
#' variation in the observed values than can the observed mean, such a model can
#' have no predictive advantage.
#'
#' For negative values of COE, the model is less effective than the observed
#' mean in predicting the variation in the observations. \item \eqn{IOA}, the
#' Index of Agreement based on Willmott et al. (2011), which spans between -1
#' and +1 with values approaching +1 representing better model performance.
#'
#' An IOA of 0.5, for example, indicates that the sum of the error-magnitudes is
#' one half of the sum of the observed-deviation magnitudes.  When IOA = 0.0, it
#' signifies that the sum of the magnitudes of the errors and the sum of the
#' observed-deviation magnitudes are equivalent. When IOA = -0.5, it indicates
#' that the sum of the error-magnitudes is twice the sum of the perfect
#' model-deviation and observed-deviation magnitudes. Values of IOA near -1.0
#' can mean that the model-estimated deviations about O are poor estimates of
#' the observed deviations; but, they also can mean that there simply is little
#' observed variability - so some caution is needed when the IOA approaches -1.
#' }
#'
#' All statistics are based on complete pairs of `mod` and `obs`.
#'
#' Conditioning is possible through setting `type`, which can be a vector
#' e.g. `type = c("weekday", "season")`.
#'
#' @param mydata A data frame.
#' @param mod Name of a variable in `mydata` that represents modelled
#'   values.
#' @param obs Name of a variable in `mydata` that represents measured
#'   values.
#' @param statistic The statistic to be calculated. See details below for a
#'   description of each.
#' @param type `type` determines how the data are split i.e. conditioned,
#'   and then plotted. The default is will produce statistics using the entire
#'   data. `type` can be one of the built-in types as detailed in
#'   `cutData` e.g. \dQuote{season}, \dQuote{year}, \dQuote{weekday} and so
#'   on. For example, `type = "season"` will produce four sets of
#'   statistics --- one for each season.
#'
#'   It is also possible to choose `type` as another variable in the data
#'   frame. If that variable is numeric, then the data will be split into four
#'   quantiles (if possible) and labelled accordingly. If type is an existing
#'   character or factor variable, then those categories/levels will be used
#'   directly. This offers great flexibility for understanding the variation of
#'   different variables and how they depend on one another.
#'
#'   More than one type can be considered e.g. `type = c("season",
#'   "weekday")` will produce statistics split by season and day of the week.
#' @param rank.name Simple model ranking can be carried out if `rank.name`
#'   is supplied. `rank.name` will generally refer to a column representing
#'   a model name, which is to ranked. The ranking is based the COE performance,
#'   as that indicator is arguably the best single model performance indicator
#'   available.
#' @inheritDotParams cutData -type
#' @export
#' @return Returns a data frame with model evaluation statistics.
#' @author David Carslaw
#' @references Legates DR, McCabe GJ. (1999). Evaluating the use of
#'   goodness-of-fit measures in hydrologic and hydroclimatic model validation.
#'   Water Resources Research 35(1): 233-241.
#'
#'   Legates DR, McCabe GJ. (2012). A refined index of model performance: a
#'   rejoinder, International Journal of Climatology.
#'
#'   Willmott, C.J., Robeson, S.M., Matsuura, K., 2011. A refined index of model
#'   performance. International Journal of Climatology.
#' @family model evaluation functions
#' @examples
#'
#' ## the example below is somewhat artificial --- assuming the observed
#' ## values are given by NOx and the predicted values by NO2.
#'
#' modStats(mydata, mod = "no2", obs = "nox")
#'
#' ## evaluation stats by season
#'
#' modStats(mydata, mod = "no2", obs = "nox", type = "season")
#'
modStats <- function(
  mydata,
  mod = "mod",
  obs = "obs",
  statistic = c(
    "n",
    "FAC2",
    "MB",
    "MGE",
    "NMB",
    "NMGE",
    "RMSE",
    "r",
    "COE",
    "IOA"
  ),
  type = "default",
  rank.name = NULL,
  ...
) {
  ## function to calculate model evaluation statistics
  ## the default is to use the entire data set.
  ## Requires a field "date" and optional conditioning variables representing measured and modelled values

  ## extract variables of interest
  vars <- c(mod, obs)

  if (any(type %in% dateTypes)) {
    vars <- c("date", vars)
  }

  theStats <- c(
    "n",
    "FAC2",
    "MB",
    "MGE",
    "NMB",
    "NMGE",
    "RMSE",
    "r",
    "COE",
    "IOA"
  )

  matching <- statistic %in% theStats

  if (any(!matching)) {
    ## not all variables are present
    stop(cat("Can't find the statistic(s)", statistic[!matching], "\n"))
  }

  ## check the data
  mydata <- checkPrep(
    mydata,
    vars,
    type,
    remove.calm = FALSE,
    strip.white = FALSE
  )

  mydata <- cutData(mydata, type, ...)

  ## calculate the various statistics

  if ("n" %in% statistic) {
    res.n <- mydata %>%
      group_by(across(type)) %>%
      do(n(., mod, obs))
  } else {
    res.n <- NULL
  }

  if ("FAC2" %in% statistic) {
    res.FAC <- mydata %>%
      group_by(across(type)) %>%
      do(FAC2(., mod, obs))
  } else {
    res.FAC <- NULL
  }

  if ("MB" %in% statistic) {
    res.MB <- mydata %>%
      group_by(across(type)) %>%
      do(MB(., mod, obs))
  } else {
    res.MB <- NULL
  }

  if ("MGE" %in% statistic) {
    res.MGE <- mydata %>%
      group_by(across(type)) %>%
      do(MGE(., mod, obs))
  } else {
    res.MGE <- NULL
  }

  if ("NMB" %in% statistic) {
    res.NMB <- mydata %>%
      group_by(across(type)) %>%
      do(NMB(., mod, obs))
  } else {
    res.NMB <- NULL
  }

  if ("NMGE" %in% statistic) {
    res.NMGE <- mydata %>%
      group_by(across(type)) %>%
      do(NMGE(., mod, obs))
  } else {
    res.NMGE <- NULL
  }

  if ("RMSE" %in% statistic) {
    res.RMSE <- mydata %>%
      group_by(across(type)) %>%
      do(RMSE(., mod, obs))
  } else {
    res.RMSE <- NULL
  }

  if ("r" %in% statistic) {
    res.r <- mydata %>%
      group_by(across(type)) %>%
      do(r(., mod, obs, ...))
  } else {
    res.r <- NULL
  }

  if ("COE" %in% statistic) {
    res.COE <- mydata %>%
      group_by(across(type)) %>%
      do(COE(., mod, obs))
  } else {
    res.COE <- NULL
  }

  if ("IOA" %in% statistic) {
    res.IOA <- mydata %>%
      group_by(across(type)) %>%
      do(IOA(., mod, obs))
  } else {
    res.IOA <- NULL
  }

  ## merge them all into one data frame
  results <- list(
    res.n,
    res.FAC,
    res.MB,
    res.MGE,
    res.NMB,
    res.NMGE,
    res.RMSE,
    res.r,
    res.COE,
    res.IOA
  )

  ## remove NULLs from lits
  results <- results[!sapply(results, is.null)]
  results <- Reduce(
    function(x, y, by = type) merge(x, y, by = type, all = TRUE),
    results
  )

  results <- sortDataFrame(results, key = type)

  ## simple ranking of models?
  if (!is.null(rank.name)) {
    types <- setdiff(type, rank.name)

    if (length(types) == 0) {
      results <- rankModels(results, rank.name)
    } else {
      results <- results %>%
        group_by(across(types)) %>%
        do(rankModels(., rank.name = rank.name))
    }
  }

  return(dplyr::tibble(results))
}

sortDataFrame <- function(x, key, ...) {
  ## function to sort a data frame given one or more column names (key)
  ## from http://tolstoyc.newcastle.edu.au/R/help/04/07/1076.html

  if (missing(key)) {
    rn <- rownames(x)
    if (all(rn %in% 1:nrow(x))) {
      rn <- as.numeric(rn)
    }
    x[order(rn, ...), , drop = FALSE]
  } else {
    x[do.call("order", c(x[key], ...)), , drop = FALSE]
  }
}


rankModels <- function(mydata, rank.name = "group") {
  ## sort by COE
  mydata <- sortDataFrame(mydata, "COE", decreasing = TRUE)
}

## number of valid readings
n <- function(x, mod = "mod", obs = "obs") {
  x <- na.omit(x[, c(mod, obs)])
  res <- nrow(x)
  data.frame(n = res)
}

## fraction within a factor of two
FAC2 <- function(x, mod = "mod", obs = "obs") {
  x <- na.omit(x[, c(mod, obs)])
  ratio <- x[[mod]] / x[[obs]]
  ratio <- na.omit(ratio)
  len <- length(ratio)
  if (len > 0) {
    res <- length(which(ratio >= 0.5 & ratio <= 2)) / len
  } else {
    res <- NA
  }
  data.frame(FAC2 = res)
}

## mean bias
MB <- function(x, mod = "mod", obs = "obs") {
  x <- na.omit(x[, c(mod, obs)])
  res <- mean(x[[mod]] - x[[obs]])
  data.frame(MB = res)
}

## mean gross error
MGE <- function(x, mod = "mod", obs = "obs") {
  x <- na.omit(x[, c(mod, obs)])
  res <- mean(abs(x[[mod]] - x[[obs]]))
  data.frame(MGE = res)
}

## normalised mean bias
NMB <- function(x, mod = "mod", obs = "obs") {
  x <- na.omit(x[, c(mod, obs)])
  res <- sum(x[[mod]] - x[[obs]]) / sum(x[[obs]])
  data.frame(NMB = res)
}

## normalised mean gross error
NMGE <- function(x, mod = "mod", obs = "obs") {
  x <- na.omit(x[, c(mod, obs)])
  res <- sum(abs(x[[mod]] - x[[obs]])) / sum(x[[obs]])
  data.frame(NMGE = res)
}

## root mean square error
RMSE <- function(x, mod = "mod", obs = "obs") {
  x <- na.omit(x[, c(mod, obs)])
  res <- mean((x[[mod]] - x[[obs]])^2)^0.5
  data.frame(RMSE = res)
}

## correlation coefficient
# when SD=0; will return(NA)
r <- function(x, mod = "mod", obs = "obs", ...) {
  x <- na.omit(x[, c(mod, obs)])
  res <- suppressWarnings(stats::cor.test(x[[mod]], x[[obs]], ...))

  data.frame(r = res$estimate, P = res$p.value)
}

##  Coefficient of Efficiency
COE <- function(x, mod = "mod", obs = "obs") {
  x <- na.omit(x[, c(mod, obs)])

  res <- 1 - sum(abs(x[[mod]] - x[[obs]])) / sum(abs(x[[obs]] - mean(x[[obs]])))

  data.frame(COE = res)
}

##  Index of Agreement
IOA <- function(x, mod = "mod", obs = "obs") {
  x <- na.omit(x[, c(mod, obs)])

  LHS <- sum(abs(x[[mod]] - x[[obs]]))
  RHS <- 2 * sum(abs(x[[obs]] - mean(x[[obs]])))

  if (LHS <= RHS) {
    res <- 1 - LHS / RHS
  } else {
    res <- RHS / LHS - 1
  }

  data.frame(IOA = res)
}
