#' Bin data, calculate mean and bootstrap confidence interval in the mean
#'
#' [binData()] summarises data by intervals and calculates the mean and
#' bootstrap confidence intervals (by default 95% CI) in the mean of a chosen
#' variable in a data frame. Any other numeric variables are summarised by their
#' mean intervals. This occurs via [bootMeanDF()], which calculates the
#' uncertainty intervals in the mean of a vector.
#'
#' There are three options for binning. The default is to bin `bin` into 40
#' intervals. Second, the user can choose an binning `interval`, e.g., `interval
#' = 5`. Third, the user can supply their own `breaks` to use as binning
#' intervals. Note that intervals are calculated on the whole dataset before the
#' data is cut into categories using `type`.
#'
#' @param mydata Name of the data frame to process.
#' @param bin The name of the column to divide into intervals.
#' @param uncer The name of the column for which the mean, lower and upper
#'   uncertainties should be calculated for each interval of `bin`.
#' @param type Used for splitting the data further. Passed to [cutData()]. Note
#'   that intervals are calculated on the whole dataset before the data is
#'   categorised, meaning intervals will be the same for the different groups.
#' @param n The number of intervals to split `bin` into.
#' @param interval The interval to be used for binning the data.
#' @param breaks User specified breaks to use for binning.
#' @param conf.int The confidence interval, defaulting to `0.95` (i.e., the 95%
#'   Confidence Interval).
#' @param B The number of bootstrap simulations.
#' @param ... Other parameters that are passed on to [cutData()], for use with
#'   `type`.
#'
#' @return Returns a summarised data frame with new columns for the mean and
#'   upper / lower confidence intervals in the mean.
#' @export
#'
#' @rdname bootMeans
#' @order 1
#'
#' @examples
#' # work with vectors
#' test <- rnorm(20, mean = 10)
#' bootMeanDF(test)
#'
#' # how does nox vary by intervals of wind speed?
#' results <- binData(mydata, bin = "ws", uncer = "nox")
#' \dontrun{
#' library(ggplot2)
#' ggplot(results, aes(x = ws, y = mean, ymin = min, ymax = max)) +
#'   geom_pointrange()
#' }
#'
#' # what about weekend vs weekday?
#' results2 <- binData(mydata, bin = "ws", uncer = "nox", type = "weekend")
#' \dontrun{
#' ggplot(results2, aes(x = ws, y = mean, ymin = min, ymax = max)) +
#'   geom_pointrange() +
#'   facet_wrap(vars(weekend))
#' }
#'
binData <- function(
  mydata,
  bin = "nox",
  uncer = "no2",
  type = "default",
  n = 40,
  interval = NA,
  breaks = NA,
  conf.int = 0.95,
  B = 250,
  ...
) {
  if (!is.na(interval)) {
    mydata$interval <- cut(
      mydata[[bin]],
      sort(unique(round_any(
        mydata[[bin]],
        interval
      ))),
      include.lowest = TRUE
    )
  } else if (!anyNA(breaks)) {
    mydata$interval <- cut(
      mydata[[bin]],
      breaks = breaks,
      include.lowest = TRUE
    )
  } else {
    mydata$interval <- cut(mydata[[bin]], breaks = n)
  }

  # cut for type
  mydata <- cutData(mydata, type = type, ...)

  # remove any missing intervals
  mydata <- mydata[!is.na(mydata$interval), ]

  # calculate 95% CI in mean
  uncert <- dplyr::reframe(
    mydata,
    bootMeanDF(.data[[uncer]], conf.int = conf.int, B = B),
    .by = dplyr::all_of(c("interval", type))
  )

  # calculate mean for other variables
  means <- dplyr::summarise(
    mydata,
    dplyr::across(dplyr::where(is.numeric), function(x) {
      mean(x, na.rm = TRUE)
    }),
    .by = dplyr::all_of(c("interval", type))
  )

  # join dataframes
  by <- c("interval", type)
  mydata <- dplyr::inner_join(means, uncert, by = by)

  # drop type column if default
  if (type == "default") {
    mydata$default <- NULL
  }

  # return
  return(mydata)
}

#' @param x A vector from which the mean and bootstrap confidence intervals in
#'   the mean are to be calculated
#' @rdname bootMeans
#' @order 2
#' @export
bootMeanDF <- function(x, conf.int = 0.95, B = 1000) {
  if (!is.vector(x)) {
    cli::cli_abort(c(
      "x" = "{.field x} should be a vector.",
      "i" = "{.field x} is a {class(x)}."
    ))
  }

  res <- bootMean(x = x, conf.int = conf.int, B = B)
  res <- data.frame(
    mean = res[1],
    min = res[2],
    max = res[3],
    n = length(na.omit(x))
  )
  return(res)
}

#' bootsrap confidence intervals in the mean from Hmisc
#' @noRd
bootMean <- function(x, conf.int = 0.95, B = 1000, ...) {
  x <- x[!is.na(x)] # remove missings
  n <- length(x)
  xbar <- mean(x)
  if (n < 2) {
    return(c(
      Mean = xbar,
      Lower = NA,
      Upper = NA
    ))
  }
  z <- unlist(lapply(
    1:B,
    function(i, x, N) {
      sum(x[(sample.int(N, N, TRUE, NULL))])
    },
    x = x,
    N = n
  )) /
    n
  quant <- quantile(z, c((1 - conf.int) / 2, (1 + conf.int) / 2))
  names(quant) <- NULL
  res <- c(
    Mean = xbar,
    Lower = quant[1],
    Upper = quant[2]
  )

  res
}

#' difference between bootmeans
#' @noRd
bootMeanDiff <- function(
  mydata,
  x = "x",
  y = "y",
  conf.int = 0.95,
  B = 1000,
  na.rm = FALSE
) {
  ## calculates bootstrap mean differences
  ## assumes y - x
  x.name <- x
  y.name <- y
  x <- na.omit(mydata[[x]])
  y <- na.omit(mydata[[y]])
  Mean <- mean(y) - mean(x)

  if (nrow(mydata) < 2 | is.na(Mean)) {
    res1 <- data.frame(
      variable = x.name,
      Mean = mean(x),
      Lower = NA,
      Upper = NA,
      stringsAsFactors = FALSE
    )

    res2 <- data.frame(
      variable = y.name,
      Mean = mean(y),
      Lower = NA,
      Upper = NA,
      stringsAsFactors = FALSE
    )

    res <- data.frame(
      variable = paste(y.name, "-", x.name),
      Mean = Mean,
      Lower = NA,
      Upper = NA,
      stringsAsFactors = FALSE
    )

    res <- bind_rows(res1, res2, res)
    res$variable <- factor(res$variable)
    return(res)
  }

  x <- bootMean(x, B = B)
  y <- bootMean(y, B = B)
  quant1 <- quantile(
    x,
    c((1 - conf.int) / 2, (1 + conf.int) / 2),
    na.rm = na.rm
  )
  quant2 <- quantile(
    y,
    c((1 - conf.int) / 2, (1 + conf.int) / 2),
    na.rm = na.rm
  )
  quant <- quantile(
    y - x,
    c((1 - conf.int) / 2, (1 + conf.int) / 2),
    na.rm = na.rm
  )
  names(quant1) <- NULL
  names(quant2) <- NULL
  names(quant) <- NULL

  res1 <- data.frame(
    variable = x.name,
    Mean = mean(x),
    Lower = quant1[1],
    Upper = quant1[2],
    stringsAsFactors = FALSE
  )

  res2 <- data.frame(
    variable = y.name,
    Mean = mean(y),
    Lower = quant2[1],
    Upper = quant2[2],
    stringsAsFactors = FALSE
  )

  res <- data.frame(
    variable = paste(y.name, "-", x.name),
    Mean = Mean,
    Lower = quant[1],
    Upper = quant[2],
    stringsAsFactors = FALSE
  )

  res <- bind_rows(res1, res2, res)
  res$variable <- factor(res$variable)
  res
}
