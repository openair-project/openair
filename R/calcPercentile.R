#' Calculate percentile values from a time series
#'
#' Calculates multiple percentile values from a time series, with flexible time
#' aggregation. This function is a wrapper for [timeAverage()], making it easier
#' to calculate several percentiles at once. Like [timeAverage()], it requires a
#' data frame with a `date` field and one other numeric variable.
#'
#' @inheritParams timeAverage
#' @param pollutant Name of column containing variable to summarise, likely a
#'   pollutant (e.g., `"o3"`).
#' @param percentile A vector of percentile values; for example, `percentile =
#'   50` will calculate median values. Multiple values may also be provided as a
#'   vector, e.g., `percentile = c(5, 50, 95)` or `percentile = seq(0, 100,
#'   10)`.
#' @param prefix Each new column is named by appending a `prefix` to
#'   `percentile`. For example, the default `"percentile."` will name the new
#'   column as `percentile.95` when `percentile = 95`.
#' @export
#'
#' @return Returns a `data.frame` with a `date` column plus an additional
#'   column for each given `percentile`.
#' @author David Carslaw
#' @seealso [timePlot()], [timeAverage()]
#' @examples
#' # 95th percentile monthly o3 concentrations
#' percentiles <- calcPercentile(mydata,
#'   pollutant = "o3",
#'   avg.time = "month", percentile = 95
#' )
#'
#' head(percentiles)
#'
#' # 5, 50, 95th percentile monthly o3 concentrations
#' \dontrun{
#' percentiles <- calcPercentile(mydata,
#'   pollutant = "o3",
#'   avg.time = "month", percentile = c(5, 50, 95)
#' )
#'
#' head(percentiles)
#' }
calcPercentile <- function(
  mydata,
  pollutant = "o3",
  avg.time = "month",
  percentile = 50,
  type = "default",
  data.thresh = 0,
  start.date = NA,
  end.date = NA,
  prefix = "percentile."
) {
  # check pollutant is valid
  if (!pollutant %in% names(mydata)) {
    cli::cli_abort(
      c(
        "x" = "{.field pollutant} '{pollutant}' not present in data.",
        "i" = "{.emph Columns in {.field mydata}}: {names(mydata)}"
      )
    )
  }

  # iterate over 'percentile's to calculate %tiles
  mydata <- purrr::map(
    .x = percentile,
    .f = function(x) {
      make.percentile(
        mydata,
        pollutant = pollutant,
        avg.time = avg.time,
        data.thresh = data.thresh,
        percentile = x,
        type = type,
        start.date = start.date,
        end.date = end.date,
        prefix = prefix
      )
    }
  )

  # get joining columns
  by <- "date"
  if (any(type != "default")) {
    by <- append(by, type)
  }

  # bind into one
  mydata <- purrr::reduce(mydata, function(x, y) {
    dplyr::full_join(x, y, by = by)
  })

  # return
  return(mydata)
}

#' Use timeAverage to calculate percentiles, with some custom naming
#' @noRd
make.percentile <- function(
  mydata,
  pollutant,
  avg.time,
  percentile,
  type,
  data.thresh,
  start.date,
  end.date,
  prefix
) {
  # summarise percentiles with timeAverage
  mydata <- timeAverage(
    mydata,
    avg.time,
    statistic = "percentile",
    percentile = percentile,
    type = type,
    data.thresh = data.thresh,
    start.date = start.date,
    end.date = end.date,
    progress = FALSE
  )

  # change output column name
  new.name <- paste0(prefix, percentile)
  names(mydata)[names(mydata) == pollutant] <- new.name

  # columns to return
  cols <- c("date", type[type != "default"], new.name)

  # construct new dataframe with just new column & date
  results <- mydata[, cols]

  # return
  return(results)
}
