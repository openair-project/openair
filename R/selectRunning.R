#' Function to extract run lengths greater than a threshold
#'
#' This is a utility function to extract runs of values above a certain
#' threshold. For example, for a data frame of hourly NOx values we would like
#' to extract all those hours where the concentration is at least 500 for
#' contiguous periods of 5 or more hours.
#'
#' This function is useful, for example, for selecting pollution episodes from a
#' data frame where concentrations remain elevated for a certain period of time.
#' It may also be of more general use when analysing air pollution and
#' atmospheric composition data. For example, [selectRunning()] could be used to
#' extract continuous periods of rainfall --- which could be important for
#' particle concentrations.
#'
#' @param mydata A data frame with a `date` field and at least one numeric
#'   `pollutant` field to analyse.
#' @param pollutant Name of variable to process.
#' @param criterion Condition to select run lengths e.g. `">"` with select data
#'   more than `threshold`.
#' @param run.len Run length for extracting contiguous values of `pollutant`
#'   meeting the `criterion` in relation to the `threshold`.
#' @param threshold The threshold value for `pollutant` above which data should
#'   be extracted.
#' @param type Used for splitting the data further. Passed to [cutData()].
#' @param name The name of the column to be appended to the data frame when
#'   `mode = "flag"`.
#' @param result A vector of length 2, defining how to label the run lengths
#'   when `mode = "flag"`. The first object should be the label for the `TRUE`
#'   label, and the second the `FALSE` label - e.g., `c("yes", "no")`.
#' @param mode Changes how the function behaves. When `mode = "flag"`, the
#'   default, the function appends a column flagging where the criteria was met.
#'   Alternatively, `"filter"` will filter `mydata` to only return rows where
#'   the criteria was met.
#' @param ... Additional parameters passed to [cutData()]. For use with `type`.
#' @export
#' @return A data frame
#' @author David Carslaw
#' @examples
#' # extract those hours where there are at least 5 consecutive NOx
#' # concentrations above 500 units
#' mydata <- selectRunning(mydata, run.len = 5, threshold = 500)
#'
#' # make a polar plot of those conditions, which shows that those
#' # conditions are dominated by low wind speeds, not
#' # in-canyon recirculation
#' \dontrun{
#' polarPlot(mydata, pollutant = "nox", type = "criterion")
#' }
selectRunning <- function(
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
) {
  # check inputs are valid
  mode <- rlang::arg_match(mode)
  criterion <- rlang::arg_match(
    criterion,
    c("<", ">", "<=", ">=", "==", "!="),
    multiple = FALSE
  )

  # construct expression
  expr <- paste(pollutant, criterion, threshold)

  # handle type
  mydata <- cutData(mydata, type = type, ...)

  # check for multiple sites (for example)
  checkDuplicateRows(mydata, type)
  
  # pad out missing data
  thedata <- date.pad(mydata, type = type)

  # save input for later
  mydata <- thedata

  # check input data - ensures `date` are in correct order
  vars <- unique(c("date", names(mydata)))
  thedata <- checkPrep(
    mydata,
    Names = vars,
    type = type,
    remove.calm = FALSE
  )

  # calculate run lengths
  thedata <-
    thedata %>%
    # create flags of the criterion, and work out run length
    dplyr::mutate(
      `__flag__` = rlang::eval_tidy(rlang::parse_expr(expr)),
      `__run__` = dplyr::consecutive_id(.data[["__flag__"]]),
      .by = dplyr::all_of(type)
    ) %>%
    # ensure runs are unique per type
    dplyr::mutate(`__run__` = paste(.data[["__run__"]], .data[[type]])) %>%
    # count length of runs
    dplyr::mutate(`__len__` = dplyr::n(), .by = "__run__") %>%
    # check if run length is greather than run.len for positive flags
    dplyr::mutate(
      `__flag__` = dplyr::if_else(
        condition = .data[["__flag__"]] &
          .data[["__len__"]] >= run.len,
        true = TRUE,
        false = FALSE,
        missing = FALSE
      )
    )

  # format outputs
  if (mode == "filter") {
    mydata <- mydata[thedata$`__flag__`, ]
  }

  if (mode == "flag") {
    mydata[[name]] <- thedata$`__flag__`
    mydata[[name]] <- ifelse(mydata[[name]], result[1], result[2])
  }

  if (type == "default") {
    mydata$default <- NULL
  }

  # return
  return(mydata)
}
