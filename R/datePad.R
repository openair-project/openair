#' Pad a time-series dataframe and optionally fill values by block
#'
#' Expand a dataframe that contains a 'date' column to a regular sequence of
#' timestamps between specified start and end dates. The function can operate in
#' two modes:
#' - `fill = FALSE`: simply complete the sequence at the target interval.
#' - `fill = TRUE`: regularise the data at the native interval to create
#' explicit blocks, then expand to the target interval and carry the block's
#' values forward so that intra-block timestamps inherit the block's measured
#' value (block-filling behaviour).
#'
#' The function detects the native input interval automatically if `interval` is
#' not supplied, supports grouping via `type`, and preserves timezones for
#' POSIXt date columns.
#'
#' @param mydata Data.frame or tibble containing at least a 'date' column (Date
#'   or POSIXt).
#'
#' @param type `NULL` or character vector of column names to group by.
#'
#' @param interval `NULL` or character string describing target interval (e.g.
#'   "1 min", "1 hour"). If `NULL`, the native interval is used.
#'
#' @param start.date Optional start date/time. If `NULL`, the group's minimum
#'   date is used.
#'
#' @param end.date Optional end date/time. If `NULL`, the group's maximum date
#'   is used.
#'
#' @param fill Logical; when `TRUE` performs block-based filling described
#'   above. When `FALSE` just completes the sequence leaving `NA` values.
#'
#' @param print.int Logical; when `TRUE` prints detected/selected interval
#'   messages.
#'
#' @param ... Passed to [cutData()] for use with `type`.
#'
#' @return A dataframe expanded to the requested sequence with values filled
#'   according to 'fill'. The returned object preserves the 'date' column type
#'   and timezone (for POSIXt).
#'
#' @examples
#' df <- mydata[-c(2, 4, 7), ] # Remove some rows to create gaps
#' datePad(df)
#'
#' @export
datePad <- function(
  mydata,
  type = NULL,
  interval = NULL,
  start.date = NULL,
  end.date = NULL,
  fill = FALSE,
  print.int = FALSE,
  ...
) {
  # Basic validation
  if (nrow(mydata) < 2) {
    return(mydata)
  }
  if (!"date" %in% names(mydata)) {
    stop("Dataframe must contain a 'date' column.")
  }

  # 1. Detect Native Interval (The resolution of the INPUT data)
  # We need this to correctly establish the "blocks" of time before expanding
  native_interval <- find_time_interval(mydata$date)

  # 2. Determine Target Interval
  if (!is.null(interval)) {
    target_interval <- interval
    if (print.int) message("Target interval (User): ", target_interval)
  } else {
    target_interval <- native_interval
    if (print.int) message("Target interval (Auto): ", target_interval)
  }

  # 3. Handle Timezones/Dates
  tz_str <- attr(mydata$date, "tzone") %||% "GMT" # Helper if NULL

  align_date <- function(input, ref, tz) {
    if (is.null(input)) {
      return(NULL)
    }
    if (inherits(ref, "POSIXt")) {
      return(as.POSIXct(input, tz = tz))
    }
    if (inherits(ref, "Date")) {
      return(as.Date(input))
    }
    input
  }

  start.date <- align_date(start.date, mydata$date, tz_str)
  end.date <- align_date(end.date, mydata$date, tz_str)

  # -----------------------------------------------------------------------
  # Core Logic Helper
  # -----------------------------------------------------------------------
  process_group <- function(df) {
    check_duplicate_rows(df, type, fn = cli::cli_abort)

    # A. Define limits for this group
    s_date <- if (is.null(start.date)) {
      min(df$date, na.rm = TRUE)
    } else {
      start.date
    }
    e_date <- if (is.null(end.date)) max(df$date, na.rm = TRUE) else end.date

    # B. If fill=TRUE, we must strictly respect the 'Native' blocks.
    #    We first pad to the NATIVE interval to materialize missing NAs.
    if (fill) {
      # 1. Regularize at NATIVE resolution (creates explicit NAs for gaps)
      df_native <- df |>
        tidyr::complete(date = seq(s_date, e_date, by = native_interval)) |>
        dplyr::mutate(.block_id = dplyr::row_number()) # Unique ID for every native step

      # 2. Expand to TARGET resolution
      df_expanded <- df_native |>
        dplyr::select("date", ".block_id") |> # Keep only ID and Date for structure
        tidyr::complete(date = seq(s_date, e_date, by = target_interval)) |>
        tidyr::fill(".block_id", .direction = "down") # Carry the ID down (ID 1 covers 10:00, 10:15...)

      # 3. Join original values back using the Block ID
      #    This ensures 10:15 gets 10:00's value (5), and 11:15 gets 11:00's value (NA)
      df_out <- df_expanded |>
        dplyr::left_join(
          dplyr::select(df_native, -"date"),
          by = ".block_id"
        ) |>
        dplyr::select(-".block_id")

      return(df_out)
    } else {
      # C. Simple case (No fill) - Just expand
      df |>
        tidyr::complete(date = seq(s_date, e_date, by = target_interval))
    }
  }

  # 4. Execution

  mydata <- cutData(mydata, type = type, ...)

  out <- map_type(
    mydata,
    type = type,
    fun = process_group,
    .include_default = FALSE
  )

  # 5. Restore Timezone
  if (inherits(mydata$date, "POSIXt")) {
    attr(out$date, "tzone") <- tz_str
  }

  return(out)
}
