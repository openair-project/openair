#' Calculate summary statistics for air pollution data by year
#'
#' This function calculates a range of common and air pollution-specific
#' statistics from a data frame. The statistics are calculated on an annual
#' basis and the input is assumed to be hourly data. The function can cope with
#' several sites and years, e.g., using `type = "site"`. The user can control
#' the output by setting `transpose` appropriately. Note that the input data is
#' assumed to be in mass units, e.g., ug/m3 for all species except CO (mg/m3).
#'
#' The following statistics are calculated:
#'
#' For all pollutants:
#'
#' - **data.capture** --- percentage data capture over a full year.
#'
#' - **mean** --- annual mean.
#'
#' - **minimum** --- minimum hourly value.
#'
#' - **maximum** --- maximum hourly value.
#'
#' - **median** --- median value.
#'
#' - **max.daily** --- maximum daily mean.
#'
#' - **max.rolling.8** --- maximum 8-hour rolling mean.
#'
#' - **max.rolling.24** --- maximum 24-hour rolling mean.
#'
#' - **percentile.95** --- 95th percentile. Note that several percentiles
#' can be calculated.
#'
#' When `pollutant == "o3"`:
#'
#' - **roll.8.O3.gt.100** --- number of days when the daily maximum
#' rolling 8-hour mean ozone concentration is >100 ug/m3. This is the target
#' value.
#'
#' - **roll.8.O3.gt.120** --- number of days when the daily maximum
#' rolling 8-hour mean ozone concentration is >120 ug/m3. This is the Limit
#' Value not to be exceeded > 10 days a year.
#'
#' - **AOT40** --- is the accumulated amount of ozone over the threshold
#' value of 40 ppb for daylight hours in the growing season (April to
#' September). Note that `latitude` and `longitude` can also be passed to this
#' calculation.
#'
#' When `pollutant == "no2"`:
#'
#' - **hours** --- number of hours NO2 is more than 200 ug/m3.
#'
#' When `pollutant == "pm10"`:
#'
#' - **days** --- number of days PM10 is more than 50 ug/m3.
#'
#' For the rolling means, the user can supply the option `align`, which can be
#' "centre" (default), "left" or "right". See [rollingMean()] for more details.
#'
#' There can be small discrepancies with the AURN due to the treatment of
#' rounding data. The [aqStats()] function does not round, whereas AURN data can
#' be rounded at several stages during the calculations.
#'
#' @inheritParams timeAverage
#' @param mydata A data frame containing a `date` field of hourly data.
#' @param pollutant The name of a pollutant e.g. `pollutant = c("o3", "pm10")`.
#'   Additional statistics will be calculated if `pollutant %in% c("no2",
#'   "pm10", "o3")`.
#' @param percentile Percentile values to calculate for each pollutant.
#' @param transpose The default is to return a data frame with columns
#'   representing the statistics. If `transpose = TRUE` then the results have
#'   columns for each pollutant-type combination.
#' @param ... Other arguments, currently unused.
#' @export
#' @author David Carslaw
#' @examples
#'
#' # Statistics for 2004. NOTE! these data are in ppb/ppm so the
#' # example is for illustrative purposes only
#' aqStats(selectByDate(mydata, year = 2004), pollutant = "no2")
aqStats <- function(
  mydata,
  pollutant = "no2",
  type = "default",
  data.thresh = 0,
  percentile = c(95, 99),
  transpose = FALSE,
  progress = TRUE,
  ...
) {
  # variables we need
  vars <- c("date", pollutant, type)

  # cut data by type
  mydata <- cutData(mydata, type)

  # check for duplicate dates
  checkDuplicateRows(mydata, type, fn = cli::cli_abort)

  # check we have the variables
  mydata <- checkPrep(
    mydata,
    vars,
    "default",
    remove.calm = FALSE,
    strip.white = FALSE
  )

  # reorganise data
  mydata <-
    mydata |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(pollutant),
      names_to = "pollutant"
    ) |>
    dplyr::mutate(year = lubridate::year(date))

  vars <- c(type, "pollutant", "year")

  # calculate the statistics
  results <-
    mapType(
      mydata,
      type = vars,
      fun = \(df) {
        rlang::exec(
          calcStats,
          !!!rlang::list2(
            mydata = df,
            data.thresh = data.thresh,
            percentile = percentile,
            ...
          )
        )
      },
      .include_default = TRUE,
      .progress = progress
    )

  # transpose if requested
  if (transpose) {
    unite_vars <- c(type[type != "default"], "pollutant")

    results <-
      results |>
      tidyr::pivot_longer(-dplyr::all_of(c(vars, "date"))) |>
      tidyr::unite(site_pol, dplyr::all_of(unite_vars)) |>
      tidyr::pivot_wider(names_from = "site_pol") |>
      dplyr::rename_with(function(x) {
        gsub("_", " ", x)
      })
  }

  # return
  return(results)
}

# function to calculate statistics
calcStats <- function(mydata, data.thresh, percentile, ...) {
  # fill any missing hours
  start.date <- lubridate::floor_date(min(mydata$date), "year")
  end.date <- lubridate::ceiling_date(max(mydata$date), "year") - 3600

  # find time interval of data and pad any missing times
  interval <- find.time.interval(mydata$date)

  mydata <- datePad(
    mydata,
    start.date = start.date,
    end.date = end.date,
    interval = interval
  )

  # Ensure year is present after padding
  mydata$year <- lubridate::year(mydata$date)

  # --- 1. Main Annual Statistics (Mean, Min, Max, Median, Data Capture, Percentiles) ---
  # We calculate these in one pass using dplyr rather than calling timeAverage repeatedly

  # Helper to format percentile names
  p_names <- paste0("percentile.", percentile)

  main_stats <- mydata |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      # Representative date for the year (start of year)
      date = min(date, na.rm = TRUE),

      # Data Capture
      dat.cap = sum(!is.na(value)) / dplyr::n() * 100,

      # Basic Stats
      mean = mean(value, na.rm = TRUE),
      min = suppressWarnings(min(value, na.rm = TRUE)),
      max = suppressWarnings(max(value, na.rm = TRUE)),
      median = median(value, na.rm = TRUE),

      # Percentiles (returns a dataframe column we unpack later)
      pct_vals = list(quantile(
        value,
        probs = percentile / 100,
        na.rm = TRUE,
        type = 7
      )),

      # Pollutant Specific: NO2 Hours > 200
      hours = if (grepl("no2", .data$pollutant[1], ignore.case = TRUE)) {
        sum(value > 200, na.rm = TRUE)
      } else {
        NA_real_
      },

      .groups = "drop"
    ) |>
    tidyr::unnest_wider(pct_vals, names_sep = "") |>
    dplyr::rename_with(~p_names, dplyr::starts_with("pct_vals"))

  # --- 2. Rolling Means (8h and 24h) ---
  # Calculate on the whole dataset ONCE, then summarize by year.
  # This avoids splitting data into thousands of yearly chunks.

  # Calculate 8-hour rolling
  mydata_roll8 <- rollingMean(
    mydata,
    pollutant = "value",
    width = 8,
    new.name = "roll8",
    data.thresh = data.thresh,
    ...
  )

  # Calculate 24-hour rolling
  mydata_roll24 <- rollingMean(
    mydata,
    pollutant = "value",
    width = 24,
    new.name = "roll24",
    data.thresh = data.thresh,
    ...
  )

  # Aggregating rolling maxes
  roll_stats <- dplyr::tibble(
    year = main_stats$year,
    roll8_val = mydata_roll8$roll8,
    roll24_val = mydata_roll24$roll24
  ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      roll_8_max = suppressWarnings(max(roll8_val, na.rm = TRUE)),
      roll_24_max = suppressWarnings(max(roll24_val, na.rm = TRUE)),
      .groups = "drop"
    )

  # --- 3. Maximum Daily Mean ---
  # We use timeAverage for daily means to respect data thresholds logic per day
  daily_stats <- timeAverage(
    mydata,
    avg.time = "day",
    statistic = "mean",
    data.thresh = data.thresh,
    print.int = FALSE
  )

  max_daily <- daily_stats |>
    dplyr::mutate(year = lubridate::year(date)) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      max_daily = suppressWarnings(max(value, na.rm = TRUE)),

      # Pollutant Specific: PM10 Days > 50
      days = if (
        length(grep("pm10", mydata$pollutant[1], ignore.case = TRUE)) == 1
      ) {
        sum(value > 50, na.rm = TRUE)
      } else {
        NA_real_
      },
      .groups = "drop"
    )

  # --- 4. Ozone Specifics (AOT40 and Rolling Targets) ---
  o3_stats <- NULL
  if (grepl("o3", mydata$pollutant[1], ignore.case = TRUE)) {
    # Rolling Targets (>100, >120) based on DAILY MAX of 8-hour rolling
    # We reuse the mydata_roll8 calculated in Step 2
    o3_rolling_targets <- mydata_roll8 |>
      dplyr::mutate(day = lubridate::floor_date(date, "day")) |>
      dplyr::group_by(year, day) |>
      dplyr::summarise(
        max_roll8 = suppressWarnings(max(roll8, na.rm = TRUE)),
        .groups = "drop_last"
      ) |>
      dplyr::summarise(
        roll.8.O3.gt.100 = sum(max_roll8 > 100, na.rm = TRUE),
        roll.8.O3.gt.120 = sum(max_roll8 > 120, na.rm = TRUE),
        .groups = "drop"
      )

    # AOT40
    # Filter for growing season (Apr-Sept)
    mydata_aot <- selectByDate(mydata, month = 4:9)

    if (nrow(mydata_aot) > 0) {
      # cutData can be expensive, only run on filtered subset
      mydata_aot <- cutData(mydata_aot, "daylight", ...)

      aot_calc <- mydata_aot |>
        dplyr::filter(daylight == "daylight") |>
        dplyr::group_by(year) |>
        dplyr::summarise(
          AOT40 = sum(pmax(value - 80, 0), na.rm = TRUE) * 0.50, # 0.5 conversion for ppb
          .groups = "drop"
        )

      o3_stats <- dplyr::full_join(o3_rolling_targets, aot_calc, by = "year")
    } else {
      o3_stats <- o3_rolling_targets
    }
  }

  # --- 5. Merge and Apply Thresholds ---

  # Combine all frames
  final_df <- main_stats |>
    dplyr::left_join(max_daily, by = "year") |>
    dplyr::left_join(roll_stats, by = "year")

  if (!is.null(o3_stats)) {
    final_df <- dplyr::left_join(final_df, o3_stats, by = "year")
  }

  # Drop temp columns if pollutant didn't match (clean up NA columns for strict output match)
  # (Optional cleanup based on original structure, but keys are handled via logic above)
  if (!grepl("no2", mydata$pollutant[1], ignore.case = TRUE)) {
    final_df$hours <- NULL
  }
  if (!length(grep("pm10", mydata$pollutant[1], ignore.case = TRUE)) == 1) {
    final_df$days <- NULL
  }

  # Apply Data Threshold to Summary Stats
  # If data capture is below threshold, set stats to NA (except data capture itself)
  # Note: Count-based stats (hours, days, exceedances) are typically not nulled by capture in AQ stats
  # unless strictly specified, but here we follow general pattern: if data is insufficient, mean is invalid.

  cols_to_mask <- setdiff(
    names(final_df),
    c(
      "year",
      "date",
      "dat.cap",
      "hours",
      "days",
      "roll.8.O3.gt.100",
      "roll.8.O3.gt.120",
      "AOT40"
    )
  )

  final_df <- final_df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(cols_to_mask),
        ~ if_else(dat.cap < data.thresh, NA_real_, .)
      )
    )

  return(final_df)
}
