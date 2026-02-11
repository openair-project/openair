#' Rolling regression for pollutant source characterisation.
#'
#' This function calculates rolling regressions for input data with a set window
#' width. The principal use of the function is to identify "dilution lines"
#' where the ratio between two pollutant concentrations is invariant. The
#' original idea is based on the work of Bentley (2004).
#'
#' The intended use is to apply the approach to air pollution data to extract
#' consecutive points in time where the ratio between two pollutant
#' concentrations changes by very little. By filtering the output for high R2
#' values (typically more than 0.90 to 0.95), conditions where local source
#' dilution is dominant can be isolated for post processing. The function is
#' more fully described and used in the `openair` online manual, together
#' with examples.
#'
#' @param mydata A data frame with  columns for `date` and at least two
#'   variables for use in a regression.
#' @param x The column name of the `x` variable for use in a linear
#'   regression `y = m.x + c`.
#' @param y The column name of the `y` variable for use in a linear
#'   regression `y = m.x + c`.
#' @param run.len The window width to be used for a rolling regression. A value
#'   of 3 for example for hourly data will consider 3 one-hour time sequences.
#' @param date.pad Should gaps in time series be filled before calculations are
#'   made?
#' @return A tibble with `date` and calculated regression coefficients and
#'   other information to plot dilution lines.
#' @importFrom stats coefficients
#' @importFrom stats .lm.fit
#' @export
#' @references
#'
#'
#' For original inspiration:
#'
#' Bentley, S. T. (2004). Graphical techniques for constraining estimates of
#' aerosol emissions from motor vehicles using air monitoring network data.
#' Atmospheric Environment,(10), 1491–1500.
#' https://doi.org/10.1016/j.atmosenv.2003.11.033
#'
#' Example for vehicle emissions high time resolution data:
#'
#' Farren, N. J., Schmidt, C., Juchem, H., Pöhler, D., Wilde, S. E., Wagner, R.
#' L., Wilson, S., Shaw, M. D., & Carslaw, D. C. (2023). Emission ratio
#' determination from road vehicles using a range of remote emission sensing
#' techniques. Science of The Total Environment, 875.
#' https://doi.org/10.1016/j.scitotenv.2023.162621.
#'
#' @examples
#' # Just use part of a year of data
#' output <- runRegression(selectByDate(mydata, year = 2004, month = 1:3),
#'   x = "nox", y = "pm10", run.len = 3
#' )
#'
#' output
runRegression <- function(
  mydata,
  x = "nox",
  y = "pm10",
  run.len = 3,
  date.pad = TRUE
) {
  # 1. Preparation
  vars <- c("date", x, y)
  mydata <- checkPrep(mydata, vars, type = "default")

  if (date.pad) {
    mydata <- datePad(mydata)
  }

  # Extract vectors for cleaner code
  x_vec <- mydata[[x]]
  y_vec <- mydata[[y]]
  d_vec <- mydata$date
  n <- length(x_vec)

  # 2. Vectorized Rolling Sums (using stats::filter)
  # We create a kernel of 1s to sum 'run.len' items
  K <- rep(1, run.len)

  # sides=1 calculates a rolling window looking backwards (convolution)
  # Result aligns with the END of the window
  sum_x <- as.vector(stats::filter(x_vec, K, sides = 1))
  sum_y <- as.vector(stats::filter(y_vec, K, sides = 1))
  sum_xx <- as.vector(stats::filter(x_vec^2, K, sides = 1))
  sum_yy <- as.vector(stats::filter(y_vec^2, K, sides = 1))
  sum_xy <- as.vector(stats::filter(x_vec * y_vec, K, sides = 1))

  # 3. Calculate Regression Statistics
  N <- run.len

  # Calculate scaled variances/covariances (N * Var) to avoid early division
  # Sxx_N = N * Sum(x^2) - (Sum(x))^2
  Sxx_N <- (N * sum_xx) - (sum_x^2)
  Syy_N <- (N * sum_yy) - (sum_y^2)
  Sxy_N <- (N * sum_xy) - (sum_x * sum_y)

  # Slope (beta) = Sxy / Sxx
  slope <- Sxy_N / Sxx_N

  # Intercept (alpha) = (Sum(y) - beta * Sum(x)) / N
  intercept <- (sum_y - slope * sum_x) / N

  # R-squared = (Sxy)^2 / (Sxx * Syy)
  r_squared <- (Sxy_N^2) / (Sxx_N * Syy_N)

  # Handle special case: Perfect fit or Constant Y (Variance ~ 0)
  # Matches original logic where low residuals implies R2=1
  # If Syy is effectively 0, it means y is constant, so perfect fit.
  r_squared[is.na(r_squared) & abs(Syy_N) < 1e-9] <- 1

  # 4. Calculate Rolling Min/Max (for x1, x2) using 'embed'
  # embed creates a matrix where each row is a window of length run.len
  # e.g., row 10 is: x[10], x[9], x[8]
  x_mat <- stats::embed(x_vec, run.len)

  # Use do.call + pmin/pmax to find row-wise min/max efficiently
  # We split the matrix columns into a list for pmin/pmax arguments
  x_mat_cols <- split(x_mat, col(x_mat))
  x1_vec <- do.call(pmin, x_mat_cols)
  x2_vec <- do.call(pmax, x_mat_cols)

  # 5. Align Dates and Subset Valid Results
  # 'filter' (sides=1) puts results at the index of the last element.
  # 'embed' result has (n - run.len + 1) rows, corresponding to indices run.len:n

  valid_indices <- run.len:n

  # Calculate start/end dates
  d_end <- d_vec[valid_indices]
  d_start <- d_vec[valid_indices - run.len + 1]

  # Calculate median date strictly (midpoint between start and end)
  # This handles even/odd run.len correctly and matches the 'center' of the window
  d_median <- d_start + (d_end - d_start) / 2

  # 6. Construct Final Table
  # We subset the regression vectors using valid_indices to match the 'embed' size
  results <- tibble::tibble(
    date = d_median,
    date_start = d_start,
    date_end = d_end,
    intercept = intercept[valid_indices],
    slope = slope[valid_indices],
    r_squared = r_squared[valid_indices],
    x1 = x1_vec,
    x2 = x2_vec
  )

  # Remove rows that had any NAs in input (calculated stats will be NA)
  results <- results[complete.cases(results), ]

  # 7. Final Coordinates (Delta/Line Points)
  results <- results |>
    dplyr::mutate(
      y1 = slope * x1 + intercept,
      y2 = slope * x2 + intercept,
      delta_x = x2 - x1,
      delta_y = y2 - y1
    )

  # Dynamic renaming to match original output
  names(results)[names(results) == "x1"] <- paste0(x, "_1")
  names(results)[names(results) == "x2"] <- paste0(x, "_2")
  names(results)[names(results) == "y1"] <- paste0(y, "_1")
  names(results)[names(results) == "y2"] <- paste0(y, "_2")
  names(results)[names(results) == "delta_x"] <- paste0("delta_", x)
  names(results)[names(results) == "delta_y"] <- paste0("delta_", y)

  return(results)
}
