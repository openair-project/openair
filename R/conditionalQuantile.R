#' Conditional quantile estimates for model evaluation
#'
#' Function to calculate conditional quantiles with flexible conditioning. The
#' function is for use in model evaluation and more generally to help better
#' understand forecast predictions and how well they agree with observations.
#'
#' Conditional quantiles are a very useful way of considering model performance
#' against observations for continuous measurements (Wilks, 2005). The
#' conditional quantile plot splits the data into evenly spaced bins. For each
#' predicted value bin e.g. from 0 to 10~ppb the *corresponding* values of the
#' observations are identified and the median, 25/75th and 10/90 percentile
#' (quantile) calculated for that bin. The data are plotted to show how these
#' values vary across all bins. For a time series of observations and
#' predictions that agree precisely the median value of the predictions will
#' equal that for the observations for each bin.
#'
#' The conditional quantile plot differs from the quantile-quantile plot (Q-Q
#' plot) that is often used to compare observations and predictions. A Q-Q~plot
#' separately considers the distributions of observations and predictions,
#' whereas the conditional quantile uses the corresponding observations for a
#' particular interval in the predictions. Take as an example two time series,
#' the first a series of real observations and the second a lagged time series
#' of the same observations representing the predictions. These two time series
#' will have identical (or very nearly identical) distributions (e.g. same
#' median, minimum and maximum). A Q-Q plot would show a straight line showing
#' perfect agreement, whereas the conditional quantile will not. This is because
#' in any interval of the predictions the corresponding observations now have
#' different values.
#'
#' Plotting the data in this way shows how well predictions agree with
#' observations and can help reveal many useful characteristics of how well
#' model predictions agree with observations --- across the full distribution of
#' values. A single plot can therefore convey a considerable amount of
#' information concerning model performance. The `conditionalQuantile` function
#' in openair allows conditional quantiles to be considered in a flexible way
#' e.g. by considering how they vary by season.
#'
#' The function requires a data frame consisting of a column of observations and
#' a column of predictions. The observations are split up into `bins` according
#' to values of the predictions. The median prediction line together with the
#' 25/75th and 10/90th quantile values are plotted together with a line showing
#' a \dQuote{perfect} model. Also shown is a histogram of predicted values
#' (shaded grey) and a histogram of observed values (shown as a blue outline).
#'
#' Far more insight can be gained into model performance through conditioning
#' using `type`. For example, `type = "season"` will plot conditional quantiles
#' by each season. `type` can also be a factor or character field e.g.
#' representing different models used.
#'
#' See Wilks (2005) for more details and the examples below.
#'
#' @inheritParams shared_openair_params
#'
#' @param mydata A data frame containing the field `obs` and `mod` representing
#'   observed and modelled values.
#'
#' @param obs The name of the observations in `mydata`.
#'
#' @param mod The name of the predictions (modelled values) in `mydata`.
#'
#' @param bins Number of bins to be used in calculating the different quantile
#'   levels.
#'
#' @param min.bin The minimum number of points required for the estimates of the
#'   25/75th and 10/90th percentiles.
#'
#' @param ... Addition options are passed on to [cutData()] for `type` handling.
#'   Some additional arguments are also available:
#'   - `xlab`, `ylab` and `main` override the x-axis label, y-axis label, and plot title.
#'   - `layout` sets the layout of facets - e.g., `layout(2, 5)` will have 2 columns and 5 rows.
#'   - `fontsize` overrides the overall font size of the plot.
#'
#' @export
#'
#' @author David Carslaw
#' @author Jack Davison
#'
#' @family model evaluation functions
#' @seealso The `verification` package for comprehensive functions for forecast
#'   verification.
#'
#' @references
#'
#' Murphy, A. H., B.G. Brown and Y. Chen. (1989) Diagnostic Verification of
#' Temperature Forecasts, Weather and Forecasting, Volume: 4, Issue: 4, Pages:
#' 485-501.
#'
#' Wilks, D. S., 2005. Statistical Methods in the Atmospheric Sciences, Volume
#' 91, Second Edition (International Geophysics), 2nd Edition. Academic Press.
#' @examples
#' # make some dummy prediction data based on 'nox'
#' mydata$mod <- mydata$nox * 1.1 + mydata$nox * runif(seq_len(nrow(mydata)))
#'
#' # basic conditional quantile plot
#' # A "perfect" model is shown by the blue line
#' # predictions tend to be increasingly positively biased at high nox,
#' # shown by departure of median line from the blue one.
#' # The widening uncertainty bands with increasing NOx shows that
#' # hourly predictions are worse for higher NOx concentrations.
#' # Also, the red (median) line extends beyond the data (blue line),
#' # which shows in this case some predictions are much higher than
#' # the corresponding measurements. Note that the uncertainty bands
#' # do not extend as far as the median line because there is insufficient
#' # to calculate them
#' conditionalQuantile(mydata, obs = "nox", mod = "mod")
#'
#' # can split by season to show seasonal performance (not very
#' # enlightening in this case - try some real data and it will be!)
#'
#' \dontrun{
#' conditionalQuantile(mydata, obs = "nox", mod = "mod", type = "season")
#' }
conditionalQuantile <- function(
  mydata,
  obs = "obs",
  mod = "mod",
  type = "default",
  bins = 31,
  min.bin = c(10, 20),
  cols = "YlOrRd",
  key.columns = 2,
  key.position = "bottom",
  strip.position = "top",
  auto.text = TRUE,
  plot = TRUE,
  key = NULL,
  ...
) {
  # check key.position
  key.position <- check_key_position(key.position, key)

  if (length(type) > 2) {
    cli::cli_abort("Only two types can be used with this function")
  }

  extra.args <- rlang::list2(...)

  vars <- c(mod, obs)
  if (any(type %in% dateTypes)) {
    vars <- c("date", vars)
  }

  mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)
  mydata <- stats::na.omit(mydata)
  mydata <- cutData(mydata, type)

  lo <- min(mydata[c(mod, obs)])
  hi <- max(mydata[c(mod, obs)])
  bins_breaks <- seq(floor(lo), ceiling(hi), length = bins)
  bin_width <- diff(bins_breaks)[1]
  labs <- bins_breaks[-length(bins_breaks)] + 0.5 * bin_width

  if (length(cols) == 1 && cols == "greyscale") {
    ideal.col <- "black"
    col.1 <- grDevices::grey(0.75)
    col.2 <- grDevices::grey(0.5)
    col.5 <- grDevices::grey(0.25)
  } else {
    cols <- openair::openColours(cols, n = 3L)
    ideal.col <- "#0080ff"
    col.1 <- cols[1]
    col.2 <- cols[2]
    col.5 <- cols[3]
  }

  # per-group statistics
  compute_cq <- function(df) {
    obs_vec <- df[[obs]]
    pred_vec <- df[[mod]]
    pred_cut <- cut(
      pred_vec,
      breaks = bins_breaks,
      include.lowest = TRUE,
      labels = labs
    )
    n <- as.integer(tapply(obs_vec, pred_cut, length))
    n[is.na(n)] <- 0L

    safe_stat <- function(FUN, probs = NULL, min_n = 0L) {
      q <- tapply(obs_vec, pred_cut, function(x) {
        if (!length(x)) {
          return(NA_real_)
        }
        if (is.null(probs)) {
          FUN(x, na.rm = TRUE)
        } else {
          FUN(x, probs = probs, na.rm = TRUE)
        }
      })
      q <- as.vector(q)
      if (min_n > 0L) {
        q[n <= min_n] <- NA_real_
      }
      q
    }

    list(
      quant = data.frame(
        x = labs,
        med = safe_stat(stats::median),
        q1 = safe_stat(stats::quantile, probs = 0.25, min_n = min.bin[1]),
        q2 = safe_stat(stats::quantile, probs = 0.75, min_n = min.bin[1]),
        q3 = safe_stat(stats::quantile, probs = 0.10, min_n = min.bin[2]),
        q4 = safe_stat(stats::quantile, probs = 0.90, min_n = min.bin[2])
      ),
      hist = data.frame(
        x = rep(labs, 2),
        count = c(
          as.integer(table(cut(
            pred_vec,
            bins_breaks,
            include.lowest = TRUE,
            labels = labs
          ))),
          as.integer(table(cut(
            obs_vec,
            bins_breaks,
            include.lowest = TRUE,
            labels = labs
          )))
        ),
        hist_type = rep(c("predicted", "observed"), each = length(labs))
      ),
      obs_range = data.frame(obs_min = min(obs_vec), obs_max = max(obs_vec))
    )
  }

  all_res <- mydata |>
    dplyr::group_by(dplyr::across(dplyr::all_of(type))) |>
    dplyr::group_nest() |>
    dplyr::mutate(cq = purrr::map(.data$data, compute_cq), .keep = "unused")

  results <- all_res |>
    dplyr::mutate(d = purrr::map(.data$cq, "quant")) |>
    tidyr::unnest(cols = "d") |>
    dplyr::select(-"cq")

  hist_data <- all_res |>
    dplyr::mutate(d = purrr::map(.data$cq, "hist")) |>
    tidyr::unnest(cols = "d") |>
    dplyr::select(-"cq")

  obs_range_data <- all_res |>
    dplyr::mutate(d = purrr::map(.data$cq, "obs_range")) |>
    tidyr::unnest(cols = "d") |>
    dplyr::select(-"cq")

  # histogram scaling for secondary y-axis
  hist_range <-
    hist_data |>
    dplyr::filter(.data$hist_type == "predicted") |>
    dplyr::pull("count") |>
    (\(x) c(0, x))() |>
    range()

  # scale factor to convert histogram counts to the same scale as the
  # observed/predicted values
  hist_data$count_normalised <- scales::rescale(
    hist_data$count,
    from = hist_range,
    to = c(min(obs_range_data$obs_min), max(obs_range_data$obs_max))
  )

  hist_pred <- hist_data[hist_data$hist_type == "predicted", ]
  hist_obs <- hist_data[hist_data$hist_type == "observed", ]

  key_guide <- ggplot2::guide_legend(
    ncol = key.columns
  )

  # build plot
  plt <- ggplot2::ggplot(results, ggplot2::aes(x = .data$x)) +
    # histograms (drawn first, behind ribbons)
    ggplot2::geom_rect(
      data = hist_pred,
      ggplot2::aes(
        xmin = .data$x - bin_width / 2,
        xmax = .data$x + bin_width / 2,
        ymin = 0,
        ymax = .data$count_normalised
      ),
      fill = "black",
      alpha = 0.15,
      color = NA,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_rect(
      data = hist_obs,
      ggplot2::aes(
        xmin = .data$x - bin_width / 2,
        xmax = .data$x + bin_width / 2,
        ymin = 0,
        ymax = .data$count_normalised
      ),
      fill = NA,
      color = ideal.col,
      linewidth = 0.3,
      inherit.aes = FALSE
    ) +
    # quantile ribbons
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = .data$q3,
        ymax = .data$q1,
        fill = "10/90th percentile"
      ),
      na.rm = TRUE,
      alpha = 2 / 3
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = .data$q2,
        ymax = .data$q4,
        fill = "10/90th percentile"
      ),
      na.rm = TRUE,
      alpha = 2 / 3
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = .data$q1,
        ymax = .data$q2,
        fill = "25/75th percentile"
      ),
      na.rm = TRUE,
      alpha = 2 / 3
    ) +
    # median and perfect-model lines
    ggplot2::geom_line(
      ggplot2::aes(y = .data$med, color = "median"),
      linewidth = 1,
      na.rm = TRUE
    ) +
    ggplot2::geom_segment(
      data = obs_range_data,
      ggplot2::aes(
        x = .data$obs_min,
        xend = .data$obs_max,
        y = .data$obs_min,
        yend = .data$obs_max,
        color = "perfect model"
      ),
      linewidth = 1,
      inherit.aes = FALSE
    ) +
    # scales
    ggplot2::scale_fill_manual(
      values = c(
        "25/75th percentile" = col.1,
        "10/90th percentile" = col.2,
        "median" = col.5,
        "perfect model" = ideal.col
      ),
      breaks = c(
        "25/75th percentile",
        "10/90th percentile",
        "median",
        "perfect model"
      ),
      aesthetics = c("colour", "fill")
    ) +
    ggplot2::guides(
      color = key_guide,
      fill = key_guide
    ) +
    ggplot2::scale_y_continuous(
      sec.axis = ggplot2::sec_axis(
        transform = \(x) {
          scales::rescale(
            x,
            from = c(min(obs_range_data$obs_min), max(obs_range_data$obs_max)),
            to = hist_range
          )
        },
        name = "sample size for histograms"
      )
    ) +
    ggplot2::coord_cartesian(
      ratio = 1,
      xlim = c(min(obs_range_data$obs_min), max(obs_range_data$obs_max)),
      ylim = c(min(obs_range_data$obs_min), max(obs_range_data$obs_max))
    ) +
    get_facet(
      type,
      extra.args,
      "fixed",
      auto.text,
      strip.position = strip.position,
      wd.res = extra.args$wd.res %||% 8
    ) +
    theme_openair(key.position) +
    set_extra_fontsize(extra.args) +
    ggplot2::labs(
      color = NULL,
      fill = NULL,
      x = quickText(extra.args$xlab %||% "predicted value", auto.text),
      y = quickText(extra.args$ylab %||% "observed value", auto.text)
    )

  if ("main" %in% names(extra.args)) {
    plt <- plt + ggplot2::labs(title = quickText(extra.args$main, auto.text))
  }

  if (plot) {
    plot(plt)
  }

  output <- list(plot = plt, data = all_res, call = match.call())
  class(output) <- "openair"
  invisible(output)
}
