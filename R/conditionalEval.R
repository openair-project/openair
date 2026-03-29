#' Conditional quantile estimates with additional variables for model evaluation
#'
#' This function enhances [conditionalQuantile()] by also considering how other
#' variables vary over the same intervals. Conditional quantiles are very useful
#' on their own for model evaluation, but provide no direct information on how
#' other variables change at the same time. For example, a conditional quantile
#' plot of ozone concentrations may show that low concentrations of ozone tend
#' to be under-predicted. However, the cause of the under-prediction can be
#' difficult to determine. However, by considering how well the model predicts
#' other variables over the same intervals, more insight can be gained into the
#' underlying reasons why model performance is poor.
#'
#' The `conditionalEval` function provides information on how other
#' variables vary across the same intervals as shown on the conditional quantile
#' plot. There are two types of variable that can be considered by setting the
#' value of `statistic`. First, `statistic` can be another variable in
#' the data frame. In this case the plot will show the different proportions of
#' `statistic` across the range of predictions. For example `statistic
#' = "season"` will show for each interval of `mod` the proportion of
#' predictions that were spring, summer, autumn or winter. This is useful
#' because if model performance is worse for example at high concentrations of
#' `mod` then knowing that these tend to occur during a particular season
#' etc. can be very helpful when trying to understand *why* a model fails.
#' See [cutData()] for more details on the types of variable that can
#' be `statistic`. Another example would be `statistic = "ws"` (if
#' wind speed were available in the data frame), which would then split wind
#' speed into four quantiles and plot the proportions of each.
#'
#' Second, `conditionalEval` can simultaneously plot the model performance
#' of other observed/predicted variable **pairs** according to different
#' model evaluation statistics. These statistics derive from the
#' [modStats()] function and include \dQuote{MB}, \dQuote{NMB},
#' \dQuote{r}, \dQuote{COE}, \dQuote{MGE}, \dQuote{NMGE}, \dQuote{RMSE} and
#' \dQuote{FAC2}. More than one statistic can be supplied e.g. `statistic =
#' c("NMB", "COE")`. Bootstrap samples are taken from the corresponding values
#' of other variables to be plotted and their statistics with 95\% confidence
#' intervals calculated. In this case, the model *performance* of other
#' variables is shown across the same intervals of `mod`, rather than just
#' the values of single variables. In this second case the model would need to
#' provide observed/predicted pairs of other variables.
#'
#' For example, a model may provide predictions of NOx and wind speed (for which
#' there are also observations available). The `conditionalEval` function
#' will show how well these other variables are predicted for the same intervals
#' of the main variables assessed in the conditional quantile e.g. ozone. In
#' this case, values are supplied to `var.obs` (observed values for other
#' variables) and `var.mod` (modelled values for other variables). For
#' example, to consider how well the model predicts NOx and wind speed
#' `var.obs = c("nox.obs", "ws.obs")` and `var.mod = c("nox.mod",
#' "ws.mod")` would be supplied (assuming `nox.obs, nox.mod, ws.obs,
#' ws.mod` are present in the data frame). The analysis could show for example,
#' when ozone concentrations are under-predicted, the model may also be shown to
#' over-predict concentrations of NOx at the same time, or under-predict wind
#' speeds. Such information can thus help identify the underlying causes of poor
#' model performance.
#'
#' A special case is `statistic = "cluster"`. In this case a data frame is
#' provided that contains the cluster calculated by [trajCluster()]
#' and [importTraj()]. Note that `statistic = "cluster"` cannot be
#' used together with the ordinary model evaluation statistics such as MB.
#' The output will be a bar chart showing the proportion of each interval of
#' `mod` by cluster number.
#'
#' @inheritParams conditionalQuantile
#'
#' @param var.obs Other variable observations for which statistics should be
#'   calculated. Can be more than length one e.g. `var.obs = c("nox.obs",
#'   "ws.obs")`.
#'
#' @param var.mod Other variable predictions for which statistics should be
#'   calculated. Can be more than length one e.g. `var.mod = c("nox.mod",
#'   "ws.mod")`.
#'
#' @param statistic Statistic(s) to be plotted. Can be \dQuote{MB},
#'   \dQuote{NMB}, \dQuote{r}, \dQuote{COE}, \dQuote{MGE}, \dQuote{NMGE},
#'   \dQuote{RMSE} and \dQuote{FAC2}. `statistic` can also be a variable
#'   name in the data frame or a date-based type (e.g. \dQuote{season}), in
#'   which case the plot shows the proportions of that variable across the
#'   prediction intervals. A special case is \dQuote{cluster}.
#'
#' @param col.var Colours for the additional variables. See `openColours`
#'   for more details.
#'
#' @param var.names Variable names to be shown in the legend for `var.obs`
#'   and `var.mod`.
#'
#' @param ... Other graphical parameters passed onto [conditionalQuantile()]
#'   and [cutData()].
#'
#' @export
#' @author David Carslaw
#' @family model evaluation functions
#' @seealso The `verification` package for comprehensive functions for
#'   forecast verification.
#' @references Wilks, D. S., 2005. Statistical Methods in the Atmospheric
#'   Sciences, Volume 91, Second Edition (International Geophysics), 2nd
#'   Edition. Academic Press.
conditionalEval <- function(
  mydata,
  obs = "obs",
  mod = "mod",
  var.obs = "var.obs",
  var.mod = "var.mod",
  type = "default",
  bins = 31,
  statistic = "MB",
  cols = "YlOrRd",
  col.var = "Set1",
  var.names = NULL,
  auto.text = TRUE,
  plot = TRUE,
  ...
) {
  the_stats <- c("MB", "NMB", "r", "COE", "MGE", "NMGE", "RMSE", "FAC2")

  if (length(type) > 1) {
    cli::cli_abort("Only one type can be used with this function")
  }

  extra.args <- rlang::list2(...)

  # Determine mode: "other" (variable/date) or model-eval statistic
  other <- FALSE

  if (any(statistic %in% dateTypes)) {
    statistic <- statistic[statistic %in% dateTypes][1]
    mydata <- cutData(mydata, type = statistic)
    other <- TRUE
  } else if (any(statistic %in% names(mydata))) {
    statistic <- statistic[statistic %in% names(mydata)][1]
    mydata <- cutData(mydata, type = statistic)
    other <- TRUE
  } else if ("cluster" %in% statistic) {
    statistic <- "cluster"
    other <- TRUE
  }

  if (other) {
    var.obs <- NULL
    var.mod <- NULL
  } else {
    if (length(var.obs) == 0 || length(var.mod) == 0) {
      cli::cli_abort("No variables chosen to analyse")
    }
    if (length(var.obs) != length(var.mod)) {
      cli::cli_abort(
        "Number of var.obs does not equal number of var.mod variables"
      )
    }
  }

  vars <- c(mod, obs, var.obs, var.mod)
  if (other) {
    vars <- c(vars, statistic)
  }
  if (any(type %in% dateTypes)) {
    vars <- c("date", vars)
  }

  if (statistic[1] == "cluster") {
    vars <- c(vars, "cluster")
    if ("hour.inc" %in% names(mydata)) {
      mydata <- dplyr::filter(mydata, .data$hour.inc == 0)
    }
  }

  mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)
  mydata <- stats::na.omit(mydata)

  lo <- min(mydata[, c(mod, obs)])
  hi <- max(mydata[, c(mod, obs)])

  # Left panel: conditional quantile plot (suppress auto-print)
  cq_args <- c(
    list(
      mydata = mydata,
      obs = obs,
      mod = mod,
      type = type,
      bins = bins,
      cols = cols,
      key.position = "bottom",
      key.columns = 1,
      auto.text = auto.text,
      plot = FALSE
    )
  )
  cq_plt <- do.call(conditionalQuantile, cq_args)$plot

  mydata <- cutData(mydata, type)

  y_lo <- lo
  y_hi <- hi * 1.05
  bins_breaks <- seq(floor(lo), ceiling(hi), length = bins)
  bin_width <- diff(bins_breaks)[1]
  labs <- bins_breaks[-length(bins_breaks)] + 0.5 * bin_width

  # "other" mode: stacked bar chart of proportions
  if (other) {
    compute_other <- function(df) {
      pred_cut <- cut(
        df[[mod]],
        breaks = bins_breaks,
        include.lowest = TRUE,
        labels = labs
      )
      pred_cut[is.na(pred_cut)] <- labs[1]
      df$pred.cut <- as.numeric(as.character(pred_cut))

      df |>
        dplyr::group_by(.data[["pred.cut"]], .data[[statistic]]) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
        dplyr::group_by(.data[["pred.cut"]]) |>
        dplyr::mutate(Freq = .data$n / sum(.data$n)) |>
        dplyr::ungroup() |>
        dplyr::select(-"n")
    }

    other_results <- mydata |>
      dplyr::group_by(dplyr::across(dplyr::all_of(type))) |>
      dplyr::group_nest() |>
      dplyr::mutate(
        res = purrr::map(.data$data, compute_other),
        .keep = "unused"
      ) |>
      tidyr::unnest("res")

    stat_levels <- levels(other_results[[statistic]])
    cols <- openColours(col.var, length(stat_levels))

    right_plt <- ggplot2::ggplot(
      other_results,
      ggplot2::aes(
        x = .data[["pred.cut"]],
        y = .data[["Freq"]],
        fill = .data[[statistic]]
      )
    ) +
      ggplot2::geom_col(width = bin_width, position = "stack", color = NA) +
      ggplot2::scale_fill_manual(
        values = stats::setNames(cols, stat_levels),
        labels = \(x) label_openair(x, auto_text = auto.text)
      ) +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::scale_x_continuous(
        limits = c(y_lo, y_hi)
      ) +
      ggplot2::scale_y_continuous(
        limits = c(0, 1),
        transform = scales::transform_reverse()
      ) +
      get_facet(type, extra.args, "fixed", auto.text) +
      theme_openair(key.position = "bottom") +
      set_extra_fontsize(extra.args) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(direction = "vertical")
      ) +
      ggplot2::labs(
        fill = quickText(statistic, auto.text),
        x = quickText(extra.args$xlab, auto.text),
        y = quickText("proportion", auto.text)
      )

    return_data <- other_results
  } else {
    # stat mode: bootstrap CI lines
    stat_list <- statistic[statistic %in% the_stats]

    compute_stat <- function(df, stat, v_obs, v_mod) {
      pred_cut <- cut(
        df[[mod]],
        breaks = bins_breaks,
        include.lowest = TRUE,
        labels = labs
      )
      pred_cut[is.na(pred_cut)] <- labs[1]
      df$pred.cut <- as.numeric(as.character(pred_cut))

      df |>
        dplyr::group_by(.data[["pred.cut"]]) |>
        dplyr::group_modify(\(x, ...) {
          if (nrow(x) <= 4) {
            return(data.frame(
              mean = NA_real_,
              lower = NA_real_,
              upper = NA_real_
            ))
          }
          boots <- purrr::map(1:200, \(i) {
            samp <- x[sample(nrow(x), nrow(x), replace = TRUE), ]
            get(stat)(samp, obs = v_obs, mod = v_mod)
          }) |>
            dplyr::bind_rows()
          val <- boots[[stat]]
          data.frame(
            mean = mean(val, na.rm = TRUE),
            lower = stats::quantile(val, 0.025, na.rm = TRUE),
            upper = stats::quantile(val, 0.975, na.rm = TRUE)
          )
        }) |>
        dplyr::ungroup() |>
        dplyr::mutate(statistic = stat, group = v_obs)
    }

    combs <- expand.grid(
      var_obs = var.obs,
      stat = stat_list,
      stringsAsFactors = FALSE
    )
    combs$var_mod <- var.mod[match(combs$var_obs, var.obs)]

    results <- mydata |>
      dplyr::group_by(dplyr::across(dplyr::all_of(type))) |>
      dplyr::group_nest() |>
      dplyr::mutate(
        res = purrr::map(.data$data, \(df) {
          purrr::pmap(combs, \(var_obs, stat, var_mod) {
            compute_stat(df, stat, var_obs, var_mod)
          }) |>
            dplyr::bind_rows()
        }),
        .keep = "unused"
      ) |>
      tidyr::unnest("res")

    # Replace infinite values with NA
    results <- dplyr::mutate(
      results,
      dplyr::across(c("mean", "lower", "upper"), \(x) {
        replace(x, is.infinite(x), NA_real_)
      })
    )

    results$statistic <- factor(results$statistic, levels = stat_list)
    results$group <- factor(results$group, levels = var.obs)

    myColors <- openColours(col.var, length(var.obs))

    display_names <- if (is.null(var.names)) var.obs else var.names
    display_names <- lapply(display_names, quickText, auto.text)
    names(display_names) <- var.obs

    # Reference lines: h=0 for MB/NMB, h=1 for r/COE/FAC2
    stats_h0 <- stat_list[stat_list %in% c("MB", "NMB")]
    stats_h1 <- stat_list[stat_list %in% c("r", "COE", "FAC2")]
    ref_df <- data.frame(
      statistic = factor(
        c(stats_h0, stats_h1),
        levels = levels(results$statistic)
      ),
      h = c(rep(0, length(stats_h0)), rep(1, length(stats_h1)))
    )

    # Faceting for right panel
    multi_stat <- length(stat_list) > 1
    multi_type <- type != "default"

    right_facet <- if (multi_stat && multi_type) {
      ggplot2::facet_grid(
        rows = ggplot2::vars(statistic),
        cols = ggplot2::vars(.data[[type]]),
        scales = "free_y",
        labeller = labeller_openair(auto_text = auto.text)
      )
    } else if (multi_stat) {
      ggplot2::facet_wrap(
        ggplot2::vars(statistic),
        scales = "free_y",
        labeller = labeller_openair(auto_text = auto.text)
      )
    } else if (multi_type) {
      get_facet(type, extra.args, "fixed", auto.text)
    } else {
      ggplot2::facet_wrap(
        ggplot2::vars(statistic),
        scales = "free_y",
        labeller = labeller_openair(auto_text = auto.text)
      )
    }

    right_plt <- ggplot2::ggplot(
      results,
      ggplot2::aes(x = .data[["pred.cut"]], group = .data[["group"]])
    ) +
      ggplot2::geom_hline(
        data = ref_df,
        ggplot2::aes(yintercept = .data[["h"]]),
        linetype = 5,
        color = "grey50",
        inherit.aes = FALSE
      ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(
          ymin = .data[["lower"]],
          ymax = .data[["upper"]],
          fill = .data[["group"]]
        ),
        alpha = 0.3,
        na.rm = TRUE
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = mean, color = .data[["group"]]),
        linewidth = 1,
        na.rm = TRUE
      ) +
      ggplot2::scale_color_manual(
        values = stats::setNames(myColors, var.obs),
        labels = display_names,
        aesthetics = c("colour", "fill")
      ) +
      ggplot2::scale_x_continuous(
        limits = c(y_lo, y_hi)
      ) +
      right_facet +
      theme_openair("bottom") +
      set_extra_fontsize(extra.args) +
      ggplot2::labs(
        x = quickText(extra.args$xlab, auto.text),
        y = quickText(extra.args$ylab, auto.text),
        color = quickText("variable", auto.text),
        fill = quickText("variable", auto.text)
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(direction = "vertical"),
        color = ggplot2::guide_legend(direction = "vertical")
      )

    return_data <- results
  }

  if ("main" %in% names(extra.args)) {
    title <- quickText(extra.args$main, auto.text)
    cq_plt <- cq_plt + ggplot2::labs(title = title)
    right_plt <- right_plt + ggplot2::labs(title = title)
  }

  thePlot <- patchwork::wrap_plots(
    cq_plt,
    right_plt
  )

  if (plot) {
    plot(thePlot)
  }

  output <- list(
    plot = thePlot,
    data = return_data,
    call = match.call()
  )
  class(output) <- "openair"
  invisible(output)
}
