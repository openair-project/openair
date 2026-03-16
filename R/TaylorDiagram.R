#' Taylor Diagram for model evaluation with conditioning
#'
#' Function to draw Taylor Diagrams for model evaluation. The function allows
#' conditioning by any categorical or numeric variables, which makes the
#' function very flexible.
#'
#' The Taylor Diagram is a very useful model evaluation tool. The diagram
#' provides a way of showing how three complementary model performance
#' statistics vary simultaneously. These statistics are the correlation
#' coefficient R, the standard deviation (sigma) and the (centred)
#' root-mean-square error. These three statistics can be plotted on one (2D)
#' graph because of the way they are related to one another which can be
#' represented through the Law of Cosines.
#'
#' The `openair` version of the Taylor Diagram has several enhancements that
#' increase its flexibility. In particular, the straightforward way of producing
#' conditioning plots should prove valuable under many circumstances (using the
#' `type` option). Many examples of Taylor Diagrams focus on model-observation
#' comparisons for several models using all the available data. However, more
#' insight can be gained into model performance by partitioning the data in
#' various ways e.g. by season, daylight/nighttime, day of the week, by levels
#' of a numeric variable e.g. wind speed or by land-use type etc.
#'
#' To consider several pollutants on one plot, a column identifying the
#' pollutant name can be used e.g. `pollutant`. Then the Taylor Diagram can be
#' plotted as (assuming a data frame `thedata`):
#'
#' `TaylorDiagram(thedata, obs = "obs", mod = "mod", group = "model", type =
#' "pollutant")`
#'
#' which will give the model performance by pollutant in each panel.
#'
#' Note that it is important that each panel represents data with the same mean
#' observed data across different groups. Therefore `TaylorDiagram(mydata, group
#' = "model", type = "season")` is OK, whereas `TaylorDiagram(mydata, group =
#' "season", type = "model")` is not because each panel (representing a model)
#' will have four different mean values --- one for each season. Generally, the
#' option `group` is either missing (one model being evaluated) or represents a
#' column giving the model name. However, the data can be normalised using the
#' `normalise` option. Normalisation is carried out on a per `group`/`type`
#' basis making it possible to compare data on different scales e.g.
#' `TaylorDiagram(mydata, group = "season", type = "model", normalise = TRUE)`.
#' In this way it is possible to compare different pollutants, sites etc. in the
#' same panel.
#'
#' Also note that if multiple sites are present it makes sense to use `type =
#' "site"` to ensure that each panel represents an individual site with its own
#' specific standard deviation etc. If this is not the case then select a single
#' site from the data first e.g. `subset(mydata, site == "Harwell")`.
#'
#' @param mydata A data frame minimally containing a column of observations and
#'   a column of predictions.
#'
#' @param obs A column of observations with which the predictions (`mod`) will
#'   be compared.
#'
#' @param mod A column of model predictions. Note, `mod` can be of length 2 i.e.
#'   two lots of model predictions. If two sets of predictions are are present
#'   e.g. `mod = c("base", "revised")`, then arrows are shown on the Taylor
#'   Diagram which show the change in model performance in going from the first
#'   to the second. This is useful where, for example, there is interest in
#'   comparing how one model run compares with another using different
#'   assumptions e.g. input data or model set up. See examples below.
#'
#' @param group The `group` column is used to differentiate between different
#'   models and can be a factor or character. The total number of models
#'   compared will be equal to the number of unique values of `group`.
#'
#'   `group` can also be of length two e.g. `group = c("model", "site")`. In
#'   this case all model-site combinations will be shown but they will only be
#'   differentiated by colour/symbol by the first grouping variable ("model" in
#'   this case). In essence the plot removes the differentiation by the second
#'   grouping variable. Because there will be different values of `obs` for each
#'   group, `normalise = TRUE` should be used.
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
#'   Note that often it will make sense to use `type = "site"` when multiple
#'   sites are available. This will ensure that each panel contains data
#'   specific to an individual site.
#'
#' @param normalise Should the data be normalised by dividing the standard
#'   deviation of the observations? The statistics can be normalised (and
#'   non-dimensionalised) by dividing both the RMS difference and the standard
#'   deviation of the `mod` values by the standard deviation of the observations
#'   (`obs`). In this case the \dQuote{observed} point is plotted on the x-axis
#'   at unit distance from the origin. This makes it possible to plot statistics
#'   for different species (maybe with different units) on the same plot. The
#'   normalisation is done by each `group`/`type` combination.
#'
#' @param cols Colours to be used for plotting. Useful options for categorical
#'   data are available from `RColorBrewer` colours --- see the `openair`
#'   `openColours` function for more details. Useful schemes include
#'   \dQuote{Accent}, \dQuote{Dark2}, \dQuote{Paired}, \dQuote{Pastel1},
#'   \dQuote{Pastel2}, \dQuote{Set1}, \dQuote{Set2}, \dQuote{Set3} --- but see
#'   ?`brewer.pal` for the maximum useful colours in each. For user defined the
#'   user can supply a list of colour names recognised by R (type `colours()` to
#'   see the full list). An example would be `cols = c("yellow", "green",
#'   "blue")`.
#'
#' @param rms.col Colour for centred-RMS lines and text.
#'
#' @param cor.col Colour for correlation coefficient lines and text.
#'
#' @param arrow.lwd Width of arrow used when used for comparing two model
#'   outputs.
#'
#' @param annotate Annotation shown for RMS error.
#'
#' @param text.obs The plot annotation for observed values; default is
#'   "observed".
#'
#' @param key Should the key be shown?
#'
#' @param key.title Title for the key.
#'
#' @param key.columns Number of columns to be used in the key. With many
#'   pollutants a single column can make to key too wide. The user can thus
#'   choose to use several columns by setting `columns` to be less than the
#'   number of pollutants.
#'
#' @param key.position Position of the key e.g. \dQuote{top}, \dQuote{bottom},
#'   \dQuote{left} and \dQuote{right}.
#'
#' @param auto.text Either `TRUE` (default) or `FALSE`. If `TRUE` titles and
#'   axis labels will automatically try and format pollutant names and units
#'   properly e.g.  by subscripting the `2' in NO2.
#'
#' @param plot Should a plot be produced? `FALSE` can be useful when analysing
#'   data to extract trend components and plotting them in other ways.
#'
#' @param strip.position Location where the facet 'strips' are located when
#'   using `type`. When one `type` is provided, can be one of `"left"`,
#'   `"right"`, `"bottom"` or `"top"`. When two `type`s are provided, this
#'   argument defines whether the strips are "switched" and can take either
#'   `"x"`, `"y"`, or `"both"`. For example, `"x"` will switch the 'top' strip
#'   locations to the bottom of the plot.
#'
#' @param ... Other graphical parameters are passed onto `cutData` and other
#'   functions. For example, `TaylorDiagram` passes the option `hemisphere =
#'   "southern"` on to `cutData` to provide southern (rather than default
#'   northern) hemisphere handling of `type = "season"`. Similarly, common
#'   graphical parameters, such as `layout` for panel arrangement and `pch` and
#'   `cex` for plot symbol type and size, are passed on to `xyplot`. Most are
#'   passed unmodified, although there are some special cases where `openair`
#'   may locally manage this process. For example, common axis and title
#'   labelling options (such as `xlab`, `ylab`, `main`) are passed via
#'   `quickText` to handle routine formatting.
#'
#' @export
#'
#' @return an [openair][openair-package] object. If retained, e.g., using
#'   `output <- TaylorDiagram(thedata, obs = "nox", mod = "mod")`, this output
#'   can be used to recover the data, reproduce or rework the original plot or
#'   undertake further analysis. For example, `output$data` will be a data frame
#'   consisting of the group, type, correlation coefficient (R), the standard
#'   deviation of the observations and measurements.
#'
#' @author David Carslaw
#' @author Jack Davison
#'
#' @family model evaluation functions
#'
#' @references
#'
#' Taylor, K.E.: Summarizing multiple aspects of model performance in a single
#' diagram. J.  Geophys. Res., 106, 7183-7192, 2001 (also see PCMDI Report 55).
#'
#' @examples
#' # in the examples below, most effort goes into making some artificial data
#' # the function itself can be run very simply
#'
#' \dontrun{
#' library(dplyr)
#'
#' dummy model data for 2003
#' dat <- selectByDate(mydata, year = 2003) |>
#'   transmute(date, obs = nox, mod = nox, month = as.integer(format(date, "%m")))
#'
#' # now make mod worse by adding bias and noise according to the month
#' # do this for 3 different models
#' mod1 <- dat |>
#'   mutate(
#'     mod = mod + 10 * month + 10 * month * rnorm(n()),
#'     model = "model 1"
#'   ) |>
#'   # lag the results to make the correlation coefficient worse without affecting the sd
#'   mutate(mod = c(mod[5:n()], mod[(n() - 3):n()]))
#'
#' mod2 <- dat |>
#'   mutate(
#'     mod = mod + 7 * month + 7 * month * rnorm(n()),
#'     model = "model 2"
#'   )
#'
#' mod3 <- dat |>
#'   mutate(
#'     mod = mod + 3 * month + 3 * month * rnorm(n()),
#'     model = "model 3"
#'   )
#'
#' mod.dat <- bind_rows(mod1, mod2, mod3)
#'
#' # basic Taylor plot
#' TaylorDiagram(mod.dat, obs = "obs", mod = "mod", group = "model")
#'
#' # Taylor plot by season
#' TaylorDiagram(
#'   mod.dat,
#'   obs = "obs",
#'   mod = "mod",
#'   group = "model",
#'   type = "season"
#' )
#'
#' # now show how to evaluate model improvement (or otherwise)
#' mod1a <- dat |>
#'   mutate(
#'     mod = mod + 2 * month + 2 * month * rnorm(n()),
#'     model = "model 1"
#'   )
#'
#' mod2a <- mod2 |> mutate(mod = mod * 1.3)
#'
#' mod3a <- dat |>
#'   mutate(
#'     mod = mod + 10 * month + 10 * month * rnorm(n()),
#'     model = "model 3"
#'   )
#'
#' # now we have a data frame with 3 models, 1 set of observations
#' # and two sets of model predictions (mod and mod2)
#' mod.dat <- mod.dat |>
#'   mutate(mod2 = bind_rows(mod1a, mod2a, mod3a) |> pull(mod))
#'
#' # do for all models
#' TaylorDiagram(mod.dat, obs = "obs", mod = c("mod", "mod2"), group = "model")
#'
#' # all models, by season
#' TaylorDiagram(
#'   mod.dat,
#'   obs = "obs",
#'   mod = c("mod", "mod2"),
#'   group = "model",
#'   type = "season"
#' )
#'
#' # consider two groups (model/month). In this case all months are shown by
#' # model but are only differentiated by model.
#' TaylorDiagram(mod.dat, obs = "obs", mod = "mod", group = c("model", "month"))
#' }
TaylorDiagram <- function(
  mydata,
  obs = "obs",
  mod = "mod",
  group = NULL,
  type = "default",
  normalise = FALSE,
  cols = "brewer1",
  rms.col = "darkgoldenrod",
  cor.col = "black",
  arrow.lwd = 3,
  annotate = "centred\nRMS error",
  text.obs = "observed",
  key = TRUE,
  key.title = group,
  key.columns = 1,
  key.position = "right",
  strip.position = "top",
  auto.text = TRUE,
  plot = TRUE,
  ...
) {
  if (rlang::is_logical(key) && !key) {
    key.position <- "none"
  }

  # extra.args setup
  extra.args <- list(...)

  # label controls (some local xlab, ylab management in code)
  extra.args$xlab <- quickText(
    extra.args$xlab %||%
      ifelse(
        normalise,
        "standard deviation (normalised)",
        "standard deviation"
      ),
    auto.text
  )
  extra.args$ylab <- quickText(
    extra.args$ylab %||%
      ifelse(
        normalise,
        "standard deviation (normalised)",
        "standard deviation"
      ),
    auto.text
  )
  extra.args$main <- quickText(extra.args$main, auto.text)
  extra.args$pch <- extra.args$pch %||% 20
  extra.args$cex <- extra.args$cex %||% 2
  extra.args$fontsize <- extra.args$fontsize %||% 11

  # check to see if two data sets are present
  combine <- FALSE

  # if mod is of length 2, then we want to show the change in model performance
  # from the first to the second
  if (length(mod) == 2) {
    combine <- TRUE
  }

  # set up variables to be used in the function
  if (any(type %in% dateTypes)) {
    vars <- c("date", obs, mod)
  } else {
    vars <- c(obs, mod)
  }

  # assume two groups do not exist
  twoGrp <- FALSE

  # check that group is not also in type because this will cause problems with
  # the way the data are processed
  if (!missing(group)) {
    if (any(group %in% type)) {
      cli::cli_abort("Can't have {.arg group} also in {.arg type}.")
    }
  }

  mydata <- cutData(mydata, type, ...)

  if (missing(group)) {
    if ((!"group" %in% type) && (!"group" %in% c(obs, mod))) {
      mydata$group <- factor("modelled")
      group <- "group"
      npol <- 1
    }
  } else {
    # means that group is there
    mydata <- cutData(mydata, group, ...)
  }

  # if group is present, need to add that list of variables unless it is
  # a pre-defined date-based one
  if (!missing(group)) {
    npol <- length(unique((mydata[[group[1]]])))

    # if group is of length 2
    if (length(group) == 2L) {
      twoGrp <- TRUE
      grp1 <- group[1]
      grp2 <- group[2]

      if (missing(key.title)) {
        key.title <- grp1
      }
      vars <- c(vars, grp1, grp2)
      mydata$newgrp <- factor(paste(
        mydata[[group[1]]],
        mydata[[group[2]]],
        sep = "___"
      ))
      orig_group <- group
      group <- "newgrp"
    }

    if (group %in% dateTypes || any(type %in% dateTypes)) {
      vars <- unique(c(vars, "date", group))
    } else {
      vars <- unique(c(vars, group))
    }
  }

  # data checks, for base and new data if necessary
  mydata <- checkPrep(mydata, vars, type)

  # check mod and obs are numbers
  mydata <- check_numeric(mydata, vars = c(obs, mod))

  # remove missing data
  mydata <- na.omit(mydata)

  # function to calculate stats for TD
  calcStats <- function(mydata, obs = obs, mod = mod) {
    R <- cor(mydata[[obs]], mydata[[mod]], use = "pairwise")
    sd.obs <- sd(mydata[[obs]])
    sd.mod <- sd(mydata[[mod]])
    if (normalise) {
      sd.mod <- sd.mod / sd.obs
      sd.obs <- 1
    }

    res <- data.frame(R, sd.obs, sd.mod)
    res
  }

  vars <- c(group, type)

  results <-
    map_type(
      mydata,
      type = vars,
      fun = \(x) calcStats(x, obs = obs, mod = mod[1]),
      .include_default = TRUE
    )

  # if two sets of model data are present, then calculate the stats for the
  # second set and combine with the first. This will allow us to show the change
  # in model performance from the first to the second.
  if (combine) {
    results.new <-
      map_type(
        mydata,
        type = vars,
        fun = \(x) calcStats(x, obs = obs, mod = mod[2]),
        .include_default = TRUE
      )

    results <-
      dplyr::bind_rows(
        dplyr::mutate(results, taylor_mod_id = mod[1]),
        dplyr::mutate(results.new, taylor_mod_id = mod[2])
      ) |>
      dplyr::mutate(
        taylor_mod_id = factor(.data$taylor_mod_id, mod)
      )
  }

  # if no group to plot, then add a dummy one to make xyplot work
  if (is.null(group)) {
    results$MyGroupVar <- factor("MyGroupVar")
    group <- "MyGroupVar"
  }

  # calculate the grid for the centred-RMS contours. This is done by calculating
  # the centred-RMS error for a grid of correlation and standard deviation
  # values. The grid is calculated separately for each panel (i.e. each
  # combination of type variables) because the observed standard deviation can
  # differ between panels
  crmse_grid <-
    map_type(
      results,
      type,
      \(df) {
        crmse <- function(o, m, r) {
          sqrt(o^2 + m^2 - 2 * o * m * r)
        }

        nicerange <- pretty(c(results$sd.obs, results$sd.mod))

        crmse_grid <-
          expand.grid(
            m = pretty(c(0, nicerange), n = 50),
            cor = seq(0, 1, 0.01)
          ) |>
          dplyr::mutate(crmse = crmse(o = df$sd.obs[1], .data$m, .data$cor))

        return(crmse_grid)
      }
    )

  # contour functions
  if (rlang::is_installed("geomtextpath")) {
    contour_fun <- geomtextpath::geom_textcontour
  } else {
    cli::cli_inform(
      c(
        "i" = "Install the {.pkg geomtextpath} package for direct labelling of centred RMSE contours in {.fun openair::TaylorDiagram}."
      ),
      .frequency = "regularly",
      .frequency_id = "geomtextpath"
    )
    contour_fun <- ggplot2::geom_contour
  }

  # future proofing in case we want negative R values in future
  x_breaks <- c(seq(0.1, 0.9, 0.1), 0.95, 0.99)
  x_labels <- x_breaks

  # restore original groups
  if (group == "newgrp") {
    results <-
      results |>
      tidyr::separate_wider_delim(
        "newgrp",
        delim = "___",
        names = orig_group,
        cols_remove = FALSE
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(orig_group), factor)
      )
    group <- orig_group[1]
  } else {
    results$newgrp <- results[[group]]
  }

  # ensure correct number of shapes
  shapes <- extra.args$pch
  while (length(shapes) <= nlevels(results[[group]])) {
    shapes <- c(shapes, shapes)
  }
  shapes <- shapes[1:nlevels(results[[group]])]

  legend_guide <-
    ggplot2::guide_legend(
      ncol = if (missing(key.columns)) NULL else key.columns
    )

  # plotting
  thePlot <-
    ggplot2::ggplot(
      results,
      ggplot2::aes(x = .data$R, y = .data$sd.mod)
    ) +
    contour_fun(
      data = crmse_grid,
      inherit.aes = FALSE,
      na.rm = TRUE,
      ggplot2::aes(x = .data$cor, y = .data$m, z = .data$crmse),
      breaks = pretty(crmse_grid$crmse, n = 5),
      lty = 2,
      colour = rms.col
    ) +
    ggplot2::geom_hline(
      data = dplyr::slice_head(results, by = dplyr::all_of(type)),
      ggplot2::aes(yintercept = .data$sd.obs),
      lty = 2
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        y = .data$sd.obs,
        x = 1,
        colour = text.obs,
        shape = text.obs
      ),
      size = extra.args$cex * 2
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        y = .data$sd.obs,
        x = 0,
        colour = text.obs,
        shape = text.obs
      ),
      size = extra.args$cex * 2
    ) +
    ggplot2::annotate(
      geom = "text",
      y = I(0.9),
      x = I(0.9),
      hjust = 1,
      vjust = 1,
      label = annotate,
      color = rms.col,
      size = extra.args$fontsize / 3
    ) +
    ggplot2::annotate(
      x = I(0.7),
      y = I(0.7),
      geom = "text",
      label = "correlation",
      angle = -45,
      color = cor.col,
      size = extra.args$fontsize / 3
    ) +
    ggplot2::coord_radial(
      thetalim = c(0, 1),
      rlim = c(0, NA),
      start = 0,
      end = 90 * pi / 180,
      reverse = "theta"
    ) +
    ggplot2::scale_x_continuous(
      transform = scales::new_transform(
        name = "cosine",
        transform = \(x) acos(x),
        inverse = \(x) cos(x),
        domain = c(0, 1)
      ),
      breaks = x_breaks,
      labels = x_labels,
      guide = ggplot2::guide_axis_theta(angle = 90),
      expand = ggplot2::expansion()
    ) +
    ggplot2::scale_y_continuous(
      sec.axis = ggplot2::dup_axis(name = NULL),
      limits = c(0, max(crmse_grid$m)),
      breaks = scales::pretty_breaks(6),
      expand = ggplot2::expansion()
    ) +
    theme_openair_radial(key.position = key.position, panel.ontop = FALSE) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(color = "grey85"),
      panel.spacing = ggplot2::unit(0, "cm"),
      axis.line.r = ggplot2::element_line(),
      axis.line.theta = ggplot2::element_line(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line()
    ) +
    set_extra_fontsize(extra.args) +
    ggplot2::labs(
      x = extra.args$xlab,
      y = extra.args$ylab,
      title = extra.args$main,
      color = quickText(key.title, auto.text = auto.text),
      shape = quickText(key.title, auto.text = auto.text)
    ) +
    ggplot2::scale_shape_manual(
      values = c(shapes, 20),
      breaks = c(levels(results[[group]]), text.obs)
    ) +
    get_facet(
      type,
      extra.args,
      scales = "fixed",
      auto.text = auto.text,
      drop = FALSE,
      strip.position = strip.position
    )

  # if two sets of model data are present, then show the change in model
  # performance from the first to the second with arrows
  if ("taylor_mod_id" %in% names(results)) {
    results_wide <-
      results |>
      dplyr::select(-"sd.obs") |>
      tidyr::pivot_wider(
        names_from = "taylor_mod_id",
        values_from = c("sd.mod", "R")
      )

    thePlot <- thePlot +
      ggplot2::geom_segment(
        data = results_wide,
        ggplot2::aes(
          x = .data[[paste0("R_", mod[1])]],
          xend = .data[[paste0("R_", mod[2])]],
          y = .data[[paste0("sd.mod_", mod[1])]],
          yend = .data[[paste0("sd.mod_", mod[2])]],
          color = .data[[group]]
        ),
        arrow = ggplot2::arrow(
          length = ggplot2::unit(arrow.lwd / 10, "cm")
        ),
        linewidth = arrow.lwd / 3
      ) +
      ggplot2::scale_color_manual(
        values = c(openColours(cols, n = nlevels(results[[group]])), "black"),
        breaks = c(levels(results[[group]]), text.obs)
      ) +
      ggplot2::guides(
        shape = ggplot2::guide_none(),
        color = legend_guide
      )
  } else {
    thePlot <-
      thePlot +
      ggplot2::geom_point(
        ggplot2::aes(
          color = .data[[group]],
          shape = .data[[group]],
          group = .data$newgrp
        ),
        size = extra.args$cex * 2
      ) +
      ggplot2::scale_color_manual(
        values = c(openColours(cols, n = nlevels(results[[group]])), "black"),
        breaks = c(levels(results[[group]]), text.obs)
      ) +
      ggplot2::guides(
        color = legend_guide,
        shape = legend_guide
      )
  }

  if (plot) {
    plot(thePlot)
  }

  newdata <- results
  output <- list(plot = thePlot, data = newdata, call = match.call())
  class(output) <- "openair"

  invisible(output)
}
