#' Trajectory level plots with conditioning
#'
#' This function plots gridded back trajectories. This function requires that
#' data are imported using the [importTraj()] function.
#'
#' An alternative way of showing the trajectories compared with plotting
#' trajectory lines is to bin the points into latitude/longitude intervals. For
#' these purposes [trajLevel()] should be used. There are several trajectory
#' statistics that can be plotted as gridded surfaces. First, `statistic` can be
#' set to "frequency" to show the number of back trajectory points in a grid
#' square. Grid squares are by default at 1 degree intervals, controlled by
#' `lat.inc` and `lon.inc`. Such plots are useful for showing the frequency of
#' air mass locations. Note that it is also possible to set `statistic =
#' "hexbin"` for plotting frequencies (not concentrations), which will produce a
#' plot by hexagonal binning.
#'
#' If `statistic = "difference"` the trajectories associated with a
#' concentration greater than `percentile` are compared with the the full set of
#' trajectories to understand the differences in frequencies of the origin of
#' air masses of the highest concentration trajectories compared with the
#' trajectories on average. The comparison is made by comparing the percentage
#' change in gridded frequencies. For example, such a plot could show that the
#' top 10\% of concentrations of PM10 tend to originate from air-mass origins to
#' the east.
#'
#' If `statistic = "pscf"` then the Potential Source Contribution Function is
#' plotted. The PSCF calculates the probability that a source is located at
#' latitude \eqn{i} and longitude \eqn{j} (Pekney et al., 2006).The basis of
#' PSCF is that if a source is located at (i,j), an air parcel back trajectory
#' passing through that location indicates that material from the source can be
#' collected and transported along the trajectory to the receptor site. PSCF
#' solves \deqn{PSCF = m_{ij}/n_{ij}} where \eqn{n_{ij}} is the number of times
#' that the trajectories passed through the cell (i,j) and \eqn{m_{ij}} is the
#' number of times that a source concentration was high when the trajectories
#' passed through the cell (i,j). The criterion for determining \eqn{m_{ij}} is
#' controlled by `percentile`, which by default is 90. Note also that cells with
#' few data have a weighting factor applied to reduce their effect.
#'
#' A limitation of the PSCF method is that grid cells can have the same PSCF
#' value when sample concentrations are either only slightly higher or much
#' higher than the criterion. As a result, it can be difficult to distinguish
#' moderate sources from strong ones. Seibert et al. (1994) computed
#' concentration fields to identify source areas of pollutants. The
#' Concentration Weighted Trajectory (CWT) approach considers the concentration
#' of a species together with its residence time in a grid cell. The CWT
#' approach has been shown to yield similar results to the PSCF approach. The
#' openair manual has more details and examples of these approaches.
#'
#' A further useful refinement is to smooth the resulting surface, which is
#' possible by setting `smooth = TRUE`.
#'
#' @note This function is under active development and is likely to change
#'
#' @inheritParams trajPlot
#' @param smooth Should the trajectory surface be smoothed?
#' @param statistic One of:
#'   - `"frequency"` (the default) shows trajectory frequencies.
#'
#'   - `"hexbin"`, which is similar to `"frequency"` but shows a hexagonal
#'   grid of counts.
#'
#'   - `"difference"` - in this case trajectories where the associated
#'   concentration is greater than `percentile` are compared with the the full
#'   set of trajectories to understand the differences in frequencies of the
#'   origin of air masses. The comparison is made by comparing the percentage
#'   change in gridded frequencies. For example, such a plot could show that the
#'   top 10\% of concentrations of PM10 tend to originate from air-mass origins
#'   to the east.
#'
#'   - `"pscf"` for a Potential Source Contribution Function map. This statistic
#'   method interacts with `percentile`.
#'
#'   - `"cwt"` for concentration weighted trajectories.
#'
#'   - `"sqtba"` to undertake Simplified Quantitative Transport Bias
#'   Analysis. This statistic method interacts with `.combine` and `sigma`.
#'
#' @param percentile The percentile concentration of `pollutant` against which
#'   the all trajectories are compared.
#'
#' @param lon.inc,lat.inc The longitude and latitude intervals to be used for
#'   binning data. If `statistic = "hexbin"`, the minimum value out of of
#'   `lon.inc` and `lat.inc` is passed to the `binwidth` argument of
#'   [ggplot2::geom_hex()].
#'
#' @param min.bin The minimum number of unique points in a grid cell. Counts
#'   below `min.bin` are set as missing.
#'
#' @param .combine When statistic is "SQTBA" it is possible to combine lots of
#'   receptor locations to derive a single map. `.combine` identifies the column
#'   that differentiates different sites (commonly a column named `"site"`).
#'   Note that individual site maps are normalised first by dividing by their
#'   mean value.
#'
#' @param sigma For the SQTBA approach `sigma` determines the amount of back
#'   trajectory spread based on the Gaussian plume equation. Values in the
#'   literature suggest 5.4 km after one hour. However, testing suggests lower
#'   values reveal source regions more effectively while not introducing too
#'   much noise.
#'
#' @param plot Should a plot be produced? `FALSE` can be useful when analysing
#'   data to extract plot components and plotting them in other ways.
#'
#' @param ... Addition options are passed on to [cutData()] for `type` handling.
#'   Some additional arguments are also available:
#'   - `xlab`, `ylab` and `main` override the x-axis label, y-axis label, and plot title.
#'   - `layout` sets the layout of facets - e.g., `layout(2, 5)` will have 2 columns and 5 rows.
#'   - `fontsize` overrides the overall font size of the plot.
#'   - `border` sets the border colour of each tile.
#'
#' @export
#' @return an [openair][openair-package] object
#' @family trajectory analysis functions
#' @author David Carslaw
#' @author Jack Davison
#' @references
#'
#' Pekney, N. J., Davidson, C. I., Zhou, L., & Hopke, P. K. (2006). Application
#' of PSCF and CPF to PMF-Modeled Sources of PM 2.5 in Pittsburgh. Aerosol
#' Science and Technology, 40(10), 952-961.
#'
#' Seibert, P., Kromp-Kolb, H., Baltensperger, U., Jost, D., 1994. Trajectory
#' analysis of high-alpine air pollution data. NATO Challenges of Modern Society
#' 18, 595-595.
#'
#' Xie, Y., & Berkowitz, C. M. (2007). The use of conditional probability
#' functions and potential source contribution functions to identify source
#' regions and advection pathways of hydrocarbon emissions in Houston, Texas.
#' Atmospheric Environment, 41(28), 5831-5847.
#' @examples
#'
#' # show a simple case with no pollutant i.e. just the trajectories
#' # let's check to see where the trajectories were coming from when
#' # Heathrow Airport was closed due to the Icelandic volcanic eruption
#' # 15--21 April 2010.
#' # import trajectories for London and plot
#' \dontrun{
#' lond <- importTraj("london", 2010)
#' }
#' # more examples to follow linking with concentration measurements...
#'
#' # import some measurements from KC1 - London
#' \dontrun{
#' kc1 <- importAURN("kc1", year = 2010)
#' # now merge with trajectory data by 'date'
#' lond <- merge(lond, kc1, by = "date")
#'
#' # trajectory plot, no smoothing - and limit lat/lon area of interest
#' # use PSCF
#' trajLevel(subset(lond, lat > 40 & lat < 70 & lon > -20 & lon < 20),
#'   pollutant = "pm10", statistic = "pscf"
#' )
#'
#' # can smooth surface, suing CWT approach:
#' trajLevel(subset(lond, lat > 40 & lat < 70 & lon > -20 & lon < 20),
#'   pollutant = "pm2.5", statistic = "cwt", smooth = TRUE
#' )
#'
#' # plot by season:
#' trajLevel(subset(lond, lat > 40 & lat < 70 & lon > -20 & lon < 20),
#'   pollutant = "pm2.5",
#'   statistic = "pscf", type = "season"
#' )
#' }
trajLevel <- function(
  mydata,
  lon = "lon",
  lat = "lat",
  pollutant = "height",
  type = "default",
  smooth = FALSE,
  statistic = "frequency",
  percentile = 90,
  lon.inc = 1.0,
  lat.inc = lon.inc,
  min.bin = 1,
  .combine = NULL,
  sigma = 1.5,
  cols = "default",
  crs = 4326,
  map = TRUE,
  map.fill = TRUE,
  map.cols = "grey40",
  map.border = "black",
  map.alpha = 0.3,
  map.lwd = 1,
  map.lty = 1,
  grid.col = "deepskyblue",
  grid.nx = 9,
  grid.ny = grid.nx,
  origin = TRUE,
  key = TRUE,
  key.position = "right",
  key.columns = NULL,
  auto.text = TRUE,
  plot = TRUE,
  ...
) {
  rlang::check_installed("sf")

  # checks
  statistic <- tolower(statistic)
  rlang::arg_match(
    statistic,
    c("frequency", "hexbin", "difference", "pscf", "cwt", "sqtba")
  )

  # check that SQTBA is not being used with a type
  if (statistic == "sqtba" && type != "default") {
    cli::cli_abort("{.arg type} not available when {.arg statistic} = 'SQTBA'.")
  }

  # variables needed in trajectory plots
  vars <- c("date", "lat", "lon", "hour.inc", pollutant)

  # to combine the effects of several receptors
  if (!is.null(.combine)) {
    vars <- c(vars, .combine)
  }

  # check data and set up
  mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)

  # extra.args
  extra.args <- rlang::list2(...)

  extra.args$ylab <- extra.args$ylab %||% ""
  extra.args$xlab <- extra.args$xlab %||% ""
  extra.args$main <- extra.args$main %||% ""
  extra.args$border <- extra.args$border %||% NA
  extra.args$key.footer <- extra.args$key.footer %||% NULL

  if ("method" %in% names(extra.args)) {
    cli::cli_warn(
      "{.arg method} is no longer supported in {.fun openair::trajLevel}, please use {.arg statistic}."
    )
    if (extra.args$method == "hexbin") {
      cli::cli_warn("Setting {.arg statistic} to 'hexbin'. ")
      statistic <- "hexbin"
    }
  }

  if (!"key.header" %in% names(extra.args)) {
    header <- switch(
      statistic,
      "frequency" = "% trajectories",
      "hexbin" = "Counts",
      "pscf" = "PSCF \nprobability",
      "sqtba" = paste0("SQTBA \n", pollutant),
      "difference" = paste0(
        "gridded differences\n(",
        percentile,
        "th percentile)"
      ),
      NULL
    )

    # special override for normalised SQTBA
    if (statistic == "sqtba" && !is.null(.combine)) {
      header <- paste0("SQTBA \n(Normalised)\n", pollutant)
    }

    extra.args$key.header <- header
  }

  # cut data by type
  mydata <- cutData(mydata, type, ...)

  # location of receptor for map projection, used to show location on maps
  sf_origins <- mydata |>
    dplyr::filter(hour.inc == 0) |>
    dplyr::slice_head(n = 1, by = c("lat", "lon", type)) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

  # bin data
  if (statistic != "sqtba") {
    mydata$ygrid <- round_any(mydata[[lat]], lat.inc)
    mydata$xgrid <- round_any(mydata[[lon]], lon.inc)
  } else {
    mydata$ygrid <- mydata[[lat]]
    mydata$xgrid <- mydata[[lon]]
  }

  if (statistic == "sqtba") {
    mydata <-
      dplyr::select(
        mydata,
        dplyr::any_of(c(
          "date",
          "lon",
          "lat",
          "hour.inc",
          type,
          pollutant,
          .combine
        ))
      )
  } else {
    mydata <- mydata[, c("date", "xgrid", "ygrid", "hour.inc", type, pollutant)]
  }

  # grouping variables
  vars <- c("xgrid", "ygrid", type)

  # plot mean concentration - CWT method
  if (statistic == "cwt") {
    ## calculate the mean of points in each cell
    mydata <- mydata |>
      dplyr::group_by(dplyr::across(vars)) |>
      dplyr::summarise(
        N = length(date),
        date = head(date, 1),
        count = mean(.data[[pollutant]], na.rm = TRUE)
      )

    mydata[[pollutant]] <- mydata$count

    # adjust at edges

    id <- which(mydata$N > 20 & mydata$N <= 80)
    mydata[id, pollutant] <- mydata[id, pollutant] * 0.7

    id <- which(mydata$N > 10 & mydata$N <= 20)
    mydata[id, pollutant] <- mydata[id, pollutant] * 0.42

    id <- which(mydata$N <= 10)
    mydata[id, pollutant] <- mydata[id, pollutant] * 0.05
    attr(mydata$date, "tzone") <- "GMT" ## avoid warning messages about TZ

    # prep output data
    out_data <- dplyr::ungroup(mydata) |>
      dplyr::select(-dplyr::any_of(c("date", "count"))) |>
      dplyr::rename("count" = "N") |>
      dplyr::mutate(
        statistic = statistic,
        .before = dplyr::everything()
      )

    if (smooth) {
      out_data <-
        mapType(
          out_data,
          type = type,
          \(x) smooth_trajgrid(x, z = pollutant),
          .include_default = TRUE
        )
    }
  }

  # plot trajectory frequencies
  if (statistic %in% c("frequency", "hexbin")) {
    # needed for hexbinning
    original_data <- mydata

    # count % of times a cell contains a trajectory point
    # need date for later use of type
    mydata <- mydata |>
      summarise(
        count = length(.data$date),
        date = dplyr::first(.data$date),
        .by = dplyr::all_of(vars)
      ) |>
      dplyr::mutate(
        {{ pollutant }} := 100 * .data$count / max(.data$count)
      )

    if (smooth) {
      mydata <-
        mapType(
          mydata,
          type = type,
          \(x) smooth_trajgrid(x, z = pollutant),
          .include_default = TRUE
        )
    }

    mydata <-
      dplyr::mutate(
        mydata,
        cuts = cut(
          .data[[pollutant]],
          breaks = c(0, 1, 5, 10, 25, 100),
          labels = c("0 to 1", "1 to 5", "5 to 10", "10 to 25", "25 to 100"),
          include.lowest = TRUE
        )
      )

    # prep output data
    out_data <- mydata |>
      dplyr::select(-dplyr::any_of(c("date"))) |>
      dplyr::mutate(
        statistic = statistic,
        .before = dplyr::everything()
      )
  }

  ## Poential Source Contribution Function
  if (statistic == "pscf") {
    ## high percentile
    Q90 <- quantile(mydata[[pollutant]], probs = percentile / 100, na.rm = TRUE)

    ## calculate the proportion of points in cell with value > Q90
    mydata <- mydata |>
      group_by(across(vars)) |>
      summarise(
        N = length(date),
        date = head(date, 1),
        count = length(which(.data[[pollutant]] > Q90)) / N
      )

    mydata[[pollutant]] <- mydata$count

    ## ## adjust at edges
    n <- mean(mydata$N)
    id <- which(mydata$N > n & mydata$N <= 2 * n)
    mydata[id, pollutant] <- mydata[id, pollutant] * 0.75

    id <- which(mydata$N > (n / 2) & mydata$N <= n)
    mydata[id, pollutant] <- mydata[id, pollutant] * 0.5

    id <- which(mydata$N <= (n / 2))
    mydata[id, pollutant] <- mydata[id, pollutant] * 0.15

    if (smooth) {
      mydata <-
        mapType(
          mydata,
          type = type,
          \(x) smooth_trajgrid(x, z = pollutant),
          .include_default = TRUE
        )
    }

    # prep output data
    out_data <- dplyr::ungroup(mydata) |>
      dplyr::select(-dplyr::any_of(c("date", "count"))) |>
      dplyr::rename("count" = "N") |>
      dplyr::mutate(
        statistic = statistic,
        percentile = percentile,
        .before = dplyr::everything()
      )
  }

  # simplified quantitative transport bias analysis  ------------------------

  if (tolower(statistic) == "sqtba") {
    # calculate sigma
    mydata <- mydata |>
      mutate(sigma = sigma * abs(hour.inc)) |>
      drop_na({{ pollutant }})

    # receptor grid
    # use trajectory data to determine grid size - don't go to extremes
    r_grid <- expand_grid(
      lat = seq(
        round(quantile(mydata$lat, probs = 0.002)),
        round(quantile(mydata$lat, probs = 0.998)),
        by = lat.inc
      ),
      lon = seq(
        round(quantile(mydata$lon, probs = 0.002)),
        round(quantile(mydata$lon, probs = 0.998)),
        by = lon.inc
      )
    ) |>
      as.matrix()

    # just run
    if (is.null(.combine)) {
      mydata <- calc_sqtba(mydata, r_grid, pollutant, min.bin) |>
        rename({{ pollutant }} := SQTBA)
    } else {
      # process by site, normalise contributions by default
      mydata <- mydata |>
        group_by(across(.combine)) |>
        group_modify(~ calc_sqtba(.x, r_grid, pollutant, min.bin)) |>
        mutate(SQTBA_norm = SQTBA / mean(SQTBA)) |>
        group_by(ygrid, xgrid) |>
        summarise(
          SQTBA = mean(SQTBA),
          SQTBA_norm = mean(SQTBA_norm)
        ) |>
        ungroup() |>
        mutate(SQTBA_norm = SQTBA_norm * mean(SQTBA)) |>
        rename({{ pollutant }} := SQTBA_norm)
    }

    # prep output data
    names(mydata)[names(mydata) == "n"] <- "count"

    # set include_default to be FALSE as sqtba doesn't use type
    if (smooth) {
      mydata <-
        mapType(
          mydata,
          type = type,
          \(x) smooth_trajgrid(x, z = pollutant),
          .include_default = FALSE
        )
    }

    out_data <- dplyr::ungroup(mydata) |>
      dplyr::select(
        -dplyr::any_of(c("lat_rnd", "lon_rnd", "Q", "Q_c", "SQTBA"))
      ) |>
      dplyr::relocate(dplyr::any_of("count"), .before = pollutant) |>
      dplyr::relocate("xgrid", .before = "ygrid") |>
      dplyr::mutate(
        statistic = statistic,
        sigma = sigma,
        combine = .combine,
        .before = dplyr::everything()
      )
  }

  ## plot trajectory frequency differences e.g. top 10% concs cf. mean
  if (statistic == "difference") {
    ## calculate percentage of points for all data

    base <- mydata |>
      group_by(across(vars)) |>
      summarise(count = length(date), date = head(date, 1))

    base[[pollutant]] <- 100 * base$count / max(base$count)

    ## high percentile
    Q90 <- quantile(mydata[[pollutant]], probs = percentile / 100, na.rm = TRUE)

    ## calculate percentage of points for high data
    high <- mydata |>
      group_by(across(vars)) |>
      summarise(
        N = length(date),
        date = head(date, 1),
        count = length(which(.data[[pollutant]] > Q90))
      )

    high[[pollutant]] <- 100 * high$count / max(high$count)

    ## calculate percentage absolute difference
    mydata <- base
    mydata[[pollutant]] <- high[[pollutant]] - mydata[[pollutant]]

    if (smooth) {
      mydata <-
        mapType(
          mydata,
          type = type,
          \(x) smooth_trajgrid(x, z = pollutant),
          .include_default = TRUE
        )
    }

    mydata <-
      dplyr::mutate(
        mydata,
        cuts = cut(
          .data[[pollutant]],
          breaks = c(-Inf, -10, -5, -1, 1, 5, 10, Inf),
          labels = c(
            "<-10",
            "-10 to -5",
            "-5 to -1",
            "-1 to 1",
            "1 to 5",
            "5 to 10",
            ">10"
          ),
          include.lowest = TRUE
        )
      )

    ## select only if > min.bin points in grid cell
    mydata <- subset(mydata, count >= min.bin)

    # prep output data
    out_data <- dplyr::ungroup(mydata) |>
      dplyr::select(-dplyr::any_of(c("date"))) |>
      dplyr::mutate(
        statistic = statistic,
        percentile = percentile,
        .before = dplyr::everything()
      )
  }

  # recalculate lon.inc/lat.inc based on smoothed data
  if (smooth) {
    xtest <- dplyr::filter(out_data, .data$ygrid == .data$ygrid[[1]]) |>
      dplyr::arrange(.data$xgrid)
    xtest <- xtest$xgrid - dplyr::lag(xtest$xgrid)
    lon.inc <- unique(xtest[!is.na(xtest)])[[1]]

    ytest <- dplyr::filter(out_data, .data$xgrid == .data$xgrid[[1]]) |>
      dplyr::arrange(.data$ygrid)
    ytest <- ytest$ygrid - dplyr::lag(ytest$ygrid)
    lat.inc <- unique(ytest[!is.na(ytest)])[[1]]
  }

  # to allow for basemaps to be multicoloured, we need to work out the number of
  # panels in the plot
  n_types <- c()
  for (i in type) {
    n_types <- c(n_types, length(levels(mydata[[i]])))
  }
  n_types <- purrr::reduce(n_types, .f = `*`)

  # turn data into spatial object
  out_data_sf <-
    out_data |>
    dplyr::mutate(
      geometry = purrr::map2(
        .data$xgrid,
        .data$ygrid,
        ~ {
          # build a square polygon around each grid centre
          sf::st_polygon(list(rbind(
            c(.x - lon.inc / 2, .y - lat.inc / 2),
            c(.x + lon.inc / 2, .y - lat.inc / 2),
            c(.x + lon.inc / 2, .y + lat.inc / 2),
            c(.x - lon.inc / 2, .y + lat.inc / 2),
            c(.x - lon.inc / 2, .y - lat.inc / 2)
          )))
        }
      )
    ) |>
    sf::st_as_sf(crs = 4326) |>
    # needs to stay as lat/lng if using hexbin
    sf::st_transform(crs = ifelse(statistic == "hexbin", 4326, crs))

  # get bbox - axis limits
  bbox <- sf::st_bbox(out_data_sf)
  extra.args$xlim <- extra.args$xilm %||% unname(bbox[c(1, 3)])
  extra.args$ylim <- extra.args$ylim %||% unname(bbox[c(2, 4)])

  # base plot & themes
  thePlot <- ggplot2::ggplot(data = out_data_sf) +
    theme_openair_sf(key.position, grid.col = grid.col) +
    set_extra_fontsize(extra.args) +
    get_facet(
      type,
      extra.args,
      scales = "fixed",
      drop = FALSE,
      auto.text = auto.text
    ) +
    ggplot2::labs(
      x = quickText(extra.args$xlab, auto.text),
      y = quickText(extra.args$ylab, auto.text),
      title = quickText(extra.args$main, auto.text),
      fill = quickText(
        paste(
          extra.args$key.header,
          extra.args$key.footer,
          sep = ifelse(key.position %in% c("top", "bottom"), " ", "\n")
        ),
        auto.text = auto.text
      )
    )

  # add map if requested
  if (map) {
    thePlot <- thePlot +
      layer_worldmap(
        crs,
        n_maps = n_types,
        map.fill = map.fill,
        map.cols = map.cols,
        map.border = map.border,
        map.alpha = map.alpha,
        map.lwd = map.lwd,
        map.lty = map.lty
      )
  }

  # each statistic needs different handling

  # discrete statistics
  if (statistic %in% c("frequency", "difference")) {
    thePlot <-
      thePlot +
      ggplot2::geom_sf(
        data = out_data_sf,
        ggplot2::aes(fill = .data$cuts),
        colour = extra.args$border,
        show.legend = TRUE
      ) +
      ggplot2::scale_fill_manual(
        values = openair::openColours(
          cols,
          n = dplyr::n_distinct(levels(out_data_sf$cuts))
        ),
        drop = FALSE
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(
          reverse = key.position %in% c("left", "right"),
          theme = ggplot2::theme(
            legend.title.position = ifelse(
              key.position %in% c("left", "right"),
              "top",
              key.position
            ),
            legend.text.position = key.position
          ),
          ncol = if (missing(key.columns)) {
            if (key.position %in% c("left", "right")) {
              NULL
            } else {
              dplyr::n_distinct(levels(out_data_sf$cuts))
            }
          } else {
            key.columns
          }
        )
      )
  }

  # continuous statistics
  if (statistic %in% c("pscf", "cwt", "sqtba")) {
    thePlot <-
      thePlot +
      ggplot2::geom_sf(
        data = out_data_sf,
        ggplot2::aes(fill = .data[[pollutant]]),
        colour = extra.args$border,
        show.legend = TRUE
      ) +
      ggplot2::scale_fill_gradientn(
        colours = openair::openColours(cols),
        oob = scales::oob_squish,
        na.value = NA
      )
  }

  # scales & guides
  if (statistic != "hexbin") {
    if (map) {
      thePlot <- thePlot +
        layer_worldmap(
          crs,
          n_maps = n_types,
          map.fill = FALSE,
          map.cols = map.cols,
          map.border = map.border,
          map.alpha = 0,
          map.lwd = map.lwd,
          map.lty = map.lty
        )
    }

    if (origin) {
      thePlot <- thePlot +
        ggplot2::geom_sf(data = sf::st_transform(sf_origins, crs = crs))
    }

    thePlot <-
      thePlot +
      ggplot2::coord_sf(
        xlim = extra.args$xlim,
        ylim = extra.args$ylim,
        default_crs = crs,
        crs = crs
      ) +
      ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(grid.nx)) +
      ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(grid.ny))
  }

  # hexbin needs separate handling - everything needs to be set as lat/lng as
  # ggplot2 needs to transform everything simultaneously
  if (statistic == "hexbin") {
    thePlot <-
      thePlot +
      ggplot2::geom_hex(
        data = original_data,
        ggplot2::aes(
          x = .data$xgrid,
          y = .data$ygrid,
          alpha = ifelse(ggplot2::after_stat(.data$count) < min.bin, 0, 1)
        ),
        binwidth = min(c(lat.inc * 1.5, lon.inc * 1.5))
      )

    if (map) {
      thePlot <- thePlot +
        layer_worldmap(
          crs = 4326,
          n_maps = n_types,
          map.fill = FALSE,
          map.cols = map.cols,
          map.border = map.border,
          map.alpha = map.alpha,
          map.lwd = map.lwd,
          map.lty = map.lty
        )
    }

    if (origin) {
      thePlot <- thePlot +
        ggplot2::geom_sf(data = sf_origins)
    }

    thePlot <-
      thePlot +
      ggplot2::coord_sf(
        xlim = extra.args$xlim,
        ylim = extra.args$ylim,
        default_crs = 4326,
        crs = crs
      ) +
      ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(grid.nx)) +
      ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(grid.ny)) +
      ggplot2::scale_alpha_identity() +
      ggplot2::scale_fill_stepsn(
        transform = scales::transform_log10(),
        colors = openColours(cols),
        n.breaks = 15,
        limits = c(min.bin, NA)
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_colorsteps(show.limits = TRUE)
      )
  }

  # make legends full width
  if (key.position %in% c("left", "right")) {
    thePlot <- thePlot +
      ggplot2::theme(
        legend.key.height = ggplot2::unit(1, "null"),
        legend.key.spacing.y = ggplot2::unit(0, "cm")
      )
  }
  if (key.position %in% c("top", "bottom")) {
    thePlot <- thePlot +
      ggplot2::theme(
        legend.key.width = ggplot2::unit(1, "null"),
        legend.key.spacing.x = ggplot2::unit(0, "cm")
      )
  }

  if (plot) {
    plot(thePlot)
  }

  output <-
    list(
      plot = thePlot,
      data = out_data,
      call = match.call()
    )
  class(output) <- "openair"

  invisible(output)
}

# SQTBA functions
calc_sqtba <- function(mydata, r_grid, pollutant, min.bin) {
  # PREPARE GRID
  # Convert matrix to tibble, add IDs, and pre-calc radians
  grid_df <- dplyr::as_tibble(r_grid) |>
    dplyr::mutate(
      grid_id = row_number(),
      g_lat_rad = .data$lat * pi / 180,
      g_lon_rad = .data$lon * pi / 180,
      # Create integer "buckets" for joining
      lat_rnd = round(.data$lat),
      lon_rnd = round(.data$lon)
    )

  # PREPARE TRAJECTORIES
  # Filter valid hours, calc weights, and pre-calc radians
  traj_df <- mydata |>
    # Original logic: points 2 to max(hour.inc)
    dplyr::filter(abs(.data$hour.inc) > 1) |>
    dplyr::mutate(
      weight = 1 / dplyr::n(),
      t_lat_rad = .data$lat * pi / 180,
      t_lon_rad = .data$lon * pi / 180,
      # Create integer "buckets"
      lat_base = round(.data$lat),
      lon_base = round(.data$lon),
      .by = "date"
    )

  # 3. CREATE NEIGHBORHOOD LOOKUP (The Magic Step)
  # Instead of checking the whole grid, we look at the 9x9 integer square around a point.
  # We create offsets (-4 to +4) to bridge the gap between buckets.
  offsets <- tidyr::crossing(
    lat_off = -4:4,
    lon_off = -4:4
  )

  # Expand trajectories: Duplicate rows for every potential neighbor bucket
  # (This temporarily increases row count but makes the join fast)
  traj_expanded <- traj_df |>
    dplyr::cross_join(offsets) |>
    dplyr::mutate(
      search_lat = .data$lat_base + .data$lat_off,
      search_lon = .data$lon_base + .data$lon_off
    )

  # JOIN & FILTER
  # Join trajectory buckets to grid buckets
  pairs <- traj_expanded |>
    dplyr::inner_join(
      grid_df,
      by = c("search_lat" = "lat_rnd", "search_lon" = "lon_rnd")
    ) |>
    # Now strictly filter the exact window (original logic: +/- 4 degrees)
    dplyr::filter(
      .data$lat.y > .data$lat.x - 4,
      .data$lat.y < .data$lat.x + 4,
      .data$lon.y > .data$lon.x - 4,
      .data$lon.y < .data$lon.x + 4
    )

  # VECTORIZED MATHS
  # Calculate distance and Q for all pairs at once
  results <- pairs |>
    dplyr::mutate(
      # Vectorized Haversine/ACos distance
      dist_km = (acos(
        sin(.data$t_lat_rad) *
          sin(.data$g_lat_rad) +
          cos(.data$t_lat_rad) *
            cos(.data$g_lat_rad) *
            cos(.data$g_lon_rad - .data$t_lon_rad) -
          1e-7
      ) *
        6378.137),

      # Gaussian Plume
      Q_val = (1 / sigma^2) * exp(-0.5 * (.data$dist_km / sigma)^2),

      # Apply trajectory weight immediately
      Q_weighted = .data$Q_val * .data$weight,
      Qc_weighted = .data$Q_val * .data[[pollutant]] * .data$weight
    ) |>
    # Aggregate by Grid Cell
    dplyr::summarise(
      Q = sum(.data$Q_weighted, na.rm = TRUE),
      Q_c = sum(.data$Qc_weighted, na.rm = TRUE),
      .by = dplyr::all_of(c("grid_id", "lat.y", "lon.y"))
    ) |>
    dplyr::mutate(SQTBA = .data$Q_c / .data$Q) |>
    dplyr::rename(lat = "lat.y", lon = "lon.y")

  # MERGE BACK TO FULL GRID & FILTER
  # Ensure all original grid points exist (even if 0)
  final_output <- grid_df |>
    dplyr::select("lat", "lon") |>
    dplyr::left_join(results, by = c("lat", "lon")) |>
    dplyr::mutate(
      SQTBA = dplyr::coalesce(.data$SQTBA, 0),
      lat_rnd = round(.data$lat),
      lon_rnd = round(.data$lon)
    )

  # MIN.BIN FILTER
  grid_counts <- mydata |>
    dplyr::mutate(lat_rnd = round(.data$lat), lon_rnd = round(.data$lon)) |>
    dplyr::count(.data$lat_rnd, .data$lon_rnd) |>
    dplyr::filter(.data$n >= min.bin)

  final_data <- final_output |>
    dplyr::inner_join(grid_counts, by = c("lat_rnd", "lon_rnd")) |>
    dplyr::rename(xgrid = "lon", ygrid = "lat")

  return(final_data)
}

smooth_trajgrid <- function(mydata, z, k = 50, dist = 0.05) {
  myform <- stats::formula(paste0(z, "^0.5 ~ s(xgrid, ygrid, k = ", k, ")"))

  res <- 101
  Mgam <- mgcv::gam(myform, data = mydata)

  new.data <- expand.grid(
    xgrid = seq(min(mydata$xgrid), max(mydata$xgrid), length = res),
    ygrid = seq(min(mydata$ygrid), max(mydata$ygrid), length = res)
  )

  pred <- mgcv::predict.gam(Mgam, newdata = new.data)
  pred <- as.vector(pred)^2

  new.data[, z] <- pred

  # exlcude too far
  # exclude predictions too far from data (from mgcv)
  x <- seq(min(mydata$xgrid), max(mydata$xgrid), length = res)
  y <- seq(min(mydata$ygrid), max(mydata$ygrid), length = res)

  wsp <- rep(x, res)
  wdp <- rep(y, rep(res, res))

  ## data with gaps caused by min.bin
  all.data <-
    stats::na.omit(data.frame(xgrid = mydata$xgrid, ygrid = mydata$ygrid, z))

  ind <- with(
    all.data,
    mgcv::exclude.too.far(wsp, wdp, mydata$xgrid, mydata$ygrid, dist = dist)
  )

  new.data[ind, z] <- NA

  new.data <- tidyr::drop_na(new.data)

  dplyr::tibble(new.data)
}
