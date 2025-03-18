#' Polar plots considering changes in concentrations between two time periods
#'
#' This function provides a way of showing the differences in concentrations
#' between two time periods as a polar plot. There are several uses of this
#' function, but the most common will be to see how source(s) may have changed
#' between two periods.
#'
#' While the function is primarily intended to compare two time periods at the
#' same location, it can be used for any two data sets that contain the same
#' pollutant. For example, data from two sites that are close to one another, or
#' two co-located instruments.
#'
#' The analysis works by calculating the polar plot surface for the `before` and
#' `after` periods and then subtracting the `before` surface from the `after`
#' surface.
#'
#' @inheritParams polarPlot
#' @param before,after Data frames representing the "before" and "after" cases.
#'   See [polarPlot()] for details of different input requirements.
#' @inheritDotParams polarPlot -mydata -pollutant -x -limits -plot -type
#' @family polar directional analysis functions
#' @return an [openair][openair-package] plot.
#' @export
#'
#' @examples
#' \dontrun{
#' before_data <- selectByDate(mydata, year = 2002)
#' after_data <- selectByDate(mydata, year = 2003)
#'
#' polarDiff(before_data, after_data, pollutant = "no2")
#'
#' # with some options
#' polarDiff(before_data, after_data, pollutant = "no2", cols = "RdYlBu", limits = c(-20, 20))
#' }
polarDiff <- function(before,
                      after,
                      pollutant = "nox",
                      type = "default",
                      x = "ws",
                      limits = NULL,
                      auto.text = TRUE,
                      plot = TRUE,
                      ...) {
  if (is.null(limits)) {
    limits <- NA
  }
  
  # extra args setup
  Args <- list(...)
  
  # variables needed, check for York regression where x and y error needed
  if (all(c("x_error", "y_error") %in% names(Args))) {
    vars <- c(x, "wd", pollutant, Args$x_error, Args$y_error)
  } else {
    vars <- c(x, "wd", pollutant)
  }
  
  # check variables exists
  before <- cutData(before, type = type) %>%
    checkPrep(vars, type, remove.calm = FALSE) %>%
    dplyr::mutate(period = "before")
  
  after <- cutData(after, type = type) %>%
    checkPrep(vars, type, remove.calm = FALSE) %>%
    dplyr::mutate(period = "after")
  
  if (type == "default") {
    before$default <- "default"
    after$default <- "default"
  }
  
  # bind 'before' and 'after' into a single dataframe
  all_data <- dplyr::bind_rows(before, after)
  
  # need to pass on use limits only to final plot
  Args$new_limits <- limits
  Args$limits <- NA
  
  # map over "type" - get difference between 'before' and 'after'
  polar_data <-
    purrr::map(
      .x = unique(c(before[[type]], after[[type]])),
      .f = function(i) {
        theData <- all_data[all_data[[type]] == i, ]
        
        polar_plt <- polarPlot(
          theData,
          pollutant = pollutant,
          x = x,
          type = "period",
          plot = FALSE,
          ...
        )
        
        if (length(pollutant > 1)) {
          pollutant <- paste(pollutant, collapse = "_")
        }
        
        polar_data <-
          polar_plt$data %>%
          tidyr::pivot_wider(
            id_cols = u:v,
            names_from = period,
            values_from = z
          ) %>%
          dplyr::mutate({{ pollutant }} := after - before, {{ x }} := (u^2 + v^2)^0.5, wd = 180 * atan2(u, v) / pi, wd = ifelse(wd < 0, wd + 360, wd))
        
        polar_data[[type]] <- theData[[type]][1]
        
        return(polar_data)
      }
    ) %>%
    purrr::list_rbind()
  
  # other arguments
  # colours
  Args$cols <- if ("cols" %in% names(Args)) {
    Args$cols
  } else {
    c(
      "#002F70",
      "#3167BB",
      "#879FDB",
      "#C8D2F1",
      "#F6F6F6",
      "#F4C8C8",
      "#DA8A8B",
      "#AE4647",
      "#5F1415"
    )
  }
  
  # limits
  Args$limits <- if (is.na(Args$new_limits[1])) {
    lims_adj <- pretty(seq(0, max(abs(polar_data[[pollutant]]), na.rm = TRUE), 5))
    lims_adj <- lims_adj[length(lims_adj) - 1]
    c(-lims_adj, lims_adj)
  } else {
    Args$new_limits
  }
  
  # other useful args
  Args$key <- Args$key %||% TRUE
  Args$par.settings <- Args$par.settings %||% list(axis.line = list(col = "black"))
  Args$alpha <- Args$alpha %||% 1
  Args$key.header <- Args$key.header %||% "Difference"
  Args$key.footer <- Args$key.footer %||% paste(pollutant, collapse = ", ")
  Args$main <- Args$main %||% ""
  
  if (type == "default") {
    plt <-
      polarPlot(
        polar_data,
        pollutant = pollutant,
        x = x,
        plot = plot,
        cols = Args$cols,
        limits = Args$limits,
        force.positive = FALSE,
        alpha = Args$alpha,
        key = Args$key,
        par.settings = Args$par.settings,
        key.header = Args$key.header,
        key.footer = Args$key.footer,
        main = Args$main,
        auto.text = auto.text
      )
  } else {
    # stop polarPlot wanting 'date' for date types
    names(polar_data)[names(polar_data) == type] <- "finaltype"
    
    # final plot
    plt <-
      polarPlot(
        polar_data,
        pollutant = pollutant,
        x = x,
        type = "finaltype",
        plot = plot,
        cols = Args$cols,
        limits = Args$limits,
        force.positive = FALSE,
        alpha = Args$alpha,
        key = Args$key,
        par.settings = Args$par.settings,
        key.header = Args$key.header,
        key.footer = Args$key.footer,
        main = Args$main,
        auto.text = auto.text
      )
  }
  
  # return
  output <- list(plot = plt$plot,
                 data = polar_data,
                 call = match.call())
  
  invisible(output)
}
