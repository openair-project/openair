#' @noRd
openair <- function(x) {
  class(x) <- "openair"
  x
}

#' @noRd
is.openair <- function(x, full.test = TRUE, ...) {
  # standard test
  output <- methods::is(x)[1] == "openair"
  # full.test
  if (full.test && output) {
    output <- all(c("plot", "data", "call") %in% names(x))
  }
  # output
  output
}

#' @noRd
openairApply <- function(
  object,
  fun = summary,
  subset = "all",
  ...,
  fun.name = deparse(substitute(fun))
) {
  if (!is.openair(object)) {
    return(invisible(NULL))
  }
  if (grepl("[(]", fun.name)) {
    fun.name <- "function"
  }

  # test for subsets
  check <- "subsets" %in% names(object$data)
  if (check) {
    test <- object$data$subsets
  }

  # results are returned (not printed) so they print only when not assigned
  if (!check) {
    return(fun(object$data, ...))
  }

  if (is.null(subset) || any(subset == "all")) {
    temp <- test
  } else {
    temp <- subset[subset %in% test]
    if (length(temp) < 1) {
      cli::cli_abort(
        c(
          "x" = "In {.fun {fun.name}}: requested subset(s) not found.",
          "i" = "Suggest one (or more) of {.field {test}}."
        ),
        call = NULL
      )
    }

    if (length(temp) < length(subset)) {
      cli::cli_warn(
        c(
          "!" = "In {.fun {fun.name}}: some requested subset(s) not found, so ignored.",
          "i" = "Ignored subset(s): {.field {setdiff(subset, temp)}}.",
          "i" = "Available subset(s): {.field {test}}."
        ),
        call = NULL
      )
    }
  }

  sapply(
    temp,
    function(y) {
      fun(object$data[[y]], ...)
    },
    simplify = FALSE,
    USE.NAMES = TRUE
  )
}

#' @exportS3Method base::summary
summary.openair <- function(object, subset = "all", ...) {
  openairApply(
    object,
    fun = summary,
    subset = subset,
    ...,
    fun.name = "summary"
  )
}

#' @exportS3Method utils::head
head.openair <- function(x, subset = "all", ...) {
  openairApply(x, fun = utils::head, subset = subset, ..., fun.name = "head")
}

#' @exportS3Method utils::tail
tail.openair <- function(x, subset = "all", ...) {
  openairApply(x, fun = utils::tail, subset = subset, ..., fun.name = "tail")
}

#' @exportS3Method graphics::plot
plot.openair <- function(x, subset = "all", silent = TRUE, ...) {
  if (!is.openair(x)) {
    return(invisible(NULL))
  }
  if (!silent) {
    cli::cli_inform(c(
      "{.pkg openair} object created by:",
      " " = "{.code {deparse(x$call)}}"
    ))
  }

  test <- x$plot$subsets

  # No subsets — just print the main plot
  if (is.null(test)) {
    if (!is.null(subset) && !any(subset == "all")) {
      cli::cli_warn(c(
        "{.arg subset} option ignored.",
        "i" = "Subset requested from {.pkg openair} object without data subsets."
      ))
    }
    return(plot(x$plot, ...))
  }

  # "all" or NULL — print the assembled patchwork object
  if (is.null(subset) || any(subset == "all")) {
    if (is.null(x$main.plot)) {
      cli::cli_abort(c(
        "Bad {.pkg openair} object structure.",
        "i" = "Please contact the {.pkg openair} maintainers if this object is valid."
      ))
    }
    return(plot(x$main.plot, ...))
  }

  # Subset requested — validate and resolve to a single subset
  temp <- subset[subset %in% test]
  if (length(temp) < 1) {
    cli::cli_abort(c(
      "Requested {.arg subset} not found.",
      "i" = "Suggest one of: {.val {test}}."
    ))
  }
  if (length(temp) < length(subset)) {
    cli::cli_warn(
      "Multiple subsets requested and some not found, using first valid ({.val {temp[1]}})."
    )
  } else if (length(temp) > 1) {
    cli::cli_warn(
      "Multiple subsets requested, using first valid ({.val {temp[1]}})."
    )
  }

  return(plot(x$plot[[temp[1]]], ...))
}

#' @exportS3Method base::print
print.openair <- function(x, silent = FALSE, plot = TRUE, ...) {
  if (!is.openair(x)) {
    return(invisible(NULL))
  }
  # must have call, data and plot elements

  if (!silent) {
    # print methods write to stdout, so format the cli output and cat() it
    cat(cli::cli_format_method(describe_openair(x)), sep = "\n")
  }

  if (plot) {
    plot(x, silent = TRUE, ...)
  }

  invisible(x)
}

# cli description of an openair object, used by print.openair()
describe_openair <- function(x) {
  cli::cli_h1("{.pkg openair} object")
  cli::cli_text("Created by {.code {deparse1(x$call)}}")
  cli::cli_text("")
  describe_openair_element(x$data, "data", "data frame")
  describe_openair_element(x$plot, "plot", "plot")

  other <- setdiff(names(x), c("data", "call", "plot"))
  if (length(other) > 0) {
    cli::cli_text("{.strong Other:} {.field {paste0('$', other)}}")
  }
}

# describe the `data` or `plot` element of an openair object, listing any
# subsets
describe_openair_element <- function(el, name, what) {
  subsets <- openair_subsets(el)
  label <- paste0(toupper(substr(name, 1, 1)), substring(name, 2))

  if (is.null(subsets)) {
    cli::cli_text("{.strong {label}:} {.field {paste0('$', name)}}")
  } else {
    n <- length(subsets)
    cli::cli_text("{.strong {label}:} {n} {what}{cli::qty(n)}{?s}")
    cli::cli_ul(paste0("{.field $", name, "$", subsets, "}"))
  }
}

#' @exportS3Method grid::grid.draw
grid.draw.openair <- function(x, recording = TRUE) {
  # draw what `plot()` would show by default; also allows `ggplot2::ggsave()`
  # to save openair objects
  the_plot <- if (is.null(x$plot$subsets)) x$plot else x$main.plot
  grid::grid.draw(the_plot, recording = recording)
}

# the names of the subsets of an openair object's `data` or `plot` element, or
# NULL if there are none; avoids `$` on tibbles, which warns for unknown columns
openair_subsets <- function(x) {
  if ("subsets" %in% names(x)) x$subsets else NULL
}

# get a single data frame from an openair object, for as.data.frame() and
# as_tibble()
openair_data <- function(x, subset, call = rlang::caller_env()) {
  subsets <- openair_subsets(x$data)

  if (is.null(subsets)) {
    if (!is.null(subset)) {
      cli::cli_warn(c(
        "{.arg subset} option ignored.",
        "i" = "Subset requested from {.pkg openair} object without data subsets."
      ))
    }
    return(x$data)
  }

  if (is.null(subset) || length(subset) != 1L || !subset %in% subsets) {
    cli::cli_abort(
      c(
        "x" = "This {.pkg openair} object contains several data frames.",
        "i" = "Use {.arg subset} to choose one of: {.val {subsets}}."
      ),
      call = call
    )
  }

  x$data[[subset]]
}

#' @exportS3Method base::as.data.frame
as.data.frame.openair <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  subset = NULL,
  ...
) {
  as.data.frame(
    openair_data(x, subset),
    row.names = row.names,
    optional = optional,
    ...
  )
}

#' @exportS3Method tibble::as_tibble
as_tibble.openair <- function(x, ..., subset = NULL) {
  tibble::as_tibble(openair_data(x, subset), ...)
}

#' @exportS3Method base::names
names.openair <- function(x, ...) {
  # stuff we own up to...
  vis.elements <- c("data", "plot", "call", "clust", "clust_stats", "fit")
  # make names non-recursive
  class(x) <- "not-openair"

  names(x)[names(x) %in% vis.elements]
}

results <- function(object, ...) {
  UseMethod("results")
}

#' @export
results.default <- function(object, ...) {
  object
}

#' @export
results.openair <- function(object, subset = "all", silent = FALSE, ...) {
  if (!is.openair(object)) {
    return(invisible(NULL))
  }

  if (!silent) {
    # print function call
    cli::cli_par(id = "opening")
    cli::cli_h1("{.pkg openair} object")
    cli::cli_inform("Created by:")
    cli::cli_inform("{cli::symbol$play} {.code {deparse1(object$call)}}")
    cli::cli_end(id = "opening")
  }

  test <- openair_subsets(object$data)

  if (is.null(test)) {
    if (!is.null(subset) && !any(subset == "all")) {
      cli::cli_warn(c(
        "{.arg subset} option ignored.",
        "i" = "Subset requested from {.pkg openair} object without data subsets."
      ))
    }
    return(object$data)
  }

  if (is.null(subset) || any(subset == "all")) {
    if (!silent) {
      cli::cli_inform(c(
        "Returning a list of {length(test)} data frame{?s}:",
        " " = "{.field {paste0('$', test)}}"
      ))
    }
    return(object$data[names(object$data) != "subsets"])
  }

  temp <- subset[subset %in% test]
  if (length(temp) < 1) {
    cli::cli_abort(c(
      "Requested {.arg subset} not found.",
      "i" = "Use {.code NULL}, {.code \"all\"}, or one or more available subsets: {.val {test}}."
    ))
  }
  if (!silent && length(temp) < length(subset)) {
    cli::cli_warn(c(
      "Some requested subsets not found, so ignored: {.val {setdiff(subset, temp)}}.",
      "i" = "Available subsets: {.val {test}}."
    ))
  }
  if (length(temp) == 1) {
    return(object$data[[temp]])
  } else {
    if (!silent) {
      cli::cli_inform(c(
        "Returning a list of {length(temp)} data frame{?s}:",
        " " = "{.field {paste0('$', temp)}}"
      ))
    }
    return(object$data[names(object$data) %in% temp])
  }
}
