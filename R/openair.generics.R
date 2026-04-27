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
  fun.name <- if (grepl("[(]", fun.name)) {
    "function"
  }

  # openair header
  cli::cli_par(id = "opening")
  cli::cli_h1("{.pkg openair} object")
  cli::cli_inform("Created by:")
  cli::cli_inform("{cli::symbol$play} {.code \t{deparse(object$call)}}")
  cli::cli_end(id = "opening")

  # test for subsets
  check <- "subsets" %in% names(object$data)
  if (check) {
    test <- object$data$subsets
  }

  if (!check) {
    ans <- fun(object$data, ...)
    print(ans)
    return(invisible(ans))
  }

  if (is.null(subset) || any(subset == "all")) {
    temp <- test
  } else {
    temp <- subset[subset %in% test]
    if (length(temp) < 1) {
      cli::cli_abort(
        c(
          "x" = "In {.code {fun.name}}: requested subset(s) not found",
          "i" = "Suggest one (or more) of {.field {test}}"
        ),
        call = NULL
      )
    }

    if (length(temp) < length(subset)) {
      cli::cli_abort(
        c(
          "x" = "In {.code {fun.name}}: some requested subset(s) not found, so ignored.",
          "i" = "Ignored subset(s): {.field {subset[subset != temp]}}",
          "i" = "Available subset(s): {.field {test}}"
        ),
        call = NULL
      )
    }
  }

  ans <- sapply(
    temp,
    function(y) {
      fun(object$data[[y]], ...)
    },
    simplify = FALSE,
    USE.NAMES = TRUE
  )

  print(ans)
  return(invisible(ans))
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
    message("\nopenair object created by: \n\t", deparse(x$call), "\n")
  }

  test <- x$plot$subsets

  # No subsets — just print the main plot
  if (is.null(test)) {
    if (!is.null(subset) && subset != "all") {
      warning(
        "In plot(...): subset option ignored,",
        "\n\t[subset requested from openair object without data subsets]",
        call. = FALSE
      )
    }
    return(plot(x$plot, ...))
  }

  # "all" or NULL — print the assembled patchwork object
  if (is.null(subset) || subset == "all") {
    if (is.null(x$main.plot)) {
      stop(
        "In plot(...): bad openair object structure",
        "\n\t[please contact openair admin if valid]",
        call. = FALSE
      )
    }
    return(plot(x$main.plot, ...))
  }

  # Subset requested — validate and resolve to a single subset
  temp <- subset[subset %in% test]
  if (length(temp) < 1) {
    stop(
      "In plot(...): requested subset not found",
      "\n\t[suggest one of: ",
      paste(test, collapse = ", "),
      "]",
      call. = FALSE
    )
  }
  if (length(temp) < length(subset)) {
    warning(
      "In plot(...): multiple subsets requested and some not found, using first valid",
      call. = FALSE
    )
  } else if (length(temp) > 1) {
    warning(
      "In plot(...): multiple subsets requested, using first valid",
      call. = FALSE
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
    # print function call
    cli::cli_par(id = "opening")
    cli::cli_h1("{.pkg openair} object")
    cli::cli_inform("Created by:")
    cli::cli_inform("{cli::symbol$play} {.code \t{deparse(x$call)}}")
    cli::cli_end(id = "opening")

    # contains header
    cli::cli_h2("This contains:")

    # check if subsets exist
    if ("subsets" %in% names(x$data)) {
      test <- FALSE
    } else {
      test <- TRUE
    }

    cli::cli_par(id = "dfs")
    if (test) {
      cli::cli_inform("A single data frame:")
      cli::cli_ul("{.field $data} [with no subset structure]")
    } else {
      cli::cli_inform("Data frame(s):")
      cli::cli_ol(paste0("$data{.field $", x$data$subsets, "}"))
    }
    cli::cli_end(id = "dfs")

    # check if subsets exist
    if ("subsets" %in% names(x$plot)) {
      test <- FALSE
    } else {
      test <- TRUE
    }

    cli::cli_par(id = "plots")
    if (test) {
      cli::cli_inform("A single plot:")
      cli::cli_ul("{.field $plot} [with no subset structure]")
    } else {
      cli::cli_inform("Plot object(s):")
      cli::cli_ol(paste0("$plot{.field $", x$plot$subsets, "}"))
    }
    cli::cli_end(id = "plots")

    other <- names(x)[!names(x) %in% c("data", "call", "plot")]

    if (length(other) > 0) {
      cli::cli_par(id = "other-stuff")
      cli::cli_inform("Other object(s):")
      cli::cli_ul(paste0("{.field $", other, "}"))
      cli::cli_end(id = "other-stuff")
    }
  }

  if (plot) {
    plot(x, silent = TRUE, ...)
  }
}

#' @exportS3Method base::names
names.openair <- function(x, ...) {
  # stuff we own up to...
  vis.elements <- c("data", "plot", "call", "clust", "clust_stats")
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
    cli::cli_inform("{cli::symbol$play} {.code \t{deparse(object$call)}}")
    cli::cli_end(id = "opening")
  }

  test <- object$data$subsets

  if (is.null(test)) {
    if (!is.null(subset) && subset != "all") {
      warning(
        "In results(...): subset option ignored,",
        "\n\t[subset requested from openair object without data subsets]",
        call. = FALSE
      )
    }
    return(object$data)
  }

  if (is.null(subset) || subset == "all") {
    if (!silent) {
      message("contains", length(test), " data frame(s):")
      message("returning as list")
      message("\t$", paste(test, collapse = ", $", sep = ", $"), "\n")
    }
    return(object$data[names(object$data) != "subsets"])
  }

  temp <- subset[subset %in% test]
  if (length(temp) < 1) {
    stop(
      "In results(...): requested subset(s) not found",
      "\n\t[suggest NULL, all or one or more of available subsets]",
      "\n\t[available subset(s): ",
      paste(test, collapse = ", ", sep = ", "),
      "]",
      call. = FALSE
    )
  }
  if (!silent && length(temp) < length(subset)) {
    warning(
      "In results(...): some requested subset(s) not found, so ignored",
      "\n\t[ignored subset(s): ",
      paste(subset[subset != temp], collapse = ", ", sep = ", "),
      "]",
      "\n\t[available subset(s): ",
      paste(test, collapse = ", ", sep = ", "),
      "]",
      call. = FALSE
    )
  }
  if (length(temp) == 1) {
    return(object$data[[temp]])
  } else {
    if (!silent) {
      message("returning as list of ", length(temp), " data frame(s):")
      message("\t$", paste(temp, collapse = ", $", sep = ", $"), "\n")
    }
    return(object$data[names(object$data) %in% temp])
  }
}
