#' Workhorse function that automatically applies routine text formatting to
#' common expressions and data names used in openair.
#'
#' [quickText()] is a routine formatting lookup table. It screens the supplied
#' character vector `text` and automatically applies formatting to any
#' recognised character sub-series. The function is used in a number of
#' `openair` functions and can also be used directly by users to format text
#' in their own graphs, interactive plots (e.g., `{plotly}`) and maps (e.g.,
#' `{leaflet}`) (see below).
#'
#' @details
#'
#' [quickText()] recognises common pollutant names (e.g., `"no2"` is shown as
#' NO with a subscript 2), meteorological variables (e.g., `"ws"` is shown as
#' "wind spd."), units (e.g., `"ug/m3"` is shown as micrograms per cubic metre,
#' with a superscript -3) and a few symbols (e.g., `"Delta"` is shown as the
#' Greek letter). Many names are matched in more than one spelling (e.g.,
#' `"NO2"` and `"no2"`).
#'
#' Names are only formatted where they stand on their own, rather than as part
#' of a longer word or number. For example, `"no2"` and `"no2 (ppb)"` are
#' formatted, but `"pm100"`, `"co-located"` and `"m/sec"` are left as they are.
#' Underscores are allowed either side of most names, so that column names like
#' `"no2_roadside"` are still formatted.
#'
#' @param text A character vector. Other vectors (e.g., numbers or factors) are
#'   converted to character first. An expression is returned unchanged by
#'   `format = "expression"` and converted to plotmath code by `format =
#'   "plotmath"`, but can't be converted to other formats.
#'
#' @param auto.text A logical option. The default, `TRUE`, applies [quickText()]
#'   to `text` and returns the result. The alternative, `FALSE`, returns `text`
#'   unchanged. (A number of `openair` functions enable/disable [quickText()]
#'   using this option).
#'
#' @param format The output format. One of:
#'
#'   - `"expression"` (the default): a plotmath expression, for base R,
#'   `{lattice}` and `{ggplot2}` labels.
#'
#'   - `"plotmath"`: plotmath code as a character vector, e.g., for
#'   [ggplot2::label_parsed()]. Each element is kept to a single line.
#'
#'   - `"html"`: HTML as a character vector, e.g., for `{plotly}`, `{leaflet}`
#'   or `{ggtext}`. Subscripts and superscripts use `<sub>` and `<sup>`, and
#'   line breaks use `<br>`.
#'
#' @param ... Format-specific arguments:
#'
#'   - `"html"`: `escape`, which defaults to `TRUE`, escapes characters with a
#'   special meaning in HTML (`&`, `<` and `>`) so text is shown as written. Use
#'   `escape = FALSE` to allow HTML in `text` (e.g., `"<b>no2</b>"`).
#'
#' @export
#'
#' @return One element per element of `text`: an expression if `format =
#'   "expression"`, otherwise a character vector. `NULL` is returned unchanged.
#'
#' @author Karl Ropkins
#' @author David Carslaw
#' @author Jack Davison
#'
#' @examples
#' # see axis formatting in an openair plot, e.g.:
#' scatterPlot(
#'   mydata,
#'   x = "no2",
#'   y = "pm10"
#' )
#'
#' # using quickText in other plots
#' plot(
#'   mydata$no2,
#'   mydata$pm10,
#'   xlab = quickText("my no2 label"),
#'   ylab = quickText("pm10 (ug.m-3)")
#' )
#'
#' # HTML, e.g., for plotly or leaflet
#' quickText(c("no2 (ug/m3)", "pm2.5\nroadside"), format = "html")
quickText <- function(text, auto.text = TRUE, format = "expression", ...) {
  format <- rlang::arg_match0(format, c("expression", "plotmath", "html"))

  if (!auto.text) {
    return(text)
  }

  # `NULL` means "no label"; an empty expression would still be drawn, taking up
  # space and replacing default axis titles
  if (is.null(text)) {
    return(NULL)
  }

  if (is.expression(text)) {
    return(switch(
      format,
      expression = text,
      plotmath = purrr::map_chr(as.list(text), quicktext_plotmath_deparse),
      cli::cli_abort(
        "{.fn quickText} can't convert expressions to the {.val {format}} format."
      )
    ))
  }

  # coerce anything else (e.g., `NA`, numbers, factors) to character
  text <- paste(text)

  # format-specific arguments (e.g., `escape`) are passed on through `...`
  switch(
    format,
    expression = quicktext_expression(text, ...),
    plotmath = quicktext_plotmath(text, ...),
    html = quicktext_html(text, ...)
  )
}

# Dictionary ---------------------------------------------------------------

# Spellings using "u" for micro, plus the same spellings written with the micro
# sign or Greek mu (escaped to keep the package source ASCII)
quicktext_micro <- function(spellings) {
  stem <- sub("^u", "", spellings)
  c(spellings, paste0("\u00b5", stem), paste0("\u03bc", stem))
}

# An entry in the dictionary: what any of `spellings` is formatted as in each
# format, with one row per spelling. Each format is an argument (the
# "expression" format uses `plotmath`):
#
#   - `plotmath`: plotmath code.
#
#   - `html`: HTML, written as it is (so isn't escaped).
#
# Spellings are plain text (not regular expressions) and only match where their
# `boundary` allows (see `quicktext_boundaries`). Where spellings could match at
# the same position, the longest wins (e.g., "m/s2" over "m/s"), so the order of
# the entries doesn't matter. Each spelling may only appear once.
quicktext_entry <- function(spellings, plotmath, html, boundary = "name") {
  dplyr::tibble(
    spelling = spellings,
    boundary = boundary,
    plotmath = list(str2lang(plotmath)),
    html = html
  )
}

quicktext_dictionary <- dplyr::bind_rows(
  # gases
  quicktext_entry(
    c("NO2", "no2"),
    plotmath = '"NO"[2]',
    html = "NO<sub>2</sub>"
  ),
  quicktext_entry(
    c("NO3", "no3"),
    plotmath = '"NO"[3]',
    html = "NO<sub>3</sub>"
  ),
  quicktext_entry(
    c("NOX", "NOx", "nox"),
    plotmath = '"NO"[x]',
    html = "NO<sub>x</sub>",
    boundary = "word"
  ),
  quicktext_entry(
    "no",
    plotmath = '"NO"',
    html = "NO",
    boundary = "standalone"
  ),
  quicktext_entry(
    c("NH3", "nh3"),
    plotmath = '"NH"[3]',
    html = "NH<sub>3</sub>"
  ),
  quicktext_entry(
    c("CO2", "co2"),
    plotmath = '"CO"[2]',
    html = "CO<sub>2</sub>"
  ),
  quicktext_entry(
    "co",
    plotmath = '"CO"',
    html = "CO",
    boundary = "standalone"
  ),
  quicktext_entry("nmhc", plotmath = '"NMHC"', html = "NMHC"),
  quicktext_entry(
    c("O3", "o3", "ozone"),
    plotmath = '"O"[3]',
    html = "O<sub>3</sub>"
  ),
  quicktext_entry(
    c("SO2", "so2"),
    plotmath = '"SO"[2]',
    html = "SO<sub>2</sub>"
  ),
  quicktext_entry(
    c("H2S", "h2s"),
    plotmath = '"H"[2] * "S"',
    html = "H<sub>2</sub>S"
  ),
  quicktext_entry(
    c("CH4", "ch4"),
    plotmath = '"CH"[4]',
    html = "CH<sub>4</sub>"
  ),

  # particulate matter
  quicktext_entry(
    c("PM10", "pm10"),
    plotmath = '"PM"[10]',
    html = "PM<sub>10</sub>"
  ),
  quicktext_entry(
    c("PM1", "pm1"),
    plotmath = '"PM"[1]',
    html = "PM<sub>1</sub>"
  ),
  quicktext_entry(
    c("PM4", "pm4"),
    plotmath = '"PM"[4]',
    html = "PM<sub>4</sub>"
  ),
  quicktext_entry(
    c("PM2.5", "pm2.5", "PM25", "pm25"),
    plotmath = '"PM"[2.5]',
    html = "PM<sub>2.5</sub>"
  ),
  quicktext_entry(
    c("PMtot", "pmtot"),
    plotmath = '"PM"[total]',
    html = "PM<sub>total</sub>"
  ),
  quicktext_entry(
    c("PMcoarse", "pmcoarse", "PMc", "pmc"),
    plotmath = '"PM"[coarse]',
    html = "PM<sub>coarse</sub>"
  ),
  quicktext_entry(
    c("PMfine", "pmfine", "PMf", "pmf"),
    plotmath = '"PM"[fine]',
    html = "PM<sub>fine</sub>"
  ),

  # meteorology
  quicktext_entry(
    c("WS", "ws"),
    plotmath = '"wind spd."',
    html = "wind spd.",
    boundary = "word"
  ),
  quicktext_entry(
    c("WD", "wd"),
    plotmath = '"wind dir."',
    html = "wind dir.",
    boundary = "word"
  ),
  quicktext_entry(
    "air_temp",
    plotmath = '"temperature"',
    html = "temperature",
    boundary = "word"
  ),
  quicktext_entry(
    "rh",
    plotmath = '"relative humidity"',
    html = "relative humidity",
    boundary = "word"
  ),

  # statistics and symbols
  quicktext_entry(c("R2", "r2"), plotmath = '"R"^2', html = "R<sup>2</sup>"),
  quicktext_entry("tau", plotmath = 'tau', html = "\u03c4"),
  quicktext_entry(
    c("Delta", "delta"),
    plotmath = 'Delta',
    html = "\u0394",
    boundary = "prefix"
  ),
  quicktext_entry(
    c("degreesC", "degreeC", "dgrC", "deg. C"),
    plotmath = 'degree * "C"',
    html = "\u00b0C",
    boundary = "unit"
  ),
  quicktext_entry(
    "degrees",
    plotmath = 'degree',
    html = "\u00b0",
    boundary = "none"
  ),

  # concentrations
  quicktext_entry(
    quicktext_micro(c("ug/m3", "ug.m-3", "ug m-3", "ugm-3")),
    plotmath = 'mu * "g m"^-3',
    html = "\u00b5g m<sup>-3</sup>",
    boundary = "unit"
  ),
  quicktext_entry(
    c("mg/m3", "mg.m-3", "mg m-3", "mgm-3"),
    plotmath = '"mg m"^-3',
    html = "mg m<sup>-3</sup>",
    boundary = "unit"
  ),
  quicktext_entry(
    c("ng/m3", "ng.m-3", "ng m-3", "ngm-3"),
    plotmath = '"ng m"^-3',
    html = "ng m<sup>-3</sup>",
    boundary = "unit"
  ),
  quicktext_entry(
    "g/m3",
    plotmath = '"g m"^-3',
    html = "g m<sup>-3</sup>",
    boundary = "unit"
  ),
  quicktext_entry(
    "cm-3",
    plotmath = '"cm"^-3',
    html = "cm<sup>-3</sup>",
    boundary = "unit"
  ),
  quicktext_entry(
    quicktext_micro("umol/m2/s"),
    plotmath = 'mu * "mol m"^-2 * " s"^-1',
    html = "\u00b5mol m<sup>-2</sup> s<sup>-1</sup>",
    boundary = "unit"
  ),
  quicktext_entry(
    quicktext_micro("umol/m2"),
    plotmath = 'mu * "mol m"^-2',
    html = "\u00b5mol m<sup>-2</sup>",
    boundary = "unit"
  ),

  # speeds and rates
  quicktext_entry(
    c("km/hour/s", "km/hr/s", "km/h/s"),
    plotmath = '"km hr"^-1 * " s"^-1',
    html = "km hr<sup>-1</sup> s<sup>-1</sup>",
    boundary = "unit"
  ),
  quicktext_entry(
    c("km/hour", "km/hr", "km/h"),
    plotmath = '"km hr"^-1',
    html = "km hr<sup>-1</sup>",
    boundary = "unit"
  ),
  quicktext_entry(
    "m/s2",
    plotmath = '"m s"^-2',
    html = "m s<sup>-2</sup>",
    boundary = "unit"
  ),
  quicktext_entry(
    c("m/s", "m.s-1", "m s-1"),
    plotmath = '"m s"^-1',
    html = "m s<sup>-1</sup>",
    boundary = "unit"
  ),
  quicktext_entry(
    "km2",
    plotmath = '"km"^2',
    html = "km<sup>2</sup>",
    boundary = "unit"
  ),
  quicktext_entry(
    "g/km",
    plotmath = '"g km"^-1',
    html = "g km<sup>-1</sup>",
    boundary = "unit"
  ),
  quicktext_entry(
    "g/s",
    plotmath = '"g s"^-1',
    html = "g s<sup>-1</sup>",
    boundary = "unit"
  ),
  quicktext_entry(
    c("g/hour", "g/hr"),
    plotmath = '"g hour"^-1',
    html = "g hour<sup>-1</sup>",
    boundary = "unit"
  ),
  quicktext_entry(
    "g/kg",
    plotmath = '"g kg"^-1',
    html = "g kg<sup>-1</sup>",
    boundary = "unit"
  ),
  quicktext_entry(
    "kW/t",
    plotmath = '"kW t"^-1',
    html = "kW t<sup>-1</sup>",
    boundary = "unit"
  )
)

# Where a spelling may match, given as regular expressions checked `before` and
# `after` it:
#
#   - "name": not preceded by a letter, nor followed by a letter, digit or
#   decimal part. This stops, e.g., "pm100", "pm1.5" or "year2020" being
#   formatted, but allows underscores either side so that names like
#   "no2_roadside" are formatted.
#
#   - "standalone": preceded by the start of the text, whitespace, "(", "[" or
#   "/", and followed by the end of the text, whitespace, ")", "]", ",", ";",
#   ":" or "/". For short names that are also common words or prefixes, so that,
#   e.g., "co-located" and "no. 5" are not formatted.
#
#   - "prefix": not preceded by a letter, nor followed by a lowercase letter, so
#   it can prefix a name (e.g., "DeltaNO2") but not start a word (e.g.,
#   "Deltaville").
#
#   - "word": a regular expression word boundary (`\b`) either side, so
#   underscores also prevent a match (e.g., "ws_max" is not formatted).
#
#   - "unit": anything before, but not followed by a letter or digit. This
#   allows, e.g., the "m/s" in "km/s", but stops "m/sec" being formatted as
#   "m s^-1 ec" or "km25" as "km^2 5".
#
#   - "none": matched anywhere, e.g., the "degrees" in "degreesF".
quicktext_boundaries <- dplyr::tribble(
  ~boundary    , ~before            , ~after                     ,
  "name"       , "(?<![A-Za-z])"    , "(?![A-Za-z0-9]|\\.[0-9])" ,
  "standalone" , "(?<![^\\s(\\[/])" , "(?![^\\s)\\],;:/])"       ,
  "prefix"     , "(?<![A-Za-z])"    , "(?![a-z])"                ,
  "word"       , "\\b"              , "\\b"                      ,
  "unit"       , ""                 , "(?![A-Za-z0-9])"          ,
  "none"       , ""                 , ""
)

local({
  clashes <- unique(quicktext_dictionary$spelling[
    duplicated(quicktext_dictionary$spelling)
  ])
  if (length(clashes) > 0L) {
    cli::cli_abort(
      "{.fn quickText} spellings in more than one entry: {.val {clashes}}"
    )
  }
})

# One regular expression that matches any spelling, so each piece of text is
# matched once and replacements are never re-matched by other spellings
quicktext_regex <- quicktext_dictionary |>
  # errors if a row uses an unknown boundary
  dplyr::inner_join(
    quicktext_boundaries,
    by = "boundary",
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) |>
  # the first alternative that matches wins, so try the longest first
  dplyr::arrange(dplyr::desc(nchar(.data$spelling))) |>
  # `\Q...\E` matches the text between them literally (e.g., "." is a dot)
  dplyr::mutate(
    pattern = paste0(.data$before, "\\Q", .data$spelling, "\\E", .data$after)
  ) |>
  dplyr::pull(.data$pattern) |>
  paste(collapse = "|")

# Matching -----------------------------------------------------------------

# Split text into lines, keeping a trailing empty line
quicktext_lines <- function(text) {
  # "\r\n" for text with Windows line endings
  lines <- strsplit(text, "\r?\n")[[1]]
  # `strsplit()` drops a trailing empty line
  if (endsWith(text, "\n") || length(lines) == 0L) {
    lines <- c(lines, "")
  }
  lines
}

# Split a line into pieces of unmatched text (passed through `text_fn`) and the
# `format` column of the dictionary for each matched spelling, skipping empty
# text (e.g., when a match is at the start or end of the line). For example,
# for HTML, "NO2 (ppb)" -> `list("NO<sub>2</sub>", " (ppb)")`
quicktext_match <- function(line, format, text_fn = identity) {
  matches <- gregexpr(quicktext_regex, line, perl = TRUE)

  # the matched text, and the text around it (always one more piece)
  matched <- regmatches(line, matches)[[1]]
  between <- regmatches(line, matches, invert = TRUE)[[1]]
  replacements <- quicktext_dictionary[[format]][
    match(matched, quicktext_dictionary$spelling)
  ]

  # interleave them: between[1], match 1, between[2], match 2, between[3], ...
  # (base R rather than purrr here, as this runs for every line of every label
  # and `purrr::map2()` + `purrr::list_flatten()` is ~50x slower)
  pieces <- list()
  if (nzchar(between[1])) {
    pieces <- list(text_fn(between[1]))
  }
  for (i in seq_along(matched)) {
    pieces <- c(pieces, list(replacements[[i]]))
    if (nzchar(between[i + 1])) {
      pieces <- c(pieces, list(text_fn(between[i + 1])))
    }
  }
  pieces
}

# Formats ------------------------------------------------------------------

# Each format is a function, `quicktext_<format>(text, ...)`, that formats a
# character vector using its column of the dictionary. Any format-specific
# arguments come from the `...` of `quickText()`

# Plotmath expressions, with lines stacked
quicktext_expression <- function(text) {
  exprs <- purrr::map(text, \(x) {
    lines <- purrr::map(quicktext_lines(x), quicktext_plotmath_line)
    if (length(lines) == 1L) lines[[1]] else quicktext_plotmath_stack(lines)
  })
  as.expression(exprs)
}

# Plotmath code as strings, kept to one line, e.g., for facet labels
quicktext_plotmath <- function(text) {
  purrr::map_chr(text, \(x) {
    quicktext_plotmath_deparse(quicktext_plotmath_line(gsub("\r?\n", " ", x)))
  })
}

quicktext_plotmath_line <- function(line) {
  bquote(paste(..(quicktext_match(line, "plotmath"))), splice = TRUE)
}

# Turn a plotmath expression into one string of plotmath code
quicktext_plotmath_deparse <- function(expr) {
  paste(deparse(expr, width.cutoff = 500L), collapse = "")
}

# Stack plotmath expressions as lines. Plotmath has no line break, but `atop()`
# stacks two items, so it is nested to stack any number of lines, working back
# from the last line, e.g.,
# `atop(displaystyle(a), displaystyle(atop(displaystyle(b), displaystyle(c))))`.
# Each part is wrapped in `displaystyle()` as nested `atop()` otherwise shrinks
# each level's text
quicktext_plotmath_stack <- function(exprs) {
  exprs <- purrr::map(exprs, \(expr) call("displaystyle", expr))
  purrr::reduce(
    exprs,
    \(line, below) call("displaystyle", call("atop", line, below)),
    .dir = "backward"
  )
}

# HTML, e.g., for `{plotly}`, `{leaflet}` or `{ggtext}`, with lines separated by
# `<br>`. `escape = TRUE` escapes characters with a special meaning in HTML, so
# that text like "a < b" is shown as written; `escape = FALSE` allows HTML in
# the text (e.g., "<b>no2</b>")
quicktext_html <- function(text, escape = TRUE) {
  text_fn <- if (escape) quicktext_html_escape else identity
  purrr::map_chr(text, \(x) {
    lines <- purrr::map_chr(
      quicktext_lines(x),
      \(line) {
        paste(unlist(quicktext_match(line, "html", text_fn)), collapse = "")
      }
    )
    paste(lines, collapse = "<br>")
  })
}

quicktext_html_escape <- function(x) {
  # "&" first, so the "&" of other escapes isn't escaped again
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

# ggplot2 helpers ----------------------------------------------------------

# `quickText()` for ggplot2 labels (e.g., `scale_*(labels = )`). Empty input is
# returned unchanged (scales may have no breaks), and results are named after
# `x` (or keep its names), as scales can match labels by name
label_openair <- function(x, format = "expression", auto_text = TRUE) {
  if (!auto_text || length(x) == 0L) {
    return(x)
  }

  out <- quickText(x, format = format)
  names(out) <- names(x) %||%
    if (is.character(x) || is.factor(x)) as.character(x)
  out
}

# Return a labeller for facets
labeller_openair <- function(auto_text = TRUE) {
  if (auto_text) {
    ggplot2::as_labeller(
      \(x) label_openair(x, format = "plotmath", auto_text = TRUE),
      default = ggplot2::label_parsed
    )
  } else {
    ggplot2::label_value
  }
}
