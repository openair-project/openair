# Qualitative palette definitions and their maximum lengths
.qual_schemes <- c(
  "okabeito" = 9,
  "cbPalette" = 9,
  "daqi" = 10,
  "daqi.bands" = 4,
  "gaf.cat" = 6,
  "gaf.focus" = 2,
  "tableau" = 10,
  "observable" = 10,
  "tol" = 7,
  "tol.bright" = 7,
  "tol.muted" = 10,
  "tol.light" = 9
)

# Sequential palette names (excluding brewer)
.seq_schemes <- c(
  "default",
  "increment",
  "heat",
  "jet",
  "turbo",
  "viridis",
  "magma",
  "inferno",
  "plasma",
  "cividis",
  "gaf.seq"
)

# All known scheme names
.all_schemes <- c(
  .seq_schemes,
  names(.qual_schemes),
  "brewer1",
  "hue",
  "greyscale",
  namelist # from brewer.pal definitions
)


#' Pre-defined openair colours and definition of user-defined colours
#'
#' This in primarily an internal openair function to make it easy for users to
#' select particular colour schemes, or define their own range of colours of a
#' user-defined length. `openColours()` and `openColors()` are synonyms.
#'
#' @section Schemes:
#'
#'   The following schemes are made available by `openColours()`:
#'
#'   **Sequential Colours:**
#'
#'   * "default", "increment", "brewer1", "heat", "jet", "turbo", "hue",
#'   "greyscale".
#'
#'   * Simplified versions of the `viridis` colours: "viridis", "plasma",
#'   "magma", "inferno", "cividis", and "turbo".
#'
#'   * Simplified versions of the `RColorBrewer` sequential palettes: "Blues", "BuGn",
#'   "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn",
#'   "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd".
#'
#'   **Diverging Palettes:**
#'
#'   * Simplified versions of the `RColorBrewer` diverging palettes: "BrBG",
#'   "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral".
#'
#'   **Qualitative Palettes:**
#'
#'   * Simplified versions of the `RColorBrewer` qualitative palettes:
#'   "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3".
#'
#'   * "okabeito" (or "cbPalette"), a colour-blind safe palette based on
#'   the work of Masataka Okabe and Kei Ito (<https://jfly.uni-koeln.de/color/>)
#'
#'   * "tol.bright" (or "tol"), "tol.muted" and "tol.light", colour-blind safe
#'   palettes based on the work of Paul Tol.
#'
#'   * "tableau" and "observable", aliases for the
#'   "Tableau10"
#'   (<https://www.tableau.com/blog/colors-upgrade-tableau-10-56782>) and
#'   "Observable10" (<https://observablehq.com/blog/crafting-data-colors>)
#'   colour palettes. These could be useful for consistency between openair
#'   plots and with figures made in Tableau or Observable Plot.
#'
#'   **UK Government Palettes:**
#'
#'   * "daqi" and "daqi.bands", the colours associated with the UK daily air
#'   quality index; "daqi" (a palette of 10 colours, corresponding to each index
#'   value) or "daqi.bands" (4 colours, corresponding to each band - Low,
#'   Moderate, High, and Very High). These colours were taken directly from
#'   <https://check-air-quality.service.gov.uk/> and may be useful in figures
#'   like [calendarPlot()].
#'
#'   * "gaf.cat", "gaf.focus" and "gaf.seq", colours recommended by the UK Government Analysis function
#'   (<https://analysisfunction.civilservice.gov.uk/policy-store/data-visualisation-colours-in-charts/>).
#'   "gaf.cat" will return the 'categorical' palette (max 6 colours),
#'   "gaf.focus" the 'focus' palette (max 2 colours), and "gaf.seq" the
#'   'sequential' palette.
#'
#' @section Details:
#'
#'   Because of the way many of the schemes have been developed they only exist
#'   over certain number of colour gradations (typically 3--10). If less than or
#'   more than the required number of colours is supplied then `openair` will
#'   interpolate the colours.
#'
#'   Each of the pre-defined schemes have merits and their use will depend on a
#'   particular situation. For showing incrementing concentrations, e.g., high
#'   concentrations emphasised, then "default", "heat", "jet", "turbo", and
#'   "increment" are very useful. See also the description of `RColorBrewer`
#'   schemes for the option `scheme`.
#'
#'   To colour-code categorical-type problems, e.g., colours for different
#'   pollutants, "hue" and "brewer1" are useful.
#'
#'   When publishing in black and white, "greyscale" is often convenient.  With
#'   most openair functions, as well as generating a greyscale colour gradient,
#'   it also resets strip background and other coloured text and lines to
#'   greyscale values.
#'
#'   Failing that, the user can define their own schemes based on R colour
#'   names. To see the full list of names, type [colors()] into R.
#'
#' @param scheme Any one of the pre-defined `openair` schemes (e.g.,
#'   `"increment"`) or a user-defined palette (e.g., `c("red", "orange",
#'   "gold")`). See [openColours()] for a full list of available schemes.
#'
#' @param n The whole number of colours required. If not provided, sequential
#'   palettes will return `100` colours and qualitative palettes will return the
#'   maximum number of colours available for that scheme.
#'
#' @param alpha The alpha transparency level (between `0` and `1`) for the
#'   colours. `0` is fully transparent, and `1` is fully opaque.
#'
#' @param begin,end For sequential schemes, the fraction (between `0` and `1`)
#'   of the colour scheme to use. For example, if `begin = 0.2` and `end = 0.8`,
#'   only the middle 60% of the colour scheme will be used. This can be useful
#'   for avoiding very light or dark colours at the ends of schemes.
#'
#' @param direction The order of the colours. `1` is the default and gives the
#'   normal order. `-1` will reverse the order of the colours.
#'
#' @export
#' @return A character vector of hex codes
#' @author David Carslaw
#' @author Jack Davison
#' @references <https://colorbrewer2.org/>
#' @references <https://check-air-quality.service.gov.uk/>
#' @references
#' <https://analysisfunction.civilservice.gov.uk/policy-store/data-visualisation-colours-in-charts/>
#' @examples
#'
#' # to return 5 colours from the "jet" scheme:
#' cols <- openColours("jet", 5)
#' cols
#'
#' # to interpolate between named colours e.g. 10 colours from yellow to
#' #  green to red:
#' cols <- openColours(c("yellow", "green", "red"), 10)
#' cols
#'
openColours <- function(
  scheme = "default",
  n = NULL,
  alpha = 1,
  begin = 0,
  end = 1,
  direction = 1
) {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} must be between `0` and `1`.")
  }
  if (!direction %in% c(1, -1)) {
    cli::cli_abort(
      "{.arg direction} must be either `1` (normal order) or `-1` (reverse order)."
    )
  }
  if (begin < 0 || begin > 1 || end < 0 || end > 1 || begin >= end) {
    cli::cli_abort(
      "{.arg begin} and {.arg end} must be between `0` and `1`, and {.arg begin} must be less than {.arg end}."
    )
  }

  # user-supplied colour vectors
  if (length(scheme) > 1L || !scheme %in% .all_schemes) {
    if (length(scheme) > 1L && any(scheme %in% .all_schemes)) {
      cli::cli_abort(
        c(
          "x" = "Please provide {.strong either} 1 {.fun openColours} palette {.emph or} a vector of valid R colours.",
          "i" = "See {.code ?openColours} for a list of palettes."
        ),
        call = NULL
      )
    }
    check <- .areColors(scheme)
    if (!all(check)) {
      cli::cli_abort(
        c(
          "x" = "The following are {.emph neither} valid R colours {.emph nor} {.fun openColours} palettes: {.field {names(check[!check])}}"
        ),
        call = NULL
      )
    }
    cols <- grDevices::colorRampPalette(scheme)(n %||% 100L)
    return(.finalise(cols, direction, alpha))
  }

  # single named scheme
  n_out <- n %||%
    if (scheme %in% names(.qual_schemes)) .qual_schemes[[scheme]] else 100L

  # brewer1 is an alias for Set1
  if (scheme == "brewer1") {
    scheme <- "Set1"
  }

  cols <- if (scheme %in% namelist) {
    .brewerPalette(n_out, scheme, direction, begin, end, alpha)
  } else if (scheme == "hue") {
    .huePalette(n_out, direction, begin, end, alpha)
  } else if (scheme == "greyscale") {
    .greyPalette(n_out, direction, begin, end, alpha)
  } else if (scheme %in% .seq_schemes) {
    .seqPalette(n_out, scheme, direction, begin, end, alpha)
  } else if (scheme %in% names(.qual_schemes)) {
    .qualPalette(n_out, scheme, direction, alpha)
  }

  cols
}

#' @rdname openColours
#' @export
openColors <- openColours

#' Apply direction and alpha to a colour vector
#' @noRd
.finalise <- function(cols, direction, alpha) {
  if (direction == -1) {
    cols <- rev(cols)
  }
  cols <- substr(cols, 1, 7)
  paste0(cols, sprintf("%02X", round(alpha * 255)))
}

#' Check whether strings are valid R colours
#' @noRd
.areColors <- function(x) {
  sapply(x, function(col) {
    tryCatch(is.matrix(grDevices::col2rgb(col)), error = function(e) FALSE)
  })
}

#' Brewer palette dispatcher
#' @noRd
.brewerPalette <- function(n, scheme, direction, begin, end, alpha) {
  max_n <- maxcolors[[scheme]]
  if (n >= 3L && n <= max_n) {
    brewer.pal(
      n,
      scheme,
      direction = direction,
      begin = begin,
      end = end,
      alpha = alpha
    )
  } else {
    base_cols <- suppressWarnings(
      brewer.pal(
        max_n,
        scheme,
        direction = direction,
        begin = begin,
        end = end,
        alpha = alpha
      )
    )
    # alpha already baked in — strip before ramp, reapply after
    cols <- grDevices::colorRampPalette(
      substr(base_cols, 1, 7),
      interpolate = "spline"
    )(n)
    .finalise(cols, direction = 1L, alpha = alpha)
  }
}

#' Hue palette
#' @noRd
.huePalette <- function(n, direction, begin, end, alpha) {
  h <- c(0, 360) + 15
  if ((diff(h) %% 360) < 1) {
    h[2] <- h[2] - 360 / n
  }
  h <- h[1] + (h[2] - h[1]) * c(begin, end)
  cols <- grDevices::hcl(
    h = seq(h[1], h[2], length.out = n),
    c = 100,
    l = 65
  )
  .finalise(cols, direction, alpha)
}

#' Greyscale palette
#' @noRd
.greyPalette <- function(n, direction, begin, end, alpha) {
  # default begin/end for greyscale avoids pure black/white
  if (begin == 0) {
    begin <- 0.1
  }
  if (end == 1) {
    end <- 0.9
  }
  cols <- grDevices::grey(seq(end, begin, length.out = n))
  .finalise(cols, direction, alpha)
}

#' Sequential palettes
#' @noRd
.seqPalette <- function(n, scheme, direction, begin, end, alpha) {
  interpolate <- "linear"

  cols <- switch(
    scheme,
    default = {
      interpolate <- "spline"
      rev(brewer.pal(11, "Spectral"))
    },
    heat = {
      interpolate <- "spline"
      brewer.pal(9, "YlOrRd")
    },
    increment = c(
      "#B0FFF1",
      "#9CFFC7",
      "#87FF8E",
      "#A0FF73",
      "#B4FF69",
      "#CCFF60",
      "#E7FF56",
      "#FFF84D",
      "#FFCB46",
      "#FF9C40",
      "#FF6939",
      "#FF3333",
      "#CC1B62",
      "#990A7C",
      "#520066"
    ),
    viridis = c(
      "#440154FF",
      "#482878FF",
      "#3E4A89FF",
      "#31688EFF",
      "#26828EFF",
      "#1F9E89FF",
      "#35B779FF",
      "#6DCD59FF",
      "#B4DE2CFF",
      "#FDE725FF"
    ),
    inferno = c(
      "#000004FF",
      "#1B0C42FF",
      "#4B0C6BFF",
      "#781C6DFF",
      "#A52C60FF",
      "#CF4446FF",
      "#ED6925FF",
      "#FB9A06FF",
      "#F7D03CFF",
      "#FCFFA4FF"
    ),
    magma = c(
      "#000004FF",
      "#180F3EFF",
      "#451077FF",
      "#721F81FF",
      "#9F2F7FFF",
      "#CD4071FF",
      "#F1605DFF",
      "#FD9567FF",
      "#FEC98DFF",
      "#FCFDBFFF"
    ),
    plasma = c(
      "#0D0887FF",
      "#47039FFF",
      "#7301A8FF",
      "#9C179EFF",
      "#BD3786FF",
      "#D8576BFF",
      "#ED7953FF",
      "#FA9E3BFF",
      "#FDC926FF",
      "#F0F921FF"
    ),
    cividis = c(
      "#00204DFF",
      "#00336FFF",
      "#39486BFF",
      "#575C6DFF",
      "#707173FF",
      "#8A8779FF",
      "#A69D75FF",
      "#C4B56CFF",
      "#E4CF5BFF",
      "#FFEA46FF"
    ),
    jet = c(
      "#00007F",
      "#0000FF",
      "#007FFF",
      "#00FFFF",
      "#7FFF7F",
      "#FFFF00",
      "#FF7F00",
      "#FF0000",
      "#7F0000"
    ),
    turbo = c(
      "#30123BFF",
      "#4662D7FF",
      "#36AAF9FF",
      "#1AE4B6FF",
      "#72FE5EFF",
      "#C7EF34FF",
      "#FABA39FF",
      "#F66B19FF",
      "#CB2A04FF",
      "#7A0403FF"
    ),
    gaf.seq = c("#12436D", "#2073BC", "#6BACE6")
  )

  # strip any embedded alpha before subsetting/interpolating
  cols <- substr(cols, 1, 7)

  # apply begin/end
  idx <- round(1 + (length(cols) - 1) * c(begin, end))
  cols <- cols[idx[1]:idx[2]]

  cols <- grDevices::colorRampPalette(cols, interpolate = interpolate)(n)
  .finalise(cols, direction, alpha)
}

#' Qualitative palettes
#' @noRd
.qualPalette <- function(n, scheme, direction, alpha) {
  cols <- switch(
    scheme,
    cbPalette = ,
    okabeito = c(
      "#E69F00",
      "#56B4E9",
      "#009E73",
      "#F0E442",
      "#0072B2",
      "#D55E00",
      "#CC79A7",
      "#999999",
      "#000000"
    ),
    daqi = c(
      "#9CFF9C",
      "#31FF00",
      "#31CF00",
      "#FFFF00",
      "#FFCF00",
      "#FF9A00",
      "#FF6464",
      "#FF0000",
      "#990000",
      "#CE30FF"
    ),
    daqi.bands = c("#009900", "#ff9900", "#ff0000", "#990099"),
    gaf.cat = c(
      "#12436D",
      "#28A197",
      "#801650",
      "#F46A25",
      "#3D3D3D",
      "#A285D1"
    ),
    gaf.focus = c("#BFBFBF", "#12436D"),
    tol = ,
    tol.bright = c(
      "#4477AA",
      "#EE6677",
      "#228833",
      "#CCBB44",
      "#66CCEE",
      "#AA3377",
      "#BBBBBB"
    ),
    tol.muted = c(
      "#CC6677",
      "#332288",
      "#DDCC77",
      "#117733",
      "#88CCEE",
      "#882255",
      "#44AA99",
      "#999933",
      "#AA4499",
      "#DDDDDD"
    ),
    tol.light = c(
      "#77AADD",
      "#EE8866",
      "#EEDD88",
      "#FFAABB",
      "#99DDFF",
      "#44BB99",
      "#BBCC33",
      "#AAAA00",
      "#DDDDDD"
    ),
    tableau = c(
      "#5778a4",
      "#e49444",
      "#d1615d",
      "#85b6b2",
      "#6a9f58",
      "#e7ca60",
      "#a87c9f",
      "#f1a2a9",
      "#967662",
      "#b8b0ac"
    ),
    observable = c(
      "#4269D0",
      "#EFB118",
      "#FF725C",
      "#6CC5B0",
      "#3CA951",
      "#FF8AB7",
      "#A463F2",
      "#97BBF5",
      "#9C6B4E",
      "#9498A0"
    )
  )

  max_n <- length(cols)
  if (n < 1L || n > max_n) {
    cli::cli_abort(
      c(
        "!" = "Too many colours selected for {.code {scheme}}.",
        "i" = "{.code n} should be between 1 and {max_n}."
      ),
      call = NULL
    )
  }

  # reverse direction before subsampling for qualitative pals
  if (direction == -1) {
    cols <- rev(cols)
  }

  # finalise with direction = 1 as already handled above
  .finalise(cols[1:n], direction = 1, alpha)
}
