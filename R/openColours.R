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
  n = 100,
  alpha = 1,
  begin = 0,
  end = 1,
  direction = 1
) {
  # input checks
  if (begin > 1 || begin < 0 || end > 1 || end < 0 || begin > end) {
    cli::cli_abort(
      "{.arg begin} and {.arg end} must be between `0` and `1`, and {.arg begin} must be less than {.arg end}."
    )
  }

  if (!direction %in% c(1, -1)) {
    cli::cli_abort(
      "{.arg direction} must be either `1` (for normal order) or `-1` (for reverse order)."
    )
  }

  if (alpha > 1 || alpha < 0) {
    cli::cli_abort(
      "{.arg alpha} must be between `0` and `1`."
    )
  }

  # pre-defined brewer colour palettes sequential, diverging, qualitative
  brewer_schemes_qual <- c(
    "Accent" = 8,
    "Dark2" = 8,
    "Paired" = 12,
    "Pastel1" = 9,
    "Pastel2" = 8,
    "Set1" = 9,
    "Set2" = 8,
    "Set3" = 12
  )

  brewer_schemes_seq <- c(
    "Blues" = 9,
    "BuGn" = 9,
    "BuPu" = 9,
    "GnBu" = 9,
    "Greens" = 9,
    "Greys" = 9,
    "Oranges" = 9,
    "OrRd" = 9,
    "PuBu" = 9,
    "PuBuGn" = 9,
    "PuRd" = 9,
    "Purples" = 9,
    "RdPu" = 9,
    "Reds" = 9,
    "YlGn" = 9,
    "YlGnBu" = 9,
    "YlOrBr" = 9,
    "YlOrRd" = 9,
    "BrBG" = 11,
    "PiYG" = 11,
    "PRGn" = 11,
    "PuOr" = 11,
    "RdBu" = 11,
    "RdGy" = 11,
    "RdYlBu" = 11,
    "RdYlGn" = 11,
    "Spectral" = 11
  )

  brewer_schemes <- c(brewer_schemes_qual, brewer_schemes_seq)

  # max colours allowed for each brewer pal
  brewer.n <- c(rep(9, 18), rep(9, 9), c(8, 8, 12, 9, 8, 9, 8, 12))

  # sequential palettes
  seq_schemes <-
    c(
      "increment",
      "default",
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

  # qualitative palettes and maximum lengths
  qual_scheme_lengths <- c(
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

  # names of qualitative palettes
  qual_schemes <- names(qual_scheme_lengths)

  # combine all schemes into vector
  schemes <- c(
    seq_schemes,
    qual_schemes,
    "brewer1",
    "hue",
    "greyscale",
    names(brewer_schemes)
  )

  # get colours based on scheme
  if (length(scheme) == 1L) {
    if (scheme %in% c(names(brewer_schemes), "brewer1")) {
      scheme[scheme == "brewer1"] <- "Set1"
      cols <- brewerPalette(
        n,
        scheme,
        direction = direction,
        begin = begin,
        end = end,
        alpha = alpha
      )
    }

    if (scheme == "hue") {
      cols <- huePalette(
        n,
        direction = direction,
        begin = begin,
        end = end,
        alpha = alpha
      )
    }

    if (scheme == "greyscale") {
      if (missing(begin)) {
        begin <- 0.1
      }
      if (missing(end)) {
        end <- 0.9
      }
      cols <- grDevices::grey(seq(end, begin, length = n))
      if (direction == -1) {
        cols <- rev(cols)
      }
      hex_alpha <- sprintf("%02X", round(alpha * 255))
      cols <- paste0(cols, hex_alpha)
    }

    if (scheme %in% seq_schemes) {
      cols <- seqPalette(
        n,
        scheme = scheme,
        direction = direction,
        begin = begin,
        end = end,
        alpha = alpha
      )
    }

    if (scheme %in% qual_schemes) {
      # if n not provided, return max number o
      if (missing(n)) {
        n <- qual_scheme_lengths[[scheme]]
      }
      cols <- qualPalette(
        n,
        scheme = scheme,
        direction = direction,
        alpha = alpha
      )
    }
  }

  # if scheme isn't a scheme name, assume user has given own colours
  if (!any(scheme %in% schemes)) {
    check <- areColors(scheme)
    if (!all(check)) {
      bad_cols <- unique(names(check[!check]))
      cli::cli_abort(
        c(
          "x" = "The following are {.emph neither} valid R colours {.emph nor} {.fun openColours} palettes: {.field {bad_cols}}"
        ),
        call = NULL
      )
    }

    # interpolate
    user.cols <- grDevices::colorRampPalette(scheme)
    cols <- user.cols(n)

    # handle direction
    if (direction == -1) {
      cols <- rev(cols)
    }

    # handle alpha
    hex_alpha <- sprintf("%02X", round(alpha * 255))
    cols <- paste0(cols, hex_alpha)
  }

  if (any(scheme %in% schemes) && length(scheme) > 1L) {
    cli::cli_abort(
      c(
        "x" = "Please provide {.strong either} 1 {.fun openColours} palette {.emph or} a vector of valid R colours",
        "i" = "See {.code ?openColours} for a list of palettes."
      ),
      call = NULL
    )
  }

  cols
}

#' @rdname openColours
#' @export
openColors <- openColours

#' Function to build Brewer palettes
#' @noRd
brewerPalette <- function(n, scheme, direction, begin, end, alpha) {
  max_pal_n <- unname(brewer_schemes[names(brewer_schemes) == scheme])

  if (n >= 3 && n <= max_pal_n) {
    brewer.pal(
      n,
      scheme,
      direction = direction,
      begin = begin,
      end = end,
      alpha = alpha
    )
  } else {
    thefun <-
      suppressWarnings(grDevices::colorRampPalette(
        brewer.pal(
          max_pal_n,
          scheme,
          direction = direction,
          begin = begin,
          end = end,
          alpha = alpha
        ),
        interpolate = "spline"
      ))
    thefun(n)
  }
}

#' Build hue palette
#' @noRd
huePalette <- function(n, direction, begin, end, alpha) {
  h <- c(0, 360) + 15
  l <- 65
  c <- 100
  if ((diff(h) %% 360) < 1) {
    h[2] <- h[2] - 360 / n
  }

  # apply begin/end to hue range
  h <- h[1] + (h[2] - h[1]) * c(begin, end)

  cols <- grDevices::hcl(
    h = seq(h[1], h[2], length.out = n),
    c = c,
    l = l
  )

  # handle direction
  if (direction == -1) {
    cols <- rev(cols)
  }

  # apply alpha
  cols <- substr(cols, 1, 7)
  hex_alpha <- sprintf("%02X", round(alpha * 255))
  cols <- paste0(cols, hex_alpha)

  cols
}

#' Function to manage qualitative palettes
#' @noRd
qualPalette <- function(n, scheme, direction, alpha) {
  if (scheme %in% c("cbPalette", "okabeito")) {
    cols <- c(
      "#E69F00",
      "#56B4E9",
      "#009E73",
      "#F0E442",
      "#0072B2",
      "#D55E00",
      "#CC79A7",
      "#999999",
      "#000000"
    )
  }

  if (scheme %in% c("daqi")) {
    cols <- c(
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
    )
  }

  if (scheme %in% c("daqi.bands")) {
    cols <- c("#009900", "#ff9900", "#ff0000", "#990099")
  }

  if (scheme %in% c("gaf.cat")) {
    cols <-
      c(
        "#12436D",
        "#28A197",
        "#801650",
        "#F46A25",
        "#3D3D3D",
        "#A285D1"
      )
  }

  if (scheme %in% c("gaf.focus")) {
    cols <- c("#BFBFBF", "#12436D")
  }

  if (scheme %in% c("tol", "tol.bright")) {
    cols <-
      c(
        "#4477AA",
        "#EE6677",
        "#228833",
        "#CCBB44",
        "#66CCEE",
        "#AA3377",
        "#BBBBBB"
      )
  }

  if (scheme == "tol.muted") {
    cols <-
      c(
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
      )
  }

  if (scheme == "tol.light") {
    cols <-
      c(
        "#77AADD",
        "#EE8866",
        "#EEDD88",
        "#FFAABB",
        "#99DDFF",
        "#44BB99",
        "#BBCC33",
        "#AAAA00",
        "#DDDDDD"
      )
  }

  if (scheme == "tableau") {
    cols <- c(
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
    )
  }

  if (scheme == "observable") {
    cols <- c(
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
  }

  # handle direction
  if (direction == -1) {
    cols <- rev(cols)
  }

  # subset data
  max <- length(cols)
  if (n >= 1 && n <= max) {
    cols <- cols[1:n]
  } else {
    cli::cli_abort(
      c(
        "!" = "Too many colours selected for {.code {scheme}}.",
        "i" = "{.code n} should be between 1 and {max}."
      ),
      call = NULL
    )
  }

  # handle alpha
  cols <- substr(cols, 1, 7)
  hex_alpha <- sprintf("%02X", round(alpha * 255))
  cols <- paste0(cols, hex_alpha)

  cols
}


#' Function to manage sequential palettes
#' @noRd
seqPalette <- function(n, scheme, direction, begin, end, alpha) {
  interpolate <- "linear"

  if (scheme == "default") {
    cols <- rev(brewer.pal(11, "Spectral"))
    interpolate <- "spline"
  }

  if (scheme == "increment") {
    cols <- c(
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
    )
  }

  if (scheme == "heat") {
    cols <- brewer.pal(9, "YlOrRd")
    interpolate <- "spline"
  }

  if (scheme == "viridis") {
    cols <- c(
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
    )
  }

  if (scheme == "inferno") {
    cols <- c(
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
    )
  }

  if (scheme == "magma") {
    cols <- c(
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
    )
  }

  if (scheme == "plasma") {
    cols <- c(
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
    )
  }

  if (scheme == "cividis") {
    cols <- c(
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
    )
  }

  if (scheme == "jet") {
    cols <- c(
      "#00007F",
      "#0000FF",
      "#007FFF",
      "#00FFFF",
      "#7FFF7F",
      "#FFFF00",
      "#FF7F00",
      "#FF0000",
      "#7F0000"
    )
  }

  if (scheme == "turbo") {
    cols <- c(
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
    )
  }

  if (scheme == "gaf.seq") {
    cols <- c("#12436D", "#2073BC", "#6BACE6")
  }

  # handle begin/end
  len_cols <- length(cols)
  indices <- round(1 + (len_cols - 1) * c(begin, end))
  cols <- cols[indices[1]:indices[2]]

  # handle direction
  if (direction == -1) {
    cols <- rev(cols)
  }

  # interpolate sequential colours
  fun <- grDevices::colorRampPalette(colors = cols, interpolate = interpolate)
  cols <- fun(n)

  # handle alpha
  hex_alpha <- sprintf("%02X", round(alpha * 255))
  cols <- paste0(cols, hex_alpha)

  cols
}

#' Helper to check provided data are valid colours
#' @noRd
areColors <- function(x) {
  sapply(x, function(X) {
    tryCatch(
      is.matrix(grDevices::col2rgb(X)),
      error = function(e) {
        FALSE
      }
    )
  })
}
