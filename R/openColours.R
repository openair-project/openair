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
  "tol.highcontrast" = 3,
  "tol.vibrant" = 7,
  "tol.muted" = 10,
  "tol.mediumcontrast" = 6,
  "tol.pale" = 6,
  "tol.dark" = 6,
  "tol.light" = 9
)

# Diverging palette names (excluding brewer)
.div_schemes <- c(
  "broc",
  "cork",
  "vik",
  "lisbon",
  "tofino",
  "berlin",
  "roma",
  "bam",
  "vanimo",
  "managua"
)

# Sequential palette names (excluding brewer)
.seq_schemes <- c(
  "default",
  "hue",
  "greyscale",
  "increment",
  "heat",
  "jet",
  "turbo",
  "viridis",
  "magma",
  "inferno",
  "plasma",
  "cividis",
  "rocket",
  "mako",
  "gaf.seq",
  "acton",
  "bamO",
  "bamako",
  "batlow",
  "batlowK",
  "batlowW",
  "bilbao",
  "brocO",
  "buda",
  "corkO",
  "davos",
  "devon",
  "glasgow",
  "grayC",
  "hawaii",
  "imola",
  "lajolla",
  "lapaz",
  "lipari",
  "navia",
  "naviaW",
  "nuuk",
  "oslo",
  "romaO",
  "tokyo",
  "turku",
  "vikO"
)

# All known scheme names
.all_schemes <- unname(c(
  .seq_schemes,
  .brewer_seq_schemes,
  .div_schemes,
  .brewer_div_schemes,
  names(.qual_schemes),
  .brewer_qual_schemes,
  "brewer1"
))


#' Pre-defined openair colours and definition of user-defined colours
#'
#' This in primarily an internal openair function to make it easy for users to
#' select particular colour schemes, or define their own range of colours of a
#' user-defined length. `openColours()` and `openColors()` are synonyms.
#'
#' @section Schemes:
#'
#'   The following schemes are made available by `openColours()`. This list is
#'   also available as a table by using `openSchemes()`.
#'
#'   **Sequential Colours:**
#'
#'   * "default", "increment", "heat", "jet", "turbo", "hue", "greyscale".
#'
#'   * Simplified versions of the `viridis` colours: "viridis", "plasma",
#'   "magma", "inferno", "cividis", "turbo", "rocket" and "mako".
#'
#'   * Simplified versions of the `RColorBrewer` sequential palettes: "Blues", "BuGn",
#'   "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn",
#'   "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd".
#'
#'   * Simplified versions of Fabio Crameri's sequential palettes: "acton",
#'   "bamO", "bamako", "batlow", "batlowK", "batlowW", "bilbao", "brocO",
#'   "buda", "corkO", "davos", "devon", "glasgow", "grayC", "hawaii", "imola",
#'   "lajolla", "lapaz", "lipari", "navia", "naviaW", "nuuk", "oslo", "romaO",
#'   "tokyo", "turku", "vikO".
#'
#'   **Diverging Palettes:**
#'
#'   * Simplified versions of the `RColorBrewer` diverging palettes: "BrBG",
#'   "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral".
#'
#'   * Simplified versions of Fabio Crameri's diverging palettes: "bam",
#'   "berlin", "broc", "cork", "lisbon", "managua", "roma", "tofino", "vanimo",
#'   "vik"
#'
#'   **Qualitative Palettes:**
#'
#'   * Simplified versions of the `RColorBrewer` qualitative palettes:
#'   "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3".
#'
#'   * "okabeito" (or "cbPalette"), a colour-blind safe palette based on
#'   the work of Masataka Okabe and Kei Ito (<https://jfly.uni-koeln.de/color/>)
#'
#'   * "tol.bright" (or "tol"), "tol.highcontrast", "tol.vibrant", "tol.muted",
#'   "tol.mediumcontrast", "tol.pale", "tol.dark", and "tol.light"; colour-blind
#'   safe palettes based on the work of Paul Tol.
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
#'
#' @return A character vector of hex codes
#'
#' @author David Carslaw
#' @author Jack Davison
#'
#' @references Color Brewer: <https://colorbrewer2.org/>
#'
#'   DAQI Colours: <https://check-air-quality.service.gov.uk/>
#'
#'   UK Government Analysis Function:
#'   <https://analysisfunction.civilservice.gov.uk/policy-store/data-visualisation-colours-in-charts/>
#'
#'   Fabio Crameri's Color Schemes: Crameri, F. 2023. Scientific Colour Maps
#'   (version 8.0.1). Zenodo. DOI: \doi{doi:10.5281/zenodo.1243862}.
#'
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

    cols <- scheme
    idx <- round(1 + (length(cols) - 1) * c(begin, end))
    cols <- cols[idx[1]:idx[2]]
    cols <- grDevices::colorRampPalette(cols)(n %||% 100L)
    return(.finalise(cols, direction, alpha))
  }

  # single named scheme
  n_out <- n %||%
    if (scheme %in% names(.qual_schemes)) .qual_schemes[[scheme]] else 100L

  # brewer1 is an alias for Set1
  if (scheme == "brewer1") {
    scheme <- "Set1"
  }

  cols <- if (scheme %in% .brewer_schemes) {
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

#' List available colour schemes in `openair`
#'
#' This function returns a table of the available colour schemes in `openair`,
#' along with their type (sequential, diverging, or qualitative) and the maximum
#' number of colours available for that scheme (where applicable). This can be
#' useful for users to explore the available schemes and to check which schemes
#' can provide a certain number of colours.
#'
#' @param palette_type A character vector specifying which types of palettes to
#'   include in the output. Options are `"seq"` for sequential, `"div"` for
#'   diverging, and `"qual"` for qualitative. Multiple options can be selected
#'   (e.g., `c("seq", "div")`). All three are returned by default.
#'
#' @param n An optional whole number to filter the schemes by their maximum
#'   number of colours. Only schemes that can provide at least `n` colours will
#'   be included in the output. This only applies to qualitative schemes, as
#'   sequential and diverging schemes can be interpolated to any number of
#'   colours.
#'
#' @author Jack Davison
#'
#' @export
openSchemes <- function(palette_type = c("seq", "div", "qual"), n = NULL) {
  palette_type = rlang::arg_match(
    palette_type,
    c("seq", "div", "qual"),
    multiple = TRUE
  )

  out <- tibble::tibble(
    palette = character(0),
    type = character(0),
    max_n = numeric(0)
  )

  if (any(palette_type == "seq")) {
    out <- dplyr::bind_rows(
      out,
      tibble::tibble(
        palette = unique(unname(c(.seq_schemes, .brewer_seq_schemes))),
        type = "sequential",
        max_n = NA
      )
    )
  }

  if (any(palette_type == "div")) {
    out <- dplyr::bind_rows(
      out,
      tibble::tibble(
        palette = unique(unname(c(.div_schemes, .brewer_div_schemes))),
        type = "diverging",
        max_n = NA
      )
    )
  }

  if (any(palette_type == "qual")) {
    out <- dplyr::bind_rows(
      out,
      tibble::tibble(
        palette = unique(unname(c(names(.qual_schemes), .brewer_qual_schemes))),
        type = "sequential",
        max_n = as.numeric(c(
          unname(.qual_schemes),
          qualnum[.brewer_qual_schemes]
        ))
      )
    )
  }

  if (!is.null(n)) {
    out <- dplyr::filter_out(out, .data$max_n < n)
  }

  return(out)
}

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
    rocket = c(
      "#03051AFF",
      "#2A1636FF",
      "#551E4FFF",
      "#841E5AFF",
      "#B41658FF",
      "#DD2C45FF",
      "#F06043FF",
      "#F5936AFF",
      "#F6C09EFF",
      "#FAEBDDFF"
    ),
    mako = c(
      "#0B0405FF",
      "#28192FFF",
      "#3B2F5EFF",
      "#40498EFF",
      "#366A9FFF",
      "#348AA6FF",
      "#38AAACFF",
      "#54C9ADFF",
      "#A0DFB9FF",
      "#DEF5E5FF"
    ),
    gaf.seq = c("#12436D", "#2073BC", "#6BACE6"),
    bam = c(
      '#65024B',
      '#9E3C85',
      '#C86FB1',
      '#E4AED6',
      '#F5E3EF',
      '#EFF3E5',
      '#C1DAA2',
      '#7BA755',
      '#457B2A',
      '#0D4C00'
    ),
    berlin = c(
      '#9EB0FF',
      '#5AA3DA',
      '#2D7597',
      '#1A4256',
      '#11181D',
      '#270D01',
      '#501803',
      '#8A3F2A',
      '#C37469',
      '#FFADAD'
    ),
    broc = c(
      '#2C1A4C',
      '#284477',
      '#4A759F',
      '#8BA7C2',
      '#CED9E5',
      '#E8E8D2',
      '#C5C58F',
      '#8C8C55',
      '#555527',
      '#262600'
    ),
    cork = c(
      '#2C194C',
      '#284578',
      '#48739E',
      '#83A1BE',
      '#CBD7E3',
      '#D2E1D2',
      '#8EB38D',
      '#4E884D',
      '#1C5B19',
      '#0F2903'
    ),
    lisbon = c(
      '#E6E5FF',
      '#9AAED2',
      '#5177A4',
      '#1E4368',
      '#111D2B',
      '#262419',
      '#575134',
      '#8D8556',
      '#C8C28F',
      '#FFFFD9'
    ),
    managua = c(
      '#FFCF67',
      '#DC9955',
      '#B96C46',
      '#92463B',
      '#662A3C',
      '#4E315D',
      '#4E5593',
      '#5B80BC',
      '#6DB0DD',
      '#81E7FF'
    ),
    roma = c(
      '#7E1700',
      '#995215',
      '#B07F2A',
      '#C8B455',
      '#D0E3A3',
      '#A4E5D2',
      '#5DC1D3',
      '#3191C1',
      '#2064AE',
      '#033198'
    ),
    tofino = c(
      '#DED9FF',
      '#92A3DD',
      '#4A6BAD',
      '#273C65',
      '#111825',
      '#112113',
      '#244D28',
      '#3F8144',
      '#87B86F',
      '#DBE69B'
    ),
    vanimo = c(
      '#FFCDFD',
      '#D280C3',
      '#A1498E',
      '#5C244F',
      '#22141C',
      '#1B1D11',
      '#36491A',
      '#5A7C2A',
      '#84B44C',
      '#BEFDA5'
    ),
    vik = c(
      '#001261',
      '#033E7D',
      '#1D6E9C',
      '#71A8C4',
      '#CADDE7',
      '#EACEBE',
      '#D39774',
      '#BD6432',
      '#8B2706',
      '#590008'
    ),
    acton = c(
      '#260D40',
      '#3E2E5E',
      '#534C7A',
      '#71618C',
      '#93658F',
      '#BC6992',
      '#D586AA',
      '#DFABC9',
      '#E8CDE4',
      '#F0EAFA'
    ),
    bamako = c(
      '#003B47',
      '#0E433F',
      '#1F4E34',
      '#365E26',
      '#527014',
      '#728302',
      '#988D03',
      '#BEA82E',
      '#E1C76D',
      '#FFE5AD'
    ),
    bamO = c(
      '#4F3043',
      '#874979',
      '#B776A6',
      '#D7B0C9',
      '#D6CBC6',
      '#BAC4A2',
      '#839165',
      '#606448',
      '#4C423B',
      '#4E3042'
    ),
    batlow = c(
      '#011959',
      '#103F60',
      '#1B5962',
      '#3C6D56',
      '#687B3D',
      '#9C892B',
      '#D29343',
      '#F8A17B',
      '#FDB6BB',
      '#FACCFA'
    ),
    batlowK = c(
      '#04050A',
      '#1A2C41',
      '#35525F',
      '#4F6657',
      '#6F7845',
      '#A08D38',
      '#D89E50',
      '#F6A986',
      '#FDB8BE',
      '#FACCFA'
    ),
    batlowW = c(
      '#011959',
      '#103F60',
      '#1A5862',
      '#396E59',
      '#647E42',
      '#999032',
      '#D0A35A',
      '#EEAF91',
      '#FED7D5',
      '#FFFEFE'
    ),
    bilbao = c(
      '#4C0001',
      '#732529',
      '#93454B',
      '#A06257',
      '#A6775C',
      '#AC8C60',
      '#B5A772',
      '#C2BCA3',
      '#D1D1CD',
      '#FFFFFF'
    ),
    brocO = c(
      '#372F38',
      '#384164',
      '#547199',
      '#8AA3BF',
      '#C1CCD0',
      '#CACBAB',
      '#9E9E6C',
      '#6A683C',
      '#443F29',
      '#372F37'
    ),
    buda = c(
      '#B301B3',
      '#B32B9E',
      '#B94792',
      '#C2618A',
      '#CA7982',
      '#D0917B',
      '#D7AA75',
      '#DDC36E',
      '#E5DE68',
      '#FFFF66'
    ),
    corkO = c(
      '#3F3E3A',
      '#3E445D',
      '#536D93',
      '#849EBA',
      '#ADC4C7',
      '#A1C4A6',
      '#73A36F',
      '#4E723A',
      '#434F2C',
      '#3F3E3A'
    ),
    davos = c(
      '#00054A',
      '#112C71',
      '#285190',
      '#43709D',
      '#5E8598',
      '#78968D',
      '#99AD88',
      '#C9D29E',
      '#F2F2D1',
      '#FEFEFE'
    ),
    devon = c(
      '#2C1A4C',
      '#293467',
      '#275085',
      '#3669AD',
      '#6181D0',
      '#979AE6',
      '#BAB3F1',
      '#D0CCF5',
      '#E7E4FA',
      '#FFFFFF'
    ),
    glasgow = c(
      '#361338',
      '#4F1A22',
      '#6A250B',
      '#744500',
      '#716311',
      '#687B47',
      '#60927D',
      '#74A9B0',
      '#A5BDD7',
      '#DBD3FF'
    ),
    grayC = c(
      '#000000',
      '#232323',
      '#3C3C3C',
      '#565656',
      '#6C6C6C',
      '#818181',
      '#9A9A9A',
      '#B6B6B6',
      '#D7D7D7',
      '#FFFFFF'
    ),
    hawaii = c(
      '#8C0273',
      '#922A58',
      '#964642',
      '#996330',
      '#9D831E',
      '#97A829',
      '#80C55F',
      '#65D89C',
      '#6BEADA',
      '#B3F2FD'
    ),
    imola = c(
      '#1A33B3',
      '#2446A9',
      '#2D599F',
      '#396B94',
      '#497B85',
      '#5F927B',
      '#7BAE74',
      '#98CB6D',
      '#C3E967',
      '#FFFF66'
    ),
    lajolla = c(
      '#191900',
      '#33220F',
      '#5A2F22',
      '#8F403D',
      '#C7504B',
      '#E0714F',
      '#E79452',
      '#EEB555',
      '#F8DE7B',
      '#FFFECB'
    ),
    lapaz = c(
      '#1A0C64',
      '#232D7B',
      '#2A4B8E',
      '#36679D',
      '#4C80A3',
      '#6D95A1',
      '#94A298',
      '#BFB199',
      '#EED2BF',
      '#FEF2F3'
    ),
    lipari = c(
      '#031326',
      '#13385A',
      '#46587A',
      '#6B5F76',
      '#8E616C',
      '#BB6461',
      '#E57B62',
      '#E7A279',
      '#E8C89E',
      '#FDF5DA'
    ),
    navia = c(
      '#031327',
      '#07345E',
      '#14578A',
      '#28728F',
      '#388285',
      '#4A937A',
      '#65AB6C',
      '#98CB6F',
      '#D7E5A7',
      '#FCF4D9'
    ),
    naviaW = c(
      '#041427',
      '#093660',
      '#1A5C89',
      '#2E778D',
      '#3F8884',
      '#559C79',
      '#7BBA71',
      '#BEDF95',
      '#EBF5D4',
      '#FEFEFD'
    ),
    nuuk = c(
      '#05598C',
      '#296283',
      '#497183',
      '#6F878D',
      '#929C96',
      '#ABAD96',
      '#BAB98D',
      '#C7C684',
      '#DFDF8D',
      '#FEFEB2'
    ),
    oslo = c(
      '#010101',
      '#0D1B29',
      '#133250',
      '#1F4C7B',
      '#3869A8',
      '#6489C6',
      '#89A0CA',
      '#AAB6CA',
      '#D3D5DA',
      '#FFFFFF'
    ),
    romaO = c(
      '#733957',
      '#874036',
      '#A2662C',
      '#C3A34B',
      '#D5D893',
      '#B4DEC5',
      '#74BBCD',
      '#4F87B8',
      '#5B538B',
      '#723959'
    ),
    tokyo = c(
      '#1C0E34',
      '#4B2044',
      '#683F4E',
      '#705751',
      '#726753',
      '#757A54',
      '#7C9859',
      '#8DC16E',
      '#C0ECAA',
      '#EFFCDD'
    ),
    turku = c(
      '#000000',
      '#242420',
      '#414135',
      '#5F5F44',
      '#7E7C52',
      '#A89864',
      '#CFA67C',
      '#EAAD98',
      '#FCC6C2',
      '#FFE6E6'
    ),
    vikO = c(
      '#4F1A3D',
      '#3A3567',
      '#3A6696',
      '#759EBC',
      '#BEC2C5',
      '#D9AE97',
      '#C57C56',
      '#943D24',
      '#681922',
      '#50193C'
    )
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
    cbPalette = c(
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
    tol = c(
      "#4477AA",
      "#EE6677",
      "#228833",
      "#CCBB44",
      "#66CCEE",
      "#AA3377",
      "#BBBBBB"
    ),
    tol.bright = c(
      "#4477AA",
      "#EE6677",
      "#228833",
      "#CCBB44",
      "#66CCEE",
      "#AA3377",
      "#BBBBBB"
    ),
    tol.highcontrast = c(
      "#004488",
      "#DDAA33",
      "#BB5566"
    ),
    tol.vibrant = c(
      "#EE7733",
      "#0077BB",
      "#33BBEE",
      "#EE3377",
      "#CC3311",
      "#009988",
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
    tol.mediumcontrast = c(
      "#EECC66",
      "#EE99AA",
      "#6699CC",
      "#997700",
      "#994455",
      "#004488"
    ),
    tol.pale = c(
      "#BBCCEE",
      "#CCEEFF",
      "#CCDDAA",
      "#EEEEBB",
      "#FFCCCC",
      "#DDDDDD"
    ),
    tol.dark = c(
      "#222255",
      "#225555",
      "#225522",
      "#666633",
      "#663333",
      "#555555"
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
