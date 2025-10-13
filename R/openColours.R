#' Pre-defined openair colours and definition of user-defined colours
#'
#' [openColours()] is a convenience function for accessing pre-defined colours
#' or interpolating user-defined colours. Many useful and accessible themes are
#' defined within `openair` for convenience, all of which are categorised and
#' referenced within [openSchemes]. Further, [openColours()] is often used
#' within *other* openair functions; for example `polarPlot(mydata, cols =
#' "viridis")` will use `openColours("viridis")` to access a colour palette.
#'
#' @section Details:
#'
#'   Because of the way many of the schemes have been developed they only exist
#'   over certain number of colour gradations (typically 3--10). If the required
#'   number of colours is outside of this range, `openair` will interpolate the
#'   colours with a warning.
#'
#'   Each of the pre-defined schemes have merits and their use will depend on a
#'   particular situation. For showing incrementing concentrations, e.g., high
#'   concentrations emphasised, then `"default"`, `"turbo"`, `"viridis"`, and
#'   `"increment"` are very useful.
#'
#'   To colour-code categorical-type problems, e.g., colours for different
#'   pollutants, `"hue"` and `"brewer1"` are useful.
#'
#'   When publishing in black and white, `"greyscale"` is often convenient. With
#'   most `openair` functions, as well as generating a greyscale colour
#'   gradient, it also resets strip background and other coloured text and lines
#'   to greyscale values.
#'
#'   Failing that, the user can define their own schemes based on R colour
#'   names. To see the full list of names, see [colors()].
#'
#' @section Schemes:
#'
#'   The following schemes are made available by [openColours()]. They are also
#'   listed in [openSchemes].
#'
#'   **Sequential Colours:**
#'
#'   * `"default"`, `"increment"`, `"brewer1"`, `"heat"`, `"jet"`, `"turbo"`,
#'   `"hue"`, `"greyscale"`.
#'
#'   * Simplified versions of the `viridis` colours: `"viridis"`, `"plasma"`,
#'   `"magma"`, `"inferno"`, and `"cividis"`.
#'
#'   * Simplified versions of the `RColorBrewer` sequential palettes: `"Blues"`, `"BuGn"`,
#'   `"BuPu"`, `"GnBu"`, `"Greens"`, `"Greys"`, `"Oranges"`, `"OrRd"`, `"PuBu"`,
#'   `"PuBuGn"`, `"PuRd"`, `"Purples"`, `"RdPu"`, `"Reds"`, `"YlGn"`,
#'   `"YlGnBu"`, `"YlOrBr"`, `"YlOrRd"`.
#'
#'   **Diverging Palettes:**
#'
#'   * Simplified versions of the `RColorBrewer` diverging palettes: `"BrBG"`,
#'   `"PiYG"`, `"PRGn"`, `"PuOr"`, `"RdBu"`, `"RdGy"`, `"RdYlBu"`, `"RdYlGn"`,
#'   `"Spectral"`.
#'
#'   **Qualitative Palettes:**
#'
#'   * Simplified versions of the `RColorBrewer` qualitative palettes:
#'   `"Accent"`, `"Dark2"`, `"Paired"`, `"Pastel1"`, `"Pastel2"`, `"Set1"`,
#'   `"Set2"`, `"Set3"`.
#'
#'   * `"okabeito"` (or `"cbPalette"`), a colour-blind safe palette based on
#'   the work of Masataka Okabe and Kei Ito.
#'
#'   * `"tol.bright"` (or `"tol"`), `"tol.muted"` and `"tol.light"`, colour-blind safe
#'   palettes based on the work of Paul Tol.
#'
#'   * `"tableau"` and `"observable"`, aliases for the
#'   "Tableau10" and "Observable10" colour palettes. These could be useful for
#'   consistency between openair plots and with figures made in these software
#'   programs.
#'
#'   **UK Government Palettes:**
#'
#'   * `"daqi"` and `"daqi.bands"`, the colours associated with the UK daily air
#'   quality index; `"daqi"` (a palette of 10 colours, corresponding to each
#'   index value) or `"daqi.bands"` (4 colours, corresponding to each band -
#'   Low, Moderate, High, and Very High). These colours may be useful in figures
#'   like [calendarPlot()].
#'
#'   * `"gaf.cat"`, `"gaf.focus"` and `"gaf.seq"`, colours recommended by the UK Government Analysis function.
#'   `"gaf.cat"` will return the 'categorical' palette (max 6 colours),
#'   `"gaf.focus"` the 'focus' palette (max 2 colours), and `"gaf.seq"` the
#'   'sequential' palette.
#'
#' @section The `openSchemes` dataset:
#'
#'   To help understand the available palettes in [openColours()], the
#'   [openSchemes] dataset is provided. This is a `data.frame` listing all
#'   schemes, categorising them into diverging, sequential, or qualitative. This
#'   dataset can be filtered to find useful scales for your specific
#'   visualisation.
#'
#'   \describe{
#'   \item{category}{The category - one of `"div"` (diverging), `"qual"` (qualitative), or `"seq"` (sequential).}
#'   \item{scheme}{The scheme name - to be provided to [openColours()]}
#'   \item{max_n}{The maximum number of pre-defined colours in a qualitative palette. Setting `n` greater than this value is not recommended.}
#'   \item{colorblind}{Whether or not the scheme is colour-blind accessible. This is a estimation based on available information from third parties; users should assess the accesibility of all visualisations they create.}
#'   \item{origin}{The original authors of the palette.}
#'   \item{reference}{A URL reference for further reading about the colour schemes.}
#'   }
#'
#' @param scheme Any one of the pre-defined `openair` schemes (e.g.,
#'   `"increment"`) or a user-defined palette (e.g., `c("red", "orange",
#'   "gold")`). See [openColours()] for a full list of available schemes.
#'   Starting a pre-defined scheme with `"-"` (e.g., `"-increment"`) will
#'   reverse `direction`; this is mainly useful for use within other `openair`
#'   functions (e.g., `polarPlot(mydata, cols = "-viridis")`).
#'
#' @param n number of colours required.
#'
#' @param direction Sets the order of colours in the scale. If `1`, the default,
#'   outputs the default order of the `scheme`. If `-1`, the order of colours is
#'   reversed.
#'
#' @export
#' @rdname open-colours
#'
#' @return A character vector of hex codes
#' @author David Carslaw
#' @author Jack Davison
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
openColours <- function(scheme = "default", n = 100, direction = 1L) {
  if (!rlang::is_integerish(direction, n = 1L) || !direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be either {1L} or {-1L}.")
  }

  # get lists of schemes
  schemes <- openSchemes$scheme
  seq_schemes <- openSchemes$scheme[openSchemes$category %in% c("seq", "div")]
  qual_schemes <- openSchemes$scheme[openSchemes$category == "qual"]

  # get colours based on scheme
  if (length(scheme) == 1L) {
    # if scheme starts with "-", reverse direction
    if (startsWith(scheme, "-")) {
      scheme <- gsub("\\-", "", scheme)
      direction <- direction * -1
    }

    # edge case for brewer
    scheme[scheme == "brewer1"] <- "Set1"

    if (scheme %in% seq_schemes) {
      cols <- seqPalette(n, scheme = scheme)
    }
    if (scheme %in% qual_schemes) {
      # if n not provided, return max number o
      if (missing(n)) {
        n <- qual_scheme_lengths[[scheme]]
      }
      cols <- qualPalette(n, scheme = scheme)
    }
  }

  # if scheme isn't a scheme name, assume user has given own colours
  if (!any(scheme %in% schemes)) {
    check <- areColors(scheme)
    if (any(!check)) {
      bad_cols <- unique(names(check[!check]))
      cli::cli_abort(
        c(
          "x" = "The following are {.emph neither} valid R colours {.emph nor} {.fun openair::openColours} palettes: {.field {bad_cols}}",
          "i" = "See {.fun openair::openSchemes} for a list of available in-built palettes."
        ),
        call = NULL
      )
    }

    if (length(scheme) > 1) {
      # interpolate
      user.cols <- grDevices::colorRampPalette(scheme)
      cols <- user.cols(n)
    } else {
      cols <- rep(scheme, n)
    }
  }

  if (any(scheme %in% schemes) & length(scheme) > 1L) {
    cli::cli_abort(
      c(
        "x" = "Please provide {.strong either} 1 {.fun openColours} palette {.emph or} a vector of valid R colours",
        "i" = "See {.fun openair::openSchemes} for a list of available in-built palettes."
      ),
      call = NULL
    )
  }

  if (direction == -1) {
    cols <- rev(cols)
  }

  cols
}

#' Function to manage qualitative palettes
#' @noRd
qualPalette <- function(n, scheme) {
  if (scheme == "Accent") {
    cols <- c(
      "#7FC97F",
      "#BEAED4",
      "#FDC086",
      "#FFFF99",
      "#386CB0",
      "#F0027F",
      "#BF5B17",
      "#666666"
    )
  }

  if (scheme == "Dark2") {
    cols <- c(
      "#1B9E77",
      "#D95F02",
      "#7570B3",
      "#E7298A",
      "#66A61E",
      "#E6AB02",
      "#A6761D",
      "#666666"
    )
  }

  if (scheme == "Paired") {
    cols <- c(
      "#A6CEE3",
      "#1F78B4",
      "#B2DF8A",
      "#33A02C",
      "#FB9A99",
      "#E31A1C",
      "#FDBF6F",
      "#FF7F00",
      "#CAB2D6",
      "#6A3D9A",
      "#FFFF99",
      "#B15928"
    )
  }

  if (scheme == "Pastel1") {
    cols <- c(
      "#FBB4AE",
      "#B3CDE3",
      "#CCEBC5",
      "#DECBE4",
      "#FED9A6",
      "#FFFFCC",
      "#E5D8BD",
      "#FDDAEC",
      "#F2F2F2"
    )
  }

  if (scheme == "Pastel2") {
    cols <- c(
      "#B3E2CD",
      "#FDCDAC",
      "#CBD5E8",
      "#F4CAE4",
      "#E6F5C9",
      "#FFF2AE",
      "#F1E2CC",
      "#CCCCCC"
    )
  }

  if (scheme == "Set1") {
    cols <- c(
      "#E41A1C",
      "#377EB8",
      "#4DAF4A",
      "#984EA3",
      "#FF7F00",
      "#FFFF33",
      "#A65628",
      "#F781BF",
      "#999999"
    )
  }

  if (scheme == "Set2") {
    cols <- c(
      "#66C2A5",
      "#FC8D62",
      "#8DA0CB",
      "#E78AC3",
      "#A6D854",
      "#FFD92F",
      "#E5C494",
      "#B3B3B3"
    )
  }

  if (scheme == "Set3") {
    cols <- c(
      "#8DD3C7",
      "#FFFFB3",
      "#BEBADA",
      "#FB8072",
      "#80B1D3",
      "#FDB462",
      "#B3DE69",
      "#FCCDE5",
      "#D9D9D9",
      "#BC80BD",
      "#CCEBC5",
      "#FFED6F"
    )
  }

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

  max <- length(cols)

  if (n >= 1 && n <= max) {
    cols <- cols[1:n]
  } else {
    cli::cli_warn(
      c(
        "!" = "Too many colours selected for {.code {scheme}}.",
        "i" = "{.code n} should be between 1 and {max}.",
        "i" = "Colours will be interpolated to produce {n} colours."
      ),
      call = NULL
    )

    color_fun <-
      suppressWarnings(grDevices::colorRampPalette(
        cols,
        interpolate = "spline"
      ))
    cols <- color_fun(n)
  }

  cols
}


#' Function to manage sequential palettes
#' @noRd
seqPalette <- function(n, scheme) {
  interpolate <- "linear"

  if (scheme %in% c("default", "Spectral")) {
    cols <- c(
      "#9E0142",
      "#D53E4F",
      "#F46D43",
      "#FDAE61",
      "#FEE08B",
      "#FFFFBF",
      "#E6F598",
      "#ABDDA4",
      "#66C2A5",
      "#3288BD",
      "#5E4FA2"
    )

    if (scheme == "default") {
      cols <- rev(cols)
      interpolate <- "spline"
    }
  }

  if (scheme == "hue") {
    cols <-
      c(
        "#F8766D",
        "#EA8331",
        "#D89000",
        "#C09B00",
        "#A3A500",
        "#7CAE00",
        "#39B600",
        "#00BB4E",
        "#00BF7D",
        "#00C1A3",
        "#00BFC4",
        "#00BAE0",
        "#00B0F6",
        "#35A2FF",
        "#9590FF",
        "#C77CFF",
        "#E76BF3",
        "#FA62DB",
        "#FF62BC",
        "#FF6A98"
      )
  }

  if (scheme == "greyscale") {
    cols <- c(
      "#E6E6E6",
      "#D7D7D7",
      "#C8C8C8",
      "#BABABA",
      "#ABABAB",
      "#9D9D9D",
      "#8E8E8E",
      "#808080",
      "#717171",
      "#626262",
      "#545454",
      "#454545",
      "#373737",
      "#282828",
      "#1A1A1A"
    )
  }

  if (scheme == "BrBG") {
    cols <- c(
      "#543005",
      "#8C510A",
      "#BF812D",
      "#DFC27D",
      "#F6E8C3",
      "#F5F5F5",
      "#C7EAE5",
      "#80CDC1",
      "#35978F",
      "#01665E",
      "#003C30"
    )
  }

  if (scheme == "PiYG") {
    cols <- c(
      "#8E0152",
      "#C51B7D",
      "#DE77AE",
      "#F1B6DA",
      "#FDE0EF",
      "#F7F7F7",
      "#E6F5D0",
      "#B8E186",
      "#7FBC41",
      "#4D9221",
      "#276419"
    )
  }

  if (scheme == "PRGn") {
    cols <- c(
      "#40004B",
      "#762A83",
      "#9970AB",
      "#C2A5CF",
      "#E7D4E8",
      "#F7F7F7",
      "#D9F0D3",
      "#A6DBA0",
      "#5AAE61",
      "#1B7837",
      "#00441B"
    )
  }

  if (scheme == "PuOr") {
    cols <- c(
      "#7F3B08",
      "#B35806",
      "#E08214",
      "#FDB863",
      "#FEE0B6",
      "#F7F7F7",
      "#D8DAEB",
      "#B2ABD2",
      "#8073AC",
      "#542788",
      "#2D004B"
    )
  }

  if (scheme == "RdBu") {
    cols <- c(
      "#67001F",
      "#B2182B",
      "#D6604D",
      "#F4A582",
      "#FDDBC7",
      "#F7F7F7",
      "#D1E5F0",
      "#92C5DE",
      "#4393C3",
      "#2166AC",
      "#053061"
    )
  }

  if (scheme == "RdGy") {
    cols <- c(
      "#67001F",
      "#B2182B",
      "#D6604D",
      "#F4A582",
      "#FDDBC7",
      "#FFFFFF",
      "#E0E0E0",
      "#BABABA",
      "#878787",
      "#4D4D4D",
      "#1A1A1A"
    )
  }

  if (scheme == "RdYlBu") {
    cols <- c(
      "#A50026",
      "#D73027",
      "#F46D43",
      "#FDAE61",
      "#FEE090",
      "#FFFFBF",
      "#E0F3F8",
      "#ABD9E9",
      "#74ADD1",
      "#4575B4",
      "#313695"
    )
  }

  if (scheme == "RdYlGn") {
    cols <- c(
      "#A50026",
      "#D73027",
      "#F46D43",
      "#FDAE61",
      "#FEE08B",
      "#FFFFBF",
      "#D9EF8B",
      "#A6D96A",
      "#66BD63",
      "#1A9850",
      "#006837"
    )
  }

  if (scheme == "Blues") {
    cols <- c(
      "#F7FBFF",
      "#DEEBF7",
      "#C6DBEF",
      "#9ECAE1",
      "#6BAED6",
      "#4292C6",
      "#2171B5",
      "#08519C",
      "#08306B"
    )
  }

  if (scheme == "BuGn") {
    cols <- c(
      "#F7FCFD",
      "#E5F5F9",
      "#CCECE6",
      "#99D8C9",
      "#66C2A4",
      "#41AE76",
      "#238B45",
      "#006D2C",
      "#00441B"
    )
  }

  if (scheme == "BuPu") {
    cols <- c(
      "#F7FCFD",
      "#E0ECF4",
      "#BFD3E6",
      "#9EBCDA",
      "#8C96C6",
      "#8C6BB1",
      "#88419D",
      "#810F7C",
      "#4D004B"
    )
  }

  if (scheme == "GnBu") {
    cols <- c(
      "#F7FCF0",
      "#E0F3DB",
      "#CCEBC5",
      "#A8DDB5",
      "#7BCCC4",
      "#4EB3D3",
      "#2B8CBE",
      "#0868AC",
      "#084081"
    )
  }

  if (scheme == "Greens") {
    cols <- c(
      "#F7FCF5",
      "#E5F5E0",
      "#C7E9C0",
      "#A1D99B",
      "#74C476",
      "#41AB5D",
      "#238B45",
      "#006D2C",
      "#00441B"
    )
  }

  if (scheme == "Greys") {
    cols <- c(
      "#FFFFFF",
      "#F0F0F0",
      "#D9D9D9",
      "#BDBDBD",
      "#969696",
      "#737373",
      "#525252",
      "#252525",
      "#000000"
    )
  }

  if (scheme == "Oranges") {
    cols <- c(
      "#FFF5EB",
      "#FEE6CE",
      "#FDD0A2",
      "#FDAE6B",
      "#FD8D3C",
      "#F16913",
      "#D94801",
      "#A63603",
      "#7F2704"
    )
  }

  if (scheme == "OrRd") {
    cols <- c(
      "#FFF7EC",
      "#FEE8C8",
      "#FDD49E",
      "#FDBB84",
      "#FC8D59",
      "#EF6548",
      "#D7301F",
      "#B30000",
      "#7F0000"
    )
  }

  if (scheme == "PuBu") {
    cols <- c(
      "#FFF7FB",
      "#ECE7F2",
      "#D0D1E6",
      "#A6BDDB",
      "#74A9CF",
      "#3690C0",
      "#0570B0",
      "#045A8D",
      "#023858"
    )
  }

  if (scheme == "PuBuGn") {
    cols <- c(
      "#FFF7FB",
      "#ECE2F0",
      "#D0D1E6",
      "#A6BDDB",
      "#67A9CF",
      "#3690C0",
      "#02818A",
      "#016C59",
      "#014636"
    )
  }

  if (scheme == "PuRd") {
    cols <- c(
      "#F7F4F9",
      "#E7E1EF",
      "#D4B9DA",
      "#C994C7",
      "#DF65B0",
      "#E7298A",
      "#CE1256",
      "#980043",
      "#67001F"
    )
  }

  if (scheme == "Purples") {
    cols <- c(
      "#FCFBFD",
      "#EFEDF5",
      "#DADAEB",
      "#BCBDDC",
      "#9E9AC8",
      "#807DBA",
      "#6A51A3",
      "#54278F",
      "#3F007D"
    )
  }

  if (scheme == "RdPu") {
    cols <- c(
      "#FFF7F3",
      "#FDE0DD",
      "#FCC5C0",
      "#FA9FB5",
      "#F768A1",
      "#DD3497",
      "#AE017E",
      "#7A0177",
      "#49006A"
    )
  }

  if (scheme == "Reds") {
    cols <- c(
      "#FFF5F0",
      "#FEE0D2",
      "#FCBBA1",
      "#FC9272",
      "#FB6A4A",
      "#EF3B2C",
      "#CB181D",
      "#A50F15",
      "#67000D"
    )
  }

  if (scheme == "YlGn") {
    cols <- c(
      "#FFFFE5",
      "#F7FCB9",
      "#D9F0A3",
      "#ADDD8E",
      "#78C679",
      "#41AB5D",
      "#238443",
      "#006837",
      "#004529"
    )
  }

  if (scheme == "YlGnBu") {
    cols <- c(
      "#FFFFD9",
      "#EDF8B1",
      "#C7E9B4",
      "#7FCDBB",
      "#41B6C4",
      "#1D91C0",
      "#225EA8",
      "#253494",
      "#081D58"
    )
  }

  if (scheme == "YlOrBr") {
    cols <- c(
      "#FFFFE5",
      "#FFF7BC",
      "#FEE391",
      "#FEC44F",
      "#FE9929",
      "#EC7014",
      "#CC4C02",
      "#993404",
      "#662506"
    )
  }

  if (scheme %in% c("YlOrRd", "heat")) {
    cols <- c(
      "#FFFFCC",
      "#FFEDA0",
      "#FED976",
      "#FEB24C",
      "#FD8D3C",
      "#FC4E2A",
      "#E31A1C",
      "#BD0026",
      "#800026"
    )

    if (scheme == "heat") {
      interpolate <- "spline"
    }
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

  fun <- grDevices::colorRampPalette(colors = cols, interpolate = interpolate)

  cols <- fun(n)

  return(cols)
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

#' @rdname open-colours
#' @examples
#' openSchemes
"openSchemes"
