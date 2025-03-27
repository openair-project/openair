test_that("opencolours works", {
  default <- openColours()
  expect_vector(default)
  expect_type(default, "character")
  expect_length(default, 100L)

  daqi <- openColours("daqi")
  expect_length(daqi, 10L)

  viridis <- openColours("viridis", n = 10)
  expect_length(viridis, 10L)

  user <- openColours(c("red", "blue", "purple"), n = 5L)
  expect_type(user, "character")
  expect_length(user, 5L)

  # error if weird combinations are given
  expect_error(openColours("something_stupid"))
  expect_error(openColours(c("viridis", "something_stupid")))
  expect_error(openColours(c("red", "blue", "viridis")))
  expect_error(openColours(c("viridis", "heat")))
  expect_error(openColours("daqi", n = 100L))
})

test_that("all palettes work", {
  # pre-defined brewer colour palettes sequential, diverging, qualitative
  brewer.col <- c(
    "Blues",
    "BuGn",
    "BuPu",
    "GnBu",
    "Greens",
    "Greys",
    "Oranges",
    "OrRd",
    "PuBu",
    "PuBuGn",
    "PuRd",
    "Purples",
    "RdPu",
    "Reds",
    "YlGn",
    "YlGnBu",
    "YlOrBr",
    "YlOrRd",
    "BrBG",
    "PiYG",
    "PRGn",
    "PuOr",
    "RdBu",
    "RdGy",
    "RdYlBu",
    "RdYlGn",
    "Spectral",
    "Accent",
    "Dark2",
    "Paired",
    "Pastel1",
    "Pastel2",
    "Set1",
    "Set2",
    "Set3"
  )

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
    brewer.col
  )

  expect_no_error(sapply(schemes, openColours))
})
