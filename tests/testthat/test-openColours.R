# openColours is pure and fast — no mydata needed, no subsampling required.
# Run representative schemes once and reuse.
cols_default <- openColours("default", 10)
cols_jet <- openColours("jet", 10)
cols_hue <- openColours("hue", 5)

# --- Return type and length --------------------------------------------------

test_that("openColours returns a character vector of the requested length", {
  expect_type(cols_default, "character")
  expect_length(cols_default, 10L)
  expect_length(openColours("jet", 1L), 1L)
  expect_length(openColours("viridis", 50L), 50L)
})

test_that("all returned values are valid hex colours", {
  is_hex <- function(x) all(grepl("^#[0-9A-Fa-f]{6,8}$", x))
  expect_true(is_hex(cols_default))
  expect_true(is_hex(cols_jet))
  expect_true(is_hex(cols_hue))
})

# --- Sequential schemes ------------------------------------------------------

test_that("sequential schemes return the correct number of colours", {
  seq_schemes <- c(
    "increment",
    "default",
    "heat",
    "jet",
    "turbo",
    "viridis",
    "plasma",
    "magma",
    "inferno",
    "cividis",
    "gaf.seq"
  )
  for (s in seq_schemes) {
    expect_length(openColours(s, 7), 7L)
  }
})

# --- Brewer schemes ----------------------------------------------------------

test_that("RColorBrewer schemes interpolate to requested n", {
  expect_length(openColours("Blues", 5), 5L)
  expect_length(openColours("Blues", 20), 20L) # beyond native max -> interpolation
  expect_length(openColours("RdBu", 8), 8L)
})

# --- Qualitative schemes and their caps  -------------------------------------

test_that("qualitative schemes return correct colours up to their max", {
  expect_length(openColours("okabeito", 5), 5L)
  expect_length(openColours("daqi", 10), 10L)
  expect_length(openColours("daqi.bands", 4), 4L)
  expect_length(openColours("gaf.cat", 6), 6L)
  expect_length(openColours("gaf.focus", 2), 2L)
  expect_length(openColours("tol.bright", 7), 7L)
  expect_length(openColours("tol.muted", 10), 10L)
  expect_length(openColours("tableau", 10), 10L)
  expect_length(openColours("observable", 10), 10L)
})

test_that("qualitative schemes error when n exceeds maximum", {
  expect_error(openColours("daqi.bands", 5), regexp = "Too many")
  expect_error(openColours("gaf.focus", 3), regexp = "Too many")
  expect_error(openColours("gaf.cat", 7), regexp = "Too many")
})

# --- Aliases -----------------------------------------------------------------

test_that("cbPalette is an alias for okabeito", {
  expect_equal(openColours("cbPalette", 5), openColours("okabeito", 5))
})

test_that("tol is an alias for tol.bright", {
  expect_equal(openColours("tol", 5), openColours("tol.bright", 5))
})

# --- User-defined colours ----------------------------------------------------

test_that("user-supplied named colours are accepted and interpolated", {
  cols <- openColours(c("red", "blue"), 8)
  expect_length(cols, 8L)
  expect_type(cols, "character")
})

test_that("user-supplied hex colours are accepted", {
  cols <- openColours(c("#FF0000", "#00FF00", "#0000FF"), 6)
  expect_length(cols, 6L)
})

test_that("a single valid colour repeated n times is returned", {
  cols <- openColours("red", 4)
  expect_length(cols, 4L)
  expect_true(all(cols == cols[1]))
})

# --- Error handling ----------------------------------------------------------

test_that("invalid colour name raises an informative error", {
  expect_error(openColours("notacolour"), regexp = "valid R colours")
  expect_error(openColours(c("red", "notacolour")), regexp = "valid R colours")
})

test_that("mixing a scheme name with extra colours raises an error", {
  expect_error(openColours(c("jet", "red")), regexp = "either")
})

# --- Greyscale and hue -------------------------------------------------------

test_that("greyscale returns values that decode to equal R, G, B channels", {
  cols <- openColours("greyscale", 5)
  rgb_vals <- grDevices::col2rgb(cols)
  # for grey, R == G == B
  expect_true(all(rgb_vals["red", ] == rgb_vals["green", ]))
  expect_true(all(rgb_vals["green", ] == rgb_vals["blue", ]))
})

test_that("hue palette produces n distinct colours", {
  cols <- openColours("hue", 6)
  expect_length(cols, 6L)
  expect_equal(length(unique(cols)), 6L)
})
