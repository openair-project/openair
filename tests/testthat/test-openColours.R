test_that("returns correct number of colours", {
  expect_length(openColours("jet", 5), 5)
  expect_length(openColours("default", 10), 10)
  expect_length(openColours("viridis", 1), 1)
  expect_length(openColours("Blues", 7), 7)
})

test_that("returns 100 colours by default for sequential palettes", {
  expect_length(openColours("jet"), 100)
  expect_length(openColours("default"), 100)
  expect_length(openColours("viridis"), 100)
  expect_length(openColours("Blues"), 100)
})

test_that("qualitative palettes return max colours by default", {
  expect_length(openColours("okabeito"), 9)
  expect_length(openColours("daqi"), 10)
  expect_length(openColours("daqi.bands"), 4)
  expect_length(openColours("tol"), 7)
  expect_length(openColours("tableau"), 10)
})

test_that("output is a character vector of 9-character hex codes", {
  cols <- openColours("jet", 5)
  expect_type(cols, "character")
  expect_true(all(nchar(cols) == 9L))
  expect_true(all(grepl("^#[0-9A-Fa-f]{8}$", cols)))
})

test_that("alpha = 1 gives fully opaque colours (FF suffix)", {
  cols <- openColours("jet", 5, alpha = 1)
  expect_true(all(substr(cols, 8, 9) == "FF"))
})

test_that("alpha = 0 gives fully transparent colours (00 suffix)", {
  cols <- openColours("jet", 5, alpha = 0)
  expect_true(all(substr(cols, 8, 9) == "00"))
})

test_that("alpha is applied correctly at intermediate values", {
  cols <- openColours("jet", 5, alpha = 0.5)
  # 0.5 * 255 = 127.5, rounds to 128 = 0x80
  expect_true(all(substr(cols, 8, 9) == "80"))
})

test_that("alpha applies to all palette types", {
  for (scheme in c("jet", "default", "Blues", "okabeito", "hue", "greyscale")) {
    cols <- openColours(scheme, 5, alpha = 0)
    expect_true(all(substr(cols, 8, 9) == "00"))
  }
})

test_that("direction = -1 reverses colours", {
  fwd <- openColours("jet", 10)
  rev <- openColours("jet", 10, direction = -1)
  expect_equal(fwd, rev(rev))
})

test_that("direction reversal works for all palette types", {
  for (scheme in c("jet", "default", "Blues", "hue", "greyscale")) {
    fwd <- openColours(scheme, 10)
    bwd <- openColours(scheme, 10, direction = -1)
    expect_equal(fwd, rev(bwd))
  }
})

test_that("begin and end subset sequential palettes", {
  full <- openColours("viridis", 100)
  sub <- openColours("viridis", 100, begin = 0.25, end = 0.75)
  # subsetted palette should differ from full
  expect_false(identical(full, sub))
  # begin = 0, end = 1 should reproduce the full palette
  expect_equal(openColours("viridis", 100, begin = 0, end = 1), full)
})

test_that("begin and end work for brewer sequential palettes", {
  full <- openColours("Blues", 100)
  sub <- openColours("Blues", 100, begin = 0.2, end = 0.8)
  expect_false(identical(full, sub))
})

test_that("begin and end work for hue palette", {
  full <- openColours("hue", 10)
  sub <- openColours("hue", 10, begin = 0.2, end = 0.8)
  expect_false(identical(full, sub))
})

test_that("begin and end work for greyscale palette", {
  full <- openColours("greyscale", 10)
  sub <- openColours("greyscale", 10, begin = 0.2, end = 0.8)
  expect_false(identical(full, sub))
})

test_that("brewer1 is an alias for Set1", {
  expect_equal(openColours("brewer1", 5), openColours("Set1", 5))
})

test_that("cbPalette is an alias for okabeito", {
  expect_equal(openColours("cbPalette", 5), openColours("okabeito", 5))
})

test_that("tol is an alias for tol.bright", {
  expect_equal(openColours("tol", 5), openColours("tol.bright", 5))
})

test_that("openColors is a synonym for openColours", {
  expect_equal(openColors("jet", 5), openColours("jet", 5))
})

test_that("user-supplied colours are interpolated correctly", {
  cols <- openColours(c("red", "blue"), 10)
  expect_length(cols, 10)
  expect_true(all(grepl("^#[0-9A-Fa-f]{8}$", cols)))
})

test_that("user-supplied colours respect direction and alpha", {
  fwd <- openColours(c("red", "blue"), 10)
  bwd <- openColours(c("red", "blue"), 10, direction = -1)
  expect_equal(fwd, rev(bwd))

  cols <- openColours(c("red", "blue"), 5, alpha = 0)
  expect_true(all(substr(cols, 8, 9) == "00"))
})

test_that("invalid alpha throws an error", {
  expect_error(openColours("jet", 5, alpha = -0.1), "alpha")
  expect_error(openColours("jet", 5, alpha = 1.1), "alpha")
})

test_that("invalid direction throws an error", {
  expect_error(openColours("jet", 5, direction = 0), "direction")
  expect_error(openColours("jet", 5, direction = 2), "direction")
  expect_error(openColours("jet", 5, direction = "reverse"), "direction")
})

test_that("invalid begin/end throws an error", {
  expect_error(openColours("jet", 5, begin = -0.1), "begin")
  expect_error(openColours("jet", 5, end = 1.1), "end")
  expect_error(openColours("jet", 5, begin = 0.8, end = 0.2), "begin")
  expect_error(openColours("jet", 5, begin = 0.5, end = 0.5), "begin")
})

test_that("requesting too many colours from a qualitative palette throws an error", {
  expect_error(openColours("daqi.bands", 5), "Too many")
  expect_error(openColours("gaf.focus", 3), "Too many")
})

test_that("invalid scheme name throws an error", {
  expect_error(openColours("not_a_scheme", 5))
})

test_that("mixing a scheme name with colour names throws an error", {
  expect_error(openColours(c("jet", "red"), 5))
})

test_that("all sequential schemes return output without error", {
  seq_schemes <- c(
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
  for (s in seq_schemes) {
    expect_no_error(openColours(s, 10))
  }
})

test_that("all qualitative schemes return output without error", {
  qual_schemes <- c(
    "okabeito",
    "cbPalette",
    "daqi",
    "daqi.bands",
    "gaf.cat",
    "gaf.focus",
    "tableau",
    "observable",
    "tol",
    "tol.bright",
    "tol.muted",
    "tol.light"
  )
  for (s in qual_schemes) {
    expect_no_error(openColours(s))
  }
})

test_that("all brewer sequential schemes return output without error", {
  brewer_seq <- c(
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
    "YlOrRd"
  )
  for (s in brewer_seq) {
    expect_no_error(openColours(s, 10))
  }
})

test_that("all brewer diverging schemes return output without error", {
  brewer_div <- c(
    "BrBG",
    "PiYG",
    "PRGn",
    "PuOr",
    "RdBu",
    "RdGy",
    "RdYlBu",
    "RdYlGn",
    "Spectral"
  )
  for (s in brewer_div) {
    expect_no_error(openColours(s, 10))
  }
})

test_that("interpolation works when n exceeds brewer palette max", {
  # Blues max is 9 — requesting more should interpolate without error
  expect_no_error(openColours("Blues", 50))
  expect_length(openColours("Blues", 50), 50)
})

test_that("greyscale uses conservative begin/end defaults", {
  # default greyscale should avoid pure white (#FFFFFF) and pure black (#000000)
  cols <- openColours("greyscale", 10)
  rgb_vals <- grDevices::col2rgb(substr(cols, 1, 7))
  expect_true(all(rgb_vals > 0)) # no pure black
  expect_true(all(rgb_vals < 255)) # no pure white
})

test_that("openSchemes returns a tibble with the correct columns and types", {
  out <- openSchemes()
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("palette", "type", "max_n"))
  expect_type(out$palette, "character")
  expect_type(out$type, "character")
  expect_type(out$max_n, "double")
})

test_that("openSchemes returns all three types by default", {
  out <- openSchemes()
  expect_true("sequential" %in% out$type)
  expect_true("diverging" %in% out$type)
  # qualitative palettes are labelled "sequential" — check via max_n instead
  expect_true(!all(is.na(out$max_n)))
})

test_that("palette_type = 'seq' returns only sequential rows", {
  out <- openSchemes(palette_type = "seq")
  expect_true(all(out$type == "sequential"))
  expect_true(all(is.na(out$max_n)))
})

test_that("palette_type = 'div' returns only diverging rows", {
  out <- openSchemes(palette_type = "div")
  expect_true(all(out$type == "diverging"))
  expect_true(all(is.na(out$max_n)))
})

test_that("palette_type = 'qual' returns only rows with a finite max_n", {
  out <- openSchemes(palette_type = "qual")
  expect_true(!anyNA(out$max_n))
  expect_true(all(out$max_n > 0))
})

test_that("palette_type combinations return the union of the requested types", {
  seq_div <- openSchemes(palette_type = c("seq", "div"))
  seq_out <- openSchemes(palette_type = "seq")
  div_out <- openSchemes(palette_type = "div")
  expect_equal(nrow(seq_div), nrow(seq_out) + nrow(div_out))
  expect_false("diverging" %in% openSchemes(palette_type = "seq")$type)
  expect_false("sequential" %in% openSchemes(palette_type = "div")$type)
})

test_that("all three palette_type values together equals the default", {
  all_explicit <- openSchemes(palette_type = c("seq", "div", "qual"))
  all_default <- openSchemes()
  expect_equal(all_explicit, all_default)
})

test_that("n filters qualitative schemes that are too small", {
  out_unfiltered <- openSchemes(palette_type = "qual")
  out_filtered <- openSchemes(palette_type = "qual", n = 8)

  # All retained schemes must support at least n colours
  expect_true(all(out_filtered$max_n >= 8))

  # Filtering must remove at least one scheme (e.g. tol.highcontrast has 3)
  expect_lt(nrow(out_filtered), nrow(out_unfiltered))
})

test_that("n does not filter sequential or diverging rows (max_n is NA)", {
  out_seq <- openSchemes(palette_type = "seq", n = 50)
  expect_equal(nrow(out_seq), nrow(openSchemes(palette_type = "seq")))

  out_div <- openSchemes(palette_type = "div", n = 50)
  expect_equal(nrow(out_div), nrow(openSchemes(palette_type = "div")))
})

test_that("n = 1 retains all qualitative schemes", {
  out <- openSchemes(palette_type = "qual", n = 1)
  expect_equal(nrow(out), nrow(openSchemes(palette_type = "qual")))
})

test_that("n equal to a scheme's max_n retains that scheme", {
  # tol.highcontrast has max_n = 3
  out <- openSchemes(palette_type = "qual", n = 3)
  expect_true("tol.highcontrast" %in% out$palette)
})

test_that("n one above a scheme's max_n excludes that scheme", {
  out <- openSchemes(palette_type = "qual", n = 4)
  expect_false("tol.highcontrast" %in% out$palette)
})

test_that("known sequential palettes are present", {
  out <- openSchemes(palette_type = "seq")
  expect_true("viridis" %in% out$palette)
  expect_true("default" %in% out$palette)
  expect_true("jet" %in% out$palette)
})

test_that("known diverging palettes are present", {
  out <- openSchemes(palette_type = "div")
  expect_true("vik" %in% out$palette)
  expect_true("berlin" %in% out$palette)
})

test_that("known qualitative palettes are present with correct max_n", {
  out <- openSchemes(palette_type = "qual")
  expect_true("okabeito" %in% out$palette)
  expect_equal(out$max_n[out$palette == "okabeito"], 9)
  expect_equal(out$max_n[out$palette == "tol.highcontrast"], 3)
  expect_equal(out$max_n[out$palette == "daqi"], 10)
})

test_that("palettes are not duplicated", {
  out <- openSchemes()
  expect_equal(nrow(out), length(unique(out$palette)))
})

test_that("invalid palette_type is rejected", {
  expect_error(openSchemes(palette_type = "bad"))
  expect_error(openSchemes(palette_type = c("seq", "bad")))
})
