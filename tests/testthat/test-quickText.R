# quickText() builds plotmath expressions through a long series of string
# substitutions, so outputs are recorded as snapshots to catch any unintended
# changes when the rules are edited.

# show the output of quickText() for each input in both parse modes
show_quicktext <- function(x) {
  for (txt in x) {
    parsed <- paste(deparse(quickText(txt)), collapse = "")
    unparsed <- quickText(txt, parse = FALSE)
    cat(
      deparse(txt),
      "\n  parse = TRUE:  ",
      parsed,
      "\n  parse = FALSE: ",
      unparsed,
      "\n",
      sep = ""
    )
  }
}

test_that("pollutant names are formatted", {
  expect_snapshot(show_quicktext(c(
    "no2",
    "NO2",
    "no",
    "nox",
    "NOx",
    "NOX",
    "nh3",
    "NH3",
    "co",
    "co2",
    "CO2",
    "nmhc",
    "o3",
    "O3",
    "ozone",
    "so2",
    "SO2",
    "h2s",
    "H2S",
    "ch4",
    "CH4"
  )))
})

test_that("particulate matter names are formatted", {
  expect_snapshot(show_quicktext(c(
    "pm1",
    "PM1",
    "pm4",
    "PM4",
    "pm10",
    "PM10",
    "pm25",
    "PM25",
    "pm2.5",
    "PM2.5",
    "pmc",
    "PMc",
    "pmcoarse",
    "PMcoarse",
    "pmf",
    "PMf",
    "pmfine",
    "PMfine",
    "pmtot",
    "PMtot"
  )))
})

test_that("meteorological names are formatted", {
  expect_snapshot(show_quicktext(c(
    "ws",
    "WS",
    "wd",
    "WD",
    "air_temp",
    "rh"
  )))
})

test_that("units are formatted", {
  expect_snapshot(show_quicktext(c(
    "ug/m3",
    "ug.m-3",
    "ug m-3",
    "ugm-3",
    "mg/m3",
    "ng/m3",
    "g/m3",
    "cm-3",
    "m/s",
    "m.s-1",
    "m s-1",
    "m/s2",
    "km2",
    "g/km",
    "g/s",
    "kW/t",
    "g/hour",
    "g/hr",
    "g/kg",
    "km/hr",
    "km/h",
    "km/hour",
    "km/hr/s",
    "km/h/s",
    "umol/m2",
    "umol/m2/s"
  )))
})

test_that("symbols are formatted", {
  expect_snapshot(show_quicktext(c(
    "dgrC",
    "degreeC",
    "deg. C",
    "degreesC",
    "degrees",
    "Delta",
    "delta",
    "r2",
    "R2",
    "tau "
  )))
})

test_that("combinations and plain text are formatted", {
  expect_snapshot(show_quicktext(c(
    "my no2 label",
    "pm10 [ ug.m-3 ]",
    "pm2.5 (ug/m3)",
    "o3 (ppb)",
    "no2 ug/m3",
    "temp degrees",
    "wind m/s",
    "site A",
    "it's no2",
    "no2\nug/m3"
  )))
})

test_that("names are not formatted inside longer words", {
  # names are only formatted when not preceded by a letter or followed by a
  # letter or digit; underscores count as separators
  expect_snapshot(show_quicktext(c(
    "year2020",
    "no3",
    "NO3",
    "pm100",
    "Morocco data",
    "no2conc",
    "no2_roadside",
    "roadside_no2",
    "pm10_ugm3",
    "Mean no2, o3 and co",
    "no data"
  )))
})

test_that("backslashes and quotes are kept", {
  expect_identical(deparse(quickText("a\\")), deparse(expression(paste("a\\"))))
  expect_identical(
    deparse(quickText("it's")),
    deparse(expression(paste("it's")))
  )
  # quotes on their own or next to each other used to be removed along with
  # empty strings, giving invalid code
  for (txt in c("'", "''", "a''b", "\\'")) {
    out <- quickText(txt)
    expect_true(is.expression(out), info = txt)
    expect_identical(out[[1]][[2]], txt, info = txt)
  }
})

test_that("spaces between formatted names and units are kept", {
  expect_snapshot(show_quicktext(c(
    "ug/m3 no2",
    "Delta no2",
    "no2 o3",
    "co2 g/km",
    "m/s ws",
    "km/h no2",
    "Delta no2 (ug/m3)"
  )))
})

test_that("any number of newlines are stacked as lines", {
  expect_snapshot(show_quicktext(c(
    "Line 1\nLine 2",
    "a\nb\nc",
    "Mean no2\n(ug/m3)\n2020\nroadside",
    "no2\n",
    "\nno2"
  )))
})

test_that("all lines of multi-line text are kept", {
  # every line should appear in the expression, however many there are
  for (n in 2:5) {
    lines <- paste("line", seq_len(n))
    out <- quickText(paste(lines, collapse = "\n"))
    expect_true(is.expression(out))
    out_text <- paste(deparse(out), collapse = "")
    for (l in lines) {
      expect_match(out_text, l, fixed = TRUE)
    }
  }
})

test_that("adjacent symbols are formatted", {
  expect_snapshot(show_quicktext(c(
    "Delta Delta",
    "degrees degrees",
    "ug/m3 ug/m3",
    "tau tau"
  )))
})

test_that("character vectors are formatted element-wise", {
  out <- quickText(c("no2", "site A", "pm10"))
  expect_true(is.expression(out))
  expect_length(out, 3L)
  expect_identical(out[2], quickText("site A"))

  out_unparsed <- quickText(c("no2", "site A"), parse = FALSE)
  expect_type(out_unparsed, "character")
  expect_length(out_unparsed, 2L)
})

test_that("label_openair() names labels after the input", {
  out <- label_openair(c("no2", "site A"))
  expect_true(is.expression(out))
  expect_named(out, c("no2", "site A"))
  expect_named(label_openair(c(a = "no2", b = "o3")), c("a", "b"))
  expect_named(label_openair(c("no2", "o3"), parse = FALSE), c("no2", "o3"))
})

test_that("label_openair() keeps empty input empty", {
  # scales can call label functions with no breaks
  expect_length(label_openair(character(0)), 0L)
  expect_length(label_openair(character(0), parse = FALSE), 0L)
})

test_that("label_openair() returns input unchanged if auto_text = FALSE", {
  expect_identical(label_openair("no2", auto_text = FALSE), "no2")
})

test_that("NULL is returned unchanged, so no empty label is drawn", {
  expect_null(quickText(NULL))
  expect_null(quickText(NULL, parse = FALSE))
  expect_null(quickText(NULL, auto.text = FALSE))
})

test_that("short names that are common words only match on their own", {
  expect_snapshot(show_quicktext(c(
    "co",
    "co (ppm)",
    "(co)",
    "[co]",
    "co/nox",
    "co, no2",
    "co: hourly",
    "co-located",
    "co.",
    "co_roadside",
    "no",
    "no. 5",
    "no-data",
    "NO2 and no"
  )))
})

test_that("Delta can prefix names but not start words", {
  expect_snapshot(show_quicktext(c(
    "Delta",
    "DeltaNO2",
    "delta_no2",
    "Delta2",
    "Deltaville",
    "deltas"
  )))
})

test_that("names are not formatted when followed by a decimal part", {
  expect_snapshot(show_quicktext(c(
    "pm1.5",
    "pm2.5",
    "pm1.",
    "pm10. data"
  )))
})

test_that("units are not formatted when followed by a digit", {
  for (txt in c("km25", "ug/m30", "cm-30", "m/s10")) {
    expect_identical(quickText(txt), as.expression(list(call("paste", txt))))
  }
  # longer units ending in a digit still win
  expect_identical(quickText("m/s2"), expression(paste("m s"^-2)))
})

test_that("units are not formatted when followed by a letter", {
  expect_snapshot(show_quicktext(c(
    "speed (m/sec)",
    "km/hours",
    "g/second",
    "ug/m3s",
    "km/s",
    "kg/m3",
    "m/s_mean",
    "degreesF"
  )))
})

test_that("Windows line endings are treated as newlines", {
  expect_identical(quickText("a\r\nb"), quickText("a\nb"))
  expect_identical(quickText("a\r\n"), quickText("a\n"))
  expect_identical(quickText("a\r\nb", parse = FALSE), "paste(\"a b\")")
})

test_that("empty input gives empty output", {
  expect_identical(quickText(character(0)), expression())
  expect_identical(quickText(character(0), parse = FALSE), character(0))
})

test_that("micro sign and Greek mu are formatted like 'u'", {
  micro <- "\u00b5"
  mu <- "\u03bc"
  for (prefix in c(micro, mu)) {
    for (unit in c("g/m3", "g.m-3", "g m-3", "gm-3", "mol/m2", "mol/m2/s")) {
      expect_identical(
        quickText(paste0(prefix, unit)),
        quickText(paste0("u", unit)),
        info = paste0(prefix, unit)
      )
    }
  }
  expect_identical(
    quickText(paste0("PM2.5 (", micro, "g/m3)")),
    quickText("PM2.5 (ug/m3)")
  )
})

test_that("parse = FALSE deparses expressions", {
  out <- quickText(expression(NO[2], x^2), parse = FALSE)
  expect_identical(out, c("NO[2]", "x^2"))
})

test_that("label_openair() names factor labels after their levels", {
  expect_named(label_openair(factor(c("no2", "o3"))), c("no2", "o3"))
})

test_that("non-character inputs are coerced", {
  expect_identical(quickText(""), expression(paste()))
  expect_identical(quickText(NA), expression(paste("NA")))
  expect_identical(quickText(5), expression(paste("5")))
  expect_identical(quickText(factor("no2")), quickText("no2"))
})

test_that("auto.text = FALSE returns text unchanged", {
  expect_identical(quickText("no2 ug/m3", auto.text = FALSE), "no2 ug/m3")
})

test_that("expressions are returned unchanged", {
  ex <- expression(NO[2])
  expect_identical(quickText(ex), ex)
})

test_that("parse = TRUE returns an expression", {
  expect_true(is.expression(quickText("no2 ug/m3")))
  expect_true(is.expression(quickText("site A")))
})

test_that("parse = FALSE always returns valid plotmath", {
  inputs <- c(
    "no2",
    "pm10",
    "site A",
    "no2 ug/m3",
    "temp degrees",
    "wind m/s",
    "o3 (ppb)",
    "pm2.5 (ug/m3)",
    "pmcoarse",
    "co2 g/km",
    "Delta no2",
    "Delta Delta",
    "degrees degrees",
    "ug/m3 ug/m3",
    "'",
    "''"
  )
  for (txt in inputs) {
    out <- quickText(txt, parse = FALSE)
    expect_no_error(parse(text = out), message = txt)
  }
})

test_that("facet labels ending in units can be drawn", {
  dat <- data.frame(
    x = 1:4,
    y = 1:4,
    f = c("no2 ug/m3", "no2 ug/m3", "wind m/s", "wind m/s")
  )
  p <- ggplot2::ggplot(dat, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~f, labeller = labeller_openair())
  expect_no_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)))
})

test_that("long PM names are not split by shorter PM names", {
  expect_identical(
    deparse(quickText("pmcoarse")),
    deparse(quickText("pmc"))
  )
  expect_identical(
    deparse(quickText("PMfine")),
    deparse(quickText("PMf"))
  )
})
