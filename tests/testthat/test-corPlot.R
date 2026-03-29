# Small, fixed slice — just enough rows for stable correlations, fast to run
dat <- selectByDate(mydata, year = 2003, month = 1)

# Run once and reuse across most tests
result <- corPlot(dat, plot = FALSE)
result_denro <- corPlot(dat, plot = FALSE, dendrogram = TRUE)
result_noclust <- corPlot(dat, plot = FALSE, cluster = FALSE)

# --- Return value structure --------------------------------------------------

test_that("corPlot returns an openair object with the expected components", {
  expect_s3_class(result, "openair")
  expect_named(result, c("plot", "data", "call", "clust"))
  expect_s3_class(result$plot, "ggplot")
  expect_s3_class(result$data, "tbl_df")
  expect_true(all(c("row", "col", "cor", "pval") %in% names(result$data)))
})

# --- Correlation values ------------------------------------------------------

test_that("correlation values are in [-1, 1] and p-values in [0, 1]", {
  expect_true(all(result$data$cor >= -1 & result$data$cor <= 1, na.rm = TRUE))
  expect_true(all(result$data$pval >= 0 & result$data$pval <= 1, na.rm = TRUE))
})

test_that("diagonal correlations are all 1", {
  diagonal <- result$data[result$data$row == result$data$col, ]
  expect_true(all(abs(diagonal$cor - 1) < 1e-10))
})

test_that("correlation matrix is symmetric (cor[x,y] == cor[y,x])", {
  merged <- dplyr::inner_join(
    result$data,
    result$data,
    by = c("row" = "col", "col" = "row"),
    suffix = c("_xy", "_yx")
  )
  expect_equal(merged$cor_xy, merged$cor_yx, tolerance = 1e-10)
})

# --- cluster -----------------------------------------------------------------

test_that("cluster = TRUE populates clust with an hclust; cluster = FALSE leaves it NULL", {
  expect_s3_class(result$clust, "hclust")
  expect_null(result_noclust$clust)
})

# --- triangle / diagonal -----------------------------------------------------

test_that("triangle = 'upper' has no below-diagonal entries", {
  upper <- corPlot(dat, plot = FALSE, cluster = FALSE, triangle = "upper")
  expect_true(all(as.integer(upper$data$row) <= as.integer(upper$data$col)))
})

test_that("triangle = 'lower' has no above-diagonal entries", {
  lower <- corPlot(dat, plot = FALSE, cluster = FALSE, triangle = "lower")
  expect_true(all(as.integer(lower$data$row) >= as.integer(lower$data$col)))
})

test_that("diagonal = FALSE removes self-pairs and reduces row count", {
  no_diag <- corPlot(dat, plot = FALSE, diagonal = FALSE)
  expect_true(all(no_diag$data$row != no_diag$data$col))
  expect_lt(nrow(no_diag$data), nrow(result$data))
})

# --- pollutants argument -----------------------------------------------------

test_that("restricting pollutants limits variables and reduces output size", {
  limited <- corPlot(dat, pollutants = c("no2", "o3", "pm10"), plot = FALSE)
  expect_lt(nrow(limited$data), nrow(result$data))
  expect_setequal(
    unique(as.character(limited$data$row)),
    c("no2", "o3", "pm10")
  )
})

# --- method ------------------------------------------------------------------

test_that("spearman and pearson produce different correlations", {
  spearman <- corPlot(dat, method = "spearman", plot = FALSE)$data
  expect_false(isTRUE(all.equal(result$data$cor, spearman$cor)))
})

# --- input validation --------------------------------------------------------

test_that("corPlot errors with fewer than two numeric variables", {
  expect_error(corPlot(dat[, c("date", "no2")], plot = FALSE), regexp = "two")
})

test_that("corPlot errors when more than one type is supplied", {
  expect_error(
    corPlot(dat, type = c("season", "daylight"), plot = FALSE),
    regexp = "type"
  )
})
