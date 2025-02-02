test_that("bootMeanDF works", {
  out <- bootMeanDF(iris$Sepal.Length)
  expect_length(out, 4L)
  expect_equal(names(out), c("mean", "min", "max", "n"))
  expect_equal(nrow(out), 1L)
  expect_type(out$mean, "double")
  expect_type(out$min, "double")
  expect_type(out$max, "double")
  expect_type(out$n, "integer")
})

test_that("binData works", {
  out <- binData(iris, bin = "Sepal.Length", uncer = "Petal.Length")
  expect_equal(
    names(out),
    c(
      "interval",
      "Sepal.Length",
      "Sepal.Width",
      "Petal.Length",
      "Petal.Width",
      "mean",
      "min",
      "max",
      "n"
    )
  )

  out_type <- binData(iris,
                      bin = "Sepal.Length",
                      uncer = "Petal.Length",
                      type = "Species")

  expect_equal(
    names(out_type),
    c(
      "interval",
      "Species",
      "Sepal.Length",
      "Sepal.Width",
      "Petal.Length",
      "Petal.Width",
      "mean",
      "min",
      "max",
      "n"
    )
  )
})