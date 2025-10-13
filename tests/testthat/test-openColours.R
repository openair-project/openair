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
  expect_warning(openColours("daqi", n = 100L))
})

test_that("all palettes work", {
  expect_no_error(sapply(openSchemes$scheme, openColours))
})
