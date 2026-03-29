# TaylorDiagram tests — plot = FALSE throughout
# No GAM fitting — just correlation and SD calculations — so no skip_on_cran needed

set.seed(42)
n <- 500L
syn <- data.frame(
  date = seq(as.POSIXct("2023-01-01", tz = "GMT"), by = "hour", length.out = n),
  obs = rnorm(n, 50, 15),
  mod1 = rnorm(n, 52, 16), # slightly biased, similar variance
  mod2 = rnorm(n, 50, 20), # more variable
  model = rep(c("A", "B"), each = n / 2)
)

# Shared result — single model, no group
td <- TaylorDiagram(syn, obs = "obs", mod = "mod1", plot = FALSE)

# =============================================================================
# Return value structure
# =============================================================================

test_that("TaylorDiagram returns an openair object with expected components", {
  expect_s3_class(td, "openair")
  expect_named(td, c("plot", "data", "call"))
  expect_s3_class(td$plot, "ggplot")
  expect_s3_class(td$data, "data.frame")
})

test_that("$data has R, sd.obs and sd.mod columns", {
  expect_true(all(c("R", "sd.obs", "sd.mod") %in% names(td$data)))
})

# =============================================================================
# Statistical values
# =============================================================================

test_that("R is in [-1, 1]", {
  expect_true(all(td$data$R >= -1 & td$data$R <= 1))
})

test_that("R matches a direct cor() call", {
  expected_r <- cor(syn$obs, syn$mod1, use = "pairwise")
  expect_equal(td$data$R, expected_r, tolerance = 1e-9)
})

test_that("sd.obs matches sd() of the observations", {
  expected_sd <- sd(syn$obs)
  expect_equal(td$data$sd.obs, expected_sd, tolerance = 1e-9)
})

test_that("sd.mod matches sd() of the predictions", {
  expected_sd <- sd(syn$mod1)
  expect_equal(td$data$sd.mod, expected_sd, tolerance = 1e-9)
})

test_that("sd values are positive", {
  expect_true(all(td$data$sd.obs > 0))
  expect_true(all(td$data$sd.mod > 0))
})

# =============================================================================
# normalise
# =============================================================================

test_that("normalise = TRUE sets sd.obs to 1 and scales sd.mod accordingly", {
  td_norm <- TaylorDiagram(
    syn,
    obs = "obs",
    mod = "mod1",
    normalise = TRUE,
    plot = FALSE
  )
  expect_equal(td_norm$data$sd.obs, 1, tolerance = 1e-9)
  expected_sdmod_norm <- sd(syn$mod1) / sd(syn$obs)
  expect_equal(td_norm$data$sd.mod, expected_sdmod_norm, tolerance = 1e-9)
})

# =============================================================================
# group
# =============================================================================

test_that("group produces one row per unique group value", {
  td_grp <- TaylorDiagram(
    syn,
    obs = "obs",
    mod = "mod1",
    group = "model",
    plot = FALSE
  )
  expect_equal(nrow(td_grp$data), 2L)
  expect_true("model" %in% names(td_grp$data))
})

test_that("group and type cannot overlap", {
  expect_error(
    TaylorDiagram(
      syn,
      obs = "obs",
      mod = "mod1",
      group = "model",
      type = "model",
      plot = FALSE
    ),
    regexp = "group"
  )
})

# =============================================================================
# two mod columns (arrow mode)
# =============================================================================

test_that("two mod values produce a taylor_mod_id column in $data", {
  td_two <- TaylorDiagram(
    syn,
    obs = "obs",
    mod = c("mod1", "mod2"),
    plot = FALSE
  )
  expect_true("taylor_mod_id" %in% names(td_two$data))
  expect_setequal(levels(td_two$data$taylor_mod_id), c("mod1", "mod2"))
})

test_that("two mod values produce two rows (one per mod)", {
  td_two <- TaylorDiagram(
    syn,
    obs = "obs",
    mod = c("mod1", "mod2"),
    plot = FALSE
  )
  expect_equal(nrow(td_two$data), 2L)
})

# =============================================================================
# type conditioning
# =============================================================================

test_that("type = 'daylight' produces two rows in $data", {
  td_s <- TaylorDiagram(
    syn,
    obs = "obs",
    mod = "mod1",
    type = "daylight",
    plot = FALSE
  )
  expect_equal(nrow(td_s$data), 2L)
  expect_true("daylight" %in% names(td_s$data))
})
