## these are functions from Rand Wilcox, which have been slightly modified
## see http://www-rcf.usc.edu/~rwilcox/

regci <- function(
  x,
  y,
  regfun = tsreg,
  nboot = 599,
  alpha = 0.05,
  autocor = autocor,
  SEED = TRUE,
  pr = TRUE,
  ...
) {
  x <- as.matrix(x)
  p <- ncol(x)
  p1 <- p + 1

  xy <- elimna(cbind(x, y))
  x <- as.matrix(xy[, 1:p, drop = FALSE])
  y <- xy[, p1]

  if (SEED) set.seed(2)

  block.length <- if (autocor) round(length(y)^(1 / 3)) else 1
  # Buhlmann and Kunsch (1994): block length = n^(1/3)
  data <- t(samp_boot_block(length(y), nboot, block.length))

  # bvec: (p+1) x nboot — row 1 = bootstrap intercepts, row 2 = bootstrap slopes
  bvec <- apply(data, 1, regboot, x, y, regfun, ...)

  ilow <- round((alpha / 2) * nboot) + 1
  ihi  <- nboot - round((alpha / 2) * nboot)

  # Sort each parameter's bootstrap distribution, then extract CI bounds
  bvec_sorted <- apply(bvec, 1, sort)  # nboot x p1
  pvec <- apply(bvec, 1, function(b) {
    p_val <- (sum(b < 0) + 0.5 * sum(b == 0)) / nboot
    2 * min(p_val, 1 - p_val)
  })

  regci <- matrix(
    0, p1, 5,
    dimnames = list(
      c("intercept", rep("X", p)),
      c("ci.low", "ci.up", "Estimate", "S.E.", "p-value")
    )
  )
  regci[, 1] <- bvec_sorted[ilow, ]
  regci[, 2] <- bvec_sorted[ihi, ]
  regci[, 3] <- regfun(x, y)$coef
  regci[, 4] <- apply(bvec, 1, stats::sd)
  regci[, 5] <- pvec

  list(regci = regci)
}

elimna <- function(m) {
  m <- as.matrix(m)
  m[stats::complete.cases(m), , drop = FALSE]
}

regboot <- function(isub, x, y, regfun, ...) {
  regfun(x[isub, , drop = FALSE], y[isub], ...)$coef
}

tsreg <- function(x, y, iter = 10, ...) {
  # Compute Theil-Sen regression estimator.
  # Uses Gauss-Seidel algorithm for multiple predictors.
  x <- as.matrix(x)
  p <- ncol(x)

  xy <- elimna(cbind(x, y))
  x <- as.matrix(xy[, 1:p, drop = FALSE])
  y <- xy[, p + 1]

  if (p == 1) {
    fit <- tsp1reg(x, y)
    return(list(coef = fit$coef, residuals = fit$residuals))
  }

  # Multivariate: Gauss-Seidel algorithm
  temp <- vapply(seq_len(p), function(j) tsp1reg(x[, j], y)$coef[2], numeric(1))
  alpha <- stats::median(y - x %*% temp)
  r <- matrix(NA, nrow(x), p)
  for (it in seq_len(iter)) {
    for (j in seq_len(p)) {
      r[, j] <- y - x %*% temp - alpha + temp[j] * x[, j]
      temp[j] <- tsp1reg(x[, j], r[, j])$coef[2]
    }
    alpha <- stats::median(y - x %*% temp)
  }
  coef <- c(alpha, temp)
  list(coef = coef, residuals = as.vector(y - x %*% temp - alpha))
}

tsp1reg <- function(x, y, plotit = FALSE) {
  # 1. Handle missing values natively (much faster than matrix conversion)
  valid <- stats::complete.cases(x, y)
  x <- x[valid]
  y <- y[valid]

  # 2. Calculate pairwise differences
  # We do not need to sort the data first. The condition `dx > 0` naturally
  # selects exactly one of the two symmetric pairs (j>i) and automatically
  # removes ties in x, preventing division-by-zero.
  dx <- outer(x, x, "-")
  keep <- dx > 0

  # 3. Calculate slopes and extract the median
  slopes <- outer(y, y, "-")[keep] / dx[keep]
  slope <- stats::median(slopes)

  # 4. Calculate intercept and format coefficients
  intercept <- stats::median(y - slope * x)
  coef <- c("Intercept" = intercept, "slope" = slope)

  # 5. Optional Plotting
  if (plotit) {
    plot(x, y, xlab = "X", ylab = "Y", pch = 16, col = "darkgray")
    graphics::abline(coef, col = "red", lwd = 2)
  }

  # 6. Return structured list
  list(
    coef = coef,
    residuals = y - (slope * x + intercept)
  )
}
