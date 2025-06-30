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
  xout = FALSE,
  outfun = out,
  ...
) {
  ##
  ##   Compute a .95 confidence interval for each of the parameters of
  ##   a linear regression equation. The default regression method is
  ##   the Theil-Sen estimator.
  ##
  ##   When using the least squares estimator, and when n<250, use
  ##   lsfitci instead.
  ##
  ##   The predictor values are assumed to be in the n by p matrix x.
  ##   The default number of bootstrap samples is nboot=599
  ##
  ##   regfun can be any R function that returns the coefficients in
  ##   the vector regfun$coef, the first element of which contains the
  ##   estimated intercept, the second element contains the estimated of
  ##   the first predictor, etc.
  ##

  ## get rid of R check annoyances
  out <- NULL

  x <- as.matrix(x)
  p1 <- ncol(x) + 1
  p <- ncol(x)
  xy <- cbind(x, y)
  xy <- elimna(xy)
  x <- xy[, 1:p]
  y <- xy[, p1]
  if (xout) {
    m <- cbind(x, y)
    flag <- outfun(x, plotit = FALSE)$keep
    m <- m[flag, ]
    x <- m[, 1:p]
    y <- m[, p1]
  }
  x <- as.matrix(x)
  if (SEED) {
    set.seed(2)
  } ## set seed of random number generator so that
  ##             results can be duplicated.
  if (!pr) {
    # message("Taking bootstrap samples. Please wait.")
  }

  #   data <- matrix(sample(length(y), size = length(y) * nboot, replace = T), nrow = nboot)
  ## length of block set to l^(1/3)
  ## Buhlmann and Kunsch 1994 report
  block.length <- 1
  if (autocor) {
    block.length <- round(length(y)^(1 / 3))
  }
  ## need to transpose ...
  data <- t(samp.boot.block(length(y), nboot, block.length))

  bvec <- apply(data, 1, regboot, x, y, regfun, ...)

  ## bvec is a p+1 by nboot matrix. The first row
  ##                     contains the bootstrap intercepts, the second row
  ##                     contains the bootstrap values for first predictor, etc.
  regci <- matrix(0, p1, 5)
  VAL <- c("intercept", rep("X", ncol(x)))
  dimnames(regci) <- list(
    VAL,
    c("ci.low", "ci.up", "Estimate", "S.E.", "p-value")
  )
  ilow <- round((alpha / 2) * nboot)
  ihi <- nboot - ilow
  ilow <- ilow + 1
  se <- NA
  pvec <- NA
  for (i in 1:p1) {
    bsort <- sort(bvec[i, ])
    pvec[i] <- (sum(bvec[i, ] < 0) + 0.5 * sum(bvec[i, ] == 0)) / nboot
    if (pvec[i] > 0.5) {
      pvec[i] <- 1 - pvec[i]
    }
    regci[i, 1] <- bsort[ilow]
    regci[i, 2] <- bsort[ihi]
    se[i] <- sqrt(var(bvec[i, ]))
  }
  estit <- regfun(x, y)$coef
  regci[, 3] <- estit
  pvec <- 2 * pvec
  regci[, 4] <- se
  regci[, 5] <- pvec
  ##   if (pr) {
  ##      print("First row of regci is the confidence interval for the intercept,")
  ##      print("the second row is the confidence interval for the first slope, etc.")
  ##  }
  list(regci = regci)
}

elimna <- function(m) {
  #
  # remove any rows of data having missing values
  #
  m <- as.matrix(m)
  ikeep <- c(1:nrow(m))
  for (i in 1:nrow(m)) {
    if (sum(is.na(m[i, ]) >= 1)) {
      ikeep[i] <- 0
    }
  }
  elimna <- m[ikeep[ikeep >= 1], ]
  elimna
}

regboot <- function(isub, x, y, regfun, ...) {
  #
  #  Perform regression using x[isub] to predict y[isub]
  #  isub is a vector of length n,
  #  a bootstrap sample from the sequence of integers
  #  1, 2, 3, ..., n
  #
  #  This function is used by other functions when computing
  #  bootstrap estimates.
  #
  #  regfun is some regression method already stored in R
  #  It is assumed that regfun$coef contains the  intercept and slope
  #  estimates produced by regfun.  The regression methods written for
  #  this  book, plus regression functions in R, have this property.
  #
  #  x is assumed to be a matrix containing values of the predictors.
  #
  xmat <- matrix(x[isub, ], nrow(x), ncol(x))
  vals <- regfun(xmat, y[isub], ...)$coef
  vals
}

tsreg <- function(
  x,
  y,
  xout = FALSE,
  outfun = out,
  iter = 10,
  varfun = pbvar,
  ...
) {
  #
  #  Compute Theil-Sen regression estimator
  #
  #  Use Gauss-Seidel algorithm
  #  when there is more than one predictor
  #
  #

  ## get rid of R check annoyances
  out <- pbvar <- NULL

  x <- as.matrix(x)
  xx <- cbind(x, y)
  xx <- elimna(xx)
  x <- xx[, 1:ncol(x)]
  x <- as.matrix(x)
  y <- xx[, ncol(x) + 1]
  temp <- NA
  x <- as.matrix(x)
  if (xout) {
    x <- as.matrix(x)
    flag <- outfun(x, ...)$keep
    x <- x[flag, ]
    y <- y[flag]
    x <- as.matrix(x)
  }
  if (ncol(x) == 1) {
    temp1 <- tsp1reg(x, y)
    coef <- temp1$coef
    res <- temp1$res
  }
  if (ncol(x) > 1) {
    for (p in 1:ncol(x)) {
      temp[p] <- tsp1reg(x[, p], y)$coef[2]
    }
    res <- y - x %*% temp
    alpha <- median(res)
    r <- matrix(NA, ncol = ncol(x), nrow = nrow(x))
    tempold <- temp
    for (it in 1:iter) {
      for (p in 1:ncol(x)) {
        r[, p] <- y - x %*% temp - alpha + temp[p] * x[, p]
        temp[p] <- tsp1reg(x[, p], r[, p], plotit = FALSE)$coef[2]
      }
      alpha <- median(y - x %*% temp)
      tempold <- temp
    }
    coef <- c(alpha, temp)
    res <- y - x %*% temp - alpha
  }
  yhat <- y - res
  stre <- NULL
  # e.pow <- varfun(yhat)/varfun(y)
  # if (!is.na(e.pow)) {
  # if(e.pow>=1)e.pow<-corfun(yhat,y)$cor^2
  # stre=sqrt(e.pow)
  # }
  list(
    coef = coef,
    residuals = res,
    Strength.Assoc = NA,
    Explanatory.Power = NA
  )
}

tsp1reg <- function(x, y, plotit = FALSE) {
  #
  # Compute the Theil-Sen regression estimator.
  # Only a single predictor is allowed in this version
  #
  temp <- matrix(c(x, y), ncol = 2)
  temp <- elimna(temp) # Remove any pairs with missing values
  x <- temp[, 1]
  y <- temp[, 2]
  ord <- order(x)
  xs <- x[ord]
  ys <- y[ord]
  vec1 <- outer(ys, ys, "-")
  vec2 <- outer(xs, xs, "-")
  v1 <- vec1[vec2 > 0]
  v2 <- vec2[vec2 > 0]
  slope <- median(v1 / v2)
  coef <- median(y) - slope * median(x)
  names(coef) <- "Intercept"
  coef <- c(coef, slope)
  if (plotit) {
    plot(x, y, xlab = "X", ylab = "Y")
    graphics::abline(coef)
  }
  res <- y - slope * x - coef[1]
  list(coef = coef, residuals = res)
}
