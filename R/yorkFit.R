#' York regression function
#' 
#' Function for calculating the slope of variables using the York regression, 
#' which accounts for errors provided for both x and y. 
#' 
#' The York regression (YR) was suggested by Derek York from the University of Toronto in 1968.
#' YR incorporates the correlation coefficient of errors in the minimising function. 
#' The distance minimised for YR has an angle which depends on the weight of x and y errors. 
#' When errors are higher for a variable, the relevant data points will weigh less, due to their bigger uncertainty. 
#' 
#' The method employed uses an iterative approach to estimate the slope.
#' 
#' The York approach is common among the geophysical community, but very limited for other sciences.
#' 
#' The slope, slope error and intercept are calculated using the YorkFit() function.
#' 
#' YR can also be incorporated in polar plots, where the inverse effect of x/y and y/x is more distinct that Ordinary Least Squares.
#' 
#' @param input_data A data frame minimally containing 2 pollutants and at least one column with variable errors.
#' 
#' @param X Name of first variable 
#' 
#' @param Y Name of second variable 
#' 
#' @param Xstd Name of errors in first variable (X)
#' 
#' @param Ystd Name of errors in second variable (Y)
#' 
#' @param weight Can be supplied to apply to errors
#' 
#' @param Ri Correlation coefficient between errors in X and Y. Values must be within the range -1 to +1.
#' 
#' @param tol Threshold of differences of estimates of the slope to determine convergence. Should not need to be changed. 
#' 
#' @param max_iterations Maximum number of iterations to identify the slope. 
#' 
#' @author Magdalini Alexiadou, Stuart Lacy
#' 
#' @references 
#' York, D. (1966). Least-squares fitting of a straight line. Canadian Journal of Physics, 44(5), 1079-1086.
#' 
#' Wehr, R., & Saleska, S. R. (2017). The long-solved problem of the best-fit straight line: application to isotopic mixing lines. Biogeosciences, 14(1), 17-29.
#' 
#' Wu, C., & Yu, J. Z. (2018). Evaluation of linear regression techniques for atmospheric applications: the importance of appropriate weighting. Atmospheric Measurement Techniques, 11(2), 1233-1250.
#' 
#' @examples
#' 
#' # Simulate data with measurement errors.
#' true_x <- 1:50
#' true_y <- 1:50
#' 
#' set.seed(100)
#' sigma_x <- runif(length(true_x), 1, 20)
#' sigma_y <- runif(length(true_y), 1, 2)
#' 
#' noisy_x <- rnorm(length(true_x), true_x, sigma_x)
#' noisy_y <- rnorm(length(true_y), true_y, sigma_y)
#' 
#' york_data <- data.frame(
#'   X = noisy_x,
#'   Y = noisy_y,
#'   Xstd = sigma_x,
#'   Ystd = sigma_y
#' )
#' 
#' YorkFit(york_data, X = "X", Y = "Y",
#'         Xstd = "Xstd", Ystd = "Ystd")
#' @export
YorkFit <- function(input_data, X = "X", Y = "Y",
                    Xstd = "Xstd", Ystd = "Ystd",
                    weight = NA,
                    Ri = 0, tol = 1e-7, max_iterations = 100) {
  # weight can be supplied to apply to errors
  
  if (Ri < -1 || Ri > 1) {
    
    stop("Ri must be within the range -1 to +1")
    
  }
  
  
  # b0 initial guess at slope for OLR
  form <- formula(paste(Y, "~", X))
  mod <- lm(form, data = input_data)
  b0 <- mod$coefficients[2]
  
  X <- input_data[[X]]
  Y <- input_data[[Y]]
  
  Xstd <- input_data[[Xstd]]
  Ystd <- input_data[[Ystd]]
  
  # don't try regression if < 3 points
  if (sum(!is.na(X)) < 3 || sum(!is.na(Y)) < 3) return()
  
  
  Xw <- 1 / (Xstd^2) # X weights
  Yw <- 1 / (Ystd^2) # Y weights
  
  
  # ITERATIVE CALCULATION OF SLOPE AND INTERCEPT #
  
  b <- b0
  b.diff <- tol + 1
  
  n <- 0 # counter for debugging
  
  while (b.diff > tol && n < max_iterations) {
    
    n <- n + 1 # counter to keep a check on convergence
    
    b.old <- b
    alpha.i <- sqrt(Xw * Yw)
    Wi <- (Xw * Yw) / ((b^2) * Yw + Xw - 2 * b * Ri * alpha.i)
    WiX <- Wi * X
    WiY <- Wi * Y
    sumWiX <- sum(WiX, na.rm = TRUE)
    sumWiY <- sum(WiY, na.rm = TRUE)
    sumWi <- sum(Wi, na.rm = TRUE)
    Xbar <- sumWiX / sumWi
    Ybar <- sumWiY / sumWi
    Ui <- X - Xbar
    Vi <- Y - Ybar
    
    Bi <- Wi * ((Ui / Yw) + (b * Vi / Xw) - (b * Ui + Vi) * Ri / alpha.i)
    wTOPint <- Bi * Wi * Vi
    wBOTint <- Bi * Wi * Ui
    sumTOP <- sum(wTOPint, na.rm = TRUE)
    sumBOT <- sum(wBOTint, na.rm = TRUE)
    b <- sumTOP / sumBOT
    
    # zero or problematic data
    if (anyNA(b, b.old))
      return(tibble(Intercept = NA, Slope = NA,
                    Int_error = NA, Slope_error = NA,
                    OLS_slope = NA))
    
    b.diff <- abs(b - b.old)
  }
  
  a <- Ybar - b * Xbar
  wYorkFitCoefs <- c(a, b)
  
  # ERROR CALCULATION #
  
  Xadj <- Xbar + Bi
  WiXadj <- Wi * Xadj
  sumWiXadj <- sum(WiXadj, na.rm = TRUE)
  Xadjbar <- sumWiXadj / sumWi
  Uadj <- Xadj - Xadjbar
  wErrorTerm <- Wi * Uadj * Uadj
  errorSum <- sum(wErrorTerm, na.rm = TRUE)
  b.err <- sqrt(1 / errorSum)
  a.err <- sqrt((1 / sumWi) + (Xadjbar^2) * (b.err^2))
  wYorkFitErrors <- c(a.err, b.err)
  
  # GOODNESS OF FIT CALCULATION #
  lgth <- length(X)
  wSint <- Wi * (Y - b * X - a)^2
  sumSint <- sum(wSint, na.rm = TRUE)
  wYorkGOF <- c(sumSint / (lgth - 2), sqrt(2 / (lgth - 2))) # GOF (should equal 1 if assumptions are valid), #standard error in GOF
  
  ans <- tibble(Intercept = a, Slope = b,
                Int_error = a.err, Slope_error = b.err,
                OLS_slope = b0)
  
  return(ans)
}

