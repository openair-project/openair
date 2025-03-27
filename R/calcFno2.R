#' Estimate NO2/NOX emission ratios from monitoring data
#'
#' Given hourly NOX and NO2 from a roadside site and hourly NOX, NO2 and O3 from
#' a background site the function will estimate the emissions ratio of NO2/NOX
#' --- the level of primary NO2
#'
#' The principal purpose of this function is to estimate the level of primary
#' (or direct) NO2 from road vehicles. When hourly data of NOX, NO2 and O3 are
#' available, the total oxidant method of Clapp and Jenkin (2001) can be used.
#' If roadside O3 measurements are available see [linearRelation()] for details
#' of how to estimate the primary NO2 fraction.
#'
#' In the absence of roadside O3 measurements, it is rather more problematic to
#' calculate the fraction of primary NO2. Carslaw and Beevers (2005c) developed
#' an approach based on [linearRelation()] the analysis of roadside and
#' background measurements. The increment in roadside NO2 concentrations is
#' primarily determined by direct emissions of NO2 and the availability of One
#' to react with NO to form NO2. The method aims to quantify the amount of NO2
#' formed through these two processes by seeking the optimum level of primary
#' NO2 that gives the least error.
#'
#' @param input A data frame with the following fields. `nox` and`no2`
#'   (roadside NOX and NO2 concentrations), `back_nox`, `back_no2` and
#'   `back_o3` (hourly background concentrations of each pollutant). In
#'   addition `temp` (temperature in degrees Celsius) and `cl` (cloud
#'   cover in Oktas). Note that if `temp` and `cl` are not available,
#'   typical means values of 11 deg. C and cloud = 3.5 will be used.
#' @param tau Mixing time scale. It is unlikely the user will need to adjust
#'   this. See details below.
#' @param user.fno2 User-supplied f-NO2 fraction e.g. 0.1 is a NO2/NOX ratio of
#'   10% by volume. `user.no2` will be applied to the whole time series and
#'   is useful for testing "what if" questions.
#' @param main Title of plot if required.
#' @param xlab x-axis label.
#' @inheritDotParams scatterPlot
#' @export
#' @return an [openair][openair-package] object
#' @author David Carslaw
#' @seealso [linearRelation()] if you have roadside ozone
#'   measurements.
#' @references Clapp, L.J., Jenkin, M.E., 2001. Analysis of the relationship
#'   between ambient levels of O3, NO2 and NO as a function of NOX in the UK.
#'   Atmospheric Environment 35 (36), 6391-6405.
#'
#'   Carslaw, D.C. and N Carslaw (2007).  Detecting and characterising small
#'   changes in urban nitrogen dioxide concentrations.  Atmospheric Environment.
#'   Vol. 41, 4723-4733.
#'
#'   Carslaw, D.C., Beevers, S.D. and M.C. Bell (2007). Risks of exceeding the
#'   hourly EU limit value for nitrogen dioxide resulting from increased road
#'   transport emissions of primary nitrogen dioxide. Atmospheric Environment 41
#'   2073-2082.
#'
#'   Carslaw, D.C. (2005a). Evidence of an increasing NO2/NOX emissions ratio
#'   from road traffic emissions. Atmospheric Environment, 39(26) 4793-4802.
#'
#'   Carslaw, D.C. and Beevers, S.D. (2005b). Development of an urban inventory
#'   for road transport emissions of NO2 and comparison with estimates derived
#'   from ambient measurements. Atmospheric Environment, (39): 2049-2059.
#'
#'   Carslaw, D.C. and Beevers, S.D. (2005c). Estimations of road vehicle
#'   primary NO2 exhaust emission fractions using monitoring data in London.
#'   Atmospheric Environment, 39(1): 167-177.
#'
#'   Carslaw, D. C. and S. D. Beevers (2004). Investigating the Potential
#'   Importance of Primary NO2 Emissions in a Street Canyon. Atmospheric
#'   Environment 38(22): 3585-3594.
#'
#'   Carslaw, D. C. and S. D. Beevers (2004). New Directions: Should road
#'   vehicle emissions legislation consider primary NO2? Atmospheric Environment
#'   38(8): 1233-1234.
calcFno2 <- function(input,
                     tau = 60,
                     user.fno2,
                     main = "",
                     xlab = "year", ...) {
  {


    ## get rid of R check annoyances
    no2 <- nox.v <- na.locf <- NULL

    ## function to prepare data ######################################################
    prepare <- function(input) {
      input$temp <- input$temp + 273

      ## different date components
      input <- input[order(input$date), ]
      year <- as.numeric(format(input$date, "%Y"))
      month <- as.numeric(format(input$date, "%m"))
      jd <- as.numeric(format(input$date, "%j"))
      hour <- as.numeric(format(input$date, "%H"))
      weekday <- format(input$date, "%A")

      nox.v <- input$nox - input$back_nox ## estimated road contribution to NOx
      SLAT <- 52 ## latitute

      a <- 23.45 * (2 * pi / 360) * sin((jd + 284) * 2 * pi / 365)

      s <- sin(2 * pi * SLAT / 360) * sin(a) + cos(2 * pi * SLAT / 360) *
        cos(a) * cos((hour - 12) * 2 * pi / 24)

      solar <- (990 * s - 30) * (1 - 0.75 * (input$cl / 8) ^ 3.4)

      jno2 <- ifelse(solar < 0, 0, 0.0008 * exp(-10 / solar) + 0.0000074 * solar)

      k <- 0.04405 * exp(-1370 / input$temp)

      r <- jno2 / k

      cbind(input, year, month, jd, weekday, hour, jno2, k, r, nox.v)
    }
    ################################################################################

    calc.error <- function(user.fno2, nox, no2, nox.v, k, r, jno2, back_no2, back_o3, type) {

      ## first calculate new hourly means with t, f-NO2
      d <- 1 / (k * tau)
      no2.v <- nox.v * user.fno2
      no2.n <- no2.v + back_no2
      no2.o <- no2.n + back_o3
      b <- nox + no2.o + r + d
      no2.pred <- 0.5 * (b - (b ^ 2 - 4 * (nox * no2.o + no2.n * d)) ^ 0.5)

      o3 <- (jno2 * tau * no2.pred + back_o3) / (k * tau * (nox - no2.pred) + 1)

      # use RMSE on hourly predictions
      error <- mean((no2 - no2.pred) ^ 2) ^ 0.5
  
      ## return different results depending on use
      switch(type,
        err = error,
        conc = data.frame(no2 = no2.pred, o3 = o3)
      )
    }

    ## plot results ##################################################################
    plot.fno2 <- function(results, ...) {
      scatterPlot(results, x = "date", y = "fno2", ylab = "f-NO2 (%)", ...)
    }

    ### plots original monthly NO2 and predicted with  ###############################
    plot.no2 <- function(input, res, ...) {
      input <- subset(input, select = c(date, no2))
      input <- timeAverage(input, "month")
      input$variable <- "measured"

      res <- subset(res, select = c(date, no2))
      res <- timeAverage(res, "month")
      res$variable <- "predicted"

      results <- rbind(input, res)

      scatterPlot(results, x = "date", y = "no2", group = "variable", ...)
    }

    ## start of code#################################################################

    ## if cl or temp are missing, fill in with default values

    ## check to see if cloud and temperature are present; if not, set default values
    if (!any(names(input) %in% "temp")) input$temp <- 11
    if (!any(names(input) %in% "cl")) input$cl <- 4.5

    input <- checkPrep(input, Names = c(
      "date", "nox", "no2", "back_no2",
      "back_nox", "back_o3", "cl", "temp"
    ), "default")
    input <- na.omit(input)
    input.all <- prepare(input) ## process input data
    input.all <- subset(input.all, nox.v > 0) ## process only +ve increments

    if (missing(user.fno2)) {
      fun.opt <- function(x) {
        optimize(
          calc.error, c(0, 0.5), x$nox, x$no2,
          x$nox.v, x$k, x$r, x$jno2, x$back_no2, x$back_o3, "err"
        )
      }
      ## split data frame by year/month
      input.part <- split(input.all, input.all[c("month", "year")])

      input.part <- input.part[which(lapply(input.part, nrow) > 50)] # need at least 50 hours

      fno2 <- sapply(input.part, function(x) fun.opt(x)$minimum)

      dates <- lapply(input.part, function(x) format(x$date[1], "%Y-%m"))
      dates <- do.call(rbind, dates)

      dates <- as_date(paste(dates, "-01", sep = ""))

      results <- data.frame(date = dates, fno2 = 100 * fno2) ## retrun estimates

      ## calculate O3 based on best estimate of f-no2
      results$date <- as.POSIXct(results$date, "GMT")
      input.all <- merge(input.all, results, all = TRUE)

      ## copy down f-NO2 for each month

      input.all$fno2 <- approx(input.all$fno2, n = length(input.all$fno2))$y
      input.all$fno2 <- input.all$fno2 / 100

      ## now calculate o3
      hourly <- calc.error(
        input.all$fno2, input.all$nox, input.all$no2,
        input.all$nox.v, input.all$k, input.all$r,
        input.all$jno2, input.all$back_no2,
        input.all$back_o3, "conc"
      )

      ids <- which(input$date %in% input.all$date) ## indices with data

      hourly <- cbind(date = input.all$date, nox = input.all$nox, hourly)

      if (length(input$date[-ids]) > 0) {
        gaps <- data.frame(
          date = input$date[-ids], nox = input$nox[-ids],
          no2 = input$no2[-ids], o3 = NA
        )
        hourly <- rbind(hourly, gaps)
      }


      hourly <- hourly[order(hourly$date), ]

      plot.fno2(results, ...)
      results <- list(results = results, hourly = hourly)

      #################
      # output
      #################
      plt <- trellis.last.object()
      newdata <- results
      output <- list(plot = plt, data = newdata, call = match.call())
      class(output) <- "openair"
      invisible(output)
    } else { ## calculate NO2 concentrations based on user input for whole series
      res <- calc.error(
        user.fno2, input.all$nox, input.all$no2,
        input.all$nox.v, input.all$k, input.all$r,
        input.all$jno2, input.all$back_no2,
        input.all$back_o3, "conc"
      )

      ids <- which(input$date %in% input.all$date) ## indices with data

      res <- cbind(date = input.all$date, res)

      gaps <- data.frame(date = input$date[-ids], no2 = input$no2[-ids], o3 = NA)

      res <- rbind(res, gaps)
      res <- res[order(res$date), ]
      plot.no2(input.all, res, ...)

      #################
      # output
      #################
      plt <- trellis.last.object()
      newdata <- res
      output <- list(plot = plt, data = newdata, call = match.call())
      class(output) <- "openair"
      invisible(output)
    }
  }
}
