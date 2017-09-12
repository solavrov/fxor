
#' Constructor of portfolio object
#'
#' Attributes:
#' $eur - amount of euro in eur
#' $usd - amount of usd
#' $options$amounts - amounts of options in eur, positive for put, negative for call
#' $options$strikes - options strikes
#' $wTarget - target weight of euro
#' $xRate - current eurusd rate
#' $iRateUsd - usd interest rate
#' $iRateEur - eur interest rate
#' $volaty - volatility of eurusd rate
#' $daysToEnd - days to the end of period, expiration of options
#' $daysInYear - number of business days in year
#' $spreadLoss - current accumulated spread loss form FX spot rebalancing trades
#'
#' @return Portfolio object
#' @export
Portfolio <- function() {

  p <- list()
  class(p) <- "Portfolio"

  # default attributes
  p$eur <- 10^6/2/1.15
  p$usd <- 10^6/2
  p$options$amounts <- c(8000, -8000)
  p$options$strikes <- c(1.13, 1.17)
  p$wTarget <- 0.5
  p$xRate <- 1.15
  p$iRateUsd <- 0.0125
  p$iRateEur <- -0.0025
  p$volaty <- 0.07
  p$daysToEnd <- 22
  p$daysInYear <- 252
  p$spreadLoss <- 0

  return (p)

}


#' Get portfolio value in usd
#'
#' @param portfolio portfolio object
#'
#' @return portfolio value in usd
#' @export
getValue <- function(portfolio) {
  portfolio$eur * portfolio$xRate + portfolio$usd
}


#' Get euro weight in portfolio
#'
#' @param portfolio portfolio object
#'
#' @return euro weight
#' @export
getWeight <- function(portfolio) {
  portfolio$eur * portfolio$xRate / getValue(portfolio)
}


#' Get values of given portfolio options
#'
#' @param portfolio portfolio object
#' @param xRate current eurusd rate
#' @param vols volatility of eurusd rate
#'
#' @return values of options in usd
#' @export
getValuesOfPortfolioOptions <- function(portfolio,
                                        xRate = portfolio$xRate,
                                        vols =
                                          numeric(length(portfolio$options$strikes)) +
                                          portfolio$volaty) {

  values <- numeric()

  for (i in 1:length(portfolio$options$amounts)) {

    op <- fval::EurOption()
    if (portfolio$options$amounts[i] > 0) {
      op$type = "put"
    } else {
      op$type = "call"
    }
    op$strike <- portfolio$options$strikes[i]
    op$time <- portfolio$daysToEnd / portfolio$daysInYear
    op$rate <- portfolio$iRateUsd
    op$yield <- portfolio$iRateEur
    op$vol <- vols[i]

    v <- fval::getValueOfEurOption(op, xRate)

    values <- append(values, v)

  }

  return (values * abs(portfolio$options$amounts))

}


#' Check presence of options in portfolio
#'
#' @param portfolio portfolio object
#'
#' @return true if there are, false otherwise
#' @export
areOptions <- function(portfolio) {
  any(portfolio$options$amounts != 0)
}


#' Calculate amounts of options execution for given eurusd rate
#'
#' @param portfolio portfolio object
#' @param xRate eurusd rate at expiration
#'
#' @return vector of execution amounts
#' @export
calcExecutionAmts <- function(portfolio, xRate) {
  amts <- numeric()
  for (i in 1:length(portfolio$options$amounts)) {
    a <- portfolio$options$amounts[i] * (
      ((portfolio$options$amounts[i] > 0) && (xRate < portfolio$options$strikes[i])) ||
        ((portfolio$options$amounts[i] < 0) && (xRate > portfolio$options$strikes[i]))
    )
    amts <- append(amts, a)
  }
  return (amts)
}


#' Calculate losses from options execution
#'
#' @param portfolio portfolio object
#' @param xRate eurusd rate at expiration
#'
#' @return vector of losses
#' @export
calcExecutionLosses <- function(portfolio, xRate) {
  calcExecutionAmts(portfolio, xRate) * (xRate - portfolio$options$strikes)
}


#' Delete options from portfolio
#'
#' @param portfolio portfolio object
#'
#' @return portfolio object without options
#' @export
delOptions <- function(portfolio) {

  portfolio$options$amounts <- c(0, 0)
  portfolio$options$strikes <- c(0, 0)

  return(portfolio)

}


#' Simulate eurusd rate in portfolio$daysToEnd days
#'
#' @param portfolio portfolio object
#' @param xRate start eurusd rate
#' @param sampSize sample size
#' @param isRandom are samples random normal or evenly normal distributed
#'
#' @return vector of eurusd rate samples
#' @export
simXRates <- function(portfolio,
                      xRate = portfolio$xRate,
                      sampSize = 10^3,
                      isRandom = FALSE) {

  fsim::simPrices(xRate,
            portfolio$volaty,
            portfolio$daysToEnd,
            portfolio$iRateUsd - portfolio$iRateEur,
            sampSize,
            portfolio$daysInYear,
            isRandom)

}


#' Simulate random walk path of eurusd rate for portfolio$daysToEnd days
#'
#' @param portfolio portfolio object
#'
#' @return vector of eurusd rate path
#' @export
simXPath <- function(portfolio) {

  fsim::simPricePath(portfolio$xRate,
               portfolio$volaty,
               portfolio$daysToEnd,
               portfolio$iRateUsd - portfolio$iRateEur,
               portfolio$daysInYear)

}


#' Calculate final weight of euro
#'
#' @param portfolio portfolio object
#' @param finalXRates final eurusd rate
#'
#' @return final euro weight
#' @export
calcFinalWeights <- function(portfolio, finalXRates) {

  weights <- numeric()

  for (x in finalXRates) {
    amt <- sum(calcExecutionAmts(portfolio, x))
    loss <- sum(calcExecutionLosses(portfolio, x))
    w <- (portfolio$eur + amt) * x / (portfolio$eur * x + portfolio$usd + loss)
    weights <- append(weights, w)
  }

  return (weights)

}


#' Calculate expected weight of euro in given portfolio
#'
#' @param portfolio portfolio object
#' @param xRates current eurusd rate
#' @param avgNum number of samples in averaging distribution
#'
#' @return expected weight of euro
#' @export
calcExpectedFinalWeights <- function(portfolio,
                                     xRates = portfolio$xRate,
                                     avgNum = 100) {

  weights <- numeric()

  for (x in xRates) {
    xRange <- simXRates(portfolio, x, avgNum)
    w <- mean(calcFinalWeights(portfolio, xRange))
    weights <- append(weights, w)
  }

  return (weights)

}


#' Chart final weight of eur for no rebalancing, rebalancing with options only and expected weight
#'  as functions of final eurusd rate
#'
#' @param portfolio portfolio object
#' @param xRates vector of final eurusd rates
#' @param chartExpected false if not charting expected weight, true otherwise
#'
#' @return charts of final eur weight
#' @export
chartFinalWeights <- function(portfolio,
                              xRates = seq(1.05,1.25,0.001),
                              chartExpected = FALSE) {

  cat("Charting... ")

  weightNoOptions <- calcFinalWeights(delOptions(portfolio), xRates)
  weightWithOptions <- calcFinalWeights(portfolio, xRates)
  if (chartExpected) {
    weightExpected <- calcExpectedFinalWeights(portfolio, xRates)
    yRange <- range(weightNoOptions, weightWithOptions, weightExpected)
  } else {
    yRange <- range(weightNoOptions, weightWithOptions)
  }

  plot(xRates, weightNoOptions, ylim = yRange,
       type = "l", col = "black", xlab = "xRate",
       ylab = "Weight", lwd = 3)
  lines(xRates, weightWithOptions, type = "l", col = "blue", lwd = 3)
  if (chartExpected) {
    lines(xRates, weightExpected, type = "l", col = "red", lwd = 3)
  }

  if (chartExpected) {
    legend("topleft",
           legend = c("Final weight no options", "Final weight with options", "Expected final weight"),
           lty = c(1,1,1),
           lwd = c(3,3,3),
           col = c("black","blue","red"))
  } else {
    legend("topleft",
           legend = c("Final weight no options", "Final weight with options"),
           lty = c(1,1),
           lwd = c(3,3),
           col = c("black","blue"))
  }

  cat("done!\n\n")

}


#' Chart final eur weight distributions for no rebalancing and rebalancing with options only
#'
#' @param portfolio portfolio object
#'
#' @return charts of final eur weight distributions
#' @export
chartWeightDistributions <- function(portfolio) {

  xRates <- simXRates(portfolio)
  weightNoOptions <- calcFinalWeights(delOptions(portfolio), xRates)
  weightWithOptions <- calcFinalWeights(portfolio, xRates)

  breaks <- seq(min(weightNoOptions, weightWithOptions) - 0.001,
               max(weightNoOptions, weightWithOptions) + 0.001, 0.001)

  hist(weightWithOptions, breaks = breaks,
       col = rgb(0,0,1,1/2), xlab = "Weight",
       main = c("Final weight frequencies (total 1000)"), freq = TRUE)

  hist(weightNoOptions, breaks = breaks, col = rgb(1,0,0,1/2), freq = TRUE, add = T)

  legend("topleft", legend = c("No options", "With options"), fill = c("red", "blue"))

}


#' Demonstrate seeking of best options
#'
#' @return best options
#' @export
demoSeekingBestOptions <- function() {

  cat("Demo of seeking best options. Symmetric case\n")
  cat("Given portfolio: value = $1mio, eur = 50%, usd = 50%, eurusd = 1.15\n")
  cat("Task: find short put and call to maximize probability of getting eur weight in 49.5%-50.5% range in one month\n\n")

  p <- Portfolio()

  wRange <- c(0.495, 0.505)

  x <- simXRates(p)

  strikeShifts <- seq(0.000, 0.0500, 0.0050)
  amounts <- seq(0, 15000, 500)

  probs <- matrix(0, ncol = length(strikeShifts), nrow = length(amounts))

  counter <- 0
  completed <- 0
  cat ("Progress: ")

  for (j in 1:length(amounts)) {
    for (i in 1:length(strikeShifts)) {

      p$options$strikes <- c(p$xRate - strikeShifts[i], p$xRate + strikeShifts[i])
      p$options$amounts <- c(amounts[j], -amounts[j])

      w <- calcFinalWeights(p, x)
      probs[j,i] <- hlpr::calcPercentInsideInclusive(w, wRange)

      counter <- counter + 1
      progress <- round(counter / length(strikeShifts) / length(amounts) * 100)
      if (progress > completed) {
        completed <- progress
        cat(completed,"%  ", sep = "")
      }

    }
  }

  best <- hlpr::getIndicesOfMax(probs)

  cat("done!\n\n")
  cat("Best strike of put  =", p$xRate - strikeShifts[best[2]], "\n")
  cat("Best strike of call =", p$xRate + strikeShifts[best[2]], "\n")
  cat("Options amounts = $", amounts[best[1]], "\n", sep = "")


  persp(amounts, strikeShifts, probs,
        theta = 390, phi = 30, col = hlpr::getSurfColors(probs),
        xlab = "Options amounts", ylab = "Strike shift", zlab = "Probability",
        main = "Probability of staying in range")

}


#' Get trade eurusd rate with spread loss
#' Default function
#'
#' @param xRate fair market eurusd rate
#' @param amount amount of trade
#'
#' @return eurusd trade rate
#' @export
getTradeRateDefault <- function(xRate, amount) {
  xRate + 0.0005 * sign(amount)
}


#' Calculate R-factor
#'
#' @param portfolio portfolio object
#' @param newXRate new eurusd rate
#' @param avgNum number of samples in averaging distribution
#'
#' @return R-factor
#' @export
calcRFactor <- function(portfolio, newXRate, avgNum = 100) {

  f <- 0
  xRange <- simXRates(portfolio, newXRate, avgNum)

  for (x in xRange) {
    loss <- sum(calcExecutionLosses(portfolio, x))
    f <- f + x / (portfolio$eur * x + portfolio$usd + loss)
  }

  f <- f / avgNum

  return (f)

}


#' Calculate euro amount for rebalancing trade toward expected weight if with options,
#' toward target weight otherwise
#'
#' @param portfolio portfolio object
#' @param newXRate  new eurusd rate
#' @param avgNum number of samples in averaging distribution
#'
#' @return euro amount of rebalancing trade
#' @export
calcRebalanceAmt <- function(portfolio, newXRate, avgNum = 100) {

  if (areOptions(portfolio)) {
    amt <- (portfolio$wTarget - calcExpectedFinalWeights(portfolio, newXRate, avgNum)) /
      calcRFactor(portfolio, newXRate, avgNum)
  } else {
    amt <- portfolio$wTarget * (portfolio$eur + portfolio$usd / newXRate) - portfolio$eur
  }

  return (amt)

}


#' Rebalance portfolio
#'
#' @param portfolio portfolio object
#' @param newXRate new eurusd rate
#' @param avgNum number of samples in averaging distribution
#' @param getTradeRate function that returns trade rate from market rate and amount
#'
#' @return rebalanced portfolio object
#' @export
rebalance <- function(portfolio, newXRate,
                      avgNum = 100,
                      getTradeRate = getTradeRateDefault) {

  amt <- calcRebalanceAmt(portfolio, newXRate, avgNum)

  portfolio$eur <- portfolio$eur + amt
  tradeRate <- getTradeRate(newXRate, amt)
  portfolio$usd <- portfolio$usd - amt * tradeRate
  portfolio$xRate <- newXRate
  portfolio$spreadLoss <- portfolio$spreadLoss + amt * (newXRate - tradeRate)

  if (areOptions(portfolio) && portfolio$daysToEnd == 0) {

    optionAmts <- calcExecutionAmts(portfolio$options, newXRate)
    portfolio$eur <- portfolio$eur + sum(optionAmts)
    portfolio$usd <- portfolio$usd - sum(optionAmts * portfolio$options$strikes)

    portfolio <- delOptions(portfolio)

  }

  return (portfolio)

}


#' Rebalance portfolio along given path
#'
#' @param portfolio portfolio object
#' @param path vector of eurusd path rates
#' @param avgNum number of samples in averaging distribution
#'
#' @return rebalanced portfolio object
#' @export
rebalanceAlongPath <- function(portfolio, path, avgNum = 100) {

  for (x in path) {
    portfolio$daysToEnd <- portfolio$daysToEnd - 1
    portfolio <- rebalance(portfolio, x, avgNum)
  }

  return (portfolio)

}


#' Get rebalance analytics
#'
#' @param portfolio portfolio object
#' @param path vector of eurusd path rates
#' @param avgNum number of samples in averaging distribution
#' @param getTradeRate unction that returns trade rate from market rate and amount
#'
#' @return data.frame with data of trade amounts, spread losses and weights
#' @export
getRebalanceAnalytics <- function(portfolio, path,
                                  avgNum = 100,
                                  getTradeRate = getTradeRateDefault) {

  action <- character()
  tradeAmt <- numeric()
  spreadLoss <- numeric()
  actWght <- numeric()
  expWght <- numeric()

  day <- 1:portfolio$daysToEnd
  day <- append(day, portfolio$daysToEnd)

  xRate <- path
  xRate <- append(xRate, hlpr::getLast(path))

  for (x in path) {

    action <- append(action, "fx trade")
    portfolio$daysToEnd <- portfolio$daysToEnd - 1
    portfolio$xRate <- x

    tradeAmt <- append(tradeAmt, calcRebalanceAmt(portfolio, x, avgNum))

    tradeRate <- getTradeRate(x, hlpr::getLast(tradeAmt))
    portfolio$eur <- portfolio$eur + hlpr::getLast(tradeAmt)
    portfolio$usd <- portfolio$usd - hlpr::getLast(tradeAmt) * tradeRate

    spreadLoss <- append(spreadLoss, hlpr::getLast(tradeAmt) * (x - tradeRate))
    actWght <- append(actWght, getWeight(portfolio))
    expWght <- append(expWght, calcExpectedFinalWeights(portfolio))

    if (portfolio$daysToEnd == 0) {

      action <- append(action, "exec opt")
      spreadLoss <- append(spreadLoss, 0)

      if (areOptions(portfolio)) {
        optionAmts <- calcExecutionAmts(portfolio, x)
        tradeAmt <- append(tradeAmt, sum(optionAmts))
        portfolio$eur <- portfolio$eur + sum(optionAmts)
        portfolio$usd <- portfolio$usd - sum(optionAmts * portfolio$options$strikes)
      } else {
        tradeAmt <- append(tradeAmt, 0)
      }

      portfolio <- delOptions(portfolio)

      actWght <- append(actWght, getWeight(portfolio))
      expWght <- append(expWght, calcExpectedFinalWeights(portfolio))

    }

  }

  analytics <- data.frame(day, xRate, action, tradeAmt, spreadLoss, actWght, expWght)

  return (analytics)

}


#' Simulate total spread losses accumulated during the period
#'
#' @param portfolio portfolio object
#' @param sampSize sample size
#' @param avgNum number of samples in averaging distribution
#'
#' @return vector of spread loss samples
#' @export
simSpreadLoss <- function(portfolio, sampSize = 10^3, avgNum = 100) {

  loss <- numeric()

  counter <- 0
  completed <- 0
  cat ("Progress: ")

  for (i in 1:sampSize) {

    p <- portfolio
    path <- simXPath(p)
    p <- rebalanceAlongPath(p, path, avgNum)
    loss <- append(loss, p$spreadLoss)

    counter <- counter + 1
    progress <- round(counter / sampSize * 100)
    if (progress > completed) {
      completed <- progress
      cat(completed,"%  ", sep = "")
    }

  }

  cat("done!\n\n")

  return (loss)

}


#' Find example of path with high total spread loss
#'
#' @param portfolio portfolio object
#' @param sampSize sample size
#' @param avgNum number of samples in averaging distribution
#'
#' @return vector of eurusd path
#' @export
findMaxSpreadLossPath <- function(portfolio, sampSize = 10^3, avgNum = 100) {

  maxLoss <- 0

  counter <- 0
  completed <- 0
  cat ("Progress: ")

  for (i in 1:sampSize) {

    p <- portfolio
    path <- simXPath(p)
    p <- rebalanceAlongPath(p, path, avgNum)

    if (p$spreadLoss < maxLoss) {
      maxLoss <- p$spreadLoss
      maxPath <- path
    }

    counter <- counter + 1
    progress <- round(counter / sampSize * 100)
    if (progress > completed) {
      completed <- progress
      cat(completed,"%  ", sep = "")
    }

  }

  cat("done!\n\n")
  cat("Max loss =", maxLoss, "\n")

  return (maxPath)

}


#' Chart spread loss distributions
#'
#' @param portfolio portfolio object
#' @param sampSize sample size
#'
#' @return charts of spread loss distributions for no option and with option rebalancing
#' @export
chartSpreadLossDistributions <- function(portfolio, sampSize = 10^2) {

  cat("Calculating no options case...\n\n")
  noOptLosses <- simSpreadLoss(delOptions(portfolio), sampSize)
  cat("Calculating options case...\n\n")
  wthOptLosses <- simSpreadLoss(portfolio, sampSize)


  histBreakStep <- (max(noOptLosses, wthOptLosses) - min(noOptLosses, wthOptLosses)) / 50

  breaks <- seq(min(noOptLosses, wthOptLosses) - histBreakStep,
                max(noOptLosses, wthOptLosses) + histBreakStep, histBreakStep)

  hist(wthOptLosses,
       col = rgb(0,0,1,1/2), breaks = breaks, xlab = "Spread loss",
       main = paste("Spread loss frequencies (total ", sampSize, ")", sep = ""), freq = TRUE)

  hist(noOptLosses, breaks = breaks, col = rgb(1,0,0,1/2), freq = TRUE, add = T)

  legend("topleft", legend = c("No options", "With options"), fill = c("red", "blue"))

  avgProfit <- mean(wthOptLosses) - mean(noOptLosses)

  cat("\n")
  cat("Average loss of no options case =", mean(noOptLosses), "\n")
  cat("Average loss of options case =", mean(wthOptLosses), "\n")
  cat("Average profit from options rebalancing =", avgProfit, "\n")

}


#' Simulate option losses
#'
#' @param portfolio portfolio object
#' @param sampSize sample size
#'
#' @return vector of option losses
#' @export
simOptionsLoss <- function(portfolio, sampSize = 10^3) {

  loss <- numeric()

  xRates <- simXRates(portfolio, sampSize = sampSize)

  for (x in xRates) {
    loss <- append(loss, sum(calcExecutionLosses(portfolio, x)))
  }

  return (loss)

}


#' Simulate financial results
#'
#' @param portfolio portfolio object
#' @param optTradeVols vector of options trade volatilities
#' @param sampSize sample size
#' @param avgNum number of samples in averaging distribution
#'
#' @return data frame of following samples:
#'  no options spread loss, with options spread loss, optrions execution loss,
#'   premium received, financial result
#' @export
simFinResult <- function(portfolio, optTradeVols, sampSize = 10^3, avgNum = 100) {

  noOptSprLoss <- numeric()
  optSprLoss <- numeric()
  optExecLoss <- numeric()

  counter <- 0
  completed <- 0
  cat("Progress: ")

  for (i in 1:sampSize) {

    path <- simXPath(portfolio)

    pNoOpt <- rebalanceAlongPath(delOptions(portfolio), path, avgNum)
    pWthOpt <- rebalanceAlongPath(p, path, avgNum)

    noOptSprLoss <- append(noOptSprLoss, pNoOpt$spreadLoss)
    optSprLoss <- append(optSprLoss, pWthOpt$spreadLoss)
    optExecLoss <- append(optExecLoss, sum(calcExecutionLosses(portfolio, hlpr::getLast(path))))

    counter <- counter + 1
    progress <- round(counter/sampSize * 100)
    if (progress > completed) {
      completed <- progress
      cat(completed, "%  ", sep = "")
    }

  }

  cat("done!\n\n")

  premRecvd <- numeric(sampSize) +
    sum(getValuesOfPortfolioOptions(portfolio, vols = optTradeVols))

  finRes <- optSprLoss - noOptSprLoss + premRecvd + optExecLoss

  results <- data.frame(noOptSprLoss, optSprLoss, optExecLoss, premRecvd, finRes)

  cat("Average spread loss of fx spot, no options case =", mean(noOptSprLoss), "\n")
  cat("Average spread loss of fx spot, options case =", mean(optSprLoss), "\n")
  cat("Average loss from options =", mean(premRecvd + optExecLoss), "\n")
  cat("Average financial resilt =", mean(finRes), "\n")

  return (results)

}

