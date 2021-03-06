% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fxor.R
\name{chartFinalWeights}
\alias{chartFinalWeights}
\title{Chart final weight of eur for no rebalancing, rebalancing with options only and expected weight
 as functions of final eurusd rate}
\usage{
chartFinalWeights(portfolio, xRates = seq(1.05, 1.25, 0.001),
  chartExpected = FALSE)
}
\arguments{
\item{portfolio}{portfolio object}

\item{xRates}{vector of final eurusd rates}

\item{chartExpected}{false if not charting expected weight, true otherwise}
}
\value{
charts of final eur weight
}
\description{
Chart final weight of eur for no rebalancing, rebalancing with options only and expected weight
 as functions of final eurusd rate
}
