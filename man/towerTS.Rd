\name{towerTS}
\alias{towerTS}
\title{
Tower for Times Series
}
\description{
fits linear model to lagged elements; k is the number of nearest neighbors; predicts only the missing; the component 'naIdxs' records the indices of the predicted elements (some will be NA)
}
\usage{
towerTS(xts,lag,k,regFtnName='lm')
}
\arguments{
  \item{xts}{A time series.}
  \item{lag}{Lag. A positive integer.}
  \item{regFtnName}{Regression model to be used, currently 'lm', 'glm'
     (\code{family=binomial}), or 'towerKNN'.}
  \item{k}{k nearest neighbors}
}
\details{
See ?towerLM and ?regtools::TStoX
}
\value{
\item{preds}{predicts only the missing; may return some missing.}
\item{NAIdx}{Indices of the original missing.}
}
\references{
See https://github.com/matloff/toweranNA for detail and latest links. 

X. Gu and N. Matloff, A Different Approach to the Problem of Missing
Data, Proceedings of the Joint Statistical Meetings, 2015

M. Jones, Indicator and Stratification Methods for Missing Explanatory
Variables in Multiple Linear Regression, JASA 1996

N. Matloff, Statistical Regression and Classification: from Linear
Models to Machine Learning, 2017, CRC Press; summarizing N. Matloff,
Use of Regression Functions for Improved Estimation of Means,
Biometrika, 1981

O.S. Miettinen, Theoretical Epidemiology:
Principles of Occurrence Research in Medicine, 1985

U. Nur et al, Modelling relative survival in the presence of
incomplete data: a tutorial, Int. J. Epidemiology, 2010

S. Soysal et al, the Effects of Sample Size and Missing Data Rates on
Generalizability Coefficients, Eurasian J. of Ed. Res., 2018
}
\author{
Norm Matloff,
Pete Mohanty 
}
\examples{
# create noisy cylcic series 
set.seed(2020)
x <- rnorm(1000)
x <- runif(1000) * sin(x) + rnorm(1000)
w <- x
# introduce 5 percent missingness
x[sample(1000, 50)] <- NA
# make predictions with lag 3 using k=4 nearest neighbors 
out <- towerTS(x, lag=3, k=4)
# 838 complete cases out of n = 997 observations.
# fitting model...
# Calling toweranNA() for 4 nearest neighbors with linear model estimates.
# $preds
# [1] -0.04555139 -0.06299555 -0.13276715 -0.07856161 -0.06832957 -0.06121348
#
# $naIdxs
# [1]  23  54  61  82  99 140
mean(abs(out$pred - w[out$naIdxs]))
# 0.7709702
}
