\name{towerTS}
\alias{towerTS}
\title{
Tower for Times Series
}
\description{

Fits a linear model or other regression method to to lagged elements,
using the Tower approach.  Here k is the number of nearest neighbors as
in \code{toweranNA}.  Currently predicts only the component that is just
past the end of the data.

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
Predicted value for the next item in the series.
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
# introduce 10 percent missingness
x[sample(1000, 100)] <- NA
# make predictions with lag 3 using k=4 nearest neighbors 
towerTS(x, lag=10, k=4)
#  -0.1685019   
}
