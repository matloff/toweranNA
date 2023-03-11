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
out <- towerTS(x, lag=10, k=4)
#  -0.1685019   
}
