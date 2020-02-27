\name{checkConstCols}
\alias{checkConstCols}
\title{Check for constant columns.}
\description{
Report whether any columns in data frame are constant, including the case of constant value plus some NAs.
}
\usage{
checkConstCols(dfr)
}
\arguments{
  \item{dfr}{A data frame.}
}
\value{
Boolean vector indicating whether each column is constant.
}
\author{Norm Matloff}
\examples{
# create 10x10 matrix with random entries between 0 and 1
X <- matrix(runif(100), ncol=10)
# make column 3 constant and then convert to data.frame
X[,3] <- 1
X <- as.data.frame(X)
checkConstCols(X)
#   V1    V2    V3    V4    V5    V6    V7    V8    V9   V10 
#FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE 

}