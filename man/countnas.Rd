\name{countnas}
\alias{countnas}
\title{Count NAs in each column.}
\description{
see how many NAs in each column of dfr; counts are reported unles prop is TRUE, in which case proportions of NAs are shown
}
\usage{
countnas(dfr, prop = FALSE)
}
\arguments{
  \item{dfr}{A data.frame that may contain missing values.}
  \item{prop}{Return a proportion? Default: FALSE (i.e., return counts.)}
}
\value{A vector of counts of the number of missing values found in each column.}
\author{Norm Matloff}
\examples{
set.seed(2020)
# create matrix with 1000 rows and 10 columns each with a random number between 0 and 1
X <- matrix(runif(10000), ncol=10)
# turn approximately 5 percent of entries into missing values 
X[X < 0.05] <- NA
X <- as.data.frame(X)
countnas(X)
# V1  V2  V3  V4  V5  V6  V7  V8  V9 V10 
# 60  42  53  53  53  42  60  57  44  56 
}
