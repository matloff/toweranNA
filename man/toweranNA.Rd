\name{toweranNA}
\alias{toweranNA}
\title{
does prediction using the Tower Method to deal with NAs
}
\description{
In the case of regression or a 2-class classification problem,
'fittedReg' will be the estimated regression function, evaluated at x; 
e.g. the compoment 'fitted.values' from lm() output.

For a multiclass classification problem, 'fittedReg' will be a matrix,
with number of columns equal to number of classes, and number of rows
equal to that of 'newx'; the (i,j) element will be the estimated
conditional probability of that class, given row i of x.

The purpose of scaling x and newx is that the k-NN ops will be better
if all the predictor variables are commensurate
}
\usage{
toweranNA(x,fittedReg,k=1,newx,scaleX=TRUE)
}
\arguments{
  \item{x}{matrix/data frame of "X" values, numeric, all complete cases.}
  \item{fittedReg}{fitted regression values} 
  \item{k}{number of nearest neighbors}
  \item{scaleX}{Scale x and newx before prediction? Default: TRUE.}
  \item{newx}{matrix/data frame of new "X" values}

}

\value{vector of predicted values}
\references{
The intended class of applications is predictive modeling, rather than estimation. Predictive methods of any type can be used with our Tower Method, including both linear/generalized linear models and nonparametric/ML methods.

Most of the MV literature, both in the statistics and machine learning realms, concerns estimation of some relationship, say estimation of regression coefficients and the like. By constrast, our emphasis here is on PREDICTION, especially relevant in our AI era. The main contribution of this package is a novel technique that we call the Tower Method, which is directly aimed at prediction. It is nonimputational. (For some other nonimputational methods, though not motivated by prediction, see for instance (Soysal, 2018) and (Gu, 2015).)

To make things concrete, say we are regressing Y on a vector X of p predictors. We have data on X in a matrix A of n rows, thus of dimensions n x p. Some of the elements of A are missing, i.e. are NA values in the R language. We are definitely including the classification case here, so that Y is a vector of 0s and 1s (two-class case), or an n x k matrix of 0s and 1s (k-class case).

Note carefully that in describing our methods as being for regression applications, we do NOT mean imputing missing values through some regression technique. Instead, our context is that of regression applications themselves, with the goal being prediction.

See https://github.com/matloff/toweranNA for detail and latest links,
}
\author{
Norm Matloff,
Pete MOhanty
}
\examples{
# split off new data
newx <- mtcars[-c(1:10),-1]
# insert missing value in each of the first ten rows
for(i in 1:10)
  newx[i, i] <- NA
head(newx)
#                     cyl  disp  hp drat    wt  qsec vs am gear carb
# Merc 280C            NA 167.6 123 3.92 3.440 18.90  1  0    4    4
# Merc 450SE            8    NA 180 3.07 4.070 17.40  0  0    3    3
# Merc 450SL            8 275.8  NA 3.07 3.730 17.60  0  0    3    3
# Merc 450SLC           8 275.8 180   NA 3.780 18.00  0  0    3    3
# Cadillac Fleetwood    8 472.0 205 2.93    NA 17.98  0  0    3    4
# Lincoln Continental   8 460.0 215 3.00 5.424    NA  0  0    3    4
 
pred <- toweranNA(x=mtcars[1:10,-1],
                  fittedReg=lm(mpg~., data=mtcars[1:10,])$coefficients, 
                  k=3, 
                  newx=newx)
head(pred)
#[1] -0.3051317  2.1920280  2.0616099  2.1920280  2.0616099  2.1920280
}
