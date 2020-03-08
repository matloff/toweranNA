\name{towerLM}
\alias{towerLM}
\title{Wrapper for toweranNA for linear models and generalized linear models.}
\description{Wrapper for toweranNA() in lm() case; here x is a data matrix of X, y is Y; useGLM() means glm instead of lm(); other args as in toweranNA().}
\usage{
towerLM(x, y, k, newx, useGLM = FALSE, scaleX = FALSE, noisy=TRUE)
}
\arguments{
  \item{x}{A data frame or matrix of features/predictors which may contain missing data.}
  \item{y}{A vector containing response/outcome which may contain missing data.}
  \item{k}{K nearest neighbors. A hyperparameter that is passed onto toweranNA().}
  \item{newx}{matrix/data frame of new "X" values}
  \item{useGLM}{Fit generalized linear model via maximum likelihood? Default: FALSE (fit linear model via ordinary least squares). GLM currently only implemented for family="binomia."}
  \item{scaleX}{scale (standardize) x and newx before prediction? The purpose of scaling x and newx is that the k-NN ops will be better if all the predictor variables are commensurate Default: FALSE.}
  \item{noisy}{Display updates? Default TRUE.}
}
\value{Returns a vector of predictions (based on newx) via toweranNA().}
\references{
The intended class of applications is predictive modeling, rather than estimation. Predictive methods of any type can be used with our Tower Method, including both linear/generalized linear models and nonparametric/ML methods.

Most of the MV literature, both in the statistics and machine learning realms, concerns estimation of some relationship, say estimation of regression coefficients and the like. By constrast, our emphasis here is on PREDICTION, especially relevant in our AI era. The main contribution of this package is a novel technique that we call the Tower Method, which is directly aimed at prediction. It is nonimputational. (For some other nonimputational methods, though not motivated by prediction, see for instance (Soysal, 2018) and (Gu, 2015).)

To make things concrete, say we are regressing Y on a vector X of p predictors. We have data on X in a matrix A of n rows, thus of dimensions n x p. Some of the elements of A are missing, i.e. are NA values in the R language. We are definitely including the classification case here, so that Y is a vector of 0s and 1s (two-class case), or an n x k matrix of 0s and 1s (k-class case).

Note carefully that in describing our methods as being for regression applications, we do NOT mean imputing missing values through some regression technique. Instead, our context is that of regression applications themselves, with the goal being prediction.

See https://github.com/matloff/toweranNA for detail and latest links. 

X. Gu and N. Matloff, A Different Approach to the Problem of Missing
Data, Proceedings of the Joint Statistical Meetings, 2015

M. Jones, Indicator and Stratification Methods for Missing Explanatory
Variables in Multiple Linear Regression, JASA 1996

N. Matloff, Statistical Regression and Classification: from Linear
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
\note{
This function is based on a general version Tower Property: E[ E(Y|U,V) | U ] = E(Y | U). What this theoretical abstraction says is that if we take the regression function of Y on U and V, and average it over V for fixed U, we get the regression function of Y on U. If V is missing but U is known, this is very useful. 
}
\examples{
# Set up a toy example by introducing 
# roughly 10 percent missingness into mtcars:
set.seed(123)
missing <- matrix(runif(prod(dim(mtcars))), ncol=ncol(mtcars)) < 0.1
xy <- mtcars
for(i in ncol(mtcars))
  xy[,i] <- ifelse(missing[,i], NA, mtcars[,i])
  
# split the data into old and new
new <- sample(c(TRUE,FALSE), size=nrow(mtcars), replace=TRUE)

# mpg, the outcome used as y, is in column 1
predicted <- towerLM(x=xy[!new, -1], y=xy$mpg[!new], newx=xy[new,-1], k=3)
head(predicted)
# [1] 21.88045 24.30537 17.38092 21.79566 16.98026 21.88045 21.88045 
cor(predicted, mtcars$mpg[new])
# [1] 0.8706723

}