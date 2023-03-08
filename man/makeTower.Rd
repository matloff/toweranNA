\name{makeTower}
\alias{makeTower}
\title{Nonimputational method for dealing with NA values in prediction
application}
\description{In a prediction application, the intended regression model
is applied to complete cases, from which marginal regression models can
be derived for predicting new cases with arbitrary NA patterns.
}
\usage{
makeTower(data,yName,regFtnName,scaling=NULL,yesYVal=NULL)
}
\arguments{
  \item{data}{Data frame or equivalent..}
  \item{yName}{Name of the column in \code{data} to be predicte to be
     predicted.} 
  \item{regFtnName}{Regression model to be used, currently 'lm', 'glm'
     (\code{family=binomial}), or 'towerKNN'.}
  \item{opts}{Options for \code{regFtnName}, a quoted string with 
     separating commas.}
  \item{k}{number of nearest neighbors}
  \item{scale}{Scaling to be applied to x and newx. Default NULL
     means no scaling.}

}

\value{vector of predicted values}
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
