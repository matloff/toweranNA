\name{makeTower}
\alias{makeTower}
\alias{predict.tower}
\title{Nonimputational method for dealing with NA values in prediction
application}
\description{In a prediction application, the intended regression model
is applied to complete cases, from which marginal regression models can
be derived for predicting new cases having arbitrary NA patterns.
}
\usage{
makeTower(data,yName,regFtnName,opts,scaling=NULL,yesYVal=NULL)
\method{predict}{tower}(object,newx,k=1,...)
}
\arguments{
  \item{data}{Data frame or equivalent.}
  \item{yName}{Name of the column in \code{data} to be
     predicted.} 
  \item{regFtnName}{Regression model to be used, currently 'lm', 'glm'
     (\code{family=binomial}), or 'towerKNN'.}
  \item{opts}{Optional arguments for \code{regFtnName}, an R list.} 
  \item{k}{number of nearest neighbors}
  \item{scaling}{Scaling to be applied to x and newx. Default NULL
     means no scaling.}
  \item{yesYVal}{In the case of dichotomous Y, this specifies the level
     to be considered positive, i.e. for which Y will be 1.}
  \item{object}{Object of type 'tower'.}
  \item{newx}{New case(s) to be predicted, in the same format as in the
     non-Y portion of \code{data}.}
  \item{...}{Other arguments need by \code{regFtnName}.}
}

\value{Object of class 'tower', to be used as input to
\code{predict.tower}.}

\author{
Norm Matloff,
Pete Mohanty
}

\examples{
towerOut <- makeTower(mtcars,'mpg','lm')

newx <- mtcars[-c(1:10),-1]
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
 
predict(towerOut,newx,k=3)
#  [1] 20.00086 15.17132 15.17132 15.17132 11.15469 11.15469 11.15469 28.52625
#  [9] 29.06067 28.52625 24.72144 17.45622 16.75827 15.52077 14.95958 28.52625
# [17] 25.34890 26.08506 15.52077 19.19484 15.37239 24.72144
}

