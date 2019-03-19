# toweranNA 

A novel, **nonimputational**  method for handling missing values (MVs) in
**prediction** applications.


## Overview

The intended class of applications is predictive modeling, rather than
estimation.  Predictive methods of any type can be used with our Tower
Method, including both linear/generalized linear models and
nonparametric/ML methods. 

Most of the MV literature, both in the statistics and machine learning
realms, concerns estimation of some relationship,  say estimation of
regression coefficients and the like.  By constrast, our emphasis here
is on **prediction**, especially relevant in this era of Big Data.  The main
contribution of this package is a novel technique that we call the Tower
Method, which is directly aimed at prediction. It is nonimputational.  

Note that one important point about distinguishing between the
estimation and prediction cases concerns assumptions.  Most MV methods
(including ours) make strong assumptions, which are difficult or
impossible to verify.  We posit that the prediction context is more
robust to assumptions than is estimation.  This would be similar to the
non-MV setting, in which models can be rather questionable yet still
have strong predictive power.

The large difference beween the estimation and prediction goals is
especially clear when one notes that, in predicting new cases that have
missing values, the classic Complete Cases method (CCM) -- use only
fully intact rows -- is useless.  Under proper assumptions, CCM can be
used for estimation, but it can't be used for prediction of new cases
with MVs.

To make things concrete, say we are regressing Y on a vector X of length
p.  We have data on X in a matrix A of n rows, thus of dimensions n X p.
Some of the elements of A are missing, i.e. are NA values in the R
language.  We are definitely including the classification case here, so
that Y consists of 0s and 1s (two-class case), or a matrix (multiclass
case).

Note carefully that in describing our methods as being for regression
applications, *we do NOT mean imputing missing values through some
regression technique.* Instead, our context is that of regression
applications themselves, with the goal being prediction.  Again, all of
our methods are **nonimputational**.  (For some other nonimputational
methods, see for instance (Soysal, 2018) and (Gu, 2015).)

## toweranNA(): A novel method based on regression averaging

The famous formula in probability theory,

```
   EY = E[E(Y | X)]
```

has a more general version known as the Tower Property.  For random
variables Y, U and V, 

``` 
   E[ E(Y|U,V) | U ] = E(Y | U) 
``` 
   
What this theoretical abstraction says is that if we take the regression
function of Y on U and V, and average it over V for fixed U, we get the
regression function of Y on U.  

If V is missing but U is known, this is very useful.  Take the Census
data above on programmer and engineer wages, with predictors age,
education, occupation, gender and number of weeks worked. Say we need to
predict a case in which age and gender are missing.  Then (under proper
assumptions), our prediction might be the estimated value of the
regression function of wage on education, occupation and weeks worked,
i.e. the marginal regression function of wage on those variables.

Since each new observation to be predicted will likely have a different
pattern of which variables are missing, we would need to estimate many
marginal regression functions, which would in many applications be
computationally infeasible.

But the Tower Property provides an alternative.  It tells us that we can
obtain the marginal regression functions from the full one.  So, we fit
the full model to the complete cases in the data, then average that
model over all data points whose values for education, occupation and
weeks worked match those in the new case to be predicted.  

In the Tower Property above, for a new case in which education,
occupation and weeks worked are known while age and gender are missing,
we would have

```
U  = (education,occupation,weeks worked)
V = (age,gender)
```

E(Y|U) is the target marginal regression function that we wish to
estimate and then use to predict the new case in hand.  The Tower
Property implies that we can obtain that estimate by the averaging
process described above.

Our function **toweranNA()** ("tower analysis with NAs") takes this
approach.  Usually, there will not be many data points having the exact
value specified for U, so we average over a neighborhood of points near
that value.  Moreover, an early paper (Matloff, 2017) showed that
regression averaging improves estimation of means, even with no MVs. 

The call form is

``` r
toweranNA(x, fittedReg, k, newx, scaleX = TRUE) 
```

where the arguments are: the data frame of the "X" data in the
training set; the vector of fitted regression function values from that
set; the number of nearest neighbors; and the "X" data frame for the
data to be predicted.

The number of neighbors is of course a tuning parameter chosen by the
analyst.  Since we are averaging fitted regression estimates, which are
by definition already smoothed, a small value of k should work well.  

*Example:*


The function **doGenExpt()** allows one to explore the behavior of the
Tower Method on real data.  Here we again look at the Census data,
predicting wage income from age, gender, weeks worked, education and
occupation, artificially injecting NA values for gender in a random 20%
of the data.  This is a nontrival NA pattern, since (a) most cases are
men and (b) conditional on all the predictor variables, women have lower
wages than comparable men.

<pre>
pe1 <- pe[,c(1,2,4,6,7,12:16,3)]
naIdxs <- sample(1:nrow(pe1),round(0.2*nrow(pe1)))
pe1$sex[naIdxs] <- NA
doGenExpt(pe1)
</pre>

The Mean Absolute Prediction Error results over five runs with the
imputational package **mice** with k = 5, were:


<pre>
Tower    mice
21508.97 22497.55
22735.38 22144.87
19909.03 20463.92
25660.07 25681.66
18237.15 18869.71
</pre>

Note we are only predicting the nonintact cases here, i.e. the ones with
one or more NAs.  In the setting here, this typically involves a few
dozen cases, so there is considerable variation from one run to the
next.  But overall, the Tower Method did quite well, **outperforming
mice in all cases but one**.

Moreover achieving better accuracy, Tower was also **a lot
faster**, about 0.5 seconds vs. about 13.

The results with another imputational package, **Amelia** were similar,
though **Amelia** was much faster than **mice**.  We have found,
by the way, that on some data sets **Amelia** or **mice** file.


## Assumptions

We will not precisely define assumptions underlying the above methods
here; roughly, they are similar to those most existing methods.
However, as noted, our view is that prediction contexts are more robust to
assumptions, as seen in the examples above.

## References

X. Gu and N. Matloff, A Different Approach to the Problem of Missing
Data, *Proceedings of the Joint Statistical Meetings*, 2015

M. Jones, Indicator and Stratification Methods for Missing Explanatory
Variables in Multiple Linear Regression, *JASA*m 1996

N. Matloff, Statistical Regression and Classification: from Linear
Models to Machine Learning, 2017, CRC Press

O.S. Miettinen, *Theoretical Epidemiology:
Principles of Occurrence Research in Medicine*, 1985

U. Nur *et al*, Modelling relative survival in the presence of
incomplete data: a tutorial, Int. J. *Epidemiology*, 2010

S. Soysal *et al*, the Effects of Sample Size and Missing Data Rates on
Generalizability Coefficients, *Eurasian J. of Ed. Res.*, 2018

