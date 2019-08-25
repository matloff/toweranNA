# toweranNA 

A novel, **nonimputational**  method for handling missing values (MVs) in
**prediction applications.** 

*Norm Matloff and Pete Mohanty*

## Overview

**The intended class of applications is predictive modeling, rather than
estimation.**  Predictive methods of any type can be used with our Tower
Method, including both linear/generalized linear models and
nonparametric/ML methods. 

Most of the MV literature, both in the statistics and machine learning
realms, concerns estimation of some relationship,  say estimation of
regression coefficients and the like.  By constrast, our emphasis here
is on **prediction**, especially relevant in our AI era.  The main
contribution of this package is a novel technique that we call the Tower
Method, which is **directly aimed at prediction**. It is nonimputational.  

To make things concrete, say we are regressing Y on a vector X of p
predictors.  We have data on X in a matrix A of n rows, thus of
dimensions n x p.  Some of the elements of A are missing, i.e. are NA
values in the R language.  We are definitely including the
classification case here, so that Y is a vector of 0s and 1s (two-class
case), or an n x k matrix of 0s and 1s  (k-class case).

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
data, included in the package,  on programmer and engineer wages, with
predictors age, education, occupation, gender and number of weeks
worked. Say we need to predict a case in which age and gender are
missing.  Then (under proper assumptions), our prediction might be the
estimated value of the regression function of wage on education,
occupation and weeks worked, i.e. the marginal regression function of
wage on those variables.

Since each new case to be predicted will likely have a different
pattern of which variables are missing, we would need to estimate many
(potentially 2<sup>p</sup>) marginal regression functions. This would
in many applications be computationally infeasible, as each marginal
model would need to be fitted and run through diagnostic plots and the
like.

But the Tower Property provides an alternative.  It tells us that we can
obtain the marginal regression functions from the full one.  So, we fit
the full model to the complete cases in the data, then average that
model over all data points whose values for education, occupation and
weeks worked match those in the new case to be predicted.  Thus only the
full model need be estimated, rather than 2<sup>p</sup> models.

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
approach.  Usually, there may not be many data points having the exact
value specified for U, if any, so we average over a neighborhood of
points near that value.  

Moreover, an early *Biometrika* paper by one of us (summarized in
(Matloff, 2017, Sec. 7.5)) showed that regression averaging improves
estimation of means, even with no MVs, thus an added bonus. 

The call form is

``` r
toweranNA(x, fittedReg, k, newx, scaleX = TRUE) 
```

where the arguments are: 

* **x**: the data frame of the "X" data in the training set, used for
  finding nearest neighbors. 

* **fittedReg**: the vector of fitted regression function values
  (parameter or nonparametric) over that set 

* **k**, the number of nearest neighbors 

* **newx**: the "X" data frame for the
data to be predicted  

* **sacleX**: if TRUE, scale X before performing the analysis

The scaling argument should be set to TRUE if the
**fittedReg** was derived with scaled X data; if so, **newx** will also
be run through R's **scale()** function.

The number of neighbors is of course a tuning parameter chosen by the
analyst.  Since we are averaging fitted regression estimates, which are
by definition already smoothed, a small value of **k** should work well.  

*Example:  Vocabulary acquisition*

This data is from the Stanford University Wordbank project.  The data,
**english**, is included in the <strong>toweranNA</strong> package.  Of
the non-administrative variables, e.g. excluding 'Language', which is
always English in this data, about 43 percent of the values are missing.
To illustrate how fitting and prediction occur, let's fit to the
observations having missing values:

``` r
data(english)
eng <- english[,c(2,5:8,10)] 
eng <- factorsToDummies(eng)  # since use neighbors, can't have factors
cc <- complete.cases(eng)
engcc <- eng[cc,]
engicc <- eng[!cc,]
lmout <- lm(vocab ~ .,data=engcc) 
# let's predict the incomplete cases
incomp <- !complete.cases(engtst[,-20])
towerout <- toweranNA(engcc,lmout$fitted.values,5,engicc[,-20],scaleX=FALSE) 
```

The predicted values for **engicc** are now in the vector **towerout**.

To assess how well the process works, let's do the following.  In
ordinary regression analysis, R<sup>2</sup> is the squared correlation
between predicted Y and actual Y.  Let's compute that here, pretending
our Y values are missing:

``` r
> cor(towerout,engicc[,20])^2
[1] 0.6415378
```

We've compared **toweranNA** on this data to the two leading MV packages
in R, **mice** and **Amelia**.  Unfortunately, **mice** generated a
runtime error on this data.  In 5 runs comparing **toweranNA** and
**Amelia**, we had these results for mean absolute prediction error:

```
MAPE, Tower   MAPE, Amelia 

125.4055      131.2602 
100.7641      100.3371
105.0529      107.3157 
114.0210      115.2230
98.4792       104.0339
```

Also, on the **prgeng** data included in the package, we predicted wage
income.  The Mean Absolute Prediction Error results over five runs with
the imputational package **mice** with k = 5, were:


```
Tower    mice
21508.97 22497.55
22735.38 22144.87
19909.03 20463.92
25660.07 25681.66
18237.15 18869.71
```

Note we are only predicting the nonintact cases here, i.e. the ones with
one or more NAs.  In the setting here, this typically involves a few
dozen cases, so there is considerable variation from one run to the
next.  But overall, the Tower Method did quite well, **outperforming
mice in all cases but one**.

In addition to achieving better accuracy, Tower was also **a lot
faster**, than **mice** about 0.5 seconds vs. about 13.  (**Amelia** is
comparable to our Tower Method in speed.)

**Amelia** was much faster than **mice**.  We have found,
though, that on some data sets **Amelia** also fails to run.


## Assumptions

Note that one important point about distinguishing between the
estimation and prediction cases concerns assumptions.  Most MV methods
(ours too, though to a lesser extent) make strong assumptions, which are
difficult or impossible to verify.  We posit that the prediction context
is more robust to assumptions than is estimation.  This would be similar
to the non-MV setting, in which models can be rather questionable yet
still have strong predictive power.

## Future Work

For some datasets, there may be few, if any, complete cases.  Say p =
10, and that while there are no complete cases, there are many cases
with just one predictor missing.  Then it may be worth computing the 10
marginal regression functions, and proceeding as before.  In that
manner, we may cover most new cases to be predicted (and use another
method for the rest).  Code to automate this will be added.

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

