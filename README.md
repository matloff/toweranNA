# toweranNA 

A **nonimputational**  method for handling missing values (MVs) in
**prediction applications.** 

*Norm Matloff (UC Davis) and Pete Mohanty (Google)*

## Overview: the goal is prediction, not statistical inference

**The intended class of applications is predictive modeling, rather than
estimation.**  Predictive methods of any type can be used with our Tower
Method, including both linear/generalized linear models and
nonparametric/machine learning methods. 

Most of the MV literature, both in the statistics and machine learning
realms, concerns estimation of some relationship,  say estimation of
regression coefficients and the like.  By constrast, our emphasis here
is on **PREDICTION**, especially relevant in our AI era.  The main
contribution of this package is a technique that we call the Tower
Method, which is **directly aimed at prediction**. It is
nonimputational.  (For some other nonimputational methods, though not
motivated by prediction, see for instance (Soysal, 2018) and (Gu,
2015).)

To make things concrete, say we are regressing Y on a vector X of p
predictors.  We have data on X in a matrix A of n rows, thus of
dimensions n x p.  Some of the elements of A are missing, i.e. are NA
values in the R language.  We are definitely including the
classification case here, so that Y is a vector of 0s and 1s (two-class
case), or an n x k matrix of 0s and 1s  (k-class case).

Note carefully that in describing our methods as being for regression
applications, *we do NOT mean imputing missing values through some
regression technique*; again, our technique is non-imputational. 
Instead, our context is that of regression applications themselves, with the 
goal being prediction of Y.  

## toweranNA: A method based on regression averaging

### Motivating example

Take the Census data, included in the package,  on programmer and
engineer wages, with predictors Age, Education, Occupation, Gender and
Weeks Worked. Say we need to predict a case in which age and
gender are missing.  Then (under proper assumptions), our prediction
might be the estimated value of the regression function of wage on
Education, Occupation and Weeks Worked, i.e. the *marginal regression
function* of wage on those variables.

Since each new case to be predicted will likely have a different
pattern of which variables are missing, we would need to estimate many
(potentially 2<sup>p</sup>) marginal regression functions. This would
in many applications be computationally infeasible, as each marginal
model would need to be fitted and run through diagnostic plots and the
like.

**But the Tower Property provides an alternative.**  It tells us that **we can
obtain the marginal regression functions from the full one.**  

### The Tower Property

The famous formula in probability theory,

```
   EY = E[E(Y | X)]
```

has a more general version known as the Tower Property.  For random
variables Y, U and V, 

``` 
   E[ E(Y|U,V) | U ] = E(Y | U) 
``` 

### How it solves our problem:
 
What this theoretical abstraction says is that if we take the regression
function of Y on U and V, and average it over V for fixed U, we get the
regression function of Y on U.  If V is missing but U is known, this is
very useful.  

In the example above, for a new case in which Education,
Occupation and Weeks Worked are known while Age and Gender are missing,
we would have

```
U  = (Education,Occupation,Weeks Worked)
V = (Age,Gender)
```

E(Y|U) is the target marginal regression function that we wish to
estimate and then use to predict the new case in hand.  The Tower
Property implies that we can obtain that estimate by the averaging
process described above.

Specifically, we fit the full model to the complete cases in the data,
then average that model over all data points whose values for Education,
Occupation and Weeks Worked match those in the new case to be predicted.
Thus only the full model need be estimated, rather than 2<sup>p</sup>
models.

Our function **toweranNA()** ("tower analysis with NAs") takes this
approach.  Usually, there may not be many data points having the exact
value specified for U, if any, so we average over a neighborhood of
points near that value.  

Moreover, an early *Biometrika* paper by one of us (summarized in
(Matloff, 2017, Sec. 7.5)) showed that regression averaging improves
estimation of means, even with no MVs, thus an added bonus. 

### Usage

The call form is

``` r
toweranNA(x, fittedReg, k, newx, scaleX = TRUE) 
```

where the arguments are: 

* **x**: The data frame of the "X" data in the training set, used for
  finding nearest neighbors.  These must be complete cases.

* **fittedReg**: The vector of fitted regression function values
  (parameter or nonparametric) over that set. 

* **k**, The number of nearest neighbors. 

* **newx**: The "X" data frame for the data to be predicted.  

* **scaleX**: If TRUE, scale **newx** before performing the analysis

The scaling argument should be set to TRUE if the
**fittedReg** was derived with scaled X data; if so, **newx** will also
be run through R's **scale()** function.  (The same scaling will be used
for **x** and **newx**.)

The number of neighbors is of course a tuning parameter chosen by the
analyst.  Since we are averaging fitted regression estimates, which are
by definition already smoothed, a small value of **k** should work well.  

## Example:  Vocabulary acquisition

This data is from the Stanford University Wordbank project.  The data,
**english**, is included in the <strong>toweranNA</strong> package
(inherited from the included **regtools** package).  Of
the non-administrative variables, e.g. excluding 'Language', which is
always English in this data, about 43 percent of the values are missing.
To illustrate how fitting and prediction occur, let's fit to the
observations having missing values:

``` r
data(english)
eng <- english[,c(2,5:8,10)] 
# since use neighbors, can't have factors; use
# regtools::factorsToDummies()
eng <- factorsToDummies(eng)  
cc <- complete.cases(eng)
engcc <- eng[cc,]
engicc <- eng[!cc,]
# temp back to data frame for lm()
lmout <- lm(vocab ~ .,data=as.data.frame(engcc)) 
# let's predict the incomplete cases
towerout <- toweranNA(engcc,lmout$fitted.values,5,engicc[,-20],scaleX=FALSE) 
```

The predicted values for **engicc** are now in the vector **towerout**.

## Example:  Gold time series 

Rob Hyndman's **forecast** package includes a time series **gold**,
consisting of 1108 daily gold prices.   The series does have some NAs,
including two in the final 10 data points:

``` r
> gold[1099:1108]
 [1] 395.30 394.10 393.40 396.00     NA     NA 391.25 383.30 384.00 382.30
```
Let's predict the 1109th data point, using the Tower Method:

``` r
> gx <- TStoX(gold,10)
> gxd <- as.data.frame(gx)
> gxdcc <- gxd[complete.cases(gxd),]
> lmout <- lm(V11 ~ .,data=gxd)
> x1109 <- gold[1099:1108]
> toweranNA(gxdcc,lmout$fitted.values,5,matrix(x1109,nrow=1))
[1] 383.0053
```

The function **TStoX()**, which the package imports from **regtools**,
transforms the data to an 11-column matrix, designed for analysis of lag 10.  
In each row of **gx**, we see a value in column 11, preceded in the
row by the 10 most recent points.  That 11th column is the original time
series, minus the first 10 observations.  So, the call to **lm()** is
loosely autoregressive, with each time point predicted from the previous
10.

## Wrapper functions

The package includes functions **towerLM()** and **towerTS()** to wrap
the several operations seen in the worked examples above, e.g.
extracting the complete cases.

## Comparison to other methods

We've compared **toweranNA** on this data to the two leading MV packages
in R, **mice** and **Amelia**.  Unfortunately, **mice** generated a
runtime error on this English data.  In 5 runs comparing **toweranNA** and
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

For time series, we tried the **NH4** data in **imputeTS** package,
using the latter's 'na.ma' method (simple moving average).  The
resulting Mean Absolute Prediction Errors were 1.51 for **imputeTS**,
1.37 for Tower.

Of course, these investigations are just preliminary.

## Assumptions

Note that one important point about distinguishing between the
estimation and prediction cases concerns assumptions.  Most MV methods
(ours too, though to a lesser extent) make strong assumptions, which are
difficult or impossible to verify.  We posit that the prediction context
is more robust to assumptions than is estimation.  This would be similar
to the non-MV setting, in which models can be rather questionable yet
still have strong predictive power.

Compared to **Amelia** and **mice**, **toweranNA** has far less
restrictive assumptions.  E.g. **Amelia** assumes multivariate
normality of the X vector, an assumption not even approximately met when
some components of X are categorical variables.

## Future Work

For some datasets, there may be few, if any, complete cases.  Say p =
10, and that while there are no complete cases, there are many cases
with just one predictor missing.  Then it may be worth computing the 10
marginal regression functions, and proceeding as before.  In that
manner, we may cover most new cases to be predicted (and use another
method for the rest).  Code to automate this will be added.

## Conclusions

The **toweranNA** method handles missing values in applications in which
the main goal is prediction, not estimation.  Our method has major
advantages over **mice** and **Amelia**, in that our method (a) has no
convergence or execution erorr problem, (b) does not make heavy
distributional assumptions, and (c) performs as well or better than the
other two methods in test we've run.

## References

X. Gu and N. Matloff, A Different Approach to the Problem of Missing
Data, *Proceedings of the Joint Statistical Meetings*, 2015

M. Jones, Indicator and Stratification Methods for Missing Explanatory
Variables in Multiple Linear Regression, *JASA*m 1996

N. Matloff, Statistical Regression and Classification: from Linear
Models to Machine Learning, 2017, CRC Press; summarizing N. Matloff,
Use of Regression Functions for Improved Estimation of Means,
*Biometrika*, 1981

O.S. Miettinen, *Theoretical Epidemiology:
Principles of Occurrence Research in Medicine*, 1985

U. Nur *et al*, Modelling relative survival in the presence of
incomplete data: a tutorial, Int. J. *Epidemiology*, 2010

S. Soysal *et al*, the Effects of Sample Size and Missing Data Rates on
Generalizability Coefficients, *Eurasian J. of Ed. Res.*, 2018

