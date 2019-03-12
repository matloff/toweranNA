# towerNA 

Novel, **nonimputational**  methods for handling missing values (MVs) in
**prediction** applications.

## Contributions of This Package

1. A novel approach specifically for prediction, based on the Tower 
   Property of expected values, which we call the Tower Method.  

2. Application of our package **polyreg** to develop an extension of 
   the Missing-Indicator Method, which we call the Polynomial 
   Missing-Indicator Method (pMIM).

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
that value.  

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

A planned extension is to apply our **polyreg** package to the Tower
Method.

## Extending the Missing-Indicator Method

Our first method is an extension of the Missing-Indicator Method
(Miettinen, 1983) (Jones 1996).  MIM can be described as follows.

*MIM method*

Say X includes some numeric variable, say Age. MIM add a new column to
A, say Age.NA, consisting of 1s and 0s.  Age.NA is a missingness
indicator for Age:  For any row in A for which Age is NA, the NA is
replaced by 0, and Age.NA is set to 1; otherwise Age.NA is 0.  So rather
than trying to get the missing value, we treat the missingness as
informative, with the information being carried in Age.NA.

It is clear that replacement of an NA by a 0 value -- fake and likely highly
inaccurate data -- can induce substantial bias, say in the regression
coefficient of Age.  Indeed, some authors have dismissed MIM as only
useful back in the era before modern, fast computers that 
can handle computationally intensive imputational methods
(Nur, 2010). 

*Case of categorical variables*

However, now consider categorical variables, i.e. R factors.  Here,
instead of adding a fake 0, we in essence merely add a legitimate new
level to the factor (Jones, 1996).

Say we have a predictor variable EyeColor, taking on values Brown, Blue,
Hazel and Green, thus an R factor with these four levels.  In, say,
**lm()**, R will convert this one factor to three dummy variables, say
EyeColor.Brown, EyeColor.Blue and EyeColor.Hazel.  MIM then would add a
dummy variable for missingness, EyeColor.na, and in rows having a
value of 1 for this variable, the other three dummies would be set to 0, 
We then proceed with our regression analysis as usual, using the modified
EyeColor factor/dummies.

Unlike the case of numeric variables, this doesn't use fake data, and
produces no distortion.  

**NOTE:** Below, all mentions of MIM refer specifically to that method
as applied to categorical variables.
 
*Value of MIM*

As with imputational methods, the non-imputational MIM is motivated by a
desire to make use of rows of A having non-missing values, rather than
discarding them as in CCM.  Note again that this is crucial in our
prediction context; CCM simply won't work here.

Moreover, MIM treats NA values as potentially *informative*; ignoring
them may induce a bias.  MIM may reduce this bias, by indirectly
accounting for the distributional interactions between missingness and
other variables.  This occurs, for instance, in the cross-product terms
in A'A and A'B, where the **lm()** coefficient vector is (A'A)<sup>-1</sup>
A'B.


*The polyanNA package*

The first method offered in our **polyanNA**  package implements MIM for
factors, both in its traditional form, and **with polynomial
extensions** to be described below.

The **mimPrep()** function inputs a data frame and  converts all factor
columns according to MIM.  Optionally, the function will discretize the
numeric columns as well, so that they too can be "MIM-ized."

There is also a function **lm.pa()**, with an associated method for the
generic **predict()**, to implement linear modeling and prediction in
MIM settings, including polynomial MIM (latter not yet implemented). The
point of forming polynomials is that, while MIM takes into accounting
singlet missingnesss, products of these dummy variables then account for
pair missingness, triplet missingness and so on.

*Example* 

The function **lm.pa.ex1()**, included in the package, inputs some Census
data on programmer and engineer wages in 2000.  It regresses WageIncome
against Age, Education, Occupation, Gender, and WeeksWorked.

We intentionally inject NA values in the Occupation variable,
specifically in about 10% of the cases in which Occupation has code 102.
This pattern was motivated by the fact that 102 is one of the
higher-paying categories:  

<pre>
> tapply(pe$wageinc,pe$occ,mean)
     100      101      102      106      140      141 
50396.47 51373.53 68797.72 53639.86 67019.26 69494.44 
</pre>

This experiment is interesting because the proportion of women in those
two occupations is low:

<pre>
> table(pe[,c(5,7)])
     sex occ      0    1 100 1530 3062 101 1153 3345 102 1607 5213 106
209  292 140  127  675 141  282 2595 
</pre>

Due to the fact that there are fewer women in occupations 102 and 140, two
high-paying occupations, a naive regression analysis using CCM might bias
the gender effect downward.  Let's see.

Though we are primarily interested in prediction, let's look at the
estimated coefficient for Gender.

<pre>
Run    full           CC         MIM
1      8558.765       8391.369   8581.544 
2      8558.765       8358.057   8438.852 
3      8558.765       8717.961   8544.091
4      8558.765       8573.875   8585/638
5      8558.765       8270.693   8473.888
</pre>

These numbers are very gratifying. We see that CCM produces a bias, but
that the bias is ameliorated by MIM.

*Extension using a polynomial model*

Now, note that if single NA values are informative, then pairs or
triplets of NAs and so on may carry further information.  In other
words, we should consider forming products of the dummy variables,
between one of the original factors and another.

We handle this by using our [polyreg
package](http://github/matloff/polyreg), which forms polynomial terms in
a *multivariate* context, properly handling the case of indicator
variables. It accounts for the fact that powers of dummies need not be
computed, and that products of dummy columns from the same categorical
variable will be 0.

*MIM functions in this package*

**mimPrep(xy,yCol=NULL,breaks=NULL,allCodeInfo=NULL)**

Applies MIM to all columns in **xy** that are factors, other than a Y
column if present (non-NULL **yCol**, with the value indicating the
column number of Y).  Optionally first discretizes all numeric columns
(other than Y), setting breaks levels.  

Used both on training data and later in prediction.  In the former case,
**allCodeInfo** is NULL, but in the latter case, after fitting, say,
**lm()** to the training data, one saves the value of **allCodeInfo** found
by **mimPrep()** on that data.  Then in predicting new cases, one sets
**allCodeInfo** to that saved value.  In this manner, we ensure that the
same MIM operations are used both in training and later prediction.
 
**lm.pa(paout,maxDeg=1,maxInteractDeg=1)**  The degree of polynomial
used is specified by the remaining two arguments; see the **polyreg**
documentation for details.  [DEGREE > 1 UNDER CONSTRUCTION]

This is a wrapper for **lm()**.  The argument **paout** is the return
value from a call to **mimPrep()** 

**predict.lm.pa(lmpaout,newx)**

[UNDER CONSTRUCTION]

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

O.S. Miettinen, *Theoretical Epidemiology:
Principles of Occurrence Research in Medicine*, 1985

U. Nur *et al*, Modelling relative survival in the presence of
incomplete data: a tutorial, Int. J. *Epidemiology*, 2010

S. Soysal *et al*, the Effects of Sample Size and Missing Data Rates on
Generalizability Coefficients, *Eurasian J. of Ed. Res.*, 2018

