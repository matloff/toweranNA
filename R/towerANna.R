
#########################  toweranNA  ##################################

# does prediction using the Tower Method to deal with NAs

# arguments:

#    x: matrix/data frame of "X" values, numeric, all complete cases
#    fittedReg: fitted regression values; see below 
#    k: number of nearest neighbors
#    scaleX: scale xc and newx before prediction
#    newx: matrix/data frame of new "X" values

# in the case of regression or a 2-class classification problem,
# 'fittedReg' will be the estimated regression function, evaluated at x; 
# e.g. the compoment 'fitted.values' from lm() output

# for a multiclass classification problem, 'fittedReg' will be a matrix,
# with number of columns equal to number of classes, and number of rows
# equal to that of 'newx'; the (i,j) element will be the estimated
# conditional probability of that class, given row i of x

# value: vector of predicted values

toweranNA <- function(x,fittedReg,k,newx,scaleX=TRUE) 
{
   if (sum(is.na(x)) > 0)
      stop('x must be NA-free; call complete.cases()')
   factors <- sapply(x,is.factor)
   if (any(factors)) {
      print('factors present in X data')
      stop('convert using factorsToDummies()')
   }
   allNA <- function(w) all(is.na(w))
   if (sum(apply(newx,1,allNA) > 0)) 
      stop('newx has a row of all NAs')
   require(FNN)
   if (is.matrix(fittedReg) && ncol(fittedReg) == 1) 
      fittedReg <- as.vector(fittedReg)
   multiclass <- is.matrix(fittedReg)
   nc <- ncol(x)
   if (scaleX) {
      x <- scale(x,center=TRUE,scale=TRUE)
      xmns <- attr(x,'scaled:center')
      xsds <- attr(x,'scaled:scale')
   }
   if (!multiclass) {
       preds <- vector(length = nrow(newx))
   } else {
       preds <- matrix(nrow = nrow(newx),ncol = ncol(fittedReg))
   }
   for (i in 1:nrow(newx)) {
      rw <- newx[i,]
      intactCols <- which(!is.na(rw))
      ic <- intactCols
      if (length(ic) == 0) {
         warning('a newx row has is all NAs, skipping i')
         next
      }
      rw <- rw[ic]
      # kludgy but if x is a data frame get problems with scale()
      rwm <- as.matrix(rw)
      rwm <- matrix(rwm,nrow=1)
      if (scaleX) {
         # rw <- scale(matrix(rw, nrow=1),center=xmns[ic],scale=xsds[ic])
         rw <- scale(rwm,center=xmns[ic],scale=xsds[ic])
      }
      tmp <- FNN::get.knnx(data = x[,ic],query = rw, k = k)
      nni <- tmp$nn.index
      if (!multiclass) {
         preds[i] <- mean(fittedReg[nni])
      } else {
         preds[i,] <- colMeans(fittedReg[nni,])
      }
   }
   preds
}

###########################  doExpt1()  ###############################

# arguments:

#    inputDF:  input data frame, response variable in last col
#    naCols1:  1st set of cols in which to insert NAs
#    naCols2:  2nd set of cols in which to insert NAs
#    k:  number of nearest neighbors

# training and test sets are generated, the latter of size 500 cases; in
# the latter, the first 250 will have cols naCols1 set to NA, and the
# rest will have the same for cols naCols2; the Tower method will use k
# nearest neighbors

doExpt1 <- function (inputDF,naCols1,naCols2,k=1)
{
    require(mice)
    nr <- nrow(inputDF)
    nc <- ncol(inputDF)
    idxs <- sample(1:nr, 500)
    lmo <- lm(wageinc ~ ., data = inputDF[-idxs, ])
    ftd <- lmo$fitted.values
    newx <- inputDF[idxs,-nc]
    ## newx.full <- newx  # full data, no NAs
    newx[1:250,naCols1] <- NA
    newx[251:500,naCols2] <- NA
    print(system.time(pred.tower <- toweranNA(inputDF[-idxs,-nc], 
        ftd,k,newx)))
    ## pred.full <- predict(lmo, newx.full)
    # now prepare for using mice; combine the training X data with newx,
    # yielding the original X data in inputDF except for the insertion of
    # NAs; the training data is then helping mice fill in the NAs
    inputDF2 <- rbind(inputDF[-idxs,-nc], newx)
    print(system.time(miceout <- mice(inputDF2,m=1,maxit=50, 
        meth="pmm", printFlag=F)))
    print(system.time(inputDF3 <- complete(miceout)))
    # now extract the last 500 rows, obtaining the imputed version of
    # newx
    newx.mice <- inputDF3[(nrow(inputDF3) - 499):nrow(inputDF3), ]
    pred.mice <- predict(lmo, newx.mice)
    acc.tower <- mean(abs(pred.tower - inputDF$wageinc[idxs]))
    ## acc.full <- mean(abs(pred.full - inputDF$wageinc[idxs]))
    acc.mice <- mean(abs(pred.mice - inputDF$wageinc[idxs]))
    c(acc.tower,acc.mice)
}

# temporarily combine X from training set with X to be predicted, so as
# to make mice() more effective on the latter; after NAs replaced,
# remove the old X
miceFit <- function(oldx,newx,maxit=50,meth='pmm',printFlag=F) {
   require(mice)
   bigx <- rbind(oldx,newx)
   miceout <- mice(bigx,m=1,maxit=50,meth='pmm',printFlag=F)
   tmp <- complete(miceout)
   tmp[-(1:nrow(oldx)),]
}

# same as miceFit but with Amelia
ameliaFit <- function (oldx, newx) 
{
    require(Amelia)
    bigx <- rbind(oldx, newx)
    ameliaout <- amelia(bigx, m = 1)
    tmp <- ameliaout$imputations[[1]]
    tmp[-(1:nrow(oldx)), ]
}

# # general comparison of toweranNA() and mice(); the data frame xy must
# # have Y in last col; saves 'holdout' random rows for the test set;
# # naAdder(), if non-NULL, injects NAs into xy; k is the number of
# # nearest neighbors
# 
# doExpt1 <- function (xy, naAdder = NULL, holdout = 1000, k = 5, regftn = lm) 
# { 
#     if (!is.null(naAdder)) 
#         xy <- naAdder(xy) 
#     nr <- nrow(xy) 
#     nc <- ncol(xy) 
#     idxs <- sample(1:nr, holdout) 
#     xytrain <- xy[-idxs, ] 
#     xytest <- xy[idxs, ] 
#     cc <- complete.cases(xytest) 
#     xytest <- xytest[-which(cc), ] 
#     frml <- paste(names(xy)[nc], " ~ .", sep = "") 
#     frml <- as.formula(frml) 
#     if (identical(regftn,lm)) { 
#        lmo <- lm(frml, data = xytrain) 
#     } else { 
#        lmo <- glm(frml, data = xytrain, family=binomial) 
#     } 
#     ftd <- lmo$fitted.values 
#     xytraincc <- xytrain[complete.cases(xytrain), ] 
#     print(system.time(pred.tower <- toweranNA(xytraincc[, -nc], 
#         ftd, k, xytest[, -nc]))) 
#     if (identical(regftn,glm)) 
#        pred.tower <- round(pred.tower) 
#     print(system.time(testmice <- miceFit(xytrain[,-nc], 
#        xytest[,-nc]))) 
#     if (identical(regftn,lm)) { 
#        pred.mice <- predict(lmo, testmice) 
#        mt <- mean(abs(pred.tower - xytest[, nc])) 
#        mm <- mean(abs(pred.mice - xytest[, nc])) 
#     } else { 
#        pred.mice <- round(predict(lmo, testmice,type='response')) 
#        mt <- mean(pred.tower == xytest[, nc]) 
#        mm <- mean(pred.mice == xytest[, nc]) 
#     } 
#  
#     c(mt, mm) 
# } 

# arguments:
# 
#    xy: data, X variables first then Y variable(s)
#    holdout:  size of test set
#    k:  number of nearest neighbors
#    regftn:  regression function, currently lm or glm
#    imput:  imputation method, currently 'mice' or 'amelai'
#    scaleX:  if TRUE, scale X first`
#    numY:  number of components in Y, e.g. k for a k-class 
#           classification problem

doGenExpt <- function(xy,holdout=1000,k=5,regftn=lm,
   imput='mice',scaleX=TRUE,numY=1)
{   if (numY > 1 && identical(regftn,glm))
       stop("can't use glm yet on vector Y")
    nr <- nrow(xy)
    nc <- ncol(xy)
    checkConstCols(xy)
    idxs <- sample(1:nr, holdout)
    xytrain <- xy[-idxs, ]
    xytest <- xy[idxs, ]
    cc <- complete.cases(xytest)
    if (sum(cc) == nrow(xytest)) stop('all cases in test set are complete')
    xytest <- xytest[-which(cc), ]
    if (numY == 1) {
       frml <- paste(names(xy)[nc], " ~ .", sep = "")
    } else {
       nc <- (nc-numY+1):nc
       ynames <- names(xy)[nc]
       ynlist <- paste0(ynames,collapse=',')
       frml <- paste0('cbind(',ynlist,') ~ .')
    }
    frml <- as.formula(frml)
    if (identical(regftn,lm)) {
       lmo <- lm(frml, data = xytrain)
    } else {
       lmo <- glm(frml, data = xytrain, family=binomial)
    }
    if (any(is.na(coef(lmo)))) stop('some coefs NAs')
    ftd <- lmo$fitted.values
    xytraincc <- xytrain[complete.cases(xytrain), ]
    print(system.time(pred.tower <- 
       toweranNA(xytraincc[, -nc], ftd, k, xytest[, -nc],
          scaleX=scaleX)))
    if (identical(regftn,glm))
       pred.tower <- round(pred.tower)
    if (imput == 'mice') {
       print(system.time(testimput <- miceFit(xytrain[,-nc],xytest[,-nc])))
    } else {
       print(system.time(testimput <- ameliaFit(xytrain[,-nc],xytest[,-nc])))
    }
    if (identical(regftn,lm)) {
       pred.imput <- predict(lmo, testimput) 
       mt <- mean(abs(pred.tower - xytest[, nc]))
       mm <- mean(abs(pred.imput - xytest[, nc]))
    } else {
       pred.imput <- round(predict(lmo, testimput,type='response'))
       mt <- mean(pred.tower == xytest[, nc])
       mm <- mean(pred.imput == xytest[, nc])
    }

    c(mt, mm)
}


