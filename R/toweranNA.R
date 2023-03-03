
#########################  toweranNA  ##################################

# does prediction using the Tower Method to deal with NAs

# arguments:

#    data: a data frame or equivalent, consisting of numeric and/or factor
#    variables

#    yName: name of the column in 'data' for "Y"; the other columns form
#    "X"; if "Y" is dichotomous or categorical

#    regFtnName:  current choices are 'lm','glm' (with family=binomial),
#       and kNN (from regtools package)

#    opts: optional arguments for the call to the regression function;
#       in the form of a string, e.g. 'a=3,b=8'

#    scaling: if TRUE, scaling will be done on "X", using 'scale' 

#    yesYVal: in dichotomous case, which value should be coded as 1

# value: object of class 'tower', for which the predict() method
# predict.tower() is available

# note that the predict() function will later need NA-free data, no
# matter which regression model is used

makeTower <- function(data,yName,regFtnName,opts=NULL,scaling,yesYVal=NULL) 
{
   yCol <- which(names(data) == yName)
   x <- data[,-yCol]
   y <- data[,yCol]
   classif <- is.factor(y)
   multiclass <- classif && length(levels(y)) > 2

   # convert any "X" factors
   factors <- sapply(x,is.factor)  
   origX <- x
   if (any(factors)) {
      x <- regtools::factorsToDummies(x,omitLast-TRUE)
   }

   # and for "Y"
   origY <- y
   if (classif) {
      if (!multiclass) {
         if (is.null(yesYVal)) stop('must specify yesYVal')
         y <- as.integer(y == yesYVal)
      } else {
         y <- regtools::factorsToDummies(y,omitLast=FALSE)
      }
   }

   # fit the regression model

   returnObj <- list(x=x,fittedReg=fittedReg,multiclass=multiclass)
   class(returnObj) <- 'tower'
   returnObj
}

predict.tower <- function(towerObj,newx,k=1)
{
   x <- towerObj$x
   fittedReg <- towerObj$fittedReg
   multiclass <- towerObj$multiclass

   if (is.vector(newx)) newx <- matrix(newx,nrow=1)

   # method cannot predict a newx row consisting of all NAs
   allNA <- function(w) all(is.na(w))
   allna <- apply(newx,1,allNA)
   sumAllNA <- sum(allna)
   if (sumAllNA > 0)  {
      print('some rows of newx were all NAs\n:')
      print(which(allna))
      stop("Drop rows which are all NA.")
   }

   # set up space for the predictions
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
#       if (scaleX) {
#          # rw <- scale(matrix(rw, nrow=1),center=xmns[ic],scale=xsds[ic])
#          rwm <- scale(rwm,center=xmns[ic],scale=xsds[ic])
#       }
      if (k == 1) {
         tmp <- pdist(rwm[1,],x[,ic])@dist
         nni <- which.min(tmp)
      } else {
         xic <- x[,ic,drop=FALSE]
         if (k > nrow(xic)) {
            kThisTime <- nrow(xic)
            warning('too few intact neigbors, temp reduced k')
         } else kThisTime <- k
         tmp <- get.knnx(data = xic,query = rwm, k = kThisTime)
         nni <- tmp$nn.index
      }
      if (!multiclass) {
         preds[i] <- mean(fittedReg[nni])
      } else {
         preds[i,] <- colMeans(fittedReg[nni,])
      }
   }
   # decided above to just bail if have any all-NA rows
   # if (someAllNA) {   
   #    altPreds <- rep(NA,length(allna))
   #    altPreds[!allna] <- preds
   #    preds <- altPreds
   # }
   preds
}

###########################  towerLM()  ###############################

# wrapper for toweranNA() in lm() case; here x is a data matrix of X, y
# is Y; useGLM() means glm instead of lm(); other args as in toweranNA()

towerLM <- function(x,y,k,newx,useGLM=FALSE,scaleX=FALSE,noisy=TRUE) {
   if(scaleX) 
      x <- scale(x) 
   ccs <- complete.cases(cbind(x,y))
   if(noisy) cat(sum(ccs), 'complete cases out of n =', nrow(x),'observations.\n')
   xcc <- as.data.frame(x[ccs,])
   ycc <- y[ccs]
   if(noisy) cat("fitting model...\n")
   est <- if(useGLM) glm(ycc ~ ., data=xcc, family=binomial) else lm(ycc ~ ., data=xcc)
   if(noisy) cat("Calling toweranNA() for", k, "nearest neighbors with", 
                 if(useGLM) "generalized linear model" else "linear model", "estimates.")
   toweranNA(xcc,est$fitted.values,k,newx,scaleX)
}


############################  towerTS  ###############################

# Tower for time series; fits linear model to lagged elements; k is the
# number of nearest neighbors; predicts only the missing; the component
# 'naIdxs' records the indices of the predicted elements (some will be
# NA)

towerTS <- function(xts,lag,k) {
   xy <- TStoX(xts,lag)
   l1 <- lag + 1
   x <- xy[,-l1]; y <- xy[,l1]
   NAs.orig <- which(is.na(xts))
   # can't predict before time lag+1
   NAs <- NAs.orig[NAs.orig > lag]
   # adjust for shifted indexing
   newx <- x[NAs-lag,]
   preds <- towerLM(x,y,k,newx,FALSE)
   NAsSkipped <- NAs.orig[NAs.orig <= lag]
   firstpreds <- rep(NA,length(NAsSkipped))
   preds <- c(firstpreds,preds)
   list(preds = preds, naIdxs = NAs.orig)
}

