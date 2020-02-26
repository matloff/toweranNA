
#########################  toweranNA  ##################################

# does prediction using the Tower Method to deal with NAs

# arguments:

#    x: matrix/data frame of "X" values, numeric, all complete cases
#    fittedReg: fitted regression values; see below 
#    k: number of nearest neighbors
#    scaleX: scale x and newx before prediction
#    newx: matrix/data frame of new "X" values

# in the case of regression or a 2-class classification problem,
# 'fittedReg' will be the estimated regression function, evaluated at x; 
# e.g. the compoment 'fitted.values' from lm() output

# for a multiclass classification problem, 'fittedReg' will be a matrix,
# with number of columns equal to number of classes, and number of rows
# equal to that of 'newx'; the (i,j) element will be the estimated
# conditional probability of that class, given row i of x

# the purpose of scaling x and newx is that the k-NN ops will be better
# if all the predictor variables are commensurate

# value: vector of predicted values

toweranNA <- function(x,fittedReg,k=1,newx,scaleX=TRUE) 
{
   # k-NN requires NA-free data
   if (sum(is.na(x)) > 0)  
      stop('x must be NA-free; call complete.cases()')
   # k-NN requires numerical data
   factors <- sapply(x,is.factor)  
   if (any(factors)) {
      print('factors present in X data')
      stop('convert using regtools::factorsToDummies()')
   }
   # method cannot predict a data point consisting of all NAs
   allNA <- function(w) all(is.na(w))
   allna <- apply(newx,1,allNA)
   sumAllNA <- sum(allna)
   if (sumAllNA > 0)  {
      print('some rows of newx were all NAs\n:')
      print(which(allna))
   }
   # multiclass Y will have fittedReg as a matrix, otherwise vector
   if (is.matrix(fittedReg) && ncol(fittedReg) == 1) 
      fittedReg <- as.vector(fittedReg)
   multiclass <- is.matrix(fittedReg)
   nc <- ncol(x)
   if (scaleX) {
      x <- scale(x,center=TRUE,scale=TRUE)
      # retain the scaling parameters to use in newx
      xmns <- attr(x,'scaled:center')
      xsds <- attr(x,'scaled:scale')
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
      if (scaleX) {
         # rw <- scale(matrix(rw, nrow=1),center=xmns[ic],scale=xsds[ic])
         rwm <- scale(rwm,center=xmns[ic],scale=xsds[ic])
      }
      if (k == 1) {
         tmp <- pdist(rwm[1,],x[,ic])@dist
         nni <- which.min(tmp)
      } else {
         tmp <- get.knnx(data = x[,ic],query = rwm, k = k)
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

towerLM <- function(x,y,k,newx,useGLM=FALSE,scaleX=FALSE) {
   ccs <- complete.cases(cbind(x,y))
   cat(sum(ccs),' complete cases\n')
   xcc <- x[ccs,]
   ycc <- y[ccs]
   if (!useGLM) lmout <- lm(ycc ~ xcc) else
      lmout <- glm(ycc ~ xcc,family=binomial)
   toweranNA(xcc,lmout$fitted.values,k,newx,scaleX)
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

