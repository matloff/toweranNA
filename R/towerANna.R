
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
   allna <- apply(newx,1,allNA)
   sumAllNA <- sum(allna)
   someAllNA <- sumAllNA > 0
   if (someAllNA)  {
      cat(sumAllNA,' rows of newx were all NAs\n')
      newx <- newx[!allna,]
   }
   # if (sum(apply(newx,1,allNA) > 0)) 
   #    stop('newx has a row of all NAs')
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
         rwm <- scale(rwm,center=xmns[ic],scale=xsds[ic])
      }
      tmp <- FNN::get.knnx(data = x[,ic],query = rwm, k = k)
      nni <- tmp$nn.index
      if (!multiclass) {
         preds[i] <- mean(fittedReg[nni])
      } else {
         preds[i,] <- colMeans(fittedReg[nni,])
      }
   }
   if (someAllNA) {   
      altPreds <- rep(NA,length(allna))
      altPreds[!allna] <- preds
      preds <- altPreds
   }
   preds
}

###########################  towerLM()  ###############################

# wrapper for toweranNA() in lm() case; here x is a data matrix of X, y
# is Y; other args as in toweranNA()

towerLM <- function(x,y,k,newx,scaleX=FALSE) {
   ccs <- complete.cases(cbind(x,y))
   cat(sum(ccs),' complete cases\n')
   xcc <- x[ccs,]
   ycc <- y[ccs]
   lmout <- lm(ycc ~ xcc)
   toweranNA(xcc,lmout$fitted.values,k,newx,scaleX)
}

