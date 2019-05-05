
# for now, don't split into training, test sets

doExpt <- function() 
{
   load('nh4')
   x <- TStoX(nh4,10)

   # fit to the complete cases
   y <- x[,11]  
   x <- x[,-11]
   ccrows <- which(complete.cases(x))
   allnarows <- which(apply(x,1,function(rw) all(is.na(rw))))
   
   xcc <- x[ccrows,]
   ycc <- y[ccrows]
   lmout <- lm(y ~ x)

   # predict on the incomplete (wrt X) cases; but must skip those that
   # are totally incomplete
   allnarows <- which(apply(x,1,function(rw) all(is.na(rw))))
   use <- setdiff(1:nrow(x),union(ccrows,allnarows))
   browser()
   xncc <- x[use,]
   yncc <- y[use]
   towerout <- toweranNA(xcc,lmout$fitted.values,5,xncc,scaleX=FALSE)

}

