
# for now, don't split into training, test sets

doExpt <- function() 
{
   load('nh4')
   x <- TStoX(nh4,10)

   # fit to the complete cases
   y <- x[,11]  
   x <- x[,-11]
   ccrows <- which(complete.cases(x))
   xcc <- x[ccrows,]
   ycc <- y[ccrows]
   lmout <- lm(y ~ x)

   # predict on the incomplete (wrt X) cases
   xncc <- x[-ccrows,]
   checkallna <- apply(xncc,1,function(rw) all(is.na(rw)))
   ccrows <- setdiff(ccrows,checkallna)
   xncc <- x[-ccrows,]
   yncc <- y[-ccrows]
   browser()
   towerout <- toweranNA(xcc,lmout$fitted.values,5,xncc,scaleX=FALSE)

}

