

# routines to apply Tower Method to time series

########################## TStoX() #####################################

# inputs a time series of length m, converts to an n X (p+1) matrix, where p
# is the lag of a presumed autoregressive/moving average model and 
# n = m - p + 1

# when input to toweranNA(), cols 1:p will play the role of X, and col p
# will serve as Y

# Y will consist of x_{p+1},x_{p+2},...,x_n

TStoX <- function(x,lag) 
{
   lx <- length(x)
   lag <- lag + 1
   lxl <- lx - lag + 1
   lxl2 <- lxl + 1
   mt <- cbind(x[-(lxl2:lx)],1:lxl)
   onerow <- function(mtrow) {
      i <- mtrow[2]
      s <- i
      e <- i + lag - 1
      x[s:e]
   }
   t(apply(mt,1,onerow))
}

########################## tsTower() #####################################

# wrapper for toweranNA() for time series

# forms matrix version of x, runs toweranNA(), then prepends x_1,
# x_2,...,x_p to the predicted values 

tsTower <- function(x,lag,k) {
   xy <- TStoX(x,lag)
   nc <- ncol(xy)
   x <- xy[,-nc]
   y <- xy[,nc]
   # for now, lm()
   # for now, just predict same X, some with NAs
   z <- towerLM(x,y,k,x)
   c(x[1:(nc-1)],z)
}



