
g <- function(a) 
{
   y <- NULL
   nr <- nrow(a); nc <- ncol(a)
   for (i in 2:(nr-1)) {
      for (j in 2:(nc-1)) {
         y <- c(y,a[i,j])
      }
   }
}

