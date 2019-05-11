

########################  countnas()  ###############################

# see how many NAs in each column of dfr; mvchar is the character
# indicating missingness, e.g. '?', if not coded NA

countnas <- function(dfr,mvchar=NULL)  {
   if (is.null(mvchar)) 
      sapply(dfr,function(cl) sum(is.na(cl)))
   else
      sapply(dfr,function(cl) sum(cl == mvchar))
}

########################  checkConstCols()  ###############################

# report whether any columns in data frame are constant, including the
# case of constant value plus some NAs

checkConstCols <- function(dfr) 
{
   sapply(dfr,checkConstCol)
}

# 1-column version
checkConstCol <- function(cl) 
{
   nuniq <- length(unique(cl))
   if (nuniq == 1) return(TRUE)
   if (nuniq == 2 && sum(is.na(cl)) > 0) return(TRUE)
   return(FALSE)
}

########################  factorToDumms()  ###############################

# converts the factor x to a matrix, via dummy(), but properly handling
# NAs; nm is an optional col name

factorToDumms <- function(x,nm=NULL)
{
   require(dummies)
   tmp <- dummy(x,sep='.')
   if (any(is.na(x))) {
      # need to make NA all dummies corresponding to an NA
      # get name of x
      xname <- as.character(sys.call(1))[2]
      if (is.null(nm)) nm <- xname
      # get name of the NA column
      nacolnm <- paste0(xname,'.NA')
      # which col is it?
      nacol <- which(colnames(tmp) == nacolnm)
      # jettison it
      tmp <- tmp[,-nacol]
      # where are the NAs?
      nas <- which(is.na(x))
      # properly make those rows NAs
      tmp[nas,] <- NA
   }
   colnames(tmp) <- paste0(nm,'_',levels(x))
   tmp
}

######################## dfrFactorsToDumms()  ###############################

# for each factor in dfr, replace by dummies; if omitLast, then the last
# column of each set of dummies will be omitted; calls factorToDumms()

dfrFactorsToDumms <- function(dfr,omitLast=FALSE) 
{
   if (!is.data.frame(dfr)) stop('dfr must be a data frame (or data.table)')
   browser()
   toNull <- NULL
   for (i in 1:ncol(dfr)) {
      coli <- dfr[,i]
      if (is.factor(coli)) {
         dumms <- factorToDumms(coli,names(dfr)[i])
         if (omitLast) dumms <- dumms[,-ncol(dumms)]
         toNull <- c(toNull,i)
         dfr <- cbind(dfr,dumms)
      }
   }
   dfr[,toNull] <- NULL
   dfr
}
