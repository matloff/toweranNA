

########################  countnas()  ###############################

# see how many NAs in each column of dfr; counts are reported unles prop
# is TRUE, in which case proportions of NAs are shown

countnas <- function(dfr,prop=FALSE)  {
   countFtn <- function(cl) 
   {
      tmp <- sum(is.na(cl))
      if (prop) tmp <- tmp / length(cl)
      tmp
   }
   sapply(dfr,countFtn)
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

