

########################  countnas()  ###############################

# see how many NAs in each column of df; mvchar is the character
# indicating missingness, e.g. '?', if not coded NA

countnas <- function(df,mvchar=NULL)  {
   if (is.null(mvchar)) 
      sapply(df,function(cl) sum(is.na(cl)))
   else
      sapply(df,function(cl) sum(cl == mvchar))
}

########################  checkConstCols()  ###############################

# report whether any columns in data frame are constant, including the
# case of constant value plus some NAs

checkConstCols <- function(df) 
{
   sapply(df,checkConstCol)
}

# 1-column version
checkConstCol <- function(cl) 
{
   nuniq <- length(unique(cl))
   if (nuniq == 1) return(TRUE)
   if (nuniq == 2 && sum(is.na(cl)) > 0) return(TRUE)
   return(FALSE)
}

########################  convertToDumms()  ###############################

# takes the factor xf with k levels excl. 'na', and converts it to a
# data frame of k columns, one for each non-'na' level, with labels of
# the form u.v, u being the original col name and v being the level

convertToDumms <- function(xfr,lvls,xfrname) 
{
   require(dummies)
   tmp <- dummy(xfr,sep='.')
   tmp[['xfr.na']] <- NULL
   names(tmp) <- gsub('xfr.','',names(tmp))
   lvls1 <- lvls[lvls != 'na']
   tmp[,lvls1]
}

