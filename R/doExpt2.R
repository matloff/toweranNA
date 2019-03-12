# experiments where the missingess is correlated.
# Data input option 1:
# a list containing Xy_train, X_test, and y_test
# k is the number of nearest neighbors for toweranNA.
# Data input option 2:
# Xy, a data.frame, contains complete cases only. 
# y, the dependent variable, must be in the final column.
# 500 test samples are held out (or 20\% on small data sets).
# X is assumed multivariate normal. N_test draws are taken.
# That creates a N_test * P matrix; if the absolute value
# is greater than threshold (elementwise, not a norm for the vector)
# it is assigned to be missing, inducing correlation.
# threshold is 1.64 by default.

doExpt2 <- function(data_list=NULL, Xy=NULL, k=5, threshold = 1.64){
  
  if(is.null(Xy) == is.null(data_list))
    stop("Please provide Xy or data_list but not both.")
  
  require(mice)
  require(mvtnorm)
  
  display <- function(x) cat("\t\telapsed time:", x[3], "\n")
  
  if(is.null(data_list)){
    
    if(sum(is.na(Xy))) 
      stop("Xy may not contain missing values, which are simulated by this function.")
    
    Xy <- as.data.frame(Xy)
    y_name <- colnames(Xy)[ncol(Xy)]
    X <- model.matrix(as.formula(paste(y_name, "~.")), Xy)
    X <- X[,-1] # drop intercept
    y <- Xy[,ncol(Xy)]
    mmXy <- as.data.frame(cbind(X, y))
    
    nr <- nrow(Xy)
    nc <- ncol(Xy) - 1
    idxs <- sample(nr, ifelse(nr < 500, floor(.2*nr), 500))
    y_test <- y[idxs]
    X_train <- X[-idxs,]
    
    lmo <- lm(y ~ ., data=mmXy[-idxs,])
    ftd <- lmo$fitted.values
    
    newx <- X[idxs, ]
    newx.full <- newx  # full data, no NAs
    pred.full <- predict(lmo, as.data.frame(newx.full))
    
    # missing data only to newx
    sims <- rmvnorm(length(idxs), mean = rep(0, ncol(X)), sigma = cor(X))
    to_drop <- abs(sims) > threshold
    newx[to_drop] <- NA
    
    cat("\n\tcalling toweranNA...\n")
    acc.tower <- NA
    tried <- try(tower.time <- system.time(pred.tower <- toweranNA(X_train, 
                                                           ftd, k, newx)))
    if(inherits(tried, "try-error")){
      cat("\n\nError attempting to call toweranNA. details\n\n")
      print(tried)
    }else{
      display(tower.time)
      acc.tower <- mean(abs(pred.tower - y_test))
    }
    
    # now prepare for using mice; combine the training X data with newx,
    # yielding the original X data in newPE except for the insertion of
    # NAs; the training data is then helping mice fill in the NAs
    newX2 <- rbind(X[-idxs,], newx)
    cat("\n\tcalling mice...\n")
    mice.time <- system.time(miceout <- mice(newX2, m=1, maxit=50, 
                                             meth="pmm", printFlag=FALSE))
    display(mice.time)
    
    newX3 <- complete(miceout)
    # now extract 
    newx.mice <- newX3[idxs, ]
    pred.mice <- predict(lmo, newx.mice)
    
    acc.full <- mean(abs(pred.full - y_test))
    acc.mice <- mean(abs(pred.mice - y_test))

  }else{
    
    `%out%` <- function(x, table) match(x, table, nomatch = 0L) == 0L
    
    if("Xy_train" %out% names(data_list))
      stop("Xy_train not found in data_list")
    if("X_test" %out% names(data_list))
      stop("Xy_train not found in data_list")
    if("y_test" %out% names(data_list))
      stop("Xy_train not found in data_list")
    
    y_col <- ncol(data_list$Xy_train)
    
    if(sum(colnames(data_list$Xy_train[,-y_col]) %out% colnames(data_list$X_test)))
      stop("Features used in training not found in test data.")
    
    nr <- nrow(data_list$Xy_train) + nrow(data_list$X_test)
    nc <- ncol(data_list$Xy_train) - 1
    nt <- nrow(data_list$X_test)
    
    newx <- as.matrix(data_list$X_test)
    newx <- newx[,-which(colnames(data_list$X_test) %out% colnames(data_list$Xy_train[,-y_col]))]
    newx.full <- newx  # full data, no NAs
    y_test <- unlist(data_list$y_test)
    
    # mice() doesn't seem to like specials in col names...
    colnames(data_list$Xy_train)[-y_col] <- paste0("x", 1:nc)
    colnames(newx.full) <- colnames(newx) <- paste0("x", 1:nc)
    
    lmo <- lm(y ~ ., data=data_list$Xy_train)
    ftd <- lmo$fitted.values
    pred.full <- predict(lmo, as.data.frame(newx.full))
    
    sims <- rmvnorm(nt, mean = rep(0, nc), sigma = cor(data_list$Xy_train[,-y_col]))
    to_drop <- abs(sims) > threshold
    newx[to_drop] <- NA
    
    cat("\n\tcalling toweranNA...\n")
    acc.tower <- NA
    
    tried <- try(tower.time <- system.time(pred.tower <- toweranNA(data_list$Xy_train[,-y_col], 
                                                                   ftd, k, newx)))
    
    if(inherits(tried, "try-error")){
      cat("\n\nError attempting to call toweranNA. details\n\n")
      print(tried)
    }else{
      display(tower.time)
      acc.tower <- mean(abs(pred.tower - y_test))
    }
    
    cat("\n\tcalling mice...\n")
    newX2 <- rbind(data_list$Xy_train[,-y_col], newx)
    mice.time <- system.time(miceout <- mice(newX2, m=1, maxit=50, 
                                meth="pmm", printFlag=FALSE))
    display(mice.time)
    newX3 <- complete(miceout)
    # now extract 
    newx.mice <- newX3[-c(1:nrow(data_list$Xy_train)), ]
    pred.mice <- predict(lmo, newx.mice)
    
    acc.full <- mean(abs(pred.full - y_test))
    acc.mice <- mean(abs(pred.mice - y_test))
    
  }
  out <- list(acc.tower=acc.tower, acc.full=acc.full, acc.mice=acc.mice, 
              tower.time=tower.time[3], mice.time=mice.time[3])
  cat("\n\n\t\tMAPE (sorted):\n")
  print(sort(unlist(out)[1:3]))
  return(out)
  
}