# wvs_make_data

# returns nested listed with X_train, y_train, X_test, y_test, by country
# after perfoming a series of tests to ensure crossvalidation is possible
# in spite of the large number of categorical variables and other nuissances.
# if W is provided, PCA is run on W (at the country level, not the world level) 
# and the scores corresponding to 90\% variance are combined with X.
# (W should not contain missing data but is treated as an 'other' category where it occurs.)
# N_min minimum observations per level within country. Observed levels observed less 
# frequently than N_min (in that country) are converted to 'other' for cv and/or pca.
# returns comlete cases...
# examples:
# X <- wvs %>% select(country, birthyear, edu, 
#                    income_bracket,
#                    contains("econ_attitudes"))
# W <- wvs %>% select(region, religion)
# wv <- wvs_make_data(wvs$rightist, X, W)
# colMeans(wv$Argentina$Xy_train)
# colMeans(wv$Argentina$X_test)
# mean(wv$Argetina$y_test, na.rm=TRUE)
# out <- lm(y ~ ., wv$Argentina$Xy_train)
# y_hat_arg <- predict(out, wv$Argentina$X_test)
# MAPE_arg <- mean(abs(unlist(y_hat_arg - wv$Argentina$y_test)))
# # 1.37 MAPE for Argentina on 10 point scale
#
# loop over each country
# MAPE <- data.frame(country = wv$countries)
# MAPE$MAPE <- NA
#for(i in 1:nrow(MAPE)){
#  out <- lm(y ~ ., wv[[wv$countries[i]]][["Xy_train"]])
#  y_hat <- predict(out, wv[[wv$countries[i]]][["X_test"]])
#  MAPE$MAPE[i] <- mean(abs(unlist(y_hat - wv[[wv$countries[i]]][["y_test"]])))
#}
#barplot(MAPE$MAPE, names.arg = MAPE$country, las=2)

wvs_make_data <- function(y, X, W=NULL, noisy=TRUE, N_min=50){
  
  require(dplyr)
  
  W[is.na(W)] <- "other"
  
  stopifnot(all_equal(nrow(X), nrow(W), length(y)))
  X <- as.data.frame(X)
  stopifnot("country" %in% colnames(X))
  
  for(i in 1:ncol(X)){
    if(is.factor(X[,i]))
      X[,i] <- as.character(X[,i])
    if(is.character(X[,i])){
      X[,i] <- gsub(" ", "_", X[,i])
      X[,i] <- gsub(":", "_", X[,i])
    }
      
  }
  
  N <- nrow(wvs)
  N_countries <- n_distinct(X$country)
  countries <- unique(X$country)
  
  all_data <- cbind(X, y)
  if(!is.null(W))
    all_data <- cbind(W, X, y)
    
  # drop countries which were not asked or which only had one level on a particular variable ethnicity
  
  var_names <- colnames(all_data)
  var_names <- var_names[-which(var_names == "country")]
  non_missing_unique <- function(x, dat){
    u <- unique(dat[[x]])
    return(length(u) - (NA %in% u))
  }
  
  for(i in 1:N_countries){
    
    country_data <- filter(all_data, country == countries[i])
    drop_country <- (var_names %>% lapply(non_missing_unique, country_data) %>% unlist %>% min) < 2
    if(drop_country)
      all_data <- filter(all_data, country != countries[i])
    
  }

  rare2other <- function(x, N_min = 50){
    if(is.numeric(x)){
      return(x)
    }else{
      tallies <- table(x)
      rare <- names(tallies)[which(tallies < N_min)]
      return(ifelse(x %in% rare, "_other", x))
    }
  }
  
  
  newW <- all_data[,1:ncol(W)]
  newW <- as.data.frame(lapply(newW, rare2other, N_min), stringsAsFactors = FALSE)
  # pca could be done about here if it made sense too...
  # pca <- prcomp(model.matrix(~., newW))
  
  Xy <- all_data[,-c(1:ncol(W))]
  #Xy <- data.frame(lapply(Xy, rare2other, N_min), stringsAsFactors = FALSE)
  
  if(noisy) print(summary(Xy))
  
  countries <- unique(Xy$country)
  N_countries <- length(countries)

  out <- list()
  out[["countries"]] <- countries

  
  for(i in 1:N_countries){
    
    if(noisy) cat("preparing data for ", countries[i], "\n")
    
    W_country <- filter(newW, Xy$country == countries[i])
    pca <- prcomp(model.matrix(~., W_country)[,-1], center = TRUE, scale. = TRUE)
    keepers <- which(summary(pca)[["importance"]][3,] < 0.9)
    N_country <- nrow(W_country)
    
    test <- sample(c(FALSE, TRUE), N_country, 
                   replace = TRUE, prob = c(.9, .1))
    
    Xy_country <- filter(Xy, country == countries[i]) %>% select(-country)

    Xy_country <- cbind(pca$x[,keepers], Xy_country)
    
    Xy_test <- Xy_country[test,]
    Xy_train <- Xy_country[!test,]
    Xy_train <- data.frame(lapply(Xy_train, rare2other, N_min), stringsAsFactors = FALSE)
    
    out[[countries[i]]][["Xy_train"]] <- model.matrix(~ ., Xy_train, na.action=na.pass)[,-1] %>% as.data.frame(stringsAsFactors=FALSE)
    
    X_test <- model.matrix(y ~ ., Xy_test, na.action=na.pass)[,-1] %>% as.data.frame(stringsAsFactors=FALSE)
    #browser()
    training_features <- colnames(out[[countries[i]]][["Xy_train"]])[-ncol(out[[countries[i]]][["Xy_train"]])]
    MIA <- setdiff(training_features, colnames(X_test))
    X_test_MIA <- matrix(0, nrow(X_test), length(MIA))
    colnames(X_test_MIA) <- MIA
    X_test <- cbind(X_test, X_test_MIA)
    out[[countries[i]]][["X_test"]] <- X_test
    
    out[[countries[i]]][["y_test"]] <- Xy_test %>% filter(!is.na(y)) %>% select(y)
    if(mean(is.na(out[[countries[i]]][["y_test"]])))
      cat("y_test missing for", countries[[i]])
  }
  return(out)
}


