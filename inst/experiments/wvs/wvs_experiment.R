
# uncomment and run. first line only needed once. paths may need updating...
# source("experiments/wvs/wvs_raw_to_csv.R")
# wvs <- read.csv("experiments/wvs.csv", stringsAsFactors = FALSE)
# source("experiments/wvs/wvs_csv_to_country_data_frames.R")

X <- wvs %>% select(country, birthyear, edu, income_bracket,
                    contains("econ_attitudes"))
W <- wvs %>% select(region, religion)
wv <- wvs_make_data(wvs$rightist, X, W)

# countries included
wv$countries
# inspect data dimensions by country
lapply(wv, function(country) lapply(country, dim))

# doExpt2 for all countries, found in 'wv' (assumed formatted as above)
# the `...` passes additional arguments to doExpt2()
# for example, 
# batch_Expt2(12345, wv, k = 7) 
batch_Expt2 <- function(seed, wv, ...){
  
  N_countries <- length(wv$countries)
  tmp <- matrix(nrow=N_countries, ncol=5)
  dimnames(tmp) <- list(wv$countries, c("tower", "full", "mice", "tower.time", "mice.time"))
  results <- data.frame(tmp)
  cat("seed:", seed, "\n")
  set.seed(seed)
  for(i in 1:length(wv$countries)){
    
    cat("starting:", wv$countries[i], "\n\n")
    
    tried <- try(out <- doExpt2(wv[[wv$countries[i]]]), ...) # could do parLapply(wv$countries, doExpt2)
    if(!inherits(tried, "try-error")){
      
      results[i, ] <- out
      cat("\nfinished:", wv$countries[[i]], "\n\n")
      
    }else{
      
      cat("\n something went wrong with ", wv$countries[[i]], "\n")
      print(tried)
      
    }
    
  } 
  
  results$seed <- wv$seed
  results$metric <- "MAPE"
  results$tower_better <- results$tower < results$mice
  
  cat("countries completed without error:", sum(!is.na(results$tower)), "out of ", length(wv$countries))
  cat("countries which encountered a problem:", wv$countries[is.na(results$tower)], "\n")
  print(colMeans(results[,1:3], na.rm=TRUE))
  cat("\n")
  print(table(results$tower_better))
  
  return(results)
  
}

batch <- list()
seeds <- 1:25
for(i in seeds){
  batch[[i]] <- batch_Expt2(i, wv)
  save(batch, file = "batch.Rdata")
}

results <- do.call(rbind, batch)
results$country <- gsub("[[:digit:]]", "", rownames(results))
rownames(results) <- paste0(results$country, "_", results$seed)

write.csv(results, file="wvs_results.csv", row.names = FALSE)






