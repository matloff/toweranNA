
e1 <- read.csv('~/Research/DataSets/WordBank/English.csv',header=T)
e1 <- e1[,c(2,5:8,10)]
e2 <- factorsToDummies(e1)
doGenExpt(e2,holdout=250,imput='amelia') 

