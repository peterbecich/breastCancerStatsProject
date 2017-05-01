# this can't be packaged into a function because of a variable scope issue

load("cancer.RData")
source("func.R")

make.shuffle.out<-make.shuffle()
xshuffle<-make.shuffle.out[,2:33]

make.train.out<-make.train(512)
xtrain<-make.train.out[,2:33]
xtrain.predictors<-get.predictors(xtrain)
xtrain.diag<-(get.diag(xtrain))
xtrain.diag.binary<-binary.diag(xtrain.diag)
