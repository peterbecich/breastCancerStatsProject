source("func.R")
source("mod.R")
source("confuse.R")
load("cancer.RData")
source("sort_plots.R")
#install.packages('verification',dependencies=TRUE)
#library('verification')
single.mod.main.repetitive<-function(best,runs){
	pstart<-0.0
	pfinish<-0.1
	pinc<-0.002
	info.mat<-matrix(0,length(seq(pstart,pfinish,pinc)),7)
	pcount<-1
	for(p in seq(pstart,pfinish,pinc)){	
		run.info<-c(0,0,0,0)
		print(paste("threshold p: ",p," / ",pfinish))
		for(i in 1:runs){
			#print(paste("run: ",i))
			source("sort_plots.R")
			run.info<-run.info+single.mod.main(best,0,1,p,1)}
		print(paste("run.FNs (xtrain,xtest): ",run.info[1:2]))
		print(paste("run.FPs (xtrain,xtest): ",run.info[3:4]))
		print(paste("run.pcts.ave (xtrain,xtest): ",run.info[5:6]/runs))
		print(pcount)
		#info.mat[((p-pstart)/pinc)+1,1:4]<-run.info
		#info.mat[((p-pstart)/pinc)+1,5]<-p
		info.mat[pcount,1:6]<-c(run.info[1:4],run.info[5:6]/runs)
		info.mat[pcount,7]<-p
		pcount<-pcount+1
}
	colnames(info.mat)<-c("run.FPs (xtrain)","run.FPs (xtest)","run.FNs (xtrain)","run.FNs (xtest)","run.pcts.ave (xtrain)","run.pcts.ave (xtest)","p")
	return(info.mat)
}
single.mod.main<-function(best,verbose,thresh.alt,p,confuse.data.out){
	make.shuffle.out<-make.shuffle()
	xshuffle<-make.shuffle.out[,2:33]
	tenth.length<-floor(dim(xshuffle)[1]/10)
	test.boundaries<-(0:10)*tenth.length
	if(verbose==TRUE){
	print(paste(" -------------------------- "))
	print(paste("cross-validating"))
	print(paste(" -------------------------- "))}
	xtrain.confuse.cumulative<-matrix(0,2,2)
	xtest.confuse.cumulative<-matrix(0,2,2)
	if(verbose==TRUE){
	print("test boundaries: ")
	print(test.boundaries)}
	## real diagnosis
	xtest.diag.binary.cumulative<-c()
	## predicted diagnosis before thresholding
	#xtest.diag.predict.cumulative<-0*c(1:560)
	xtest.diag.predict.cumulative<-c()
	## predicted diagnosis
	xtest.diag.predict.thresh.cumulative<-c()
	model.deviance<-c()
	for(i in 1:10){
		if(verbose==TRUE){
		print(paste("run: ",i))}
		upper.bound<-test.boundaries[i+1]
		lower.bound<-test.boundaries[i]+1
		if(verbose==TRUE){
		print(paste("boundaries: ",lower.bound,", ",upper.bound))}
		xtest<-xshuffle[(lower.bound:upper.bound),]
		xtrain<-xshuffle[-(lower.bound:upper.bound),]
		overlap<-sample.patient.match(xtest, xtrain)
		if(!is.null(overlap)){
			print("xtest and xtrain overlap")}
		## xtrain predictors and real diag
		xtrain.predictors<-get.predictors(xtrain)
		xtrain.diag.binary<-get.diag.binary(xtrain)
		##### fitting model	
		mylogit<-buildMod.by.index(diag.in=xtrain.diag.binary,predict.in=xtrain.predictors,predict.indices=best)
		model.deviance<-c(model.deviance, mylogit$deviance)
		## predicting xtrain
		xtrain.diag.predict<-narrow(predict(mylogit,newdata=xtrain.predictors))
		if(thresh.alt==0)
			xtrain.diag.predict.thresh<-thresh(xtrain.diag.predict)
		else
			xtrain.diag.predict.thresh<-thresh.alt(xtrain.diag.predict,p)
		# -------------------------------------
		## xtest predictors and real diag
		xtest.predictors<-get.predictors(xtest)
		xtest.diag.binary<-get.diag.binary(xtest)
		xtest.diag.binary.cumulative<-c(xtest.diag.binary.cumulative,xtest.diag.binary)
		## predicting xtest
		xtest.diag.predict<-narrow(predict(mylogit,newdata=xtest.predictors))
		if(thresh.alt==0)
			xtest.diag.predict.thresh<-thresh(xtest.diag.predict)
		else
			xtest.diag.predict.thresh<-thresh.alt(xtest.diag.predict,p)
		### for ROC
		xtest.diag.predict.cumulative<-c(xtest.diag.predict.cumulative,xtest.diag.predict)
		### for cumulative confusion matrix
		xtest.diag.predict.thresh.cumulative<-c(xtest.diag.predict.thresh.cumulative,xtest.diag.predict.thresh)
		xtrain.confuse<-make.confuse(diag.predict=xtrain.diag.predict.thresh, diag.real=xtrain.diag.binary)
		xtest.confuse<-make.confuse(diag.predict=xtest.diag.predict.thresh, diag.real=xtest.diag.binary)
		
		if(verbose==TRUE){
			print(paste("sum of xtest.confuse: ",sum(xtest.confuse),"  |  sum of xtrain.confuse: ",sum(xtrain.confuse)))}
		#####
		xtrain.confuse.cumulative<-xtrain.confuse.cumulative+xtrain.confuse
		xtest.confuse.cumulative<-xtest.confuse.cumulative+xtest.confuse

	}
	if(verbose==1){
	print(" ================================== ")
	print("xtrain.confuse.cumulative")
	print(xtrain.confuse.cumulative)
	print(paste(round(100*confuse.correct(xtrain.confuse.cumulative),2),"% correct"))
	print("xtest.confuse.cumulative")
	print(xtest.confuse.cumulative)
	print(paste(round(100*confuse.correct(xtest.confuse.cumulative),2),"% correct"))
	print(paste("sum of xtest.confuse.cumulative: ",sum(xtest.confuse.cumulative),"  |  sum of xtrain.confuse.cumulative: ",sum(xtrain.confuse.cumulative)))
	}
	xtrain.pct<-(round(100*confuse.correct(xtrain.confuse.cumulative),2))
	xtest.pct<-(round(100*confuse.correct(xtest.confuse.cumulative),2))
	png(file=paste('model_deviances_hist','.png',sep=''),pointsize=16,width=640,height=480)
	hist(model.deviance,main='')
	title(main="Model deviances")
	dev.off()

	png(file=paste('real_vs_predicted','.png',sep=''),pointsize=16,width=640,height=480)
	xtest.diag.binary.cumulative.sort<-sort(xtest.diag.binary.cumulative)
	last0<-max(which(xtest.diag.binary.cumulative.sort==0))
	xtest.length<-length(xtest.diag.binary.cumulative.sort)
	#plot(xtest.diag.binary.cumulative.sort,type='line',yaxt='n',ylim=c(-0.2,1.2))
	plot(xtest.diag.binary.cumulative.sort[1:last0],type='line',ylab='',xlim=c(0,xtest.length),yaxt='n',ylim=c(-0.2,1.2))
	points(x=last0+1:xtest.length,y=xtest.diag.binary.cumulative.sort[last0+1:xtest.length],type='line')
	axis(2, at=0:1, labels=c("Benign","Malignant"))
	#points(xtest.diag.predict.thresh.cumulative[order(xtest.diag.binary.cumulative)],type='line',col='blue')

	xtest.predict.sortbydiag<-xtest.diag.predict.thresh.cumulative[order(xtest.diag.binary.cumulative)]
	for(p in 1:xtest.length){
		if(xtest.predict.sortbydiag[p]!=sort(xtest.diag.binary.cumulative)[p]){
			points(y=xtest.predict.sortbydiag[p],x=p,col='red')}
	}
	legend("topleft", inset=.05,bty='n',
	   c("sorted diagnoses",paste("predict. err.:",(xtest.confuse.cumulative[1,2]+xtest.confuse.cumulative[2,1]))), fill=c('black','red'), horiz=TRUE)
	title("cumulative tests")
	dev.off()
	if(confuse.data.out==0){
		print(length(xtest.diag.predict.cumulative))
		print(length(xtest.diag.binary.cumulative))
		return(cbind(xtest.diag.predict.cumulative,xtest.diag.binary.cumulative))}
	else
		return(cbind(xtrain.confuse.cumulative[1,2],xtest.confuse.cumulative[1,2],xtrain.confuse.cumulative[2,1],xtest.confuse.cumulative[2,1],xtrain.pct,xtest.pct))

}
