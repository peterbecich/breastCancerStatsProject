hourstorun<-0.1
chosensd<-20
source("func.R")
source("mod.R")
source("confuse.R")
load("cancer.RData")
make.shuffle.out<-make.shuffle()
xshuffle<-make.shuffle.out[,2:33]

runtime<-0
starttime<-as.numeric(Sys.time())
timepassed<-0
tenth.length<-floor(dim(xshuffle)[1]/10)

test.boundaries<-(0:10)*tenth.length
## just an example
xtrain<-xshuffle[-((test.boundaries[1]+1):(test.boundaries[2]+1)),]
xtrain.predictors<-get.predictors(xtrain)
xtrain.diag.binary<-get.diag.binary(xtrain)
print(paste("names of predictors: "))
print(paste(names(xtrain.predictors)))
print(paste("dimensions of predictors: ", I(dim(xtrain.predictors))))
print(paste("length of diag.binary: ", length(xtrain.diag.binary)))


print(paste(" -------------------------- "))
print(paste("picking random predictors to test"))
print(paste(" -------------------------- "))
mat<-brute.force.search(xtrain.diag.binary,xtrain.predictors,hourstorun/240,choosesd=chosensd)
print(paste(" -------------------------- "))
print(paste("cross-validating"))
print(paste(" -------------------------- "))



print("test boundaries: ")
print(test.boundaries)
xtest.diag.binary.cumulative<-c()
xtest.diag.predict.thresh.cumulative<-c()
model.deviance<-c()


for(j in 1:dim(mat)[1]){
predictor.length<-which(mat[j,]==0)-1

print(paste("predictor set: ",j,"/",dim(mat)[1]))
print("predictors: ")
print(mat[j,])
xtrain.confuse.cumulative<-matrix(0,2,2)
xtest.confuse.cumulative<-matrix(0,2,2)
startten<-as.numeric(Sys.time())
for(i in 1:10){
#	print(paste("j: ",j,"/",dim(mat)[1]," i: ",i))
	upper.bound<-test.boundaries[i+1]
	lower.bound<-test.boundaries[i]+1
	#print(paste("boundaries: ",lower.bound,", ",upper.bound))
	xtest<-xshuffle[(lower.bound:upper.bound),]
	xtrain<-xshuffle[-(lower.bound:upper.bound),]
	overlap<-sample.patient.match(xtest, xtrain)
	if(!is.null(overlap)){
		print("xtest and xtrain overlap")}
#	print(paste("length of xtest: ",dim(xtest)[1],"  |  length of xtrain: ",dim(xtrain)[1]))
	## predict train
	xtrain.predictors<-get.predictors(xtrain)
	xtrain.diag.binary<-get.diag.binary(xtrain)
	#print("names(xtrain.predictors)")
	#print(names(xtrain.predictors))
	#mylogit<-buildMod(predict.in=xtrain.predictors, diag.in=xtrain.diag.binary)
	#print("predictor indices: ")
	#print(mat[j,1:predictor.length])
	####### BUILDING MODEL
	mylogit<-buildMod.by.index(predict.in=xtrain.predictors, diag.in=xtrain.diag.binary, predict.indices=mat[j,1:predictor.length])
	model.deviance<-c(model.deviance, mylogit$deviance)
	xtrain.diag.predict<-narrow(predictMod(xtrain.predictors))
	xtrain.diag.predict.thresh<-thresh(xtrain.diag.predict)
	## predict test
	xtest.predictors<-get.predictors(xtest)
	xtest.diag.predict<-narrow(predictMod(xtest.predictors))
	xtest.diag.predict.thresh<-thresh(xtest.diag.predict)
	xtest.diag.predict.thresh.cumulative<-c(xtest.diag.predict.thresh.cumulative,xtest.diag.predict.thresh)

	xtest.diag.binary<-get.diag.binary(xtest)
	xtest.diag.binary.cumulative<-c(xtest.diag.binary.cumulative,xtest.diag.binary)
	#print("making train confusion matrix")
	xtrain.confuse<-make.confuse(diag.predict=xtrain.diag.predict.thresh, diag.real=xtrain.diag.binary)
	#print("making test confusion matrix")
	xtest.confuse<-make.confuse(diag.predict=xtest.diag.predict.thresh, diag.real=xtest.diag.binary)
	
	xtrain.confuse.cumulative<-xtrain.confuse.cumulative+xtrain.confuse
	xtest.confuse.cumulative<-xtest.confuse.cumulative+xtest.confuse

}
endten<-as.numeric(Sys.time())
print("xtrain.confuse.cumulative")
print(xtrain.confuse.cumulative)
print(paste(round(100*confuse.correct(xtrain.confuse.cumulative),2),"% correct"))
print("xtest.confuse.cumulative")
print(xtest.confuse.cumulative)
print(paste(round(100*confuse.correct(xtest.confuse.cumulative),2),"% correct"))
print(paste("sum of xtest.confuse.cumulative: ",sum(xtest.confuse.cumulative),"  |  sum of xtrain.confuse.cumulative: ",sum(xtrain.confuse.cumulative)))


mat[j,31:36]<-c(round(100*confuse.correct(xtrain.confuse.cumulative),2),round(100*confuse.correct(xtest.confuse.cumulative),2),sum(xtrain.confuse.cumulative),sum(xtest.confuse.cumulative),xtrain.confuse.cumulative[2,1],xtest.confuse.cumulative[2,1])
####

print(paste("time elapsed (minutes): ", round((as.numeric(Sys.time())-starttime)[1]/60),1))
togo<-round(((dim(mat)[1]-j)/j)*round(((as.numeric(Sys.time())-starttime)/60),1),1)
print(paste("time to go (hours): ", round(togo/60,2)))
print(paste("time to go (days): ", round(togo/(60*24),2)))
print(" ================================== ")
}
###
#png(file=paste('model_deviances_hist','.png',sep=''),pointsize=16,width=640,height=480)
#hist(model.deviance,main='')
#title(main="Model deviances")
#dev.off()

#png(file=paste('real_vs_predicted','.png',sep=''),pointsize=16,width=640,height=480)
#xtest.diag.binary.cumulative.sort<-sort(xtest.diag.binary.cumulative)
#last0<-max(which(xtest.diag.binary.cumulative.sort==0))
#xtest.length<-length(xtest.diag.binary.cumulative.sort)
#plot(xtest.diag.binary.cumulative.sort,type='line',yaxt='n',ylim=c(-0.2,1.2))
#plot(xtest.diag.binary.cumulative.sort[1:last0],type='line',ylab='',xlim=c(0,xtest.length),yaxt='n',ylim=c(-0.2,1.2))
#points(x=last0+1:xtest.length,y=xtest.diag.binary.cumulative.sort[last0+1:xtest.length],type='line')
#axis(2, at=0:1, labels=c("Benign","Malignant"))
#points(xtest.diag.predict.thresh.cumulative[order(xtest.diag.binary.cumulative)],type='line',col='blue')

#xtest.predict.sortbydiag<-xtest.diag.predict.thresh.cumulative[order(xtest.diag.binary.cumulative)]
#for(p in 1:xtest.length){
#	if(xtest.predict.sortbydiag[p]!=sort(xtest.diag.binary.cumulative)[p]){
#		points(y=xtest.predict.sortbydiag[p],x=p,col='red')}
#}
#legend("topleft", inset=.05,bty='n',
#   c("sorted diagnoses",paste("predict. err.:",(xtest.confuse.cumulative[1,2]+xtest.confuse.cumulative[2,1]))), fill=c('black','red'), horiz=TRUE)
#title("cumulative tests")
#dev.off()
mat<-mat[order(mat[,36]),]
colnames(mat)<-c(1:30,'xtrain.percent','xtest.percent','xtrain.sum','xtest.sum','xtrain.confuse-FN','xtest.confuse-FN')
#print(mat[,31:34])

if(length(mat[which((mat[,31]==max(mat[,31]))&(mat[,35:36]==0)),])>0){
	print(paste("max by xtrain; xtrain FN & xtest FN == 0"))
	print(mat[which((mat[,31]==max(mat[,31]))&(mat[,35:36]==0)),])
	print(paste("max by xtest; xtrain FN & xtest FN == 0"))
	print(mat[which((mat[,31]==max(mat[,31]))&(mat[,35:36]==0)),])
	write.csv(mat[which((mat[,31]==max(mat[,31]))&(mat[,35:36]==0)),], file=paste('cancer_mat_0FN_xtestsort_',format(Sys.time(),"%H:%M"),'.csv',sep=''))
	write.csv(mat[which((mat[,32]==max(mat[,32]))&(mat[,35:36]==0)),], file=paste('cancer_mat_0FN_xtrainsort_',format(Sys.time(),"%H:%M"),'.csv',sep=''))
}else{
	print("************ no tests with FN == 0 *****************")}
print(paste("max by xtrain"))
print(mat[which(mat[,31]==max(mat[,31])),])
print(paste("max by xtest"))
print(mat[which(mat[,32]==max(mat[,32])),])
write.csv(mat, file=paste('cancer_mat_',format(Sys.time(),"%H:%M"),'.csv',sep=''))

#rm(list=ls(all=TRUE))


