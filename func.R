

## make.shuffle
## make.train(train_size)
## make.test
## perimeter.ratio -- calculate ratio of real average cell perimeter,
##	over ideal average cell perimeter (calculated from average cell radius)
## diagnosis.ratio -- return ratio of maligant tumors over benign tumors; input
##	is whatever is attached
## trial.train(train_size) -- 20K trials: calculates average smoothness,
## 	perimeter ratio, and diagnosis ratio for each of the 20K trials;
#	then makes histograms
## binary.diag  -- convert character diagnosis into binary; 1 is malignant
## thresh(predicted)
## adj.thresh

## get.predictors
## get.diag
## get.diag.binary


## plot.sorted(name1) -- plot one class of predictors, sorted by their mean predictor
## plot.all.sorted -- plot all predictors sorted by their own mean predictor
##		(3 radius predictors plotted by sorted mean radius)
## plot.all.one.sorted(name1) -- plot all predictors sorted by argument's mean 
## predictor (3 radius predictors plotted by sorted mean radius)
## plot.all.sorted.by.all -- plot all combinations of sortings; see which predictors 'organize' other predictors

## sample.patient.match(sample a, sample b)
##		find out if there is overlap between samples


####
make.shuffle<-function(){
xshuffle.index<-sample(1:dim(x)[1],dim(x)[1],replace=FALSE)
return(cbind(xshuffle.index,x[xshuffle.index,]))
}
####
make.train<-function(train_size){
	xtrain.index<-sample(1:dim(xshuffle)[1],train_size,replace=FALSE)
	return(cbind(xtrain.index,xshuffle[xtrain.index,]))}


make.test<-function(){
	print(paste("y dimension of xshuffle: ",dim(xshuffle)[1]))
	print(paste("y dimension of xtrain: ",dim(xtrain)[1]))
	xtest<-xshuffle[-xtrain.index,]
	print(paste("y dimension of xtest: ",dim(xtest)[1]))
	return(xtest)}

####
perimeter.ratio<-function(){
	ideal.perimeter<-(2*pi*radius1)
	return(perimeter1/ideal.perimeter)}
####
diagnosis.ratio<-function(){
	mcount<-sum(diagnosis=='M')
	bcount<-sum(diagnosis=='B')
	return(mcount/bcount)}
####
trial.train<-function(train_size){
	cov.arr<-c()
	diag.arr<-c()
	smooth.arr<-c()
	#xtrain<-c()
#	attach(xtrain)
	for(t in 1:20000){
	detach(xtrain)
	xtrain<-make.train(train_size)
	attach(xtrain)
	perimeter.ratio<-perimeter.ratio()
	#print(cor(perimeter.ratio,concavity1))
	print(t)
	smooth.arr<-c(smooth.arr,mean(xtrain$smoothness1))
	cov.arr<-c(cov.arr,cor(perimeter.ratio,concavity1))
	diag.arr<-c(diag.arr,diagnosis.ratio())
	}
	return(cbind(smooth.arr,cov.arr,diag.arr))
}
####
binary.diag<-function(diag){
	num<-c()
	for(d in diag){
		if(d=='M'){
			num<-c(num,1)}
		else{
			num<-c(num,0)}
}
return(num)
}
####
plot.sorted<-function(name){
mean.index<-which(names(xtrain)==name)
deviate.index<-mean.index+10
worst.index<-deviate.index+10

sorted<-xtrain[order(xtrain[,mean.index]),]
#plot(sorted[,mean.index],ylim=c(0,max(sorted[,worst.index])),)

plot(sorted[1,mean.index],pch=20,cex=0.4,col=color.finder(sorted,1),xlim=c(0,dim(xtrain)[1]),ylim=c(0,max(sorted[,worst.index])),xlab='Sample patient',ylab='')

for(p in 2:dim(xtrain)[1]){
	
	points(x=p,y=sorted[p,mean.index],cex=0.4,pch=20,col=color.finder(sorted,p))
}
points(sorted[,worst.index],type='line',col='orange')

points(sorted[,deviate.index],type='line',col='green')
legend("topleft", inset=.05, 
   c("mean","worst","std.err."), fill=c('black','orange','green'), horiz=TRUE)

title(name)

savePlot(filename=paste('train',name,'sort.png',sep='_'),type='png')
}
####
color.finder<-function(sorted, index){
	diag<-sorted[index,2]
	if(diag=='B'){
		color='black'}
	else{
		color='red'}
	return(color)
}
#####
plot.all.sorted<-function(){
for(n in 3:12){
plot.sorted(names(xtrain)[n])}}

####
plot.all.one.sorted<-function(name, saveplot, plotworst){
	mean.index<-which(names(xtrain)==name)
	deviate.index<-mean.index+10
	worst.index<-deviate.index+10
	sorted<-xtrain[order(xtrain[,mean.index]),]
	for(n in 3:12){
		mean.index<-n
		deviate.index<-mean.index+10
		worst.index<-deviate.index+10
		print(paste(mean.index,deviate.index,worst.index))
		if(plotworst==1){
			upper.y=max(sorted[,worst.index])}
		else{
			upper.y=max(sorted[,mean.index])}
		if(saveplot==1){
			png(file=paste('train',names(xtrain)[n],'sort_by',name,'.png',sep='_'),pointsize=16,width=500,height=400)}
		plot(sorted[1,which(names(xtrain)==name)],col=color.finder(sorted,1),pch=19,cex=.5,xlim=c(0,dim(xtrain)[1]),ylim=c(0,upper.y),xlab='Sample patient',ylab='')
		for(p in 2:dim(xtrain)[1]){
			points(x=p,y=sorted[p,mean.index],pch=19,cex=.5,col=color.finder(sorted,p))
		}
		if(plotworst==1){
			points(sorted[,worst.index],type='line',col='orange')
		}
		points(sorted[,deviate.index],type='line',col='green')
		if(plotworst==1){
			legend("topleft", inset=.05, c("mean","worst","std.err."), fill=c('black','orange','green'), horiz=TRUE)}
		else{
			legend("topleft", inset=.05,c("mean; benign","mean; malignant","std.err."), fill=c('black','red','green'),bty='n', horiz=TRUE)}
		#title(names(xtrain)[n])
		title(paste('train',names(xtrain)[n],'sort_by',name,sep='_'))
		if(saveplot==1){
			dev.off()
		##	savePlot(filename=paste('train',names(xtrain)[n],'sort_by',name,'.png',sep='_'),type='png')
}}}
####
plot.all.sorted.by.all<-function(arg){
arg<-(arg-1)*10
for(n in (arg+3):(arg+12)){
	print(paste(names(xtrain)[n],(c(n,arg+12))))
	plot.all.one.sorted(names(xtrain)[n],1,1)}}
####
plot.all.sorted.by.all.window<-function(arg){
arg<-(arg-1)*10

original.margins<-par()$mar
for(n in (arg+3):(arg+12)){
	print(c(n,12))
	#pdf(file=paste('train','_sort_by_',names(xtrain)[n],'.pdf',sep=''),paper='letter')
	png(file=paste('sample','_sort_by_',names(xtrain)[n],'.png',sep=''),pointsize=16,width=700,height=1000)
	par(mar=c(2,2,1,1))
	par(mfrow=c(5,2))
	plot.all.one.sorted(names(xtrain)[n],0,1)
	#text(1,line=1,'titletitletitle')
	dev.off()
#	savePlot(filename=paste('train','sort_by',name,'.pdf',sep='_'),type='pdf')
	}
par(mfrow=c(1,1))
par(mar=original.margins)
}
####
narrow<-function(predicted){
	return((1/(1+exp(-predicted))))}
thresh<-function(predicted){
	return(round(predicted))}
thresh.alt<-function(predicted,p){
	out<-c()
	for(i in predicted){
		if(i<p)
			out<-c(out,0)
		else
			out<-c(out,1)}
	return(out)
	
}
modwidth<-function(mat){
	remove.rows<-which(mat[1:dim(mat)[1],1]==NA)
	mat<-mat[-remove.rows,]
	width.arr<-c()
	for(r in 1:dim(mat)[1]){
		first0<-min(which(mat[r,]==0))
		if(first0>31)
			width.arr<-c(width.arr,30)
		else
			width.arr<-c(width.arr,(first0-2))
}}
####
adj.thresh<-function(){
	ratio.arr<-c()
	for(a in seq(0.05,2,0.0005)){
		binary.diag.pred<-round(a/(1+exp(-diag.pred)))
		ratio<-sum(binary.diag.pred==1)/length(binary.diag.pred)
		print(c(a,ratio))
		ratio.arr<-c(ratio.arr,ratio)}
	plot(ratio.arr,type='line')

}

get.predictors<-function(data.in){
	if(sum((names(data.in)=='diagnosis')==TRUE)){
		first=which(names(data.in)=='diagnosis')+1}
	else{
		first=1}
	last=dim(data.in)[2]
	return(data.in[,first:last])
}
	

get.diag<-function(data.in){
	if(sum((names(data.in)=='diagnosis')==TRUE)){
		diag.index=which(names(data.in)=='diagnosis')
		return(data.in[,diag.index])}
	else{
		return(NULL)}
}
get.diag.binary<-function(data.in){
	return(binary.diag(get.diag(data.in)))}
get.patient<-function(data.in){
	if(sum(names(data.in)=='id_number')==0){
		return(NULL)}
	else{
		return(data.in$id_number)}}

sample.patient.match<-function(a,b){
	amatches<-c()
	bmatches<-c()
	a_id<-get.patient(a)
	b_id<-get.patient(b)

	for(i in 1:length(a_id)){
		for(j in 1:length(b_id)){
			if(a_id[i]==b_id[j]){
				#print("match")
				amatches<-c(amatches,i)
				bmatches<-c(bmatches,j)
				}}}
	#return(cbind(amatches,bmatches))
	return(cbind(amatches,bmatches))
}
