buildMod<-function(diag.in,predict.in){
	## using GPU Glm
	attach(predict.in)
#	out<-(glm(diag.in~.,data=predict.in, family=binomial(link=logit)))
	out<-(glm(diag.in~area1+perimeter1+radius1, family=binomial(link=logit)))
	#out<-(glm(diag.in~area1+perimeter1+radius1, family=binomial(link=logit)))
	#form<-paste(predict.in[,
#	out<-(glm(diag.in~paste('area1','perimeter1','radius1',sep='+'), family=binomial(link=logit)))
	detach(predict.in)
	return(out)}

buildMod.by.index<-function(diag.in,predict.in,predict.indices){
	## using GPU Glm
	#attach(predict.in)
	predict.names<-names(predict.in)
	form.1<-paste("diag.in ~")
	form.2<-paste(predict.names[predict.indices],sep='',collapse='+')
	form<-paste(form.1,form.2,sep='')
	#print(form)
	out<-glm(form, family=binomial(link=logit),data=cbind(diag.in,predict.in))
	#detach(predict.in)
	return(out)}


predictMod<-function(predictors){
	return(predict(mylogit, newdata=predictors))}


brute.force.search<-function(diag.in,predict.in,maxdays, choosesd){
	## 30 predictors total
	## taken 1 at a time, + 2 at a time, ...

	tree<-list()

	predict.names<-names(predict.in)
	form.1<-paste("diag.in ~")
	form.2<-paste(predict.names[1:30],sep='',collapse='+')
	form<-paste(form.1,form.2,sep='')
	possible.combos<-sum(combinations(30,1:30))
	print(paste(possible.combos," possible combinations (choose 1 through 30)"))
	now<-Sys.time()
	testmod<-glm(form, family=binomial(link=logit),data=cbind(diag.in,predict.in))
	later<-Sys.time()
	runonetime<-as.numeric(difftime(later,now))
	runalltime<-possible.combos*as.numeric(difftime(later,now))
	print(paste("Would take ",round(10*runalltime/(60*60*24),0)," days to run them all (included 10-fold cross validation.",sep=''))
	times<-c()
	for(t in 1:30){
		form.1<-paste("diag.in ~")
		form.2<-paste(predict.names[1:30],sep='',collapse='+')
		form<-paste(form.1,form.2,sep='')
		possible.combos<-combinations(30,t)
		#print(paste(possible.combos," possible combinations (choose 1 through 30)"))
		now<-Sys.time()
		testmod<-glm(form, family=binomial(link=logit),data=cbind(diag.in,predict.in))
		later<-Sys.time()
		runalltime<-possible.combos*as.numeric(difftime(later,now))
		times<-c(times,runalltime/(60*60*24))
	}
	#plot(times,ylab="Days",xlab="Choose",title="Comp. time for every combination of 30 choose n predictors",type='line')

	runs<-floor(maxdays/(runonetime/(60*60*24)))
	print(paste("Will test ",runs," models."))
	donemat<-matrix(0,runs,36)
	while(runs>0){
		while(1){
			choose<-floor(10*rnorm(1,mean=15,sd=choosesd)/10)
			if(choose>30){break}
			else if(choose<1){break}
			chosen.combo.arr<-sample(1:30,size=choose,replace=FALSE)
		#	if(check.donemat(sort(chosen.combo.arr),donemat)==TRUE){
		#		print("********* duplicate random sample; resample ************")
		#		break}
			print(paste("chosen: ",choose," combo: "))
			print(chosen.combo.arr)
			#########
		#	buildMod.by.index(diag.in,predict.in,chosen.combo.arr)

			donemat[dim(donemat)[1]-runs+1,1:length(chosen.combo.arr)]<-sort(chosen.combo.arr)
			runs<-runs-1
			break
		
}
}
#print(donemat)
return(donemat)
}
#check.donemat<-function(combo,donemat){
#for(j in 1:dim(donemat)[1]){
#		if((combo[1:length(combo)]==(donemat[j,1:length(combo)]))&(donemat[j,length(combo)+1]==0)){
#			print(paste("combo length: ",length(combo)))
#			print("combo:")
#			print(combo)
#			print("donemat entry")
#			print((donemat[j,]))
#			return(TRUE)}
#		}
#return(FALSE)}

#check.tree<-function(combo,tree){

#for(d in combo){
		
#}}
#matsort<-function(mat){
#mat<-mat[order(mat[,1]),]
#for(i in 2:30){
#	for(u in unique(mat[,i-1])){
#		urange<-c(min(which(mat[,i-1]==u)),max(which(mat[,i-1]==u)))
#		print(paste("i: ",i," urange: ",urange[1],urange[2]))
#		mat[urange,]<-mat[order(mat[urange,i]),]
#	}
#}
#return(mat)
#}
combinations<-function(all,choose){
return(factorial(all)/(factorial(choose)*factorial(all-choose)))}
