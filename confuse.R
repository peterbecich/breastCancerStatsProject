make.confuse<-function(diag.real, diag.predict){
	mat<-matrix(0,2,2)
	##   true positives  |  false positives
	##   flase negatives |  true negatives
	#print(diag.real)
	#print(diag.predict)
	for(d in 1:length(diag.real)){
		## true positives
		if((diag.real[d]==1) & (diag.predict[d]==1)){
		#	print(paste("TP",d))
			mat[1,1]<-mat[1,1]+1}
		## true negatives
		if((diag.real[d]==0) & (diag.predict[d]==0)){
		#	print(paste("TN",d))
			mat[2,2]<-mat[2,2]+1}
		## false negatives
		if((diag.real[d]==1)&(diag.predict[d]==0)){
		#	print(paste("FN",d))
			mat[2,1]<-mat[2,1]+1}
		## false positives
		if((diag.real[d]==0)&(diag.predict[d]==1)){
		#	print(paste("FP",d))
			mat[1,2]<-mat[1,2]+1}
		}
	return(mat)}
confuse.correct<-function(mat){
	return((mat[1,1]+mat[2,2])/sum(mat))}
