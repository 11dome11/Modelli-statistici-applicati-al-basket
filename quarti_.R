#Funzione per predire i risultati del primo turno di playoff
quarti<-function(conference){
	conf<-rep(0,4)
	x<-meglio7(conference,1,8)
	if(x[[1]]=="top")
		conf[1]=names(conference)[1]
	else
		conf[1]=names(conference)[8]
	results<-rbind(as.matrix(x[[2]]),as.matrix(x[[3]]))
	
	x<-meglio7(conference,4,5)
	if(x[[1]]=="top")
		conf[2]=names(conference)[4]
	else
		conf[2]=names(conference)[5]
	results<-cbind(results,rbind(as.matrix(x[[2]]),as.matrix(x[[3]])))

	x<-meglio7(conference,3,6)
	if(x[[1]]=="top")
		conf[3]=names(conference)[3]
	else
		conf[3]=names(conference)[6]
	results<-cbind(results,rbind(as.matrix(x[[2]]),as.matrix(x[[3]])))

	x<-meglio7(conference,2,7)
	if(x[[1]]=="top")
		conf[4]=names(conference)[2]
	else
		conf[4]=names(conference)[7]
	results<-cbind(results,rbind(as.matrix(x[[2]]),as.matrix(x[[3]])))
	return (list(results,conf))
}
	