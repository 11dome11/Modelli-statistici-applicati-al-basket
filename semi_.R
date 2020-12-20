#Funzione per predire i risultati del secondo turno di playoff
semi<-function(conference){
	conf<-rep(0,4)
	x<-meglio7(conference,1,2)
	if(x[[1]]=="top")
		conf[1]=names(conference)[1]
	else
		conf[1]=names(conference)[2]
	results<-rbind(as.matrix(x[[2]]),as.matrix(x[[3]]))
	
	x<-meglio7(conference,3,4)
	if(x[[1]]=="top")
		conf[2]=names(conference)[3]
	else
		conf[2]=names(conference)[4]
	results<-cbind(results,rbind(as.matrix(x[[2]]),as.matrix(x[[3]])))

	return (list(results,conf))
}
