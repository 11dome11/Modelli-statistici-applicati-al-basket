#Funzione per predire i risultati delle finali di Conference
finale<-function(conference){
	conf<-rep(0,4)
	x<-meglio7(conference,1,2)
	if(x[[1]]=="top")
		conf[1]=names(conference)[1]
	else
		conf[1]=names(conference)[2]
	results<-rbind(as.matrix(x[[2]]),as.matrix(x[[3]]))
	return (list(results,conf))
}
