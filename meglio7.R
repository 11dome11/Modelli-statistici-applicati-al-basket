#Funzione che gestisce il giusto ordine in cui due squadre si affrontano in una serie "al meglio delle 7 partite"
meglio7<-function(conf,pos1,pos2){
	i=1
	score_down=0
	score_top=0
	while(score_down<4&score_top<4){
		
		if(i==1||i ==2||i ==5||i ==7){
			top<-predict(model, data.frame(home=1, team=names(conf)[pos1],opponent=names(conf)[pos2]),type="response")
			down<-predict(model, data.frame(home=0, team=names(conf)[pos2],opponent=names(conf)[pos1]),type="response")
		}
		if((i ==3|i ==4|i==6)){
			top<-predict(model, data.frame(home=0, team=names(conf)[pos1],opponent=names(conf)[pos2]),type="response")
			down<-predict(model, data.frame(home=1, team=names(conf)[pos2],opponent=names(conf)[pos1]),type="response")
		}
		i=i+1
		if(top>down)
			score_top=score_top+1
		if(top<down)
			score_down=score_down+1
	}
	if(score_top>score_down)
		pass=list("top",score_top,score_down)
	if(score_down>score_top)
		pass=list("down",score_top,score_down)
	return (pass)
}	


