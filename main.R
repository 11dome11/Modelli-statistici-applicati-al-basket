#Caricamento funzioni 
source("Meglio7.R")
source("quarti_.R")
source("semi_.R")
source("finale_.R")
#Caricamento dati
d<-read.csv(file="DATIX.csv", head=TRUE, sep=";")

#Preparazione dati
x=rbind(
	data.frame(score=d$PUNTI.HOME.TEAM,
		     team=d$HOME.TEAM,
		     opponent=d$AWAY.TEAM,
		     home=1),
	data.frame(score=d$PUNTI.AWAY.TEAM,
		     team=d$AWAY.TEAM,
		     opponent=d$HOME.TEAM,
		     home=0))
#Stima dei coefficienti del modello di Poisson per la predizione dei risultati delle partite
model <- glm(score ~ home + team + opponent, family=poisson (link=log) , data=x)
summary(model)


team=d$HOME.TEAM
opponent=d$AWAY.TEAM
ScoreHome<-matrix(0,nrow=length(team),ncol=1)

rownames(ScoreHome)<-team

ScoreAway<-matrix(0,nrow=length(opponent),ncol=1)

rownames(ScoreAway)<-opponent

n=0

#Ciclo per la predizione dei risultati di ogni match
for(i in 1:length(team)){

Home<-predict(model, data.frame(home=1, team=team[i],opponent=opponent[i]),type="response")
Away<-predict(model, data.frame(home=0, team=opponent[i],opponent=team[i]),type="response")

n=n+1
if(Home>Away)
	ScoreHome[i]<-1+ScoreHome[i]
if(Home<Away)
	ScoreAway[i]<-1+ScoreAway[i]
}

#Classifica finale della regular season
sum(ScoreHome)+sum(ScoreAway)


TeamStand<-rownames(ScoreHome)
TeamStand<-TeamStand[!duplicated(TeamStand)]
standings<-rep(0,30)
names(standings)<-TeamStand 
cbind((standings))

for(i in 1:length(standings))
	for(j in 1:length(ScoreHome)) {
		if((rownames(ScoreHome)[j])==(names(standings)[i]))
			if(ScoreHome[j]==1)
				standings[i]=standings[i]+1
		if((rownames(ScoreAway)[j])==(names(standings)[i]))
			if(ScoreAway[j]==1)
				standings[i]=standings[i]+1
	}

standings<-sort(standings,T)
cbind(standings)

#Suddivisione squadre in conference 

C<-read.csv(file="CONFERENCE.csv", head=TRUE, sep=";")
O<-rep(0,15)
E<-rep(0,15)
names(O)<-C[,2]
names(E)<-C[,1]


for (j in 1:length(E))
	for (i in 1:length(standings)) 
		if ((names(standings)[i])==names(E)[j])
			E[j]=standings[i]
	
for (j in 1:length(O))
	for (i in 1:length(standings)) 
		if ((names(standings)[i])==names(O)[j])
			O[j]=standings[i]

E<-sort(E,T)

O<-sort(O,T)

E<-E[1:8]
O<-O[1:8]

#Svolgimento playoff

quartiEst<-quarti(E)
Est<-rep(0,4)
names(Est)<-quartiEst[[2]]
quartiOvest<-quarti(O)
Ovest<-rep(0,4)
names(Ovest)<-quartiOvest[[2]]
semiEst<-semi(Est)
semiOvest<-semi(Ovest)
Est<-rep(0,2)
names(Est)<-semiEst[[2]][1:2]
Ovest<-rep(0,2)
names(Ovest)<-semiOvest[[2]][1:2]
finaleEst<-finale(Est)
finaleOvest<-finale(Ovest)
finals<-rep(0,2)
names(finals)<-cbind(finaleEst[[2]][1],finaleOvest[[2]][1])
winner<-meglio7(finals,1,2)
if(winner[[1]]=="down")
	win<-names(finals)[[2]]
if(winner[[1]]=="top")
	win<-names(finals)[[1]]
win
