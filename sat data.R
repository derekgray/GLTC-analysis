lakes<-read.csv(file.choose(),stringsAsFactors=F)
islakes<-data.frame(lakes$Lakes)
satlakes<-data.frame(lakes$Lake.sat.data)
johnlakes<-data.frame(lakes$John.s.file.in.situ)

islakes[,2]<-rep("insitu",times=nrow(islakes))
satlakes[,2]<-rep("sat",times=nrow(satlakes))
satlakes[,3]<-lakes$JPLID
johnlakes[,2]<-rep("john",times=nrow(johnlakes))
names(islakes)<-c("lake","type")
names(satlakes)<-c("lake","type","ID")
names(johnlakes)<-c("lake","type")

allofthem<-merge(islakes,satlakes,by ="lake",all=T)
allofthem<-merge(allofthem,johnlakes,by="lake")


all<-data.frame("lakes"=unique(alllakes))
library(doBy)
all<-orderBy(lakes~.,data=all)

write.csv(allofthem,file="alllakenames.csv")