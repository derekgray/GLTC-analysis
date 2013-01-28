temp<-read.csv(file.choose())
temp$MNTM<-temp$MNTM/10
months<-as.numeric(substr(temp$DATE, 5,6))
summertemp<-temp[which(months==7|months==8|months==9),]

summertemp$years<-substr(summertemp$DATE,1,4)

library(doBy)

meansummer<-summaryBy(MNTM~years, FUN=mean,data=summertemp)
names(meansummer)<-c("Year","Temp")
plot(Temp~Year,data=meansummer)

#metkey<-read.csv(file.choose())
names(metkey)<-c("Location","Lake","MetStation","Distance from lake","Temporal range","Station ID","Notes")
lake<-metkey$MetStation[which(metkey$Station==substr(temp$STATION[1], 7, nchar(as.character(temp$STATION[1]))))][1]

write.csv(meansummer,file=paste("Z:/GLTC/Climate Working Group/Summer temperatures/",lake,".csv"), row.names=FALSE)



temp<-read.csv(file.choose())
names(temp)<-c("Month","Year","Yearmo","Min","Max","X")
temp$mean<-(temp$Min+temp$Max)/2
temp$mean<-(temp$mean-32)*5/9
summertemp<-temp[which(temp$Month==7|temp$Month==8|temp$Month==9),]


temp$months<-as.numeric(substr(temp$ObsDate, 4,5))
temp$years<-as.numeric(substr(temp$ObsDate, 7,10))
names(temp)<-c("Date","Min","Max","Months","Years")

library(doBy)
final<-summaryBy(mean~Year, data=summertemp, FUN=mean)
write.csv(final,file="Z:/GLTC/Climate Working Group/Summer air temperatures/Annie.csv", row.names=FALSE)



#combine met stations:


