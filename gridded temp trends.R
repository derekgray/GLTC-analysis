master<-read.csv(file.choose(),stringsAsFactors=F)


#plot lakes with in situ data
library(mapproj)
datin<-master[,which(master[13,]=="Y")] #in situ yes
datapts<-mapproject(as.numeric(datin[5,3:ncol(datin)]),as.numeric(datin[4,3:ncol(datin)]),projection="rectangular", parameters=100, orientation=NULL)
map("world",proj="rectangular",parameters=100, xlim=(datapts$range[1:2]*1.2),ylim=(datapts$range[3:4]*1.2)) 
points(datapts, col="red", pch=16, cex=0.75)
map.axes()

#analyses with mann-kendall tests
library(fume)
pval<-c()
slope<-c()
noyear<-c()
tau<-c()

for (i in 1:ncol(datin)){
  noyear[i]<-25-length(na.exclude(as.numeric(datin[133:157,i])))
}

isitu<-datin[,which(noyear<=5)] #take lakes with 5 or fewer missing years
holder<-isitu
# this part replaces missing values with averages
for (i in 1:ncol(isitu)){
  dta<-as.numeric(isitu[504:528,i])
  dta[which(is.na(dta)==T)]<-mean(dta,na.rm=T)
  if(i==1){isituavg<-data.frame(dta)}
  isituavg[,i]<-dta
}
names(isituavg)<-names(isitu)
isitu<-isituavg

#this part uses the mkTrend function to get the trends
pval<-c()
slope<-c()
tau<-c()
noyear<-c()
for (i in 1:ncol(isitu)){
  pval[i]<-NA
  slope[i]<-NA
  try(answer<-unlist(mkTrend(as.numeric(isitu[1:nrow(isitu),i]))))
  pval[i]<-answer[4]
  slope[i]<-answer[7]
  tau[i]<-answer[5]
  noyear[i]<-length(na.exclude(as.numeric(isitu[1:nrow(isitu),i])))
}
missing<-data.frame("missing years"=21-noyear, "lake"=names(holder))


#create data frame with mann kendall results
res<-data.frame("Lake"=names(isitu), "No years data"=noyear, "Tau correlation coefficient"=tau, "Sen slope"=slope, "pval"=round(pval,3))
res$Lake[which(res$pval<=0.1)]
res$Lake[which(res$pval<=0.05)]
res$Tau[which(res$pval<=0.1)] #

#put together a csv with metadata and the mann-kendall results
newres<-cbind(res,t(holder[1:19,])) #final results in situ with metadata
names(newres)[6:ncol(newres)]<-t(master[1:19,1])
write.csv(newres,"Gridded temp trends.csv") #write the final 
