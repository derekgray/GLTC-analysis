master<-read.csv(file.choose(),stringsAsFactors=F)

#sat or insitu? ("sat","insitu")
datatype<-"insitu"

#Mann Kendall or linear model? ("mk" or "lm")
analysis<-"mk"

if (datatype=="insitu"){
  datin<-master[,which(master[15,]=="Y")] #in situ yes
  dt1<-140
  dt2<-164
}

if (datatype=="sat"){
  datin<-master[,which(master[16,]=="Y")] #in situ yes
  dt1<-23
  dt2<-47
}

#how many years of missing in situ or satellite data?
noyear<-c()
for (i in 1:ncol(datin)){
  noyear[i]<-25-length(na.exclude(as.numeric(datin[dt1:dt2,i]))) 
}

#only take lakes with less than x years missing data
datin<-datin[,which(noyear<=5)] #take lakes with 5 or fewer missing years
holder<-datin

#plot lakes 
library(mapproj)
datapts<-mapproject(as.numeric(datin[6,3:ncol(datin)]),as.numeric(datin[5,3:ncol(datin)]),projection="rectangular", parameters=100, orientation=NULL)
map("world",proj="rectangular",parameters=100, xlim=(datapts$range[1:2]*1.2),ylim=(datapts$range[3:4]*1.2)) 
points(datapts, col="red", pch=16, cex=0.75)
map.axes()

if (analysis=="mk"){
#analyses with mann-kendall tests
library(fume)
pval<-c()
slope<-c()
noyear<-c()
tau<-c()

#this part uses the mkTrend function to get the trends
for (i in 1:ncol(datin)){
  pval[i]<-NA
  slope[i]<-NA
  try(answer<-unlist(mkTrend(as.numeric(datin[173:197,i]))))
  pval[i]<-answer[4]
  slope[i]<-answer[7]
  tau[i]<-answer[5]
  noyear[i]<-length(na.exclude(as.numeric(datin[173:197,i])))
}


#create data frame with mann kendall results
res<-data.frame("Lake"=names(datin), "No years data"=noyear, "Tau correlation coefficient"=tau, "Slope"=slope, "pval"=round(pval,3))
res$Lake[which(res$pval<=0.1)]
res$Lake[which(res$pval<=0.05)]
res$Tau[which(res$pval<=0.1)] #

#put together a csv with metadata and the mann-kendall results
newres<-cbind(res,t(holder[1:19,])) #final results in situ with metadata
names(newres)[6:ncol(newres)]<-t(master[1:19,1])
write.csv(newres,paste("NCEP trends", analysis,datatype,".csv",sep=""),row.names=F) #write the final 
}

if (analysis=="lm"){
  #regression using lm 
  noyear<-c()
  slope<-c()
  rsquared<-c()
  p<-c()
  for (i in 1: ncol(isitu)){
    slope[i]<-NA
    rsquared[i]<-NA
    p[i]<-NA
    mod<-lm(as.numeric(datin[173:197,i])~as.numeric(1985:2009))
    try(slope[i]<-coefficients(mod)[2])
    try(temp<-summary(mod))
    try(rsquared[i]<-temp[9])
    try(p[i]<-temp$coefficients[2,4])
    noyear[i]<-length(na.exclude(as.numeric(datin[173:197,i])))
  }
  missing<-data.frame("missing years"=21-noyear, "lake"=names(isitu))
  #create data frame with mann kendall results
  res<-data.frame("Lake"=names(isitu), "No years data"=noyear, "rsquared"=unlist(rsquared), "Slope"=slope, "pval"=round(p,3))
  
  #put together a csv with metadata and the lm results
  newres<-cbind(res,t(holder[1:19,])) #final results in situ with metadata
  names(newres)[6:ncol(newres)]<-t(master[1:19,1])
  write.csv(newres,paste("NCEP trends", analysis,datatype,".csv",sep=""),row.names=F) #write the final 
}

#plot trends on map
library(plotrix)
datapts<-mapproject(as.numeric(datin[6,3:ncol(datin)]),as.numeric(datin[5,3:ncol(datin)]),projection="rectangular", parameters=100, orientation=NULL)
b<-color.scale(as.numeric(slope[which(slope<0)]),extremes=c("darkblue","lightblue"))
map("world",proj="rectangular",parameters=100)
map.axes()
mapcols<-(color.scale(as.numeric(slope),c(0,1,1),c(1,1,0),0,na.color="black"))
mapcols[which(as.numeric(slope)<0)]<-b
points(datapts, col=mapcols, pch=16, cex=1.1)
n<-sort(as.numeric(slope))
lgnd<-round(seq(min(n),max(n),length.out=7),3)
color.legend(200,-90,220,90,legend=lgnd,rect.col=color.scale(sort(na.exclude(as.numeric(slope))),c(0,1,1),c(1,1,0),c(0,0,0)),gradient="y")
color.legend(200, -90, 220, 0,rect.col=sort(b),gradient="y")

title("NCEP trends 1985-2009")
