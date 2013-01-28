#hotspot analysis

sat.trends<-read.csv("satmktrends.csv",row.names=NULL,stringsAsFactors=F) #file with trends for in sat lakes
NCEP.sat<-read.csv("NCEPtrendsmksat.csv",row.names=NULL,stringsAsFactors=F) #file with NCEP trends for sat lakes

#_________________________________________________________________________________________
#first try looking at a fixed number of nearest neighbours
nneigh<-5 #number of neighbours to consider
satlats<-as.matrix(data.frame(sat.trends$Longitud,sat.trends$Latitude))
bob.a<-knearneigh(satlats, k=nneigh,longlat=T) #fixed number nearest neighbors probably not a good way (different spatial scales incorporated into one analysis)
plot(knn2nb(bob.a),satlats)

#calculate mean warming trend for nneigh nearest neighbours for each lake
sdgroup<-c()
for (i in 1:nrow(bob.a$nn)){
  sdgroup[i]<-mean(sat.trends$Slope[bob.a$nn[i,]])
  }
library(mapproj)
library(plotrix)
mapcols<-color.scale(as.numeric(sdgroup),c(0,1,1),c(1,1,0),0)
datapts<-mapproject(satlats[,1],satlats[,2],projection="rectangular", parameters=100, orientation=NULL)
map("world",proj="rectangular",parameters=100)
map.axes()
plotchar<-rep(1, length(sdgroup))

#calculate the bootstrap mean warming level and confidence interval
library(boot)
b <- boot(sat.trends$Slope, function(u,i) mean(sample(u[i],nneigh,replace=F)), R = 999)
b.ci<-boot.ci(b, type = "norm")
plotchar[which(sdgroup>b.ci$normal[1,3])]<-16 #change plot character to filled for those exeeding 95% CI
plotchar[which(sdgroup<b.ci$normal[1,2])]<-16 #change plot character to filled for those below 95% CI

#plot lakes on map with colours for trends
points(datapts,col=mapcols,pch=plotchar)
n<-sort(as.numeric(sdgroup))
lgnd<-round(seq(min(n),max(n),length.out=7),3)
color.legend(200,-90,220,90,legend=lgnd,rect.col=color.scale(sort(na.exclude(as.numeric(sdgroup))),c(0,1,1),c(1,1,0),c(0,0,0)),gradient="y")
title(paste("Satellite temperatures, Number of neighbors=",nneigh, sep=" "))
#______________________________________________________________________________________________________
#now for in isitu data

insitu.trends<-read.csv("insitumktrends.csv",row.names=NULL,stringsAsFactors=F) #file with trends for in situ lakes
NCEP.insitu<-read.csv("NCEPtrendsmkinsitu.csv",row.names=NULL,stringsAsFactors=F) #file with NCEP trends for in situ lakes

#first try looking at a fixed number of nearest neighbours
nneigh<-10 #number of neighbours to consider
insitulats<-as.matrix(data.frame(insitu.trends$Longitud,insitu.trends$Latitude))
bob.a<-knearneigh(insitulats, k=nneigh,longlat=T) #fixed number nearest neighbors probably not a good way (different spatial scales incorporated into one analysis)
plot(knn2nb(bob.a),insitulats)

#calculate mean warming trend for nneigh nearest neighbours for each lake
sdgroup<-c()
for (i in 1:nrow(bob.a$nn)){
  sdgroup[i]<-mean(insitu.trends$Slope[bob.a$nn[i,]])
}
library(mapproj)
library(plotrix)
mapcols<-color.scale(as.numeric(sdgroup),c(0,1,1),c(1,1,0),0)
datapts<-mapproject(insitulats[,1],insitulats[,2],projection="rectangular", parameters=100, orientation=NULL)
map("world",proj="rectangular",parameters=100)
map.axes()
plotchar<-rep(1, length(sdgroup))

#calculate the bootstrap mean warming level and confidence interval
library(boot)
b <- boot(insitu.trends$Slope, function(u,i) mean(sample(u[i],nneigh,replace=F)), R = 9999)
b.ci<-boot.ci(b, type = "norm")
plotchar[which(sdgroup>b.ci$normal[1,3])]<-16 #change plot character to filled for those exeeding 95% CI
plotchar[which(sdgroup<b.ci$normal[1,2])]<-16 #change plot character to filled for those below 95% CI

#plot lakes on map with colours for trends
points(datapts,col=mapcols,pch=plotchar)
n<-sort(as.numeric(sdgroup))
lgnd<-round(seq(min(n),max(n),length.out=7),3)
color.legend(200,-90,220,90,legend=lgnd,rect.col=color.scale(sort(na.exclude(as.numeric(sdgroup))),c(0,1,1),c(1,1,0),c(0,0,0)),gradient="y")
title(paste("In situ temperatures, Number of neighbors=",nneigh, sep=" "))



#_______________________________________________________________________

#now try a fixed distance to define neighbourhood
dist<-
bob.b<-dnearneigh(satlats, 0, 750,longlat=T)
plot(bob.b,satlats)
