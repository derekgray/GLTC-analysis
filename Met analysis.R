temp<-read.csv(file.choose())

temp$years<-as.numeric(substr(temp$Date.NZST.,7,10))
temp$months<-as.numeric(substr(temp$Date.NZST.,4,5))
temp<-temp[which(temp$months==6|temp$months==7|temp$months==8),]

library(doBy)
series<-summaryBy(Tair.C.~years,data=temp)
names(series)<-c("Year", "Temp")
write.csv(series, file="Rotorua.csv", row.names=FALSE)



data<-read.csv(file.choose()) #lakes with lats and longs
key<-read.csv(file.choose()) #met stations

#compile met data
temps<-c()
combined<-data.frame("Date"=c(1850:2012))
for (i in 1:nrow(key)){
metstat<-key$Closest.Met.station[i]
lakename<-key$Lake.name[i]
metdat<-read.csv(file=paste("C:/Users/Derek/Dropbox/Postdoc/GLTC/Climate Working Group/Summer air temperatures/",metstat,".csv", sep=""))
row1<-which(combined$Date==metdat$Year[1])
temps<-c(rep("NA",nrow(combined)))
temps[row1:(nrow(metdat)+(row1-1))]<-metdat[,2]
combined[,i+1]<-as.numeric(temps)
}
names(combined)<-append("Date",as.character(key$Closest.Met.station),after=1)

#how many years of data?
nyears<-c()
for (i in 1:ncol(combined)){
nyears[i]<-length(which(is.na(combined[,i])==FALSE))}
nyears<-data.frame("Noyears"=nyears, "Station"=names(combined))
nyears<-summaryBy(Noyears~Station,FUN=mean, data=nyears)

#Figure out groups of stations (by lake) for plots
groups<-list()
for (j in 1:length(key$Closest.Met.station)){
  groups[[j]]<-which(key$Lake.name==unique(key$Lake.name[j]))}
groups<-unique(groups)

colbars<-sample(colours(), length(groups)) 

#colors by lake
cols<-c()
for (i in 1:length(groups)){
  cols<-append(cols,rep(colbars[i],length(groups[[i]])),after=length(cols))
  }
#mean number of years by lake
lngsbylake<-c()
for (i in 1:length(groups)){
lngsbylake[i]<-mean(lngs[unlist(groups[i])])}


#Figure out groups of stations for plots
groups<-list()
for (j in 1:length(key$Closest.Met.station)){
  groups[[j]]<-which(key$Closest.Met.station==unique(key$Closest.Met.station[j]))}
groups<-unique(groups)

#colors by station
coltext<-sample(colours(), length(groups)) 
colstext<-c()
for (i in 1:length(groups)){
  colstext<-append(colstext,rep(coltext[i],length(groups[[i]])),after=length(colstext))
}

#calculate trends through time for each station
m<-c()
lngs<-c()
for (i in 1:(ncol(combined)-1)){
mod<-"NA"
if(length(which(is.na(combined[136:163,i+1])==FALSE))>15){try(mod<-lm(combined[136:163,i+1]~combined[136:163,1],na.action="na.exclude"), silent=T)}
try(m[i]<-coefficients(mod)[2], silent=T)
lngs[i]<-length(which(is.na(combined[136:163,i+1])==FALSE))
}
m<-data.frame("Trend"=m, "Station"=key$Closest.Met.station, "Lake"=key$Lake.name)
library(doBy)
mbylake<-summaryBy(Trend~Lake, data=m, FUN=mean)


#plot bar graph of slopes for each station
pdf(file="laketrends.pdf", width=11, height=8.5) #open graphics device for pdf
par(oma = c(6, 0, 0, 0 ) )
mp<-barplot(as.vector(mbylake$Trend), ylab="Temperature trend", space=0,col=cols, ylim=c((min(mbylake$Trend,na.rm=T)-0.01),(max(mbylake$Trend,na.rm=T)+0.02)))
text(mp, par("usr")[3] -0.005, srt = 30, adj = 1, labels = mbylake$Lake, xpd = TRUE, font = 1, cex=0.75)

text(mp, mbylake$Trend + 0.01, paste("(",round(lngsbylake,0),")",sep=""),cex=0.65) 
#legend("topleft",legend=unique(key$Lake.name), fill=cols, cex=0.65)
dev.off() #close graphics device to flush figure to pdf



library(mapproj)

datapts<-mapproject(data$longitude, data$latitude, projection="rectangular", parameters=100, orientation=NULL)
map("world",proj="rectangular",parameters=100, xlim=(datapts$range[1:2]*1.2),ylim=(datapts$range[3:4]*1.2)) #,parameters=100) #,orient=c(0,-100,0)) 
points(datapts, col="red")


#read in NCEP data
mncep2<-read.csv(file.choose())

#make dataframe for NCEP, MET, and Satellite
MET<-c()
NP<-c()
SAT<-c()
for (i in 1:nrow(mbylake)){
MET[i]<-mbylake$Trend[i]*10
try(NP[i]<-mncep2$Trend[match(mbylake$Lake[i], mncep2$Lake)])
try(SAT[i]<-mncep2$Sat[match(mbylake$Lake[i], mncep2$Lake)])
}

plot(MET~NP, col="green", ylab="Met station trend", xlab="NCEP or Satellite trend")
mod1<-lm(MET~NP,na.action="na.exclude")
mod2<-lm(MET~SAT,na.action="na.exclude")
lines(fitted(mod1)~NP, col="green")
points(MET~SAT,col="red")
lines(fitted(mod2)~SAT, col="red")

insitu<-read.csv(file.choose())
commonlakes<-intersect(mbylake$Lake, insitu$Lake)

matchIndices <- function(x, y) {
  matched <- match(x, y)
  names(matched) <- x
  return(matched)
}

metstat<-c()
isitu<-c()
for (i in 1:length(mbylake$Lake)){
  metstat[i]<-mbylake$Trend.mean[i]
  loc<-which(as.character(insitu$Lake)==as.character(mbylake$Lake[i]))
  isitu[i]<-"NA"
  if(length(loc)>0){isitu[i]<-insitu$Trend.over.decade[loc]}
  
}
metstat<-metstat*10 #convert to decade
isitu<-as.numeric(isitu)

pdf(file="metvsinsitu.pdf", width=11, height=8.5) #open graphics device for pdf

plot(isitu~metstat, xlab="Met trend or NCEP trend", ylab="In situ trend", main="In situ surface temp vs. local met trend or NCEP trend (n=18)")
mod<-lm(as.numeric(isitu)~metstat, na.action=na.exclude) 
abline(0,1)

points(isitu~NP, col="red")
legend(x="bottomright",c("local met", "NCEP", "1:1 line"), col=c("black","red"), lty=c(0,0,1),pch=c(1,1,NA_integer_),cex=1)
dev.off() #close graphics device to flush figure to pdf

write.csv(data.frame('Met trend'=MET, "Sat trend"=SAT, "In situ trend"=isitu, "NCEP trend"=NP, "Lake"=mbylake$Lake), file="summary.csv")

#In situ vs. satellite
pdf(file="satvsinsitu.pdf", width=11, height=8.5) #open graphics device for pdf
plot(isitu~SAT, xlab="Met trend or NCEP trend", ylab="In situ trend", main="In situ surface temp vs. local met trend or NCEP trend (n=18)")
abline(0,1)