master<-read.csv(file.choose(),stringsAsFactors=F)


#these numbers don't line up anymore due to additional fields being added!
for (i in 3:ncol(master)){

#In situ data  
if(length(which(is.na(as.numeric(master[50:166,i]))==F))>0)
{master[15,i]<-"Y"}

#if(length(which(is.na(as.numeric(master[46:157,i]))==F))==0)
#{master[13,i]<-"N"}

#Sat data  
#if(length(which(is.na(as.numeric(master[19:45,i]))==F))>0)
#{master[14,i]<-"Y"}

#if(length(which(is.na(as.numeric(master[19:45,i]))==F))==0)
#{master[14,i]<-"N"}

#NCEP data  
#if(length(which(is.na(as.numeric(master[158:184,i]))==F))>0)
#{master[15,i]<-"Y"}

#if(length(which(is.na(as.numeric(master[158:184,i]))==F))==0)
#{master[15,i]<-"N"}

#MET data  
#if(length(which(is.na(as.numeric(master[195:306,i]))==F))>0)
#{master[16,i]<-"Y"}

#if(length(which(is.na(as.numeric(master[195:306,i]))==F))==0)
#{master[16,i]<-"N"}

}

#date range in situ
for (i in 3:ncol(master)){
  if(master[13,i]=="Y"){
    ll<-master[(which(master[47:158,i]>0)+46),1][1]
    ll2<-master[(which(master[47:158,i]>0)+46),1][length(which(master[47:158,i]>0))]
    master[17,i]<-paste(ll,ll2,sep="-")}
  }
  
}


#number of missing years
for (i in 3:ncol(master)){
  if(master[13,i]=="Y"){
    dtt<-length(which(master[47:158,i]>0))
    ll<-master[(which(master[47:158,i]>0)+46),1][1]
    ll2<-master[(which(master[47:158,i]>0)+46),1][length(which(master[47:158,i]>0))]
    
    master[18,i]<-(as.numeric(ll2)-as.numeric(ll))-dtt+1}
}

#15 years overlapping sat and in situ
for (i in 3:ncol(master)){
a<-length(which(master[134:160,i]>0))
b<-length(which(master[22:48,i]>0))
if (a>15&b>15){master[19,i]<-"Y"}
if (a<15|b<15){master[19,i]<-"N"}
}
#lists

#which lakes do we have in situ data for?
insit<-names(master)[which(master[13,]=="Y")]

#which lakes do we have metadata for but no data (need to contact people)
missing<-names(master[which(master[12,]=="Y")])
contr<-(master[11,which(master[12,]=="Y")])
region<-(master[3,which(master[12,]=="Y")])
write.csv(data.frame("Lake"=names(contr), "Contact"=t(contr),row.names=NULL,"Region"=t(region)),file="missing.csv")

#lakes where we need sat data (have in situ data)
tt<-which(master[13,]=="Y"|master[12,]=="Y"|master[17,]=="Y") #lakes that we could have data for
uu<-which(master[14,]=="N"&as.numeric(master[9,])>10) #lakes that we don't have satellite data for
latt<-as.numeric(master[4,][tt[tt%in%uu]])
longg<-as.numeric(master[5,][tt[tt%in%uu]])
write.csv(data.frame("Lake"=names(master)[tt[tt%in%uu]],"Latitude"=latt,"Longitude"=longg),file="satneeds.csv") #which insitu lakes don't have satellite data?

#lakes where we need MET data (have or can possibly get in situ data)
tt<-which(master[13,]=="Y") #lakes that we could have data for
uu<-which(master[16,]=="N") #lakes that we don't have met data for
write.csv(data.frame(names(master)[tt[tt%in%uu]]),file="metneeds.csv") #which insitu lakes don't have satellite data?

#lakes where we need NCEP data (have or can possibly get in situ data)
tt<-which(master[13,]=="Y"|master[12,]=="Y"|master[17,]=="Y") #lakes that we could have data for
uu<-which(master[15,]=="N") #lakes that we don't have satellite data for
write.csv(data.frame(names(master)[tt[tt%in%uu]]),file="NCEPneeds.csv") #which insitu lakes don't have satellite data?



#which lakes do we have sat data for?
sat<-names(master)[which(master[14,]=="Y")]

#lakes with both sat data and in situ data (current overlap lakes)
write.csv(data.frame(names(master[which(master[19,]=="Y")])),file="overlap.csv")

names(master[which(master[19,]=="Y")])

#possible overlap lakes (lakes where we have or expect to have in situ data)
write.csv(data.frame(names(master)[which(master[13,]=="Y"|master[12,]=="Y"|master[17,]=="Y")]),file="possover.csv")




library(mapproj)
datapts<-mapproject(as.numeric(master[6,3:ncol(master)]),as.numeric(master[5,3:ncol(master)]),projection="rectangular", parameters=100, orientation=NULL)
map("world",proj="rectangular",parameters=100, xlim=(datapts$range[1:2]*1.2),ylim=(datapts$range[3:4]*1.2)) 
map.axes()

library(plotrix)
mapcols<-(color.scale(as.numeric(ncepfinal[33,2:ncol(ncepfinal)]),c(0,1,1),c(1,1,0),0,na.color="black"))
points(datapts, col="red", pch=16)


#plot lakes with in situ data
library(mapproj)
datin<-master[,which(master[13,]=="Y")] #in situ yes
datapts<-mapproject(as.numeric(datin[5,3:ncol(datin)]),as.numeric(datin[4,3:ncol(datin)]),projection="rectangular", parameters=100, orientation=NULL)
map("world",proj="rectangular",parameters=100, xlim=(datapts$range[1:2]*1.2),ylim=(datapts$range[3:4]*1.2)) 
points(datapts, col="red", pch=16, cex=0.75)
map.axes()
#-------------------------------------------------------------------------------------

#analyses to look at trends in water temperatures
#sat or insitu? ("sat","insitu")
datatype<-"sat"

#Mann Kendall or linear model? ("mk" or "lm")
analysis<-"mk"
if (datatype=="sat"){
datin<-master[,which(master[15,]=="Y")] #in situ yes
dt1<-140
dt2<-164
}

if (datatype=="sat"){
datin<-master[,which(master[16,]=="Y")] #in situ yes
dt1<-23
dt2<-47
}

library(openair)
pval<-c()
slope<-c()
noyear<-c()
tau<-c()

#how many years of missing data?
for (i in 1:ncol(datin)){
noyear[i]<-25-length(na.exclude(as.numeric(datin[dt1:dt2,i]))) 
}

#only take lakes with less than x years missing data
isitu<-datin[,which(noyear<=5)] #take lakes with 5 or fewer missing years
holder<-isitu

# this part replaces missing values with averages
for (i in 1:ncol(isitu)){
dta<-as.numeric(isitu[dt1:dt2,i]) 
dta[which(is.na(dta)==T)]<-mean(dta,na.rm=T)
if(i==1){isituavg<-data.frame(dta)}
isituavg[,i]<-dta
}
names(isituavg)<-names(datin)[which(noyear<=5)]
isitu<-isituavg

if (analysis=="mk"){
#this part uses the mkTrend function to get the trends
pval<-c()
slope<-c()
tau<-c()
noyear<-c()
for (i in 1:ncol(isitu)){
  pval[i]<-NA
  slope[i]<-NA
  try(answer<-unlist(
    
    answer<-MannKendall(mydata=data.frame("Temp"=as.numeric(isitu[1:nrow(isitu),i]), "date"=seq(from=as.Date("1985-01-01"), to=as.Date("2009-01-01"),by="years")),pollutant="Temp")
  answer$data$res2
    answer$data$main.data
    pval[i]<-answer[4]
  slope[i]<-answer[7]
  tau[i]<-answer[5]
  noyear[i]<-length(na.exclude(as.numeric(isitu[1:nrow(isitu),i])))
}
missing<-data.frame("missing years"=21-noyear, "lake"=names(isitu))
#create data frame with mann kendall results
res<-data.frame("Lake"=names(isitu), "No years data"=noyear, "Tau correlation coefficient"=tau, "Slope"=slope, "pval"=round(pval,3))
res$Lake[which(res$pval<=0.1)]
res$Lake[which(res$pval<=0.05)]
res$Tau[which(res$pval<=0.1)] #

#put together a csv with metadata and the mann-kendall results
newres<-cbind(res,t(holder[1:19,])) #final results in situ with metadata
names(newres)[6:ncol(newres)]<-t(master[1:19,1])
write.csv(newres,paste(datatype,analysis,"trends.csv",sep=""),row.names=F) #write the final
}
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
  mod<-lm(as.numeric(isitu[1:nrow(isitu),i])~as.numeric(1985:2009))
  try(slope[i]<-coefficients(mod)[2])
  try(temp<-summary(mod))
  try(rsquared[i]<-temp[9])
  try(p[i]<-temp$coefficients[2,4])
  noyear[i]<-length(na.exclude(as.numeric(isitu[1:nrow(isitu),i])))
}
missing<-data.frame("missing years"=21-noyear, "lake"=names(isitu))
#create data frame with mann kendall results
res<-data.frame("Lake"=names(isitu), "No years data"=noyear, "rsquared"=unlist(rsquared), "Slope"=slope, "pval"=round(p,3))

#put together a csv with metadata and the lm results
newres<-cbind(res,t(holder[1:19,])) #final results in situ with metadata
names(newres)[6:ncol(newres)]<-t(master[1:19,1])
write.csv(newres,paste(datatype,analysis,"trends.csv",sep=""),row.names=F) #write the final
}

#---------------------------------------------------------------------------------------
#plot trends on map
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

#comparisons between in situ lakes and sat lakes
insit<-newres
satlake<-newres

#Remove in situ lake duplicates
insit<-insit[-which(insit$Lake=="Huron.B45008"),]
insit<-insit[-which(insit$Lake=="Superior.B45001"),]
insit<-insit[-which(insit$Lake=="Superior.B45006"),]
insit<-insit[-which(insit$Lake=="Tahoe.TB1"),]
insit<-insit[-which(insit$Lake=="Michigan.B45002"),]

insit[,14]<-as.numeric(levels(insit[,14])[insit[,14]])
satlake[,14]<-as.numeric(levels(satlake[,14])[satlake[,14]])

#histogram of surface areas
library(ggplot2)
isSA<-data.frame("Surface Area"=log10(insit[,14]))
satSA<-data.frame("Surface Area"=log10(satlake[,14]))
isSA$type<-"insitu"
satSA$type<-"sat"
combined<-rbind(isSA,satSA)
ggplot(combined, aes(Surface.Area, fill = type)) + geom_histogram(alpha = 0.5, xlab="Surface Area")

#comparison of trends
xrange<-range(log10(append(insit[,14], satlake[,14])))
yrange<-range((append(insit$Sen.slope, satlake$Sen.slope)))
plot(insit$Sen.slope~(log10(insit[,14])), xlim=xrange,ylim=yrange, col="blue",pch=16, xlab="log(Surface Area)", ylab="Trend (Sen Slope)")
points(satlake$Sen.slope~(log10(satlake[,14])), col="red",pch=16)

#spatial autocorrelation?

library(fields)
#in situ data
locs<-data.frame(as.numeric(as.character(master[,6])),as.numeric((as.character(insit[,5])))) #dataframe of long and lat
result<-rdist.earth(locs,miles=F) #distance matrix of distances on earth
trendmatrix<-dist(insit[,4], method = "euclidean") #distance matrix of trends
library(ade4)
spamod<-mantel.rtest(as.dist(result), trendmatrix, nrepet = 9999) #mantel tests



#sat data
locs<-data.frame(as.numeric(as.character(satlake[,10])),as.numeric(as.character(satlake[,9]))) #dataframe of long and lat
result<-rdist.earth(locs,miles=F) #distance matrix of distances on earth
trendmatrix<-dist(satlake[,4], method = "euclidean") #distance matrix of trends
library(ade4) 
spamod<-mantel.rtest(as.dist(result), trendmatrix, nrepet = 9999) #mantel tests



#other stuff

library(plotrix)

library(mapdata)


#convince myself that the colours are lining up correctly
barplot(slope, col=mapcols)


#take only significant changes
new<-datin[,which(pval<0.05)]
slope<-slope[which(pval<0.05)]

#factors influencing warming?
cor.test(as.numeric(new[4,]),slope, method="pearson") #latitude
cor.test(as.numeric(new[5,]),slope, method="pearson") #longitude
cor.test(as.numeric(new[6,]),slope, method="pearson") #elevation
cor.test(as.numeric(new[7,]),slope, method="pearson") #mean depth
cor.test(as.numeric(new[8,]),slope, method="pearson") #max depth
cor.test(as.numeric(new[9,]),slope, method="pearson") #surface area


#all lakes including those with non-significant trends
#factors influencing warming?
cor.test(as.numeric(datin[4,]),slope, method="pearson") #latitude
cor.test(as.numeric(datin[5,]),slope, method="pearson") #longitude
cor.test(as.numeric(datin[6,]),slope, method="pearson") #elevation
cor.test(as.numeric(datin[7,]),slope, method="pearson") #mean depth
cor.test(as.numeric(datin[8,]),slope, method="pearson") #max depth
cor.test(as.numeric(datin[9,]),slope, method="pearson") #surface area

#plots
par(mfrow=c(2,3))
plot(slope~as.numeric(datin[4,]), xlab="Latitude", ylab="Sen slope")
plot(slope~as.numeric(datin[5,]),xlab="Longitude", ylab="Sen slope")
plot(slope~as.numeric(datin[6,]), xlab="Elevation", ylab="Sen slope")
plot(slope~as.numeric(datin[7,]), xlab="Mean depth", ylab="Sen slope")
plot(slope~as.numeric(datin[8,]), xlab="Maximum depth", ylab="Sen slope")
plot(slope~as.numeric(datin[9,]),xlab="Surface area", ylab="Sen slope")

hist(as.numeric(datin[4,]), xlab="Latitude")
hist(as.numeric(datin[5,]),xlab="Longitude")
hist(log10(as.numeric(datin[6,])), xlab="Elevation") #log transformed
hist(log10(as.numeric(datin[7,])), xlab="Mean depth")
hist(log10(as.numeric(datin[8,])), xlab="Maximum depth")
hist(log10(as.numeric(datin[9,])),xlab="Surface area")

#significant autocorrelation in residuals?
library(lmtest)
auto<-c()
for (i in 1: ncol(datin)){
mod<-lm(as.numeric(datin[134:159,i])~as.numeric(master[134:159,1]))
  auto[i]<-try(dwtest(mod)[4])}


#regression using lm 

slope<-c()
rsquared<-c()
p<-c()
for (i in 1: ncol(datin)){
  slope[i]<-NA
  rsquared[i]<-NA
  p[i]<-NA
  mod<-lm(as.numeric(datin[134:159,i])~as.numeric(master[134:159,1]))
  try(slope[i]<-coefficients(mod)[2])
  try(temp<-summary(mod))
  try(rsquared[i]<-temp[9])
  try(p[i]<-temp$coefficients[2,4])
  }

# All Subsets Regression
library(leaps)

results<-regsubsets(slope~as.numeric(datin[4,])+as.numeric(datin[5,])+as.numeric(datin[6,])+as.numeric(datin[7,]),as.numeric(datin[8,]),as.numeric(datin[9,]),nbest=10)
# view results 
summary(leaps)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps,scale="r2")
# plot statistic by subset size 
library(car)
subsets(leaps, statistic="rsq")

names(datin)[which(is.na(as.numeric(datin[8,]))==T)]


########extract NCEP data for a square that coforms to the area of the lake
#NCEP data using RNCEP
# Calculates the geodesic distance between two points specified by radian latitude/longitude using the

# Haversine formula (hf)
gcd.hf <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1) #need to solve for long2
  delta.lat <- (lat2 - lat1) #need to solve for lat2
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}

library(RNCEP)
library(tcltk)


loc<-data.frame(as.numeric(master[5,3:ncol(master)]), as.numeric(master[6,3:ncol(master)]))
names(loc)<-c("Latitude", "Longitude")
offset<-(sqrt(as.numeric(master[10,3:ncol(master)]))*1000)/2

#for one particular lake
ref<-grep("Ontario.B45135",names(master))
loc<-data.frame(as.numeric(master[5,ref]),as.numeric(master[6,ref]))
names(loc)<-c("Latitude", "Longitude")
offset<-(sqrt(as.numeric(master[10,ref]))*1000)/2

latneeded<-function(latitude,offset){ #offset in meters
#Earth’s radius, sphere
R=6378137
#Coordinate offsets in radians
dLat = offset/R
#offset position decimal degrees
lat0 = latitude + dLat * 180/pi
lat1 = latitude - dLat * 180/pi
return(c(lat1,lat0))
}

lonneeded<-function(longitude,offset,latitude){
  R=6378137
  dLon = offset/(R*cos(pi*latitude/180))
  lon0 = longitude + dLon * (180/pi)
  lon1 = longitude - dLon * (180/pi)
  
  return(c(lon1,lon0))
}


library(doBy)
library(RNCEP)
for (i in 1: nrow(loc)){

if (as.numeric(loc[i,1])<23.5&as.numeric(loc[i,1])>0){monthsuse<-c(1,3)}
if (as.numeric(loc[i,1])>(-23.5)&as.numeric(loc[i,1])<0){monthsuse<-c(6,8)}
if (as.numeric(loc[i,1])>23.5){monthsuse<-c(6,8)}
if (as.numeric(loc[i,1])<(-23.5)){monthsuse<-c(1,3)}
                              
data<-NCEP.gather(variable="air.2m","gaussian",months.minmax=monthsuse,years.minmax=c(1979,2011),lat.southnorth=latneeded(loc[i,1],offset[i]), lon.westeast=lonneeded(loc[i,2],offset[i],loc[i,1]), reanalysis2 =T )
NCEPdt1<-NCEP.array2df(NCEP.aggregate(wx.data=data, YEARS=TRUE, MONTHS=FALSE, DAYS=FALSE, HOURS=FALSE, fxn="mean"))

if(i==1){NCEPdata<-data.frame(as.vector(summaryBy(variable1~datetime, data=NCEPdt1)[,2]))}
if(i>1){NCEPdata[,i]<-as.vector(summaryBy(variable1~datetime, data=NCEPdt1)[,2])}
}

#add ncep data to master file
ncepfinal<-NCEPdata-273.15
write.csv(ncepfinal,"NCEPdata.csv")
master[158:190,3:ncol(master)]<-ncepfinal[,2:ncol(ncepfinal)]

#write new file
write.csv(master,"master file.csv",row.names=F)


