###Approach 1 -------------------------------------------------------------------

#only use significant trends in analyses?
sigtrends<-"N"

insitu.trends<-read.csv("insitumktrends.csv",row.names=NULL,stringsAsFactors=F) #file with trends for in situ lakes
NCEP.insitu<-read.csv("NCEPtrendsmkinsitu.csv",row.names=NULL,stringsAsFactors=F) #file with NCEP trends for in situ lakes
sat.trends<-read.csv("satmktrends.csv",row.names=NULL,stringsAsFactors=F) #file with trends for in sat lakes
NCEP.sat<-read.csv("NCEPtrendsmksat.csv",row.names=NULL,stringsAsFactors=F) #file with NCEP trends for sat lakes


#remove duplicates for in situ measurements
insitu.trends<-insitu.trends[-which(insitu.trends$Lake=="Huron.B45008"),]
insitu.trends<-insitu.trends[-which(insitu.trends$Lake=="Superior.B45001"),]
insitu.trends<-insitu.trends[-which(insitu.trends$Lake=="Superior.B45006"),]
insitu.trends<-insitu.trends[-which(insitu.trends$Lake=="Tahoe.TB1"),]
insitu.trends<-insitu.trends[-which(insitu.trends$Lake=="Michigan.B45002"),]
NCEP.insitu<-NCEP.insitu[-which(NCEP.insitu$Lake=="Huron.B45008"),]
NCEP.insitu<-NCEP.insitu[-which(NCEP.insitu$Lake=="Superior.B45001"),]
NCEP.insitu<-NCEP.insitu[-which(NCEP.insitu$Lake=="Superior.B45006"),]
NCEP.insitu<-NCEP.insitu[-which(NCEP.insitu$Lake=="Tahoe.TB1"),]
NCEP.insitu<-NCEP.insitu[-which(NCEP.insitu$Lake=="Michigan.B45002"),]

#remove duplicates for sat measurements
sat.trends<-sat.trends[-which(sat.trends$Lake=="Huron.B45008"),]
sat.trends<-sat.trends[-which(sat.trends$Lake=="Superior.B45001"),]
sat.trends<-sat.trends[-which(sat.trends$Lake=="Superior.B45006"),]
sat.trends<-sat.trends[-which(sat.trends$Lake=="Tahoe.TB1"),]
sat.trends<-sat.trends[-which(sat.trends$Lake=="Michigan.B45002"),]
NCEP.sat<-NCEP.sat[-which(NCEP.sat$Lake=="Huron.B45008"),]
NCEP.sat<-NCEP.sat[-which(NCEP.sat$Lake=="Superior.B45001"),]
NCEP.sat<-NCEP.sat[-which(NCEP.sat$Lake=="Superior.B45006"),]
NCEP.sat<-NCEP.sat[-which(NCEP.sat$Lake=="Tahoe.TB1"),]
NCEP.sat<-NCEP.sat[-which(NCEP.sat$Lake=="Michigan.B45002"),]

if (sigtrends=="Y"){
  sat.trends.1<-sat.trends[which(sat.trends$pval<=0.1&NCEP.sat$pval<=0.1),]
  NCEP.sat<-NCEP.sat[which(sat.trends$pval<=0.1&NCEP.sat$pval<=0.1),]
  insitu.trends.1<-insitu.trends[which(insitu.trends$pval<=0.1&NCEP.insitu$pval<=0.1),]
  NCEP.insitu<-NCEP.insitu[which(insitu.trends$pval<=0.1&NCEP.insitu$pval<=0.1),]
  sat.trends<-sat.trends.1
  insitu.trends<-insitu.trends.1
}

#plot lake trend vs. ncep trend for in situ measurements
plot(insitu.trends$Slope~NCEP.insitu$Slope, xlab="NCEP trend",ylab="In situ trend", pch=15)
abline(a=0,b=1, col="red") #add 1:1 line
mod1<-lm(insitu.trends$Slope~NCEP.insitu$Slope)
lines(fitted(mod1)~NCEP.insitu$Slope, col="blue")
legend("topright",legend=c("1:1 line", "linear fit"),col=c("red","blue"),lty=1)
summary(mod1)
confint(mod1) #does slope 1 fall within the confidence interval for the slope?


#how many above 1:1 line? 
insitu.trends$Lake[which(NCEP.insitu$Slope>0&NCEP.insitu$Slope<insitu.trends$Slope)]
#how mamy below 1:1 line?
insitu.trends$Lake[which(NCEP.insitu$Slope>0&NCEP.insitu$Slope>insitu.trends$Slope)]
#how many with NCEP cooling?
insitu.trends$Lake[which(NCEP.insitu$Slope<0)]


#plot lake trend vs. ncep trend for satellite measurements

plot(sat.trends$Slope~NCEP.sat$Slope, xlab="NCEP trend",ylab="Satellite trend", pch=16)
abline(a=0,b=1, col="red") #add 1:1 line

#fit a linear model of sat trends vs. air trends
mod1<-lm(sat.trends$Slope~NCEP.sat$Slope)
lines(fitted(mod1)~NCEP.sat$Slope, col="blue")
legend("topleft",legend=c("1:1 line", "linear fit"),col=c("red","blue"),lty=1)
summary(mod1)
confint(mod1) #does slope 1 fall within the confidence interval for the slope?

#generate a 95% confidence interval and add to plot
b<-predict(mod1, interval="confidence") 
lines(sort(NCEP.sat$Slope), sort(b[,2]), lty=2,col="blue")
lines(sort(NCEP.sat$Slope), sort(b[,3]), lty=2,col="blue")

#test for heteroscadasticity
library(lmtest)
bptest(sat.trends$Slope~NCEP.sat$Slope)

#lilliefor's test for normality
library(nortest)
lillie.test(residuals(mod1))

#qqplot of residuals
library(car)
qqPlot(mod1)

#histogram of residuals vs. normal distribution
library(MASS)
sresid <- studres(mod1) 
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

#Spatial autocorrelation? Plot variogram on residuals
library(geoR)
plot(variog(coords=cbind(spdata$Latitude,spdata$Longitude),data=residuals(mod1)))

#Test for spatial autocorrelation with Moran's I
library(ape)
locs<-data.frame(as.numeric(spdata$Longitude),as.numeric(spdata$Latitude)) #dataframe of long and lat
library(fields)
result<-rdist.earth(locs,miles=F)
library(gdata)
upperTriangle(result)<-lowerTriangle(result) 
result<-1/result #need to use the inverse... high numbers closer, low numbers further away
diag(result)<-rep(0,nrow(result)) #diagonal should be 0
Moran.I(residuals(mod1),result) 

#Mantel test of residuals
result<-rdist.earth(locs,miles=F) #distance matrix of distances on earth
trendmatrix<-dist(residuals(mod1), method = "euclidean") #distance matrix of trends
library(ade4) 
spamod<-mantel.rtest(as.dist(result), trendmatrix, nrepet = 9999) #mantel tests
spamod

#global test of assumptions
library(gvlma)
gvmodel <- gvlma(mod1) 
summary(gvmodel)


#________________________________________________________________________________________
#Plots that divide lakes into quadrants (useful for looking at patterns of warming/cooling)

#quadrant plot for satellite
plot(sat.trends$Slope~NCEP.sat$Slope, xlab="NCEP trend",ylab="Satellite trend", pch=15)

#colour the plot by trends
ssig<-which(sat.trends$pval<=0.1) #sat trend significant?
nsig<-which(NCEP.sat$pval<=0.1) #ncep trend significant?
bsig<-which(sat.trends$pval<=0.1&NCEP.sat$pval<=0.1)#both sat and ncep trend significant?
colrs<-rep("black",times=108)
colrs[ssig]<-"orange"
colrs[nsig]<-"blue"
colrs[bsig]<-"red"
plot(sat.trends$Slope~NCEP.sat$Slope, xlab="NCEP trend",ylab="Satellite trend", pch=15,col=colrs)
abline(h=0, col="red",lty=2); abline(v=0,col="red",lty=2)
text(-0.1, 0.06,"Air cooling, lake warming",col="darkgreen")
text(0.085, 0.1,"Air warming, lake warming",col="darkgreen")
text(-0.1, -0.006,"Air cooling, lake cooling",col="darkgreen")
text(0.1, -0.006,"Air warming, lake cooling",col="darkgreen")
legend("topleft",legend=c("NCEP significant","Water significant","Both significant"),pch=15,col=c("blue","orange","red"))


#Quadrant plot for in situ
plot(insitu.trends$Slope~NCEP.insitu$Slope, xlab="NCEP trend",ylab="Satellite trend", pch=15)
ssig<-which(insitu.trends$pval<=0.1) #sat trend significant?
nsig<-which(NCEP.insitu$pval<=0.1) #ncep trend significant?
bsig<-which(insitu.trends$pval<=0.1&NCEP.insitu$pval<=0.1)#both sat and ncep trend significant?
colrs<-rep("black",times=108)
colrs[ssig]<-"orange"
colrs[nsig]<-"blue"
colrs[bsig]<-"red"
plot(insitu.trends$Slope~NCEP.insitu$Slope, xlab="NCEP trend",ylab="In situ trend", pch=15,col=colrs)
abline(h=0, col="red",lty=2); abline(v=0,col="red",lty=2)
text(-0.03, 0.06,"Air cooling, lake warming",col="darkgreen",cex=0.75)
text(0.085, 0.1,"Air warming, lake warming",col="darkgreen",cex=0.75)
text(-0.03, -0.006,"Air cooling, lake cooling",col="darkgreen",cex=0.75)
text(0.08, -0.006,"Air warming, lake cooling",col="darkgreen",cex=0.75)
legend("topleft",legend=c("NCEP significant","Water significant","Both significant"),pch=15,col=c("blue","orange","red"))


##Approach 2, using hierarchical modeling_______________________________________________________
#allow intercept to vary randomly according to region? (blocking)
master<-read.csv(file.choose(),stringsAsFactors=F)

#sat or insitu? ("sat","insitu")
datatype<-"insitu"

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


noyear<-c()
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

isitu[23:47,]<-isituavg
datin<-cbind(master[,1:2],isitu)

#make dataframe needed for lm analysis
for(i in 23:47){
  a<-(names(datin[1,3:ncol(datin)]))
  b<-as.numeric(datin[i,3:ncol(datin)])
  c<-rep(datin[i,1],(ncol(datin)-2))
  d<-as.character(datin[4,3:ncol(datin)])
  e<-as.numeric(datin[i+150,3:ncol(datin)]) #NCEP data
  f<-as.numeric(datin[5,3:ncol(datin)])
  g<-as.numeric(datin[6,3:ncol(datin)])
  h<-as.numeric(datin[7,3:ncol(datin)])
  j<-as.numeric(datin[8,3:ncol(datin)])
  k<-as.numeric(datin[9,3:ncol(datin)])
  l<-as.numeric(datin[10,3:ncol(datin)])
  m<-as.numeric(datin[11,3:ncol(datin)])
  n<-as.numeric(datin[i+117,3:ncol(datin)])
  
  temp<-data.frame(a,b,c,d,e,f,g,h,j,k,l,m,n)
  if(i==23){test<-temp}
  if(i>23){test<-rbind(test,temp)}
}
names(test)<-c("Lake","Temperature","Year","Region","NCEP","latitude","longitude","elevation","zdepth","zmax","SA","volume","insitu")

test$Year<-as.numeric(as.character(test$Year))
test$NCEP<-as.numeric(as.character(test$NCEP))
test$latitude<-as.numeric(as.character(test$latitude))
test$longitude<-as.numeric(as.character(test$longitude))
test$elevation<-log10(as.numeric(as.character(test$elevation)))
test$zdepth<-log10(as.numeric(as.character(test$zdepth)))
test$zmax<-log10(as.numeric(as.character(test$zmax)))
test$SA<-log10(as.numeric(as.character(test$SA)))
test$volume<-log10(as.numeric(as.character(test$volume)))
test$insitu<-as.numeric(as.character(test$insitu))

test1<-test[,-5]; test1[,13]<-rep("IS",nrow(test1))
test2<-test[,-2]; names(test2)[4]<-"Temperature"; test2[,13]<-rep("NCEP",nrow(test2))
complete<-rbind(test1,test2); names(complete)[13]<-"Type"
complete$Type<-as.factor(complete$Type)


library(nlme)
#Model with lakes nested within region
lme1 = lme(fixed=Temperature~Year*Type,random=~1|Region/Lake,method="ML",na.action=na.exclude,data=complete)
summary(lme1)

#determine percent variance at each level
VC = function(model) {
  vars = as.numeric(VarCorr(model)[,1][c(2,4,5)])
  round(vars/sum(vars),2)
}
data.frame(c("between regions", "between lakes", "within lakes"),VC(lme1))

#model with random effects for lakes
lme2 <- lme(fixed=Temperature~Year*Type,random=~1|Lake,method="ML",na.action=na.exclude,data=complete)
summary(lme2)

anova(lme1,lme2) #model with year better?

#plot variogram on residuals of hierarchical mdoel
library(geoR)
plot(variog(coords=cbind(complete$latitude,complete$longitude),data=residuals(lme2)))

#plot variogram for non-hierarchical linear model for comparison
lm1<-lm(Temperature~Year*Type,data=complete)
plot(variog(coords=cbind(complete$latitude,complete$longitude),data=residuals(lm1)))

#Examine the differences between In situ and air and their interaction with time (year)
pdata <- expand.grid(Year=seq(1985, 2009, by=1), Type=c("IS", "NCEP"))
pdata$Temp <- predict(lme2, pdata, level=0)
plot(pdata$Year,pdata$Temp, type="n",xlab="Year", ylab="Temperature")
points(pdata$Year[1:25], pdata$Temp[1:25], type="b", pch=19, lwd=2)
points(pdata$Year[26:50], pdata$Temp[26:50], type="b", pch=22, lty=2, lwd=2)
legend("topleft", c("In situ","NCEP Air temp"), pch=c(19, 22), lty=c(1,2), lwd=2)



###Approach 3 (not pursued)-----------------------------------------------------------------

library(nlme)
dummy <- rep(1, length(insit[,4]))
spdata <- as.data.frame(cbind(insit[,4],insit[,9],insit[,10], dummy))
names(spdata)<-c("trend","lat","long","dummy")
null.model <- lme(fixed = trend ~ 1, data = spdata, random = ~ 1 | dummy)
summary(null.model)

exp.sp <- update(null.model, correlation = corExp(1, form = ~ lat + long), method = "ML")
summary(exp.sp)

gaus.sp <- update(null.model, correlation = corGaus(1, form = ~ lat + long), method = "ML")
summary(exp.sp)

sph.sp <- update(null.model, correlation = corSpher(1, form = ~ lat + long), method = "ML")
summary(sph.sp)

anova(exp.sp,gaus.sp)
