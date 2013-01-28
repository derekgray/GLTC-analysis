master<-read.csv(file.choose(),stringsAsFactors=F)

datin<-master[,which(master[15,]=="Y")] #in situ yes
datin<-cbind(master[,1:2],datin)

#how many years of missing data?
for (i in 1:ncol(datin)){
  noyear[i]<-25-length(na.exclude(as.numeric(datin[dt1:dt2,i]))) 
}

#only take lakes with less than x years missing data
datin<-datin[,which(noyear<=5)] #take lakes with 5 or fewer missing years
holder<-datin

#replace NA values with averages
for (i in 3:ncol(datin)){
  dta<-as.numeric(datin[140:164,i])
  dta[which(is.na(dta)==T)]<-mean(dta,na.rm=T)
  if(i==3){isituavg<-data.frame(dta)}
  if(i>3){isituavg[,i-2]<-dta}
}

names(isituavg)<-names(datin)[3:ncol(datin)]
datin[140:164,3:ncol(datin)]<-isituavg

#make dataframe needed for lm analysis
for(i in 140:164){
  a<-(names(datin[1,3:ncol(datin)]))
  b<-as.numeric(datin[i,3:ncol(datin)])
  c<-rep(datin[i,1],(ncol(datin)-2))
  d<-datin[4,3:ncol(datin)]
  temp<-data.frame(a,b,c,d)
  if(i==140){test<-temp}
  if(i>140){test<-rbind(test,temp)}
  }
names(test)<-c("Lake","Temperature","Year","Region")


test$Temperature<-as.numeric(test$Temperature)
test$Year<-as.factor(test$Year)
test$Lake<-as.factor(test$Lake)

g1 <- lm(Temperature~as.numeric(Year)+Lake,data=test)
anova(g1)
summary(g1)


library(nlme)

g <- gls(Temperature~Year,data=test,correlation = corAR1(form= ~1),method="ML")
resu<-summary(g)
page(resu, method = "print")
summary(g)$tTable

#how to get global p value for test
fit1 <- gls(Temperature~ 1,data=test, correlation = corAR1(form= ~1),method="ML")
anova(fit1, g)


fit1=gls(height~year, data=labdata, correlation = corAR1(form=~1))