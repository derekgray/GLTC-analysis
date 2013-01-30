setwd("/Users/gray/Dropbox/Processed Data (with curve-fit for extrapolation)")
files<-list.files(path="/Users/gray/Dropbox/Processed Data (with curve-fit for extrapolation)")
library(stringr)
lakenames<-files
"%w/o%" <- function(x, y) x[!x %in% y] #--  x without y
lakenames<-lakenames %w/o% grep("png",lakenames,value=T) #remove png image file names
lakenames<-lakenames %w/o% grep("log",lakenames,value=T) #remove log file names
lakenames<-lakenames %w/o% grep("DWA",lakenames,value=T) #remove random file names with DWA

#STEP 1, SELECT BEST DEPTHS FOR ANALYSIS AND MAKE A DATAFRAME
#this is for JFM
winterlakenames<-grep("JFM", lakenames, value=T)
library(xlsx)
library(gdata)
temp<-data.frame("Year"=1900:2012)
available.depths<-list()
gap.characteristics<-data.frame("Lake"=character(),"Best.Depth"=numeric(),"Avail.depths"=character(), "maxgap"=numeric(), "sd.maxgap"=numeric(),"meangap"=numeric(),"sd.meangap"=numeric(), "nogaps"=numeric(),"sd.nogaps"=numeric(), stringsAsFactors=FALSE)

for (i in 1:length(winterlakenames)){

  #loop over all lakes and make list of available depths
  #sheetCount(winterlakenames[i])
  available.depths[[i]]<-as.numeric(gsub("[a-z_]", "", sheetNames(winterlakenames[i])))
  surface.depths<-available.depths[[i]][which(available.depths[[i]]<5)]
  
  #This part reads in sheets for depths less than 5m and checks to see which has the best missing data characteristics
  if (length(surface.depths)>1){ #only run if more than one depth is available
  data.quality<-data.frame("Depth"=numeric(), "maxgap"=numeric(), "sd.maxgap"=numeric(),"meangap"=numeric(),"sd.meangap"=numeric(), "nogaps"=numeric(),"sd.nogaps"=numeric(), stringsAsFactors=FALSE)
  years<-c()
  for (j in 1:length(surface.depths)){
    sheet <- read.xlsx(winterlakenames[i], which(available.depths[[i]]==surface.depths[j]),colClasses="numeric")
    years[j]<-length(which(sheet$Year>1984))
    data.quality[j,]<-c(surface.depths[j],round(mean(sheet$max.gap,na.rm=T),2),round(sd(sheet$max.gap,na.rm=T),2),round(mean(sheet$mean.gap,na.rm=T),2),round(sd(sheet$mean.gap,na.rm=T),2),round(mean(sheet$number.of.gaps,na.rm=T),2),round(sd(sheet$number.of.gaps,na.rm=T),2))
    }
 #calculate percent difference in number of years from depth with most years >1984
  percent.diff<-years[which(1:length(years)!=which.max(years))]/years[which.max(years)]
  percent.diff.all<-years/years[which.max(years)]
 
#Check to see if number of years available in next best depth is less than 0.8 of best depth.
#If so, stick with the best depth
  if (max(percent.diff)<0.8){
    best.sheet<-which(years==max(years))
  }

#If next best depth has at least 80% of the years available in the best depth, then
#compare in terms of their gap characteristics to decide which depth is best
  if (max(percent.diff)>0.8){
  data.quality<-data.quality[which(percent.diff.all>0.8),]
  best.depth<-data.quality$Depth[which.min(rowMeans(apply(data.quality[,-1], FUN=rank,MARGIN=2)))]
  best.sheet<-which(available.depths[[i]]==best.depth)
  }
}
  if (length(surface.depths)==1){best.sheet<-1
  best.depth<-surface.depths[1]}  
      
#loop over all lakes and add temps to the "temp" dataframe
res <- read.xlsx(winterlakenames[i], best.sheet)
temp.lake<-data.frame("Year"=res$Year, "Temp"=res$JFM_mean)
names(temp.lake)<-c("Year",winterlakenames[i])
temp.accum<-merge(temp,temp.lake,by="Year", all.x=T)
temp<-temp.accum
  
#make a dataframe with missing data characteristics for the depth chosen
  gap.characteristics[i,]<-c(winterlakenames[i],best.depth,paste(available.depths[[i]],collapse=","),round(mean(res$max.gap,na.rm=T),2),round(sd(res$max.gap,na.rm=T),2),round(mean(res$mean.gap,na.rm=T),2),round(sd(res$mean.gap,na.rm=T),2),round(mean(res$number.of.gaps,na.rm=T),2),round(sd(res$number.of.gaps,na.rm=T),2))

}

write.csv(temp, file="temps.csv")
write.csv(gap.characteristics,"gaps_data.csv")



#STEP 2, SELECT BEST STATIONS FOR LAKES THAT HAVE MULTIPLE STATIONS


#filter file names to end up with lake name 
winterlakenames<-grep("JFM", lakenames, value=T)
lakes<-gsub("GLTC","",winterlakenames) #remove GLTC from file names
lakes<-gsub("JFM","",lakes) #remove JFM from file names
lakes<-gsub("JAS","",lakes) #remove JAS from file names
lakes<-gsub("xls","",lakes) #remove xls from file names
lakes<-gsub("Reservoir","",lakes) #remove reservoir from file names
lakes<-gsub("NTL","",lakes) #remove NTL from file names
lakes<-gsub("[_-]", "",lakes) #remove _ and - from file names
lakes<-gsub("[.]", "",lakes) #remove . from file names
lakes<-sort(lakes)

uniquelakes<-unique(fix(lakes)) #manually edit to remove dupliate lake names

lake.groups<-list()
for (i in 1:length(uniquelakes)){
  lake.groups[[i]]<-grep(uniquelakes[i],lakes,value=T)
}

multi.station.lakes<-uniquelakes[which(lapply(lake.groups, function(x) length(x))>1)]

for (i in 1:length(mult.station.lakes)){
#first pick the stations with the most years after 1985
all.stations<-temp[85:113,agrep(multi.station.lakes[i],names(temp))]
no.years<-sapply(all.stations, function(x) length(which(x!="NA" )))
all.stations<-all.stations[,which(no.years>max(no.years)*0.8)]

#next gather data on gap lengths
data.quality<-gap.characteristics[which(gap.characteristics[,1]%in%names(all.stations)),]
data.quality$Lake[which.min(rowMeans(apply(data.quality[4:9,-1], FUN=rank,MARGIN=2)))]
