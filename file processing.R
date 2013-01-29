setwd("/Users/gray/Dropbox/Processed Data (with curve-fit for extrapolation)")
files<-list.files(path="/Users/gray/Dropbox/Processed Data (with curve-fit for extrapolation)")
library(stringr)
lakenames<-files
"%w/o%" <- function(x, y) x[!x %in% y] #--  x without y
lakenames<-lakenames %w/o% grep("png",lakenames,value=T) #remove png image file names
lakenames<-lakenames %w/o% grep("log",lakenames,value=T) #remove log file names
lakenames<-lakenames %w/o% grep("DWA",lakenames,value=T) #remove random file names with DWA

#FIRST MAKE A DATAFRAME OF JFM VALUES
winterlakenames<-grep("JFM", lakenames, value=T)
library(xlsx)
library(gdata)
temp<-data.frame("Year"=1900:2012)
available.depths<-list()
data.quality<-data.frame("Lake"=character(), "maxgap"=numeric(), "sd.maxgap"=numeric(),"meangap"=numeric(),"sd.meangap"=numeric(), "nogaps"=numeric(),"sd.nogaps"=numeric(), stringsAsFactors=FALSE)

for (i in 1:length(winterlakenames)){

#loop over all lakes and add temps to the "temp" dataframe
res <- read.xlsx(winterlakenames[i], 1)
temp.lake<-data.frame("Year"=res$Year, "Temp"=res$JFM_mean)
names(temp.lake)<-c("Year",winterlakenames[i])
temp.accum<-merge(temp,temp.lake,by="Year", all.x=T)
temp<-temp.accum

#loop over all lakes and make list of available depths
#sheetCount(winterlakenames[i])
available.depths[[i]]<-sheetNames(winterlakenames[i])

#loop over all lakes and create a dataframe of missing data characteristics
gaps<-c(winterlakenames[i],mean(res$max.gap,na.rm=T),sd(res$max.gap,na.rm=T),mean(res$mean.gap,na.rm=T),sd(res$mean.gap,na.rm=T),mean(res$number.of.gaps,na.rm=T),sd(res$number.of.gaps,na.rm=T))
data.quality[1,]<-gaps

}

temp[[i]]<-res$JFM_mean
year[[i]]<-
}
names(temp)

#filter file names to end up with lake name 
lakenames<-str_replace_all(files, "[^[:alnum:]]", " ")
lakenames<-gsub("\\d", "", lakenames) #remove numbers from file names
lakenames<-gsub("GLTC","",lakenames) #remove GLTC from file names
lakenames<-gsub("JFM","",lakenames) #remove GLTC from file names
lakenames<-gsub("JAS","",lakenames) #remove GLTC from file names
lakenames<-gsub("xls","",lakenames) #remove GLTC from file names
lakenames<-str_trim(lakenames, side = "both")

grep("JAS", lakenames)

