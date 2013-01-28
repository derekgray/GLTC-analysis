#Take interpolated temp data from U of Delaware and make it into a dataframe


#######Functions
# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Haversine formula (hf)
gcd.hf <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}

geo.dist <- function(point1, point2)
{
  R <- 6371
  p1rad <- point1 * pi/180
  p2rad <- point2 * pi/180
  d <- sin(p1rad[2])*sin(p2rad[2])+cos(p1rad[2])*cos(p2rad[2])*cos(abs(p1rad[1]-p2rad[1]))  
  d <- acos(d)
  R*d
}

deg2rad <- function(deg) return(deg*pi/180) #covert degrees to radians

#read all of the data in
bob<-list()
x=0
for (year in 1900:2010){
x=x+1
source<-paste("air_temp.",year,sep="")
ff<-file(description=source,open="r")
bob[[x]]<-read.fortran(ff, c("F8.0","F8.0","12F8.0"))
unlink(ff)
}

#grid locations
lats<-bob[[1]][,2]
lons<-bob[[1]][,1]

byLake<-list()
for (w in 3:ncol(master)){ #master is the csv file with all the lake data
loc<-as.numeric(c(master[4,w], master[5,w]))

ds<-c()
for (i in 1:length(lats)){
ds[i]<-geo.dist(point1=c(lons[i],lats[i]),point2=c(loc[2],loc[1]))
  #ds[i]<-gcd.hf(deg2rad(lons[i]), deg2rad(lats[i]),deg2rad(loc[2]),deg2rad(loc[1]))
}

bestlat<-lats[which.min(ds)]
bestlon<-lons[which.min(ds)]

JJAtemp<-c()
for (b in 1:length(bob)){
all<-bob[[b]][which(bob[[b]][,2]==bestlat&bob[[b]][,1]==bestlon),][8:10]
JJAtemp[b]<-(all[1]*30+all[2]*31+all[3]*31)/92
}

byLake[[w]]<-JJAtemp
}

df <- data.frame(matrix(unlist(byLake), nrow=length(byLake[[3]]), byrow=F))
names(df)<-names(master)[3:ncol(master)]


#plot location of lakes

library(mapproj)

datapts<-mapproject(as.numeric(master[5,3:ncol(master)]),as.numeric(master[4,3:ncol(master)]),projection="rectangular", parameters=100, orientation=NULL)
map("world",proj="rectangular",parameters=100, xlim=(datapts$range[1:2]*1.2),ylim=(datapts$range[3:4]*1.2)) #,parameters=100) #,orient=c(0,-100,0)) 
points(datapts, col="red")

#plot individual points

datapt1<-mapproject(as.numeric(master[5,4]),as.numeric(master[4,4]),projection="rectangular", parameters=100, orientation=NULL)
datapt2<-mapproject(as.numeric(bestlon),as.numeric(bestlat),projection="rectangular", parameters=100, orientation=NULL)

map("world",proj="rectangular",parameters=100, xlim=(datapts$range[1:2]*1.2),ylim=(datapts$range[3:4]*1.2)) #,parameters=100) #,orient=c(0,-100,0)) 
points(datapt1, col="red")
points(datapt2, col="blue")


text(data)

plot(as.numeric(master[5,3:ncol(master)]),as.numeric(master[4,3:ncol(master)]))
