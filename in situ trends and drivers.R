library(rpart)
library(vegan)
library(packfor)


#**************************
#Input and Manipulate Data
#**************************


insitu = read.table("insitutrendsanddrivers.txt", header=TRUE, row.names=1, sep="\t")
head(insitu)
log.elevation=log(insitu[,4])
log.elevation
log.meandepth=log(insitu[,5])
log.meandepth
log.maxdepth=log(insitu[,6])
log.area=log(insitu[,7])
env=cbind(insitu[,c(2:3)], insitu[,8], log.elevation, log.meandepth, log.maxdepth, log.area)
env
insitu.std=cbind(insitu[,1], env)
insitu.std



#Multiple regression  #insitu [,1] = swt trends; insitu[,8]= NCEP data
#-------------------

forward.insitu=forward.sel(insitu[,1], env, alpha=0.1)
forward.insitu
#No variables selected
#linear model assuming linear distribution
lm.insitu= lm(insitu[,1]~latitude + longitude + log.elevation + log.meandepth + log.maxdepth + log.area + insitu[,8], data=as.data.frame(insitu.std))
summary(lm.insitu)


#Regression tree for SWT trends
#--------------------------------
library(rpart)
tree.insitu<-rpart(SWT.trend~latitude + longitude + elevation + meandepth + maxdepth + area + NCEPtrend, minsplit=6, xval=10, data=insitu)
tree.insitu #what are the mean values for the response variable (last one in each row)
summary(tree.insitu) #split number and relative error (1-r2)
print (tree.insitu)
plot(tree.insitu)
text(tree.insitu, use.n=TRUE)
rsq.rpart(tree.insitu)
plotcp(tree.insitu) #how many levels (depth = size of tree e.g. 3-7)
complexity.parameter<-tree.insitu$cptable[which.min(tree.insitu$cptable[,"xerror"]),"CP"]
complexity.parameter #lower complexity better?
tree.insitu$cptable
relerr.insitu=tree.insitu$cptable[,"rel error"]
relerrmin.insitu=min(relerr.insitu)
rsq.insitu=1-relerrmin.insitu
rsq.insitu

#Pruned Tree - Use the size of the tree of 5 (although could change the max depth value based on complexity parameter)


tree.insitu<-rpart(SWT.trend~latitude + longitude + elevation + meandepth + maxdepth + area + NCEPtrend, minsplit=6, maxdepth = 5, xval=10, data=insitu)
tree.insitu
summary(tree.insitu)
print (tree.insitu)
plot(tree.insitu)
text(tree.insitu, use.n=TRUE)
rsq.rpart(tree.insitu)
plotcp(tree.insitu)
complexity.parameter<-tree.insitu$cptable[which.min(tree.insitu$cptable[,"xerror"]),"CP"]
complexity.parameter
tree.insitu$cptable
relerr.insitu=tree.insitu$cptable[,"rel error"]
relerrmin.insitu=min(relerr.insitu)
rsq.insitu=1-relerrmin.insitu
rsq.insitu

#Cluster Analysis
#----------------

#Cluster Analysis with trends, latitude and longitude
#----------------------------------------------------
#Didn't work well

swt.cascade=cascadeKM(insitu[,c(1:3)], inf.gr=2, sup.gr=10, iter=100, criterion="calinski")
swt.cascade
summary(swt.cascade)
plot(swt.cascade)
swt.cascade$partition

#Cluster Analysis with the raw data

rawinsitu = read.table("rawinsitu.txt", header=TRUE, row.names=1, sep="\t")
rawinsitu.tr = read.table("rawinsitutransposed.txt", header=TRUE, row.names=1, sep="\t")
head(rawinsitu.tr)
head(rawinsitu)
swt.cascade=cascadeKM(rawinsitu[c(1:25),], inf.gr=2, sup.gr=10, iter=100, criterion="calinski")
swt.cascade=cascadeKM(rawinsitu.tr[, c(1:25)], inf.gr=2, sup.gr=10, iter=100, criterion="calinski")
swt.cascade
summary(swt.cascade)
plot(swt.cascade)
a=swt.cascade$partition
write.csv(a, "Kmeans.cluster.csv")


