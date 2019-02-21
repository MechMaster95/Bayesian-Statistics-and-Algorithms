ozone=read.csv("ozone.csv",header=TRUE)
attach(ozone)
summary(ozone)
dim(ozone)
###question 1
mean_centres=rowMeans(ozone[,1:31],na.rm = TRUE)
length(mean_centres)
std_centres=apply(ozone[,1:31],1,sd,na.rm = TRUE)
length(std_centres)
std_days=apply(ozone[,2:32],2,sd,na.rm=TRUE)
std_days
mean_days=apply(ozone[,2:32],2,mean,na.rm=TRUE)
mean_days
mean_centres1=apply(ozone[,2:32],1,mean,na.rm=TRUE)
mean_centres1
length(mean_centres1)



