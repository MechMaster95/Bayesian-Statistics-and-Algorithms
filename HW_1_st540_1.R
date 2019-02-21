ozone=read.csv("ozone.csv",header=TRUE)
attach(ozone)
summary(ozone)
dim(ozone)
###question 1
# mean_centres=rowMeans(ozone[,1:31],na.rm = TRUE)
# length(mean_centres)
# std_centres=apply(ozone[,1:31],1,sd,na.rm = TRUE)
# length(std_centres)
# std_days=apply(ozone[,2:32],2,sd,na.rm=TRUE)
# std_days
# mean_days=apply(ozone[,2:32],2,mean,na.rm=TRUE)
# mean_days
# mean_centres1=apply(ozone[,2:32],1,mean,na.rm=TRUE)
# mean_centres1
# length(mean_centres1)
#####################################################
#########First question##############################
mean(as.matrix(ozone[,2:32]),na.rm=T)
sd(as.matrix(ozone[,1:3]),na.rm=T)
percentage_na=sum(is.na(ozone))/prod(dim(ozone))
##################################################
#########Second Question###########################
mean_centres=rowMeans(ozone[,1:31],na.rm = TRUE)
length(mean_centres)
std_centres=apply(ozone[,1:31],1,sd,na.rm = TRUE)
site_var=std_centres^2
length(std_centres)
na_centres=rowMeans(is.na(ozone[,1:31]))
par(mfrow=c(1,3))
hist(mean_centres)
hist(site_var)
hist(na_centres)
par(mfrow=c(1,3))
plot(mean_centres,site_var,pch=19)
plot(site_var,na_centres,pch=19)
plot(mean_centres,na_centres,pch=19)
#################################################
##############Third Question#####################
fit1=lm(site_var~mean_centres)
fit2=lm(na_centres~mean_centres)
par(mfrow=c(1,2))
plot(fit1,1:2,pch=19)
plot(fit2,1:2,pch=19)

##########################################################