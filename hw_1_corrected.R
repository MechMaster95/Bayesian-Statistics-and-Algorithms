ozone=read.csv("ozone.csv",header=TRUE)
##################################################
#########First question###########################
df_1={}
df_1$col1=mean(as.matrix(ozone[,2:32]),na.rm=T)
df_1$col2= sd(as.matrix(ozone[,2:32]),na.rm=T)
df_1$col3=percentage_na=mean(is.na(ozone))*100
#df1=apply_labels(df_1,df_1$col1="Mean",df_1$col2="Standard Deviation",df_1$col="Percentage of Null")
df_2=data.frame(Overall_mean=df_1$col1,Overall_Std_Dev=df_1$col2,Overall_Null_Percentage=df_1$col3)
##################################################
#########Second Question##########################
#######Without using loops########################

mean_centres=rowMeans(ozone[,2:32],na.rm = TRUE)
std_centres=apply(ozone[,2:32],1,sd,na.rm = TRUE)
site_var=std_centres^2
na_centres=rowMeans(is.na(ozone[,2:32]))*100
################################################
################################################
## Second Question ##with loops#################
################################################
################################################
no_rows = nrow(ozone)
n_col= ncol(ozone)
row_mean<-list()
r=rowSums(is.na(ozone))
###############row_means loop################################################
for(i in 1:no_rows){ 
  sum = 0
  for(j in 2:n_col){
    if (is.na(ozone[i,j])){
      next
    }
    else{
      sum = (sum + ozone[i,j])
      
    }
  }
  row_mean <- append(row_mean,sum/(n_col-r[i]-1))
}
df_mean <- data.frame(col1 = matrix(unlist(row_mean), nrow=1106, byrow=T),stringsAsFactors=FALSE)
############################variance loop###################################
row_variance<-list()
for(i in 1:no_rows){ 
  var_sum=0
    for(j in 2:n_col){
    if (is.na(ozone[i,j])){
      next
    }
    else{
      temp=(ozone[i,j]-as.double(row_mean[i]))^2
      var_sum=temp+var_sum
      
    }
    
  }
  row_variance <- append(row_variance,var_sum/(n_col-r[i]-2))
}

row_variance[100]
site_var[100]
df_var <- data.frame(col1 = matrix(unlist(row_variance), nrow=1106, byrow=T),stringsAsFactors=FALSE)

########################Percentage of null loop#######################
per_null<-list()
sum_2=0
for(i in 1:no_rows){ 
  sum_2 = 0
  for(j in 2:n_col){
    if (is.na(ozone[i,j])){
      sum_2=sum_2+1
    }
    else{
      next
      
    }
    
  }
  per_null <- append(per_null,(sum_2/(n_col-1))*100)
}

###########################################################
per_null[1]
df_per <- data.frame(col1 = matrix(unlist(row_variance), nrow=1106, byrow=T),stringsAsFactors=FALSE)

###########################################################
par(mfrow=c(1,3))
hist(df_mean$col1,xlab="Row Mean",main="Means of Rows",col="light blue")
hist(df_var$col1,xlab="Variance",main="Variances of Rows",col="light blue")
hist(df_per$col1,xlab="Null Percentage",main="Percetages of Null of Rows",col="light blue")
par(mfrow=c(1,3))
plot(mean_centres,site_var,pch=19,xlab="Means of Rows",ylab="Variance of rows")
plot(site_var,na_centres,pch=19,ylab="Percetages of Null of Rows",xlab="Variances of Rows")
plot(mean_centres,na_centres,pch=19,ylab="Percetages of Null of Rows",xlab="Means of Rows")

###########################################################
##############Third Question###############################

fit3=lm(mean_centres~(site_var+na_centres))
par(mfrow=c(1,2))
plot(fit3,1:2,pch=19)
summary(fit3)
##########################################################
##########################################################



