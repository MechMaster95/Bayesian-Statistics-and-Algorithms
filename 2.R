n=20
sse=15
##calculated in the attached notes

#shape=a
a<-c(0.1,1,0.1,1)
#rate=b
b<-c(0.1,1,0.1,1)
#given c
c<- c(1,1,2,2)

sample_size=1000000
matrix_data = matrix(0,sample_size,4)

temp_matrix= matrix(0,sample_size,4)
for(i in 1:4){
  matrix_data[,i]=1/rgamma(sample_size,shape=(n/2)+a[i],rate=(sse/2)+b[i])
  
#create 0 and 1 matrix by comparing c value
  for(j in 1:sample_size){
    temp=matrix_data[j,i]
    if(temp > c[i]){
      temp_matrix[j,i]=1
    }
  }
  
}

tempmat_summary<-colSums(temp_matrix,na.rm= F, dims = 1)

#calculating the proportion
tempmat_proportion<- tempmat_summary/sample_size

print(tempmat_proportion)

#checking the sensitivity and calculating the ratio between 4a and 4b
ratio1<-tempmat_proportion[1]/tempmat_proportion[2]
ratio2<-tempmat_proportion[3]/tempmat_proportion[4]
print(ratio1)
print(ratio2)