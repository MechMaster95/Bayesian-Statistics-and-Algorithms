variance=100
mean = 75
#assume a to be 0.1 and calculate the value of b from the variance formula
a=0.1
b=sqrt(a/variance)
mean1<-qgamma(0.5,shape = a,rate = b)
#calculate median of gamma
#deafult it takes scale; scale = 1/rate = 1/b
difference=abs(mean-mean1)
while(difference>0.01){
  e=0.01
  a1=a+e
  b1=sqrt(a1/variance)
  mean2=qgamma(0.5,a1,b1)
  difference_1=abs(mean-mean2)
  a=a1
  difference=difference_1
}

print(a)
print(b1)
#check
pgamma(75,a,rate = b1)

