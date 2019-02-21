## Question 5
## Given that X~N(0,1), alpha~N(0,C^2) and Beta~N(0,C^2)
## to find the value of c such that p(X) is roughly uniform between 0,1

par(mfrow=c(2,3))
c<-c(1,1.1,1.125,1.13,1.15,1.3)

for (i in c){
N=1000000
alpha<- rnorm(N,0,i^2)
beta<- rnorm(N,0,i^2)

x<- rnorm(N,0,1)
p_numr <- exp(alpha + x*beta)
p_denmr <- 1 + exp(alpha + x*beta)
p <- p_numr/p_denmr 
hist(p, xlab=i)
}

## Hence we can see that when c=1.13, its a good approximation for uniform 
## distribution for the p(x) distribution