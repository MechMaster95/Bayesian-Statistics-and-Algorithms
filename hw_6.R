##6th Question

## Part a
## Given that N=50 and lambda=1.5
## Gamma(0.01,0.01) prior 
## Mean N*lambda=75

## Since for Poisson Likelihood, Gamma is a conjugate prior
## we study the posterior with gamma distribution and give its
## mean and 95% credible interval

N<- 50
lambda<-1.5
Y <- N*lambda

a<-0.01
b<-0.01
##for the above values, the parameters of the posterior would
posterior_a<- Y+a
posterior_b<- b+N

grid <- seq(0,10,0.01)
prior_dist<- dgamma(grid,shape = a,rate = b)
posterior_dist<-dgamma(grid,shape = posterior_a,rate = posterior_b)
## plot the likelihood_dist<-
## overlay plots

plot(grid,prior_dist,type="l",col="red",xlim=c(0,4),ylim = c(0,3),ylab="Prior and Posterior Dist")
lines(grid,posterior_dist,col="green")
#plot(grid,posterior_dist,type="l",xlab="Prob",ylab="density",main="posterior distribution")

## Posterior Mean
## calculating using MC sampling
sample_size=1000
post_mc<- rgamma(sample_size,shape = posterior_a,rate = posterior_b)
print(post_mean<- mean(post_mc))

## 95% Credible interval for this posterior would be
quantile(post_mc,c(0.025,0.975))

## Part b
## Given that N=50 and lambda=1.5
## Gamma(0.1,0.1);Gamma(0.1,0.1) prior 
## Mean N*lambda=75

## Since for Poisson Likelihood, Gamma is a conjugate prior
## we study the posterior with gamma distribution and give its
## mean and 95% credible interval

a1<-0.1
b1<-0.1

posterior_a<- Y+a1
posterior_b<- b1+N

grid <- seq(0,10,0.01)
prior_dist<- dgamma(grid,shape = a1,rate = b1)
posterior_dist<-dgamma(grid,shape = posterior_a,rate = posterior_b)
## plot the likelihood_dist<-
## overlay plots

plot(grid,prior_dist,type="l",col="red",xlim=c(0,4),ylim = c(0,3),ylab="Prior and Posterior Dist")
lines(grid,posterior_dist,col="green")

## Posterior Mean
## calculating using MC sampling
sample_size=1000
post_mc1<- rgamma(sample_size,shape = posterior_a,rate = posterior_b)
print(post_mean1<- mean(post_mc1))

## 95% Credible interval for this posterior would be
quantile(post_mc1,c(0.025,0.975))

## Prior a=1 and b=1
a2<-1
b2<-1

posterior_a2<- Y+a2
posterior_b2<- b2+N

grid <- seq(0,10,0.01)
prior_dist<- dgamma(grid,shape = a2,rate = b2)
posterior_dist<-dgamma(grid,shape = posterior_a2,rate = posterior_b2)
## plot the likelihood_dist<-
## overlay plots

plot(grid,prior_dist,type="l",col="red",xlim=c(0,4),ylim = c(0,3),ylab="Prior and Posterior Dist")
lines(grid,posterior_dist,col="green")

## Posterior Mean
## calculating using MC sampling
sample_size=1000
post_mc<- rgamma(sample_size,shape = posterior_a2,rate = posterior_b2)
print(post_mean1<- mean(post_mc))

## 95% Credible interval for this posterior would be
quantile(post_mc,c(0.025,0.975))

## Part c
## The prior rate is exactly Mayo's Rate
## This means that the mean of the prior distribution a/b = 1.5 
## this gives a linear relationship for parameters in the prior
## (i): So Prior can be taken as gamma(b*1.5,1) or gamma(b*1.5,0.1)
## (ii): When we say that the rate is approx 1.5, we can consider a/b = 1.49
##       hence similarly the prior would be gamma(0.1*1.49,0.1)
## (iii): If Mayo's rate is unrelated a/b could be any value like 100
##        in this case prior would be gamma(0.1*100,0.1)










