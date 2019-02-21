#data
##proportion 0.845
# the posterior is theta|Y~Beta(A,B)

#for a uniform distribution is same as beta distribution with parameteres 
#a = 1 and b = 1
a = 1
b = 1

# no of independent trails
N<- c(75,95,63,39,83,26,41,82,54,16)
# no of successes
Y<- c(64,72,55,27,75,24,28,66,40,13)
# vector of probabilities
p <- seq(0,1,0.001)
overall_proportion<- c(0.845,0.847,0.880,0.674,0.909,0.898,0.770,0.801,0.802,0.875)

##Part b -- plotting posteriors
par(mfrow=c(2,5))
for(i in 1:length(N)){
  prior<- dbeta(p,a,b)
  posterior<- dbeta(p,a+Y[i],b+N[i])
  plot(p,posterior,type="l")
  lines(p,prior,type="l",lty=2)
  }

##part c
posterior_mean<-rep(0,10)
post_var<-rep(0,10)
bayes<- rep(0,10)

for (i in 1:length(N)){
  #Mean: E(theta)= a/a+b
  posterior_mean[i]=(a+Y[i])/(a+N[i]+b)
  #variance V(theta)=ab/(a+b)^2*(a+n[i]+b+1))
  post_var[i] = (a+Y[i])*(N[i]-Y[i]+b)/((a+N[i]+b)^2*(a+N[i]+b+1))
  bayes[i] = pbeta(overall_proportion[i],a+Y[i],N[i]-Y[i]+b)
  
  }

summary_beta_1<- data.frame(overall_proportion,Y,N,posterior_mean,post_var,bayes)

summary_beta_1

#part d 

all(overall_proportion==bayes)

####part E
no_of_players = 10
a = 0.5
b = 0.5
post_mean <- rep(0,10)
post_var <- rep(0,10)
bayes <- rep(0,10)
par(mfrow=c(2,5))
for (i in 1:no_of_players) {
  prior <- dbeta(p,a,b)
  posterior <- dbeta(p,a+Y[i],b+N[i]-Y[i])
  plot(p,posterior,type="l")
  lines(p,prior,type='l',lty=2)
}

for (i in 1:length(N)){
  #Mean: E(theta)= a/a+b
  posterior_mean[i]=(a+Y[i])/(a+N[i]+b)
  #variance V(theta)=ab/(a+b)^2*(a+n[i]+b+1))
  post_var[i] = (a+Y[i])*(N[i]-Y[i]+b)/((a+N[i]+b)^2*(a+N[i]+b+1))
  bayes[i] = pbeta(overall_proportion[i],a+Y[i],N[i]-Y[i]+b)
  
}

summary_beta_2 <- data.frame(overall_proportion,Y,N,posterior_mean,post_var,bayes)

summary_beta_2

all(summary_beta_1==summary_beta_2)


###Extra Question

## Extra question
no_of_players=10
# No of independent trials
n <- c(75,95,63,39,83,26,41,82,54,16)
# No of successes
y <- c(64,72,55,27,75,24,28,66,40,13)
A = y+1
B = n-y+1
sample_size = 10000
mat = matrix(0,sample_size,10)

# Generating posterior distribution
for (i in 1:no_of_players) {
  mat[,i] = rbeta(sample_size,A[i],B[i])
}

t = matrix(0, sample_size, 1)

for (i in 1:sample_size) {
  temp_steph = mat[i,6]
  temp_without_steph = c(mat[i,1:5],mat[i,7:10])
  
  if(temp_steph > max(temp_without_steph)){
    temp[i] = 1
  }
}

high_free = sum(temp)/sample_size
# Proportion of times Stephen Curry has the highest free throw %
high_free


  