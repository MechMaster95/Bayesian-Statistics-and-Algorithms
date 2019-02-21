dat <- read.table(text = "Y theta
                  0 0.2
                  0 0.5
                  5 0.2
                  5 0.5
                  10 0.2
                  10 0.5
                  ", header = TRUE)
#Problem 3: Works for both Gamma Distribution and Binomial, 
#but gives a non interger value for N in binomial
variance<- 3
mean<- 5
##After solving for a and b in from the given values
a<- (25/3)
b<- (5/3)
x_axis<-seq(0,1,length=500)
y_axis<-dbeta(x_axis,a,b)
plot(x_axis,y_axis,type='l',main="Gamma Distribution")
##################################################################
##Problem 4: Plotted two graphs for theta=0.2 and theta=0.5
lambda<-5
theta_grid <- seq(1,40,length=40)
prior<- dpois(theta_grid,lambda)

plot(1, type = 'n', xlim = c(0, 40), ylim = c(0, 0.4), 
     xlab = "x", ylab = "Posterior prob")

rows1<-c(1,3,5)
for (i in rows1){
  par(new=TRUE)
  Y1=dat[i,1]
  theta1=dat[i,2]
  like<- dbinom(Y1,theta_grid,theta1)
  fy<- sum(like*prior)
  post<- like*prior/fy
  lines(post,lty=i)
  lines(prior,lty=2)
}
dat1<-c('prior',0,5,10)
id <- paste("Y=",dat1,",theta=0.2",sep="")
rows1<-c(1,2,3,5)
legend("topright", legend=id,lty=rows1)
#################################################################3
plot(2, type = 'n', xlim = c(0, 40), ylim = c(0, 0.4), 
     xlab = "x", ylab = "Posterior prob")

rows2<-c(2,4,6)
for (i in rows2){
  par(new=TRUE)
  Y1=dat[i,1]
  theta1=dat[i,2]
  like<- dbinom(Y1,theta_grid,theta1)
  fy<- sum(like*prior)
  post<- like*prior/fy
  lines(post,lty=i)#
  lines(prior,lty=1)
}

dat2<-c('prior',0,5,10)
id_1 <- paste("Y=",dat1,",theta=0.5",sep="")
rows2<-c(1,2,4,6)
legend("topright", legend=id_1,lty=rows2)
