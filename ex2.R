setwd("C:/Users/cui_w/Desktop/MachineLearning/machine-learning-ex2/ex2")
edata<-read.table("ex2data1.txt",sep = ",")
#plot data points in 2d
plot(edata[,1][edata[,3]==1],edata[,2][edata[,3]==1],pch=15,xlim=c(30,100),ylim = c(30,100),xlab="Exam1 Score",ylab="Exam2 Score")
points(edata[,1][edata[,3]==0],edata[,2][edata[,3]==0],pch=19,col="blue")
legend("topright",legend=c("Admited", "Not admitted"), col=c("black", "blue"), pch=c(15,19))

sigmoid<-function(x){
  return (1/(1+exp(-x)))
}



x<- edata[,1:2]
y<- edata[,3]
m<-dim(x)[1]
n<- dim(x)[2]
x<-cbind(rep(1,m),x)
x<-data.matrix(x)
itheta=rep(0,n+1)
#calculate cost function and gradient
costFunction<-function(theta,x,y){
  h<-sigmoid(x%*%theta)
  J<-  1/m*(-t(y)%*%log(h)-t(1-y)%*%log(1-h))
  grad<- 1/m*t(x)%*%(sigmoid(x%*%theta)-y)
  a<-list(J=J,grad=grad)
  return (a)
}
fr<-function(theta){
  h<-sigmoid(x%*%theta)
  1/m*(-t(y)%*%log(h)-t(1-y)%*%log(1-h))
}
grr<-function(theta){
  1/m*t(x)%*%(sigmoid(x%*%theta)-y)
}
  
#use a fminunc
optim(itheta,fr,grr)

#plot decision boundary
theta<-optim(itheta,fr,grr)$par
abline(-theta[1]/theta[2],-theta[3]/theta[2],col='green')


#predict student with E1 45 and E2 85
sigmoid(c(1,45,85)%*%theta)

#predict accuracy
predict<-function(theta,x){
  return((x%*%theta>=0)*1)
}
p<-predict(theta,x)
mean(p==y)


