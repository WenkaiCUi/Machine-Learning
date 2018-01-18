library(R.matlab)
setwd("C:/Users/cui_w/Desktop/MachineLearning/machine-learning-ex3/ex3")
source("C:/Users/cui_w/Desktop/MachineLearning/machine-learning-ex3/R/function.R")


edata<-readMat("ex3data1.mat")
x<-edata$X
y<-edata$y
#visulize data
displayData(x)

#test cost function and gradient
theta_t = matrix(c(-2, -1, 1, 2))
x_t<-cbind(rep(1,5),matrix((1:15)/10,nrow=5))
y_t<- matrix(cbind(1,0,1,0,1))
lamda_t<-3
costfunc(theta_t,x_t,y_t,lamda_t)
grad(theta_t,x_t,y_t,lamda_t)

#one vs all
lambda<-0.1

all_theta<-onevsall(x,y,10,lambda)

#predict
pred<-predictonevsall(all_theta,x)
mean(pred==y)


#neutral network
edata<-readMat("C:/Users/cui_w/Desktop/MachineLearning/machine-learning-ex3/ex3/ex3weights.mat")
theta1<-edata$Theta1
theta2<-edata$Theta2
pred2<-predictnn(theta1,theta2,x)
#accuracy
mean(pred2==y)
displayData2(x,theta1,theta2)
