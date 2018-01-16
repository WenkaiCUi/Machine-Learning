setwd("C:/Users/cui_w/Desktop/machine-learning-ex1/ex1")
data<-read.table("ex1data1.txt",sep = ",")

plotData<-function(x,y){
  plot(x,y,pch=4,col="red")
}

computeCost<-function(x,y,theta){
  m<- length(y)
  h<-x%*%theta
  J<- t(h-y)%*%(h-y)/(2*m)
  return(J)
}

gradientDescent<-function(x, y, theta, alpha, num_iters){
  #record history of J
  Jhis<- rep(0,num_iters)
  m=length(y)
  for (i in 1:num_iters){
    theta<- theta - alpha*t(x)%*%((x%*%theta)-y)/(m)
    Jhis[i]<- computeCost(x,y,theta)
  }
  l<-list(theta=theta,Jhis=Jhis)
  return(l)
}

#test case
x=data[,1]
y=data[,2]
plotData(x,y)
m<- length(y)
x<-cbind(rep(1,m),x)
theta<-cbind(rep(0,2))

J <- computeCost(x, y, theta)
J
J = computeCost(x, y, cbind(c(-1,2)))
J

iterations = 1500
alpha = 0.01
theta <- gradientDescent(x, y, theta, alpha, iterations);
#fit
abline(theta,col="blue")


#multivariable
data2<-read.table("ex1data2.txt",sep=",",col.names = c("size","bedroom","price"))
#normalize
mdata2<-apply(data2,2,mean)
sddata2<-apply(data2,2,sd)
ndata2<-scale(data2)
theta<-theta<-cbind(rep(0,3))
x<-ndata2[,1:2]
y<-ndata2[,3]
m<-length(y)
x<-cbind(rep(1,m),x)
#gradient desent
alpha<- 0.01
iterations<-400
theta <- gradientDescent(x, y, theta, alpha, iterations)
#plot the decreasing of J
plot(1:iterations,unlist(theta[2]),type="l",col="red")
