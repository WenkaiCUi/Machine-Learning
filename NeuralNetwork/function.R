displayData<-function(dat){
  x<-x[sample(nrow(x), size=100, replace=FALSE),]
  z<-matrix(rep(0,length(x)),nrow=200)
  for (row in 0:9){
    rmin<- 1+row*20
    for (col in 0:9){
      cmin<- 1+col*20
      z[rmin:(rmin+19),cmin:(cmin+19)]<-x[row*10+col+1,]
    }
  }
  image(t(apply(z, 2, rev)),col=gray.colors(9))
}

sigmoid<-function(x){
  return (1/(1+exp(-x)))
}

#cost function
costfunc<-function(theta,x,y,lamda){
  h<-sigmoid(x%*%theta)
  m<-dim(x)[1]
  mtheta<-theta
  mtheta[1]<-0
  J<-  1/m*(-t(y)%*%log(h)-t(1-y)%*%log(1-h))+lamda/(2*m)*(t(mtheta)^2%*%matrix(1,nrow=length(theta)))
  return(J)
}

grad<-function(theta,x,y,lamda){
  h<-sigmoid(x%*%theta)
  m<-dim(x)[1]
  mtheta<-theta
  mtheta[1]<-0
  grad<-1/m*t(x)%*%(h-y)+lamda/m*mtheta
  return(grad)
}

onevsall<-function(x,y,num_labels,lambda){
  m<-dim(x)[1]
  n<-dim(x)[2]
  all_theta<-matrix(0,nrow=num_labels,ncol=n+1)
  #add 1 to x
  x<-cbind(rep(1,m),x)
  for (i in 1:num_labels){
    itheta<-matrix(0,nrow=n+1)
    result<-optim(itheta,costfunc,grad,x=x,y=(y==i),
                  lamda=lambda,method="BFGS")
    theta<-result$par
    all_theta[i,]<-t(theta)
  }
  return(all_theta)
}

predictonevsall<-function(alltheta,x){
  m<-dim(x)[1]
  p<-matrix(0,nrow=m,ncol=1)
  x<-cbind(rep(1,m),x)
  p<-sigmoid(x%*%t(alltheta))
  p<-apply(p,1,which.max)
  return(p)
}

predictnn<-function(theta1,theta2,x){
  m<-dim(x)[1]
  x<-cbind(rep(1,m),x)
  a2<-sigmoid(x%*%t(theta1))
  a2<-cbind(rep(1,m),a2)
  a3<-sigmoid(a2%*%t(theta2))
  p<-apply(a3,1,which.max)
  return(p)
  
}

displayData2<-function(dat,theta1,theta2){
  x<-x[sample(nrow(x), size=100, replace=FALSE),]
  z<-matrix(rep(0,length(x)),nrow=200)
  for (row in 0:9){
    rmin<- 1+row*20
    for (col in 0:9){
      cmin<- 1+col*20
      z[rmin:(rmin+19),cmin:(cmin+19)]<-x[row*10+col+1,]
    }
  }
  image(t(apply(z, 2, rev)),col=gray.colors(9))
  p<-predictnn(theta1 ,theta2,x)
  p[p==10]<-0
  matrix(p,nrow=10,byrow = T)
}

