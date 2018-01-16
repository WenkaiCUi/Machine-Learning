library(shiny)

edata<-as.matrix(read.table("ex2data2.txt",sep = ","))
x<-edata[,1:2]
y<-edata[,3]
plotData<-function(x,y){
  plot(x[,1][y==1],x[,2][y==1],pch=15,xlim=c(-1,1.5),ylim=c(-0.8,1.2),xlab="Test1",ylab="Test 2")
  points(x[,1][y==0],x[,2][y==0],pch=19,col="red")
  legend("topright",legend=c("y=1","y=0"),col=c("black","red"),pch=c(15,19))
}

mapFeature<-function(x,degree){
  v<-rep(1,length(x[,1]))
  for (i in 1:degree){
    for (j in 0:i){
      v<-cbind(v,x[,1]^(i-j)*x[,2]^(j))
    }
  }
  return(v)
}

sigmoid<-function(x){
  return (1/(1+exp(-x)))
}


fr<-function(theta,x,y,lamda){
  h<-sigmoid(x%*%theta)
  m<-dim(x)[1]
  J<-  1/m*(-t(y)%*%log(h)-t(1-y)%*%log(1-h))+lamda/(2*m)*(theta^2%*%rep(1,length(theta)))
  return(J)
}
grr<-function(theta,x,y,lamda){
  h<-sigmoid(x%*%theta)
  m<-dim(x)[1]
  mtheta<-theta
  mtheta[1]<-0
  grad<-1/m*t(x)%*%(h-y)+lamda/m*mtheta
  return(grad)
}

predict<-function(theta,x){
  return((x%*%theta>=0)*1)
}



#**************************************
ui <- fluidPage(
  titlePanel("Non-linear Logistic Regression Decision Boundary"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "lamda", label = "Lamda:",min = 0, max = 100, value = 1),
      sliderInput(inputId = "degree", label = "Degree:",min = 0, max = 10, value = 5)
    ),
    mainPanel(
          plotOutput("boundary")
    )
  )
)

server <- function(input, output){
  output$boundary<-renderPlot({
    idegree<- input$degree
    plotData(x,y)
    mx=mapFeature(x,idegree)
    itheta<-rep(0,length(mx[1,]))
    ilamda<-input$lamda
    result<-optim(itheta,fr,grr,x=mx,y=y,lamda=ilamda,method="BFGS")
    #plot decision boundary
    xx1<-seq(-1,1.5,length.out =100)
    xx2<-seq(-1,1.5,length.out =100)
    xx<-cbind(xx1,xx2)
    yy<-matrix(0,nrow=100,ncol=100)
    for (i in 1:100){
      for(j in 1:100){
        yy[i,j]=mapFeature(cbind(xx1[i],xx2[j]),idegree)%*%result$par
      }
    }
    p<-predict(result$par,mx)
    accuracy<-mean(p==y)
    plotData(edata[,1:2],y)
    contour(xx1,t(xx2),yy,level=0,add=TRUE,drawlabels=F,col="blue")
    title(paste("Prediction Accuracry:",round(accuracy,5)))
  })
  
}

shinyApp(ui = ui, server = server)
