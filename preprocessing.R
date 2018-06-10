
#----------------------data selection forward/backward function------------------

forwardBackward_selection<-function(dataset){
  require(glm2)
  
  #create mew Y vector (male/famle) with replace male=1 and famle=0
  y=dataset[,ncol(dataset)]
  y1 <- matrix(0,length(y),1)
  x=dataset[,1:(ncol(dataset)-1)]
  list<-as.list(as.data.frame(x))
  i<-1
  for(i in 1:length(y1)){
    
    if((y[i]=="female")==TRUE){
      y1[i,1]<-0
      
    }
    else if((y[i]=="male")==TRUE){
      y1[i,1]<-1
    }
  }
  
  #define logistic reggresion model with our dataset
  mymodel<-glm(y1~., family=gaussian(link = "identity"),x,control=glm.control(maxit=100))
  mymodel <- update(mymodel, ~ . -1)
  
  a<-step(mymodel,direction = "both",trace=1)
  b<-step(mymodel,direction = "forward",trace=1)
  c<-step(mymodel,direction = "backward",trace=1)
  
  summary(a)
  summary(b)
  summary(c)
  
  After_FB_Data <-a$model[,-1]
  After_FB_Data$label<-y
  
  After_Forward_Data <-b$model[,-1]
  After_Forward_Data$label<-y
  
  After_Backward_Data <-c$model[,-1]
  After_Backward_Data$label<-y
  
  #i decided to take the "both" direction in this project
  write.csv(After_FB_Data,'After_ForwardBackward_data.csv')
  return(After_FB_Data)
}



#--------------------------data selection PCA function---------------------


PCA_Selection<-function(dataset){
  require(factoextra)
  
  dataset.label <- dataset[, ncol(dataset)]
  dataset <- (dataset[, 1:ncol(dataset)-1])
  dataset.pca <- prcomp(dataset, scale = TRUE)
  
  pca_pc<-dataset.pca$x
  summary(dataset.pca)
  
  fviz_eig(dataset.pca)
  par(mar=c(4,4,2,2))
  fviz_pca_var(dataset.pca, col.var="steelblue")+
    theme_minimal()
  
  #create a new dataset after the PCA
  q<-summary(dataset.pca)$importance
  i<-1
  nPC<-0
  for(i in 1:ncol(q)){
    if(q[3,i]<0.8){
      nPC<-i
    }
  }
  
  nPC<-nPC+1
  After_pca_data <- data.frame(dataset.pca$x[,1:nPC])
  After_pca_data$label<-dataset.label
  write.csv(After_pca_data,'After_pc_data.csv')
  return(After_pca_data)
}




#---------------------------------data preparing functions--------------------


#Generate train, test and validation data
TrainValTest <- function(data){
  
  dataset<-data
  train<-dataset
  test<-dataset
  val<-dataset
  train<- train[-0:-nrow(dataset),]
  test<- test[-0:-nrow(dataset),]
  val<- test[-0:-nrow(dataset),]
  j<-0
  i<-1
  l<-1
  k<-1
  even<-TRUE
  
  for (j in 1:nrow(dataset)) {
    if(j%%2==0){
      if(even){
        test[i,]<-dataset[j,]
        i<-i+1
      }
      if(even==FALSE){
        val[l,]<-dataset[j,]
        l<-l+1
      }
      even<-!even
    }
    else{
      train[k,]<-dataset[j,]
      k<-k+1
    }  
  }
  
  ntrain <- nrow(train) 
  ntest <- nrow(test)
  nval <- nrow(val)
  
  result=list(train,val,test, ntrain,nval,ntest)
  
  return(result)
}

#--function for balanced rows reducing from the dataset--
balancedReductionData<-function(data ,numberForReduction){
  
  numberForReduction<-numberForReduction/2
  halfLenght<-nrow(data)/2
  from<-halfLenght-numberForReduction
  to<-halfLenght+numberForReduction
  data<-data[c(from:to),]
  
  return(data)
}

#replace the label "male" and "female" to 1 and 2
replace_Label_To_Numeric<-function(dataset){
  
  y=dataset[,ncol(dataset)]
  result <- dataset[,-ncol(dataset)]
  result<-data.frame(result,0)
  heads<-colnames(dataset)
  colnames(result)<-c(heads)
  
  i<-1
  for(i in 1:nrow(dataset)){
    
    if((y[i]=="female")==TRUE){
      result[i,ncol(dataset)]<-2
      
    }
    else if((y[i]=="male")==TRUE){
      result[i,ncol(dataset)]<-1
    }
  }
  
  return(result)
}


#separete the dataset to 2 datasets of male and famle
dataSaperete<-function(my.data){
  
  male<-my.data
  famle<-my.data
  male<- male[-0:-nrow(my.data),]
  famle<- famle[-0:-nrow(my.data),]
  i<-1
  j<-1
  k<-1
  
  for(i in 1:nrow(my.data)){
    
    if((my.data[i,ncol(my.data)]=="female")==TRUE){
      famle[j,]<-my.data[i,]
      j=j+1
    }
    
    else if((my.data[i,ncol(my.data)]=="male")==TRUE){
      male[k,]<-my.data[i,]
      k=k+1
    }
  }
  
  result<-list(male,famle)
  return(result)
}


#mixing the data
#the male and the female labels in th dataset are saperete, this function mix the rows for better using in cross-validation 
dataMix<-function(data){
  
  newData<-dataset
  newData<- newData[-0:-nrow(dataset),]
  j<-1
  k<-2
  
  for(i in 1:nrow(dataset)){
    if((dataset[i,ncol(dataset)]=="female")==TRUE){
      newData[j,]<-dataset[i,]
      j=j+2
    }
    else if((dataset[i,ncol(dataset)]=="male")==TRUE){
      newData[k,]<-dataset[i,]
      k=k+2
    }
  }
  
  return(newData)
}

