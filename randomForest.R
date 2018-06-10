#------------------------------- Random Forest function---------------


get_randomForest_bests<-function(train, val, test,treesLimit){
  require(ggplot2)
  require(randomForest)
  
  #treesLimit<-51
  numtree <- seq(1,treesLimit,1)
  numheight<-seq(1,ncol(train)-1,1)
  acc<- matrix(0,length(numtree),length(numheight))
  i<- 1
  j<-1
  
  for(height in numheight){
    i       <- 1
    for(trees in numtree){
      rf             <- randomForest(x=train[,1:ncol(train)-1], y=factor(train[,ncol(train)]), ntree=trees, mtry=height)
      preds.nn.val   <- predict(rf, newdata=val[,1:ncol(train)-1])
      acc[i,j]         <- (sum(preds.nn.val==factor(val$label)))/nval
      i              <- i+1
    }
    j<-j+1
  }
  
  #validation ratio
  ratioVal<-max(acc)
  ratioVal
  
  #the optimal number of trees
  ntrees        <- numtree[which(acc == max(acc), arr.ind = TRUE)[1,1]]
  ntrees
  
  #the height of the trees
  nheight     <-numheight[which(acc == max(acc), arr.ind = TRUE)[1,2]]
  nheight
  
  #testing the best configuration
  rf            <- randomForest(x=train[,1:ncol(train)-1], y=factor(train[,ncol(train)]), ntree=ntrees,mtry=nheight)
  preds.rf.test <- predict(rf, newdata=test[,1:ncol(train)-1])
  
  #test ratio
  ratioTest<-(sum(preds.rf.test==test$label))/ntest
  ratioTest
  
  result<-list(ratioTest,ratioVal,ntrees,nheight,p)
  return(result)
}


#--------------------------Random forest with 10 cross validation-------------------

get_randomForest_bests_cross<-function(data, test,treesLimit){
  require(ggplot2)
  require(randomForest)
  
  data<-dataMix(data)
  treesLimit<-25
  numtree <- seq(1,treesLimit,1)
  numheight<-seq(2,ncol(data)-1,1)
  acc<- matrix(0,length(numtree),length(numheight))
  kfoldsVec<- matrix(0,10,1)
  i<- 1
  j<-1
  
  kfolds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
  acc_nn_cv10     <- matrix(0,10,1)
  
  for(height in numheight){
    i       <- 1
    for(trees in numtree){
      for(l in 1:10){
        testIndexes<- which(kfolds==l,arr.ind=TRUE)
        val<- data[testIndexes, ]
        train<- data[-testIndexes, ]
        rf<- randomForest(x=train[,1:ncol(train)-1], y=factor(train[,ncol(train)]), ntree=trees, mtry=height)
        preds.nn.val<- predict(rf, newdata=val[,1:(ncol(val)-1)])
        kfoldsVec[l]<-(sum(preds.nn.val==factor(val$label)))/nrow(val)
      }
      
      acc[i,j]         <- mean(kfoldsVec)
      i              <- i+1
    }
    j<-j+1
  }
  
  #validation ratio
  ratioVal<-max(acc)
  ratioVal
  
  #the optimal number of trees
  ntrees        <- numtree[which(acc == max(acc), arr.ind = TRUE)[1,1]]
  ntrees
  
  #the height of the trees
  nheight     <-numheight[which(acc == max(acc), arr.ind = TRUE)[1,2]]
  nheight
  
  #testing the best configuration
  rf            <- randomForest(x=data[,1:ncol(data)-1], y=factor(data[,ncol(data)]), ntree=ntrees,mtry=nheight)
  preds.rf.test <- predict(rf, newdata=test[,1:(ncol(test)-1)])
  
  #test ratio
  ratioTest<-(sum(preds.rf.test==test$label))/nrow(test)
  ratioTest
  
  result<-list(ratioTest,ratioVal,ntrees,nheight,p)
  return(result)
}

