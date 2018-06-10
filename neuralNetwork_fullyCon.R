#------------------------- Neural Networks function------------------------


#---------------------neural network with 3 layers (1 hidden layers)-------------------
get_neuralNetworks_bests<-function(train, val, test,nueronsLimit,seq){
  require(nnet)
  require(RCurl)
  require(RSNNS)
  
  #nueronsLimit<-30; seq<-3
  nnum <- seq(1,nueronsLimit,seq)
  acc  <- matrix(0,length(nnum),1)
  i    <- 1 
  
  for(neurons in nnum){
    nn<- nnet(x=train[,1:ncol(train)-1], y=class.ind(train[,ncol(train)]), size=nueronsLimit, linout=FALSE, softmax=T)
    preds.nn.val   <- factor(predict(nn, newdata=val[,1:ncol(train)-1], type='class'))
    acc[i]         <- (sum(preds.nn.val==val$label))/nval
    i              <- i + 1
  }
  
  #validation ratio
  ratioVal<-max(acc)
  
  #optimal number of neurons
  neurons       <- nnum[which.max(acc)]
  
  #testing the best configuration
  nn<- nnet(x=train[,1:ncol(train)-1], y=class.ind(train[,ncol(train)]), size=neurons, linout=FALSE, softmax=T)
  preds.nn.test <- factor(predict(nn, newdata=test[,1:ncol(train)-1], type='class'))
  
  #test ratio
  ratioTest<-(sum(preds.nn.test==test$label))/ntest
  p<-plot(nn)
  
  result<-list(ratioTest,ratioVal,neurons,p)
  return(result)
}


#---------------------neural network with 5 layers (3 hidden layers)-------------------

get_neuralNetworksHiddenLayers_bests<-function(dataset,train, val, test,nueronsLimit1,nueronsLimit2,nueronsLimit3){
  require(nnet)
  require(RCurl)
  require(RSNNS)
  require(devtools)
  require(reshape)
  require(MASS)
  require(NeuralNetTools)
  require(neuralnet)
  require(gamlss.add)
  
  #nueronsLimit1<-3; nueronsLimit2<-5; nueronsLimit3<-3
  nnum1 <- seq(2,nueronsLimit1,1)
  nnum2 <- seq(2,nueronsLimit2,1)
  nnum3 <- seq(2,nueronsLimit3,1)
  train1<-cbind(train[,1:ncol(train)-1], class.ind(as.factor(train$label)))
  val1<-cbind(val[,1:ncol(val)-1], class.ind(as.factor(val$label)))
  test1<-cbind(test[,1:ncol(test)-1], class.ind(as.factor(test$label)))
  names(train1) <- c(names(train)[1:ncol(train)-1],"l1","l2")
  names(val1) <- c(names(val)[1:ncol(val)-1],"l1","l2")
  names(test1) <- c(names(test)[1:ncol(test)-1],"l1","l2")
  n <- names(train1)
  f<-as.formula(paste("l1 + l2 ~", paste(n[!n %in% c("l1","l2")], collapse = " + ")))
  
  acc  <- matrix(0,length(nnum1)*length(nnum2)*length(nnum3),1)
  n1<-acc
  n2<-acc
  n3<-acc
  i    <- 1
  
  for(neurons3 in nnum3){
    for(neurons2 in nnum2){
      for(neurons1 in nnum1){
        nn <- neuralnet(f,data=train1,hidden=c(neurons1,neurons2,neurons3),act.fct = "logistic",linear.output=FALSE, lifesign = "minimal", stepmax=1000000)
        
        # Compute predictions
        pr.nn <- compute(nn, val1[,1:(ncol(val1)-2)])
        pr.nn_ <- pr.nn$net.result
        original_values <- max.col(val1[,(ncol(val1)-1):(ncol(val1))])
        pr.nn_2 <- max.col(pr.nn_)
        mean<-mean(pr.nn_2 == original_values) 
        acc[i]<-mean
        n1[i]<-neurons1
        n2[i]<-neurons2
        n3[i]<-neurons3
        i <- i + 1  
      }
    }
  }
  
  #validation ratio
  ratioVal<-acc[which.max(acc)]
  
  #optimal number of neurons
  neurons1       <- n1[which.max(acc)]
  neurons2       <- n2[which.max(acc)]
  neurons3       <- n3[which.max(acc)]
  nn <- neuralnet(f,data=train1,hidden=c(neurons1,neurons2,neurons3),act.fct = "logistic",linear.output=FALSE, lifesign = "minimal")
  nn <- neuralnet(f,data=train1,hidden=c(3,7,5),act.fct = "logistic",linear.output=FALSE, lifesign = "minimal",stepmax=10e6)
  p<-plotnet(nn)
  plot(nn)
  
  #testing the best configuration
  pr.nn.test <- compute(nn, test1[,1:(ncol(test1)-2)])
  pr.nn_test <- pr.nn.test$net.result
  original_values_test <- max.col(test1[,(ncol(test1)-1):(ncol(test1))])
  pr.nn_2 <- max.col(pr.nn_test)
  ratioTest<-mean(pr.nn_2 == original_values_test) 
  
  result<-list(ratioTest,ratioVal,neurons1,neurons2,neurons3,p)
  
  p1<-(paste('number of neurons layer 1: ',(neurons1)))
  p2<-paste('number of neurons layer 2: ',(neurons2))
  p3<-(paste('number of neurons layer 3: ',(neurons3)))
  p5<-(paste('neural_network_val_ratio: ',(ratioVal)))
  p4<-(paste('neural_network_test_ratio: ',(ratioTest)))
  
  cat(p1,p2,p3,p4,p5,p6,p7,p8,sep="\n")
  
  return(result)
}

