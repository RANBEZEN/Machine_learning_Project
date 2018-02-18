install.packages('glm2')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('stats')
install.packages('ggbiplot')
install.packages("factoextra")
install.packages('corrplot')
install.packages('devtools')
install.packages('ggbiplot')
install_github("vqv")
install.packages('fields')
install.packages('topicmodels')
install.packages('clv')
install.packages('fpc')
install.packages('gamlss.add')
install.packages("ggpubr")
install.packages('RSNNS')
install.packages('devtools')
install.packages('neuralnet')
install.packages('reshape')
install.packages('NeuralNetTools')
library(neuralnet)
require(NeuralNetTools)
require(devtools)
require(reshape)
require(RSNNS)
library(devtools)
require(cluster)
require(clv)
require(topicmodels)
require(corrplot)
require(fields)
require(randomForest)
require(nnet)
library(vqv)
require(factoextra)
require(MASS)
require(glm2)
require(dplyr)
require(stats)
require(ggbiplot)
require(ggplot2)

#import function from Github
install.packages('RCurl')
require(RCurl)

root.url<-'https://gist.githubusercontent.com/fawda123'
raw.fun<-paste(
  root.url,
  '5086859/raw/cc1544804d5027d82b70e74b83b3941cd2184354/nnet_plot_fun.r',
  sep='/'
)
script<-getURL(raw.fun, ssl.verifypeer = FALSE)
eval(parse(text = script))
rm('script','raw.fun')



##########################################****   FUNCTIONS  ****##############################


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
  write.csv(After_FB_Data,'C:/Users/ran/Desktop/After_ForwardBackward_data.csv')
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
  write.csv(After_pca_data,'C:/Users/ran/Desktop/After_pc_data.csv')
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


###############################-------machine learning algorithms--------####################


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

#------------------------- Neural Networks function------------------------

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
 

#---------------------neural network with 3 hidden layers-------------------

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


#####------------------------- Kmeans function-------------------------


get_kmeans_bests<-function(dataset, max_iterations, start){
  require(cluster)
  require(clv)
  #max_iterations<-50; start<-1
  
  # Move to unsupervised setting
  data<-dataset[,-ncol(dataset)]
  k             <- 2
  clust_data    <- kmeans(scale(data), centers=k, iter.max=max_iterations, nstart=start)
  newData       <- replace_Label_To_Numeric(dataset)  #insert the "label" with numeric
  newData$clust <- factor(clust_data$cluster)
  
  ratio <- (sum(newData$clust==newData$label))/nrow(data)
  return(ratio)
}

KmeanForUnsupervised<-function(data,k){
  require(cluster)
  require(clv)
  require(fpc)
  require(ggplot2)
  data<-male
  data<-data[,-ncol(data)]
  
  dunn <- c() 
  DB <- c() 
  K<-6 
  
  for(k in 2:K){
    clust_data    <- kmeans(scale(data), centers=k, iter.max=25, nstart=5)
    scatt_data    <- cls.scatt.data(scale(data), clust=clust_data$cluster, dist='euclidean')
    DB            <- c(DB,   clv.Davies.Bouldin(scatt_data, 'centroid', 'centroid'))
  }
  
  clust_metrics <- data.frame(K = rep(seq(2,K,1),2), value = c(dunn, DB), metric = c(rep('DB',K-1)))
  plot1<-ggplot(clust_metrics, aes(x=K, y=value, color=factor(metric))) + geom_point() + geom_line()
  plot1
  
  
  # Determine number of clusters
  
  wss <- (nrow(scale(data))-1)*sum(apply(scale(data),2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(scale(data), 
                                       centers=i)$withinss)
  plot2<-plot(1:15, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  k<-2
  # K-Means Cluster Analysis
  fit2 <- kmeans(scale(data), k) # k cluster solution
  
  # get cluster means 
  aggregate(scale(data),by=list(fit2$cluster),FUN=mean)
  
  # append cluster assignment
  mydata <- data.frame(scale(data), fit2$cluster)
  
  # Ward Hierarchical Clustering
  distance <- dist(data, method = "euclidean") # distance matrix
  fit <- hclust(distance, method="ward.D") 
  plot3<-plot(fit) # display dendogram
  groups <- cutree(fit, k=k) # cut tree into k clusters
  rect.hclust(fit, k=k, border="red")
 
  # vary parameters for most readable graph
  plot4<-clusplot(scale(data), fit2$cluster, color=TRUE, shade=TRUE, 
           labels=2, lines=0)
  
  results<-list(plot1,plot2,plot3,plot4)
  
  return(results) 
}



#####################################****** DATA UNDERSTANDING ******############################


#separete the dataset to 2 datasets of male and famle

label<-dataSaperete(my.data)
male<-label[[1]]
female<-label[[2]]

#Histogram for 1 dimation of parameter and 2 sets of data (male and famle)
library("ggpubr")

ip<-10
p1<-hist(male[,ip],breaks=100)
p2<-hist(female[,ip],breaks=100)
plot( p1, col=rgb(0,0,1,1/4),xlab=colnames(my.data)[ip],main=paste( "Male VS famle:", colnames(my.data)[ip],sep=" ") ,xlim=c(min(my.data[,ip]),max(my.data[,ip])))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), xlim=c(min(my.data[,ip]),max(my.data[,ip])), fill =label,add=T)  # second
legend(18, 120, legend=c("male", "famle", "both"), col=c("blue", "pink", "purple"), fill=c("blue", "pink", "purple"), cex=0.8)

#for the "kurt" parameters we cut the scale to quantile 90% because the propability is non gaussian (the maximum value is 1309.613, the minimum is 2.068455, the mean:36.56846, median:8.318463)
ip<-8
p1<-hist(male[,ip],breaks=500)
p2<-hist(famle[,ip],breaks=500)
plot( p1, col=rgb(0,0,1,1/4),xlab=colnames(my.data)[ip],main=paste( "Male VS famle:", colnames(my.data)[ip],sep=" ") ,xlim=c(min(my.data[,ip]),quantile(my.data[,ip],0.9)))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), xlim=c(min(my.data[,ip]),max(my.data[,ip])), fill =label,add=T)  # second
legend(25, 150, legend=c("male", "famle", "both"), col=c("blue", "pink", "purple"), fill=c("blue", "pink", "purple"), cex=0.8)


# Density plot 
#::::::::::::::::::::::::::::::::::::::
require(ggplot2)
PList <- vector('list', (ncol(my.data)-1))
names<-colnames(my.data)
for(ip in 1:(ncol(my.data)-1)){
  PList[[ip]]<-ggdensity(my.data, x = names[ip],  fill = "label", palette = "jco")
}
PList[[1]]
PList[[2]]
PList[[3]]
PList[[4]]
PList[[5]]
PList[[6]]
PList[[7]]
PList[[8]]
PList[[9]]
PList[[10]]
PList[[11]]
PList[[12]]
PList[[13]]
PList[[14]]
PList[[15]]
PList[[16]]
PList[[17]]
PList[[18]]
PList[[19]]
PList[[20]]

#scatter plot for 2 dimations and correlation tests
ggplot(data = my.data, aes(x=Q25, y=Q75, color=label)) + geom_point()
cor(my.data$Q25, my.data$Q75, method = c("pearson", "kendall", "spearman"))

ggplot(data = my.data, aes(x=IQR, y=Q75, color=label)) + geom_point()
cor(my.data$IQR, my.data$Q75, method = c("pearson", "kendall", "spearman"))

ggplot(data = my.data, aes(x=IQR, y=Q25, color=label)) + geom_point()
cor(my.data$IQR, my.data$Q25, method = c("pearson", "kendall", "spearman"))

ggplot(data = my.data, aes(x=IQR, y=median, color=label)) + geom_point()
cor(my.data$IQR, my.data$median, method = c("pearson", "kendall", "spearman"))



#####################################****** MAIN/RUN ******###################################


my.data<- read.csv('C:/Users/ran/Desktop/voice.csv', header = TRUE) #read the csv data

#delete some parameters after feature selection by the pdf/correlation/histograms in the data understanding level
my.data<-my.data[,c(-18,-5,-4)]  
#my.data<-balancedReductionData(my.data,763)   #reduce the dataset

#feature selection
After_FB_Data<-forwardBackward_selection(my.data) #get the dataset after forward/backward selection

#feature extraction
After_pca_data<-PCA_Selection(After_FB_Data)  #get the dataset after pca

#take the original data for comparing
my.data<- read.csv('C:/Users/ran/Desktop/voice.csv', header = TRUE) #read the csv data
dataset<-my.data

#insert the 3 datasets to list
data_list<-list(my.data,After_pca_data,After_FB_Data)   #insert the datasets to a list for run them in loop

  i<-1
  #loop for test all the datasets on all the algorithms and copmere them on a table
  for(i in 1:3) {
    selected_set<-data_list[[i]]
    #-----data deviding
    dataDev<-TrainValTest(selected_set) #devide the datat to train validation and test
    train<-dataDev[[1]] 
    val<-dataDev[[2]]
    test<-dataDev[[3]]
    ntrain<-dataDev[[4]] 
    nval<-dataDev[[5]]
    ntest<-dataDev[[6]]
    
    #---run algorithms

    #run random forest and get results
    rf<-get_randomForest_bests(train,val,test,35)
    
    #run random forest with 10 folds cross validation
    rf_crossVal<-get_randomForest_bests_cross(rbind(train,val),test,20)
    
    #run neural Networks and get results
    nn<-get_neuralNetworks_bests(train,val,test,20,2)
    
    #run neural Networks with 3 hidden layers
    nn_hidden_layers<-get_neuralNetworksHiddenLayers_bests(dataset,train,val,test,5,7,5)
    
    #run Kmeans and get results
    km<-get_kmeans_bests(selected_set,100,1)
    
    #devide the data to varibles
    randomForest_test_ratio<-rf[[1]]
    randomForest_val_ratio<-rf[[2]]
    randomForest_optimal_trees<-rf[[3]]
    randomForest_optimal_height<-rf[[4]]
    
    rf_cv_test<-rf_crossVal[[1]]
    rf_cv_val<-rf_crossVal[[2]]
    rf_cv_trees<-rf_crossVal[[3]]
    rf_cv_height<-rf_crossVal[[4]]
    
    neural_network_test_ratio<-nn[[1]]
    neural_network_val_ratio<-nn[[2]]
    neural_network_optimal_neurons<-nn[[3]]
    
    nn_hedden_test<-nn_hidden_layers[[1]]
    nn_hidden_val<-nn_hidden_layers[[2]]
    nn_hidden_l1<-nn_hidden_layers[[3]]
    nn_hidden_l2<-nn_hidden_layers[[4]]
    nn_hidden_l3<-nn_hidden_layers[[5]]
    
    kmeans_ratio<-km
    
    #results printing
    p1<-(paste('randomForest_test_ratio: ',(randomForest_test_ratio)))
    p2<-paste('randomForest_val_ratio: ',(randomForest_val_ratio))
    p3<-(paste('randomForest_optimal_trees: ',(randomForest_optimal_trees)))
    p4<-(paste('randomForest_optimal_height: ',(randomForest_optimal_height)))
    p5<-(paste('rf_cv_test_ratio: ',(rf_cv_test)))
    p6<-paste('rf_cv_val_ratio: ',(rf_cv_val))
    p7<-(paste('rf_cv_optimal_trees: ',(rf_cv_trees)))
    p8<-(paste('rf_cv_optimal_height: ',(rf_cv_height)))
    p9<-(paste('neural_network_test_ratio: ',(neural_network_test_ratio)))
    p10<-(paste('neural_network_val_ratio: ',(neural_network_val_ratio)))
    p11<-(paste('neural_network_optimal_neurons: ',(neural_network_optimal_neurons)))
    p12<-(paste('nn_test_ratio: ',(nn_hedden_test)))
    p13<-(paste('nn_val_ratio: ',(nn_hidden_val)))
    p14<-(paste('nn_optimal_neurons_layer1: ',(nn_hidden_l1)))
    p15<-(paste('nn_optimal_neurons_layer2: ',(nn_hidden_l2)))
    p16<-(paste('nn_optimal_neurons_layer3: ',(nn_hidden_l3)))
    p17<-(paste('kmeans_ratio: ',(kmeans_ratio)))
    cat(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,sep="\n")
    
    #plots display
    rf[[5]]
    rf_crossVal[[5]]
    nn[[4]]
    nn_hidden_layers[[6]]
    
    #insert the results to fact table
    if(i==1){
      result_Matrix<-data.frame(randomForest_test_ratio,randomForest_val_ratio,randomForest_optimal_trees,
            randomForest_optimal_height,rf_cv_test,rf_cv_val, rf_cv_trees,
            rf_cv_height,neural_network_test_ratio, neural_network_val_ratio,
            neural_network_optimal_neurons,nn_hedden_test,nn_hidden_val,
            nn_hidden_l1,nn_hidden_l2,nn_hidden_l3, kmeans_ratio)
    }
    
    else{
    result_Matrix[i,]<-data.frame(randomForest_test_ratio,randomForest_val_ratio,randomForest_optimal_trees,
                                  randomForest_optimal_height,rf_cv_test,rf_cv_val, rf_cv_trees,rf_cv_height,neural_network_test_ratio,
                                  neural_network_val_ratio, neural_network_optimal_neurons,
                                  nn_hedden_test,nn_hidden_val,nn_hidden_l1,nn_hidden_l2,nn_hidden_l3,
                                  kmeans_ratio)
    
    } 
  }
  
rownames(result_Matrix)<-c("full data","PCA data","forward/backward selection")
result_Matrix
write.csv(result_Matrix,'C:/Users/ran/Desktop/result_Matrix.csv')
rm(result_Matrix)

###############*****************RUN Kmeans with 2 saperete populations***************

label<-dataSaperete(my.data)
male<-label[[1]]
female<-label[[2]]
k_male<-KmeanForUnsupervised(male,2)
k_female<-KmeanForUnsupervised(female,2)

#plots
m_plot_index<-k_male[[1]]
f_plot_index<-k_female[[1]]
m_plot_sum_of_squares<-k_male[[2]]
f_plot_sum_of_squares<-k_female[[2]]
m_hirrarchy_cls<-k_male[[3]]
f_hirrarchy_cls<-female[[3]]
m_scatters<-k_male[[4]]
f_scatter<-k_female[[4]]