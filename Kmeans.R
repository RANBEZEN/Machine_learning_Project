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

