source("packages installation.R")
source("preprocessing.R")
source("neuralNetwork_fullyCon.R")
source("Kmeans.R")
source("randomForest.R")


#-----------------MAIN-----------------

my.data<- read.csv('voice.csv', header = TRUE) #read the csv data

#delete some parameters after feature selection by the pdf/correlation/histograms in the data understanding level
my.data<-my.data[,c(-18,-5,-4)]  
#my.data<-balancedReductionData(my.data,763)   #reduce the dataset

#feature selection
After_FB_Data<-forwardBackward_selection(my.data) #get the dataset after forward/backward selection

#feature extraction
After_pca_data<-PCA_Selection(After_FB_Data)  #get the dataset after pca

#take the original data for comparing
my.data<- read.csv('voice.csv', header = TRUE) #read the csv data
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
write.csv(result_Matrix,'result_Matrix.csv')
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