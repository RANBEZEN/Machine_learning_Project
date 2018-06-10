#data understanding

install.packages('ggplot2')
install.packages('stats')
install.packages('ggbiplot')
require(stats)
require(ggbiplot)
require(ggplot2)
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

