library("mlbench") 
library(neuralnet) 
data(BreastCancer) 
summary(BreastCancer) 

mvindex = unique (unlist (lapply (BreastCancer, function (x) which (is.na (x))))) 
data_cleaned <- na.omit(BreastCancer) 
summary(data_cleaned) 

# using the hist() function, which computes a histogram of the given data values.
par(mfrow=c(3, 3)) 
hist(as.numeric(data_cleaned$Cl.thickness)) 
hist(as.numeric(data_cleaned$Cell.size)) 
hist(as.numeric(data_cleaned$Cell.shape)) 
hist(as.numeric(data_cleaned$Marg.adhesion))
hist(as.numeric(data_cleaned$Epith.c.size)) 
hist(as.numeric(data_cleaned$Bare.nuclei)) 
hist(as.numeric(data_cleaned$Bl.cromatin)) 
hist(as.numeric(data_cleaned$Normal.nucleoli))
hist(as.numeric(data_cleaned$Mitoses)) 


#the variables are present as a factor. We need to make a transformation for our calculations. input<-data_cleaned[,2:10] indx <- sapply(input, is.factor) 
input <- as.data.frame(lapply(input, function(x) as.numeric(as.character(x)))) 

#standardizing data
max_data <- apply(data_cleaned[,2:10], 2, max)
min_data <- apply(data_cleaned[,2:10], 2, min) 
data_scaled <- scale(data_cleaned[,2:10],center = min_data, scale = max_data - min_data)

#model.matrix used to get dummy variables 
Cancer<-data_cleaned$Class
Cancer<-as.data.frame(Cancer) 
Cancer<-with(Cancer, data.frame(model.matrix(~Cancer+0)))   

#Finally, we add the new variables to the dataset: 
  final_data<-as.data.frame(cbind(input_scaled,Cancer))
  
  #This subdivision of data in code looks like this: 
  index = sample(1:nrow(final_data),round(0.70*nrow(final_data))) 
  train_data <- as.data.frame(final_data[index,]) 
  test_data <- as.data.frame(final_data[-index,]) 
  
#Now we have to build the function to be submitted to the network: 
  n = names(final_data[1:9]) 
  f = as.formula(paste("Cancerbenign + Cancermalignant ~", paste(n, collapse = " + "))) 
  
  #output of >f
  #Cancerbenign + Cancermalignant ~ Cl.thickness + Cell.size + Cell.shape +     Marg.adhesion + Epith.c.size + Bare.nuclei + Bl.cromatin +     Normal.nucleoli + Mitoses
  
#building model  
  net = neuralnet(f,data=train_data,hidden=5,linear.output=FALSE) 
  
#making prediction
  predict_net_test <- compute(net,test_data[,1:9]) 
  predict_result<-round(predict_net_test$net.result, digits = 0) 
  
  
  