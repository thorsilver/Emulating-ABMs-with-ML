#install.packages("caret") 
#install.packages("tictoc") 
#install.packages("kernlab") 

library(e1071) # for svm
library(caTools) # for sampling
library(dplyr) # for %>% function
library(caret) # machine learning
library(Metrics) # for rmse
library(kernlab) # for kernel
library(tictoc)
library(xlsx)


## Load the data
# Read data from .csv file
svm_x= read.xlsx2("D:/E Documents/Social_care/SVM_R/Data/LPtau1600runs_x.xlsx", sheetIndex=1, header=FALSE)
svm_y= read.xlsx2("D:/E Documents/Social_care/SVM_R/Data/LPtau1600runs_y.xlsx", sheetIndex=1, header=FALSE, colClasses="numeric")
  
#svm_x= read.csv("D:/E Documents/Social_care/SVM_R/Data/LPtau1600runs_x.csv", header=FALSE)
#svm_y= read.csv("D:/E Documents/Social_care/SVM_R/Data/LPtau1600runs_y.csv", header=FALSE)

## make sure all values are numeric
svm_x[]=lapply(svm_x,type.convert,as.is=TRUE)
svm_y[]=lapply(svm_y,type.convert,as.is=TRUE)
## Create dataframe and matrix
# dataframe
rm(svmdata)

svmdata_df = data.frame()[1:1600, ]
svmdata_df$x = svm_x
svmdata_df$y = svm_y

#matrix
svmdata = matrix(as.numeric(unlist(svmdata_df)),nrow=nrow(svmdata_df)) # convert to numeric matrix

## Create subsets for both dataframe and matrix
# split the data into a training\validation 80%, validation, 20% test sets (uses caTools)
set.seed(48) 
sample = sample.split(svmdata[,1], SplitRatio = .8)
svmtrain = subset(svmdata, sample == TRUE)
svmtest = subset(svmdata, sample == FALSE)


svmtrain_df = subset(svmdata_df, sample == TRUE)
svmtest_df = subset(svmdata_df, sample == FALSE)

tic("svm")
# Fit the model on the training set 123
set.seed(123)
model <- train(
  x=svmtrain_df$x, y=svmtrain[,11], method = "svmLinear"
   #trControl = trainControl("cv", number = 10),
   #tuneLength = 10 
)


# Print the best tuning parameter sigma and C that
# maximizes model accuracy
model$bestTune

# Make predictions on the test data
predYtest <- model %>% predict(svmtest_df$x)

## MSE for SVR Model

#Calculate RMSE 
#RMSEsvm=rmse(predYtest,svmtest[,11])
#RMSEsvm %>% .^2

# calculate the MSE
error_svm <- svmtest[,11] - predYtest
svm_mse <- mean(error_svm^2)
svm_mse

toc()

## write data to file
write.xlsx(predYtest, file="D:/E Documents/Social_care/SVM_R/Results/svm1600_lin_pred.xlsx",col.names=F, row.names=F, append=F)
write.xlsx(svmtest[,11], file="D:/E Documents/Social_care/SVM_R/Results/svm1600_lin_y.xlsx",col.names=F, row.names=F, append=F)

