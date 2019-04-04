####################################### Prepare workspace ####################################

library(dplyr)
library(caret)
library(MLmetrics)
library(doParallel)
library(randomForest)

# Upload files
data_path <- "C:/Users/User/Documents/CGnal/challenge/data.csv"
data_path_out <- "C:/Users/User/Documents/CGnal/challenge/ml_case_training_output.csv"


data <- read.csv(data_path)
out <- read.csv(data_path_out)

# prepare train and test set
for(i in 1:length(data)){
  data[,i] <- as.numeric(data[,i])
}
for(i in 1:length(out)){
  out[,i] <- as.factor(out[,i])
}
tr <- 1:16096
te <- 16097:20120 
train <- data[tr,]
test <- data[te,]
df_tr <- cbind(out,train) %>%
  select(-id)

levels(df_tr$churn) <- make.names(levels(factor(df_tr$churn)))

# split train set into train and validation set
set.seed(2019)
inTraining <- createDataPartition(df_tr$churn, p = .80, list = FALSE)
training <- df_tr[inTraining,]
validation  <- df_tr[-inTraining,]

# prepare DoParallel
n_cores <- detectCores(); n_cores
registerDoParallel(cores = n_cores - 1)

# Tuning by grid search for upsampling using the same parameters from the base model
gs <- expand.grid(mtry = c(1,2,3,5,6,8,16,17,18,20,21))
for(j in 1:nrow(gs)){
  cat(paste0("Gridsearch ", j), sep = "\n")
  
  
  set.seed(12345)
  r_forest3 <- randomForest(churn ~ .,
                            training,
                            mtry = gs$mtry[j],
                            cv.folds=5,
                            verbose=T,
                            classProbs = TRUE,
                            allowParallel = TRUE,
                            strata = training$churn,
                            sampsize = c(100,80)
                            
  )
  
  pred_tr=predict(r_forest3,new_data=training)
  pred_val = predict(r_forest3, newdata = validation)
  AUC_tr=AUC(training$churn,pred_tr)
  Acc_tr=Accuracy(training$churn,pred_tr)
  F1_tr=F1_Score(training$churn,pred_tr)
  Pr_tr=Precision(training$churn,pred_tr)
  Re_tr=Recall(training$churn,pred_tr)
  Sp_tr=Specificity(training$churn,pred_tr)
  AUC_val=AUC(validation$churn,pred_val)
  Acc_val=Accuracy(validation$churn,pred_val)
  F1_val=F1_Score(validation$churn,pred_val)
  Pr_val=Precision(validation$churn,pred_val)
  Re_val=Recall(validation$churn,pred_val)
  Sp_val=Specificity(validation$churn,pred_val)
  print(cbind(gs$mtry[j],AUC_tr,Acc_tr,F1_tr,Pr_tr,Re_tr,Sp_tr,AUC_val,Acc_val,F1_val,Pr_val,Re_val,Sp_val))
  
}



















