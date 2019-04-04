####################################### Prepare workspace ####################################

library(dplyr)
library(caret)
library(MLmetrics)
library(doParallel)
library(randomForest)
library(pROC)

# upload files
data_path <- "C:/Users/User/Documents/CGnal/challenge/data.csv"
data_path_out <- "C:/Users/User/Documents/CGnal/challenge/ml_case_training_output.csv"
data_path_sub <- "C:/Users/User/Documents/CGnal/challenge/ml_case_test_output_template.csv"

data <- read.csv(data_path)
out <- read.csv(data_path_out)
sub <- read.csv(data_path_sub)

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

# split train set into train and validation set
n_cores <- detectCores(); n_cores
registerDoParallel(cores = n_cores - 1)

# Run the model with the best parameters chosen from the tuning
set.seed(12345)
random_forest <- randomForest(churn ~ .,
                            training,
                            mtry = 21,
                            cv.folds=5,
                            classProbs = TRUE,
                            allowParallel = TRUE
                            
)
  
print(random_forest)
plot(random_forest)

# fit the model with training, validation and test set
par(mfrow=c(1,2))
pred_tr <- predict(random_forest,new_data=training)
confusionMatrix(pred_tr, training$churn)
ctable <- table(pred_tr, training$churn)
fourfoldplot(ctable, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Training Confusion Matrix")
     
pred_val <- predict(random_forest, newdata = validation)
confusionMatrix(pred_val,validation$churn)
ctable <- table(pred_val, validation$churn)
fourfoldplot(ctable, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Validation Confusion Matrix")


pred_test <- predict(random_forest, newdata = test, type='prob')

# Prepare submission file
colnames(sub) <- c('','id','Churn_prediction','Churn_probability')
sub$Churn_prediction <- ifelse(pred_test[,2]<0.5,0,1)
sub$Churn_probability <- pred_test[,2]
sub <- sub[order(sub$Churn_probability, decreasing=TRUE),]
head(sub)
tail(sub)
write.csv(sub, 'ml_case_test_output_template.csv',row.names = FALSE, quote=F)
