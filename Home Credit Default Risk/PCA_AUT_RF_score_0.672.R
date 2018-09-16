#------------------------------------------------------------------------
cat("Preparing workspace...\n")

library(tidyverse)
library(data.table)
library(h2o)
h2o.init(ip = "localhost",
         port = 54321,
         nthreads = 8,
         max_mem_size="14G")

#---------------------------
cat("Loading data...\n")

bbalance <- fread("C:/Users/User/Documents/Kaggle/Home Credit Default Risk/bureau_balance.csv",header = T, showProgress = F)
bureau <- fread("C:/Users/User/Documents/Kaggle/Home Credit Default Risk/bureau.csv",header = T, showProgress = F)
cc_balance <- fread("C:/Users/User/Documents/Kaggle/Home Credit Default Risk/credit_card_balance.csv",header = T, showProgress = F)
payments <- fread("C:/Users/User/Documents/Kaggle/Home Credit Default Risk/installments_payments.csv",header = T, showProgress = F) 
pc_balance <- fread("C:/Users/User/Documents/Kaggle/Home Credit Default Risk/POS_CASH_balance.csv",header = T, showProgress = F)
prev <- fread("C:/Users/User/Documents/Kaggle/Home Credit Default Risk/previous_application.csv",header = T, showProgress = F)
train <- fread("C:/Users/User/Documents/Kaggle/Home Credit Default Risk/application_train.csv",header = T, showProgress = F) 
test <- fread("C:/Users/User/Documents/Kaggle/Home Credit Default Risk/application_test.csv",header = T, showProgress = F)
sub <- fread("C:/Users/User/Documents/Kaggle/Home Credit Default Risk/sample_submission.csv",header = T, showProgress = F)
descr <- fread("C:/Users/User/Documents/Kaggle/Home Credit Default Risk/HomeCredit_columns_description.csv",header = T, showProgress = F)

#---------------------------
cat("Preprocessing...\n")

fn <- funs(mean, sd, sum, .args = list(na.rm = TRUE))

sum_bbalance <- bbalance %>%
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_BUREAU) %>% 
  summarise_all(fn) 
rm(bbalance); gc()

sum_bureau <- bureau %>% 
  left_join(sum_bbalance, by = "SK_ID_BUREAU") %>% 
  select(-SK_ID_BUREAU) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn)
rm(bureau, sum_bbalance); gc()

sum_cc_balance <- cc_balance %>% 
  select(-SK_ID_PREV) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn)
rm(cc_balance); gc()

sum_payments <- payments %>% 
  select(-SK_ID_PREV) %>% 
  mutate(PAYMENT_PERC = AMT_PAYMENT / AMT_INSTALMENT,
         PAYMENT_DIFF = AMT_INSTALMENT - AMT_PAYMENT,
         DPD = DAYS_ENTRY_PAYMENT - DAYS_INSTALMENT,
         DBD = DAYS_INSTALMENT - DAYS_ENTRY_PAYMENT,
         DPD = ifelse(DPD > 0, DPD, 0),
         DBD = ifelse(DBD > 0, DBD, 0)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn) 
rm(payments); gc()

sum_pc_balance <- pc_balance %>% 
  select(-SK_ID_PREV) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn)
rm(pc_balance); gc()

sum_prev <- prev %>%
  select(-SK_ID_PREV) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  mutate(DAYS_FIRST_DRAWING = ifelse(DAYS_FIRST_DRAWING == 365243, NA, DAYS_FIRST_DRAWING),
         DAYS_FIRST_DUE = ifelse(DAYS_FIRST_DUE == 365243, NA, DAYS_FIRST_DUE),
         DAYS_LAST_DUE_1ST_VERSION = ifelse(DAYS_LAST_DUE_1ST_VERSION == 365243, NA, DAYS_LAST_DUE_1ST_VERSION),
         DAYS_LAST_DUE = ifelse(DAYS_LAST_DUE == 365243, NA, DAYS_LAST_DUE),
         DAYS_TERMINATION = ifelse(DAYS_TERMINATION == 365243, NA, DAYS_TERMINATION),
         APP_CREDIT_PERC = AMT_APPLICATION / AMT_CREDIT) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn) 
rm(prev); gc()


train_test <- train %>% 
  #select(-TARGET) %>% 
  bind_rows(test) %>%
  left_join(sum_bureau, by = "SK_ID_CURR") %>% 
  left_join(sum_cc_balance, by = "SK_ID_CURR") %>% 
  left_join(sum_payments, by = "SK_ID_CURR") %>% 
  left_join(sum_pc_balance, by = "SK_ID_CURR") %>% 
  left_join(sum_prev, by = "SK_ID_CURR") %>% 
  select(-SK_ID_CURR) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  mutate(na = apply(., 1, function(x) sum(is.na(x))),
         DAYS_EMPLOYED = ifelse(DAYS_EMPLOYED == 365243, NA, DAYS_EMPLOYED),
         DAYS_EMPLOYED_PERC = sqrt(DAYS_EMPLOYED / DAYS_BIRTH),
         INCOME_CREDIT_PERC = AMT_INCOME_TOTAL / AMT_CREDIT,
         INCOME_PER_PERSON = log1p(AMT_INCOME_TOTAL / CNT_FAM_MEMBERS),
         ANNUITY_INCOME_PERC = sqrt(AMT_ANNUITY / (1 + AMT_INCOME_TOTAL)),
         LOAN_INCOME_RATIO = AMT_CREDIT / AMT_INCOME_TOTAL,
         ANNUITY_LENGTH = AMT_CREDIT / AMT_ANNUITY,
         CHILDREN_RATIO = CNT_CHILDREN / CNT_FAM_MEMBERS, 
         CREDIT_TO_GOODS_RATIO = AMT_CREDIT / AMT_GOODS_PRICE,
         INC_PER_CHLD = AMT_INCOME_TOTAL / (1 + CNT_CHILDREN),
         SOURCES_PROD = EXT_SOURCE_1 * EXT_SOURCE_2 * EXT_SOURCE_3,
         CAR_TO_BIRTH_RATIO = OWN_CAR_AGE / DAYS_BIRTH,
         CAR_TO_EMPLOY_RATIO = OWN_CAR_AGE / DAYS_EMPLOYED,
         PHONE_TO_BIRTH_RATIO = DAYS_LAST_PHONE_CHANGE / DAYS_BIRTH,
         PHONE_TO_EMPLOY_RATIO = DAYS_LAST_PHONE_CHANGE / DAYS_EMPLOYED) 



## for given dataframe, display % missing for each column
mi_summary <- function(data_frame){
  mi_summary<-c()
  for (col in colnames(data_frame)){
    mi_summary <- c(mi_summary,mean(is.na(data_frame[,col])))
  }
  mi_summary_new <- mi_summary[mi_summary>0]
  mi_summary_cols <- colnames(data_frame)[mi_summary>0]
  mi_summary <- data.frame('col_name' = mi_summary_cols, 'perc_missing' = mi_summary_new)
  mi_summary <- mi_summary[order(mi_summary[,2], decreasing = TRUE), ]
  mi_summary[,2] <- round(mi_summary[,2],6)
  rownames(mi_summary) <- NULL
  return(mi_summary)
}

# which columns have missing values, and how many?
missing_summary <- mi_summary(train_test)
missing_summary

#input missing values
for (col in missing_summary$col_name){
  train_test[is.na(train_test[,col]),col] <- median(train_test[,col], na.rm = TRUE)
}

#check
pMiss <- function(x){sum(is.na(x))/length(x)*100}
pMiss <- apply(train_test,2,pMiss)
pMiss <- pMiss[pMiss > 0]
pMiss <- pMiss[order(pMiss, decreasing=T)]
pMiss

#-----------------------------------------------------------------------------------------------------------------
cat("PCA Feature engineering...\n")

# transform data to h2o data frame
train_test_h2o <- as.h2o(train_test)
response_col <- train_test_h2o[,1]

# h2o PCA model for feature engineering on train set
# get columns with data type of number
x_num <- which(!colnames(train_test_h2o) %in% c(response_col)) 

# train a PCA model
model_PCA <- h2o.prcomp( 
  training_frame = train_test_h2o,        
  x = x_num,                        
  seed = 10,                        
  
  transform = 'NORMALIZE',           
  pca_method = 'GramSVD',           
  k = 4,                          
  max_iterations = 1000,            
  impute_missing = TRUE            
)

model_PCA@model$importance

# get new featueres columns of the train data 
x_PCA_train_test <- h2o.predict(model_PCA, train_test_h2o)

head(x_PCA_train_test)

# attach the new features respectively
data_h2o <- h2o.cbind(train_test_h2o, x_PCA_train_test)

#-----------------------------------------------------------------------------------------------------------------
cat("Autoencoder Feature engineering...\n")

autoencoder <- h2o.deeplearning(training_frame = data_h2o[,-1],
                                x = names(data_h2o[,-1]),
                                autoencoder = T,
                                activation="Tanh",
                                reproducible = TRUE,
                                seed = 10,
                                sparse = T,
                                hidden = c(32, 4, 32),
                                l1=1e-4,
                                epochs = 15)

h2o.performance(autoencoder)

train_test_aec <- h2o.deepfeatures(autoencoder, data_h2o[,-1], layer = 2) %>% as.data.frame()
data_frame_aec_h2o <- as.h2o(train_test_aec)
data_new_h2o <- h2o.cbind(data_h2o,data_frame_aec_h2o)
data <- as.data.table(data_new_h2o) %>%
  data.matrix()

h2o.shutdown()


#--------------------------------------------------------------------------------------------------------
cat("Modelling...\n")

#remove feature not relevant 
data_fs <- data[,-c('FLAG_DOCUMENT_10', 'FLAG_DOCUMENT_12', 'FLAG_MOBIL', 'FLAG_DOCUMENT_7', 'FLAG_DOCUMENT_2')]

train_h2o <- as.h2o(data_fs[1:307511,])
test_h2o <- as.h2o(data_fs[307512:356255,-1])
train_h2o$TARGET <- as.factor(train_h2o$TARGET)  #encode the binary repsonse as a factor

h2o.levels(train_h2o$TARGET)

# Split dataset giving the training dataset 80% of the data
split <- h2o.splitFrame(data=train_h2o, ratios=0.80)

# Create a training set from the 1st dataset in the split
train <- split[[1]]

# Create a testing set from the 2nd dataset in the split
validation <- split[[2]]

# get input feature cols
y <-  'TARGET'
x <-  which(!h2o.colnames(train_h2o) %in% c(y))
print(x)

model_rf <- h2o.randomForest(
  
  training_frame = train,                       # training frame is the training data in h2o data format
  x = x,                                        # input columns
  y = y,                                        # response column
  nfolds = 5,                                   # n-fold cross validation
  seed = 12345,                                 # random seed set to constant
)

## Show a detailed model summary
model_rf


## Get the AUC on the validation set
h2o.auc(h2o.performance(model_rf, newdata = validation)) 


## get predictions on test set
rf_pred <- (h2o.predict(model_rf, newdata = test_h2o))

summary(rf_pred, exact_quantiles=T)

#print results
print(rf_pred)
head(rf_pred)
tail(rf_pred)

#turn in R 
pred <- as.data.table((h2o.predict(model_rf, test_h2o)))
sub <- data.table(sub$SK_ID_CURR,pred=pred[,'p1'])
colnames(sub) <-  c("SK_ID_CURR","TARGET")
head(sub)
tail(sub)
#print submission file
write.csv(sub, 'sub.csv', quote=F, na="", row.names=F)

#scoring: 0.672

