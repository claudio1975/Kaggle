#################################################################################################
#Preparing enviroment
#################################################################################################
library(tidyverse)
library(data.table)
library(caret)
library(xgboost)
library(lightgbm)
library(h2o)
h2o.init(ip = "localhost",
         port = 54321,
         nthreads = 8,
         max_mem_size="14G")

#---------------------------

#################################################################################################
#Loading data
#################################################################################################

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

#-----------------------------------------

