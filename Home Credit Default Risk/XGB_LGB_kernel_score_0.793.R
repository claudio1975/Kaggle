#-----------------------------------------------------------------------------------------
#Loading packages and datasets

## Importing packages
suppressMessages(library(tidyverse))
suppressMessages(library(data.table))
suppressMessages(library(caret))
suppressMessages(library(xgboost))
suppressMessages(library(lightgbm))

## Importing files
bbalance <- fread("../input/bureau_balance.csv",header = T, showProgress = F) 
bureau <- fread("../input/bureau.csv",header = T, showProgress = F)
cc_balance <- fread("../input/credit_card_balance.csv",header = T, showProgress = F)
payments <- fread("../input/installments_payments.csv",header = T, showProgress = F) 
pc_balance <- fread("../input/POS_CASH_balance.csv",header = T, showProgress = F)
prev <- fread("../input/previous_application.csv",header = T, showProgress = F)
train <- fread("../input/application_train.csv",header = T, showProgress = F) 
test <- fread("../input/application_test.csv",header = T, showProgress = F)
sub <- fread("../input/sample_submission.csv",header = T, showProgress = F)

#--------------------------------------------------------------------------------------
#Data pre-processing

#Join all datasets and feature engineering

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
  select(-TARGET) %>%
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
         PHONE_TO_EMPLOY_RATIO = DAYS_LAST_PHONE_CHANGE / DAYS_EMPLOYED) %>%
  data.matrix()

tri <- 1:nrow(train)
y <- train$TARGET

#------------------------------------------------------------------------------------------------------
#Handling missing values

## Function displaying % missing values for each column
missing_summary <- function(data_frame){
  missing_summary<-c()
  for (col in colnames(data_frame)){
    missing_summary <- c(missing_summary,mean(is.na(data_frame[,col])))
  }
    missing_summary_new <- missing_summary[missing_summary>0]
    missing_summary_cols <- colnames(data_frame)[missing_summary>0]
    missing_summary <- data.frame('col_name' = missing_summary_cols, 'perc_missing' = missing_summary_new)
    missing_summary <- missing_summary[order(missing_summary[,2], decreasing = TRUE), ]
    missing_summary[,2] <- round(missing_summary[,2],6)
    rownames(missing_summary) <- NULL
    return(missing_summary)
  }

# Displaying which columns have missing values
missing_values <- missing_summary(train_test)
missing_values

#input missing values in easy way with median (alla variables are in numeric format now)
for (col in missing_values$col_name){
  train_test[is.na(train_test[,col]),col] <- median(train_test[,col], na.rm = TRUE)
}

#check
pMiss <- function(x){sum(is.na(x))/length(x)*100}
pMiss <- apply(train_test,2,pMiss)
pMiss <- pMiss[pMiss > 0]
pMiss <- pMiss[order(pMiss, decreasing=T)]
pMiss


#-------------------------------------------------------------------------------
#Modelling XGboost

xgb.test <- xgb.DMatrix(data = train_test[-tri, ])
tr_val <- train_test[tri, ]
part <- caret::createDataPartition(y, p = 0.9, list = F) %>% c() 
xgb.train <- xgb.DMatrix(data = tr_val[part, ], label = y[part])
xgb.valid <- xgb.DMatrix(data = tr_val[-part, ], label = y[-part])

rm(tr_val,part); gc()

p <- list(objective = "binary:logistic",
          booster = "gbtree",
          eval_metric = "auc",
          nthread = 4,
          eta = 0.05,
          max_depth = 6,
          min_child_weight = 30,
          gamma = 0,
          subsample = 0.85,
          colsample_bytree = 0.7,
          colsample_bylevel = 0.632,
          alpha = 0,
          lambda = 0,
          nrounds = 2000)

set.seed =10
xgb.model <- xgb.train(p, xgb.train, p$nrounds, list(val = xgb.valid), print_every_n = 50, early_stopping_rounds = 300)

# predict

xgb_pred <- predict(xgb.model, xgb.test)


#Modelling LightGBM

y <- train$TARGET
lgb.test <- data.matrix(train_test[-tri, ])
tr_val <- train_test[tri, ]
part <- caret::createDataPartition(y, p = 0.9, list = F) %>% c() 
lgb.train <- lgb.Dataset(data = tr_val[part, ], label = y[part])
lgb.valid <- lgb.Dataset(data = tr_val[-part, ], label = y[-part])

rm(y, part,tr_val); gc()

params.lgb = list(
  objective = "binary", 
  booster = "gbtree",
  metric = "auc", 
  min_data_in_leaf = 1,
  min_sum_hessian_in_leaf = 100,
  feature_fraction = 0.9,
  bagging_fraction = 0.9,
  bagging_freq = 0,
  nthread = 4,
  eta = 0.05,
  max_depth = 8,
  min_child_weight = 22,
  colsample_bytree = 0.9497036,
  colsample_bylevel = 0.7
)

lgb.model <- lgb.train(
  params = params.lgb, 
  data = lgb.train, 
  valids = list(val = lgb.valid), 
  learning_rate = 0.02, 
  num_leaves = 7, 
  num_threads = 2, 
  nrounds = 3000, 
  early_stopping_rounds = 200, 
  eval_freq = 50
)

# predict
lgb_pred <- predict(lgb.model, data = lgb.test, n = lgb.model$best_score)

#-----------------------------------------------------------------------
#Ensemble and submission

sub <- data.table(sub$SK_ID_CURR,pred=0.5*xgb_pred+0.5*lgb_pred)
colnames(sub) <-  c("SK_ID_CURR","TARGET")
head(sub)
tail(sub)
#print submission file
write.csv(sub, 'sub_xgb_lgb.csv', quote=F, na="", row.names=F)

#soring: 0.793