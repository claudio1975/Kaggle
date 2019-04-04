####################################### Prepare workspace ####################################

library(dplyr)
library(ggplot2)
library(corrplot)
library(caret)
library(lubridate)
library(xlsx)
library(funModeling)

# Upload files 
data_path_tr <- "C:/Users/User/Documents/CGnal/challenge/ml_case_training_data.csv"
data_path_trout <- "C:/Users/User/Documents/CGnal/challenge/ml_case_training_output.csv"
data_path_te <- "C:/Users/User/Documents/CGnal/challenge/ml_case_test_data.csv"
data_path_tmtr <- "C:/Users/User/Documents/CGnal/challenge/ml_case_training_hist_data.csv"
data_path_tmte <- "C:/Users/User/Documents/CGnal/challenge/ml_case_test_hist_data.csv"

train <- read.csv(data_path_tr)
out <- read.csv(data_path_trout)
test <- read.csv(data_path_te)
ts_train <- read.csv(data_path_tmtr)
ts_test <- read.csv(data_path_tmte)


# Merging train and test set
df <- rbind(train,test)
tr <- 1:nrow(train)
te <- (nrow(train)+1):nrow(df)
ts <- rbind(ts_train,ts_test)

####################### Summarize data ###################

# dimensions of dataset
dim(df)
dim(out)
dim(ts)

# list types for each attribute
sapply(df, class)
sapply(out, class)
sapply(ts,class)

# take a peek at the first rows of the data
head(df,5)
head(out,5)
head(ts,5)

# summarize attribute distributions for data frame, outcome and time series
summary(df)
summary(out)
summary(ts)

########################### Running main dataset ###########################################

# Handling missing values
# check missing values both to numeric features and categorical features 
df[df== ""] <- NA
mi_summary <- function(data_frame){
  mi_summary<-c()
  for (col in colnames(data_frame)){
    mi_summary <- c(mi_summary,mean(is.na(data_frame[,col])*100))
  }
  mi_summary_new <- mi_summary[mi_summary>0]
  mi_summary_cols <- colnames(data_frame)[mi_summary>0]
  mi_summary <- data.frame('col_name' = mi_summary_cols, 'perc_missing' = mi_summary_new)
  mi_summary <- mi_summary[order(mi_summary[,2], decreasing = TRUE), ]
  mi_summary[,2] <- round(mi_summary[,2],6)
  rownames(mi_summary) <- NULL
  return(mi_summary)
}
missing_summary <- mi_summary(df)
missing_summary

# create a dummy variables for features with NA's > 10%

df <- df %>%
  mutate(channel_sales.NA = ifelse(is.na(channel_sales),1,0))

# input missing values with median for numerical columns and with the most common level for categorical columns
for (col in missing_summary$col_name){
  if (class(df[,col]) == 'factor'){
    unique_levels <- unique(df[,col])
    df[is.na(df[,col]), col] <- unique_levels[which.max(tabulate(match(df[,col], unique_levels)))]
  } else {
    df[is.na(df[,col]),col] <- median(as.numeric(df[,col]), na.rm = TRUE)
  }
}


# drop features with high missing value percentage: > 40%
drops <- c('campaign_disc_ele','date_first_activ','forecast_base_bill_ele','forecast_base_bill_year','forecast_bill_12m','forecast_cons','activity_new')
df <- df[ , !(names(df) %in% drops)]


################################## Target variable analysis ###############################

y <- as.factor(out$churn)
# summarize the class distribution
cbind(freq=table(y), percentage=prop.table(table(y))*100)
# visualize data
ggplot(out, aes(churn, fill=churn)) + geom_bar() + labs(title="Churn barplot")

######################### split dataset into types of features ################################

cat <- c('channel_sales','has_gas','origin_up')
num <- c('id','cons_12m','cons_gas_12m','cons_last_month','forecast_cons_12m','forecast_cons_year','forecast_discount_energy',
         'forecast_meter_rent_12m','forecast_price_energy_p1','forecast_price_energy_p2','forecast_price_pow_p1','imp_cons',
         'margin_gross_pow_ele', 'margin_net_pow_ele', 'nb_prod_act net_margin', 'num_years_antig','pow_max')
date <- c('date_activ','date_end','date_modif_prod','date_renewal')  

################ Numeric features analysis (whole and split dataset) #########################

df_num_ <- df[ ,(names(df) %in% num)]
df_num  <- df_num_[,-1]
df_num_tr <- df_num[tr,]
df_num_te <- df_num[te,]


# Analysis for numeric features (correlation, histogram, boxplot, densityplot,
# univariate analysis, bivariate analysis)

for(i in 1:length(df_num)){
  df_num[,i] <- as.numeric(df_num[,i])
}
for(i in 1:length(df_num_tr)){
  df_num_tr[,i] <- as.numeric(df_num_tr[,i])
}
for(i in 1:length(df_num_te)){
  df_num_te[,i] <- as.numeric(df_num_te[,i])
}


cor <- cor(df_num,method = "spearman")
corrplot(cor, type="lower", tl.col = "black", diag=FALSE, method="number")

for(i in 1:length(df_num)) {
  hist(df_num[,i], main=names(df_num)[i], col='blue')
}

for(i in 1:length(df_num)) {
  boxplot(df_num[,i], main=names(df_num)[i], col='orange')
}


for(i in 1:length(df_num)){
  plot(density(df_num[,i]), main=names(df_num)[i], col='red')
}

df_num_try <- cbind(y,df_num_tr)
cross_plot(df_num_try, input='cons_12m', target='y', plot_type= 'quantity')
cross_plot(df_num_try, input='cons_gas_12m', target='y', plot_type= 'quantity')
cross_plot(df_num_try, input='cons_last_month', target='y', plot_type= 'quantity')
cross_plot(df_num_try, input='forecast_cons_12m', target='y', plot_type= 'quantity')
cross_plot(df_num_try, input='forecast_cons_year', target='y', plot_type= 'quantity')
cross_plot(df_num_try, input='forecast_discount_energy', target='y', plot_type= 'quantity')
cross_plot(df_num_try, input='forecast_meter_rent_12m', target='y', plot_type= 'quantity')
cross_plot(df_num_try, input='forecast_price_energy_p1', target='y', plot_type= 'quantity')
cross_plot(df_num_try, input='forecast_price_energy_p2', target='y', plot_type= 'quantity')
cross_plot(df_num_try, input='forecast_price_pow_p1', target='y', plot_type= 'quantity')
cross_plot(df_num_try, input='imp_cons', target='y', plot_type= 'quantity')
cross_plot(df_num_try, input='margin_gross_pow_ele', target='y', plot_type= 'quantity')
cross_plot(df_num_try, input='margin_net_pow_ele', target='y', plot_type= 'quantity')
cross_plot(df_num_try, input='num_years_antig', target='y', plot_type= 'quantity')
cross_plot(df_num_try, input='pow_max', target='y', plot_type= 'quantity')


for(i in 1:length(df_num_tr)){
  cat(names(df_num_tr)[i],sep = "\n")
  cat("train",sep = "\n")
  print(summary(df_num_tr[,i]))
  cat("test",sep = "\n")
  print(summary(df_num_te[,i]))
  stand.deviation_tr = sd(df_num_tr[,i])
  variance_tr = var(df_num_tr[,i])
  skewness_tr = mean((df_num_tr[,i] - mean(df_num_tr[,i]))^3/sd(df_num_tr[,i])^3)
  kurtosis_tr = mean((df_num_tr[,i] - mean(df_num_tr[,i]))^4/sd(df_num_tr[,i])^4) - 3
  outlier_values_tr <- sum(table(boxplot.stats(df_num_tr[,i])$out))
  print(cbind(stand.deviation_tr, variance_tr, skewness_tr, kurtosis_tr, outlier_values_tr))
  stand.deviation_te = sd(df_num_te[,i])
  variance_te = var(df_num_te[,i])
  skewness_te = mean((df_num_te[,i] - mean(df_num_te[,i]))^3/sd(df_num_te[,i])^3)
  kurtosis_te= mean((df_num_te[,i] - mean(df_num_te[,i]))^4/sd(df_num_te[,i])^4) - 3
  outlier_values_te <- sum(table(boxplot.stats(df_num_te[,i])$out))
  print(cbind(stand.deviation_te, variance_te, skewness_te, kurtosis_te, outlier_values_te))
  print(summary(glm(y~df_num_tr[,i], data=df_num_tr, family=binomial(link='logit'))))
  print(featurePlot(x=df_num_tr[,i], y=y, plot="box"))
}

####### Dates features analysis (whole dataset) ####################################

df_date <- df[ ,(names(df) %in% date)]
df_date_tr <- df[tr ,(names(df) %in% date)]
df_date_te <- df[te ,(names(df) %in% date)]


# Analysis for dates features (plot, univariate analysis and features engineering)

for(i in 1:length(df_date)) {
  counts <- table(df_date[,i])
  name <- names(df_date)[i]
  plot(counts, main=name, col='blue')
}

getmax <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmin <- function(v) {
  uniqv <- unique(v)
  uniqv[which.min(tabulate(match(v, uniqv)))]
}

for(i in 1:length(df_date)){
  freq_max=getmax(df_date[,i])
    cat("freq_max:",names(df_date)[i],sep = " ")
    print(freq_max)
  freq_min=getmin(df_date[,i])
    cat("freq_min:",names(df_date)[i],sep = " ")
    print(freq_min)
}

for(i in 1:length(df_date)){
  df_date[,i] <- ymd(df_date[,i])
}

# Transformation of dates into numerical features and features engineering with them
df_date_feat <- df_date %>%
  mutate(date_activ_y = as.numeric(format(date_activ, format = "%Y"))) %>%
  mutate(date_activ_m = as.numeric(format(date_activ, format = "%m"))) %>%
  mutate(date_activ_d = as.numeric(format(date_activ, format = "%d"))) %>%
  mutate(date_end_y = as.numeric(format(date_end, format = "%Y"))) %>%
  mutate(date_end_m = as.numeric(format(date_end, format = "%m"))) %>%
  mutate(date_end_d = as.numeric(format(date_end, format = "%d"))) %>%
  mutate(date_modif_prod_y = as.numeric(format(date_modif_prod, format = "%Y"))) %>%
  mutate(date_modif_prod_m = as.numeric(format(date_modif_prod, format = "%m"))) %>%
  mutate(date_modif_prod_d = as.numeric(format(date_modif_prod, format = "%d"))) %>%
  mutate(date_renewal_y = as.numeric(format(date_renewal, format = "%Y"))) %>%
  mutate(date_renewal_m = as.numeric(format(date_renewal, format = "%m"))) %>%
  mutate(date_renewal_d = as.numeric(format(date_renewal, format = "%d"))) %>%
  mutate(date_activ_num = ifelse(date_activ_d == 31 & date_activ_m == 12,
                                 date_activ_y+(date_activ_m-1)/12+(date_activ_d-1)/365,
                                 date_activ_y+(date_activ_m-1)/12+date_activ_d/365)) %>%
  mutate(date_end_num = ifelse(date_end_d == 31 & date_end_m == 12,
                                 date_end_y+(date_end_m-1)/12+(date_end_d-1)/365,
                                 date_end_y+(date_end_m-1)/12+date_end_d/365)) %>%
  mutate(date_modif_prod_num = ifelse(date_modif_prod_d == 31 & date_modif_prod_m == 12,
                               date_modif_prod_y+(date_modif_prod_m-1)/12+(date_modif_prod_d-1)/365,
                               date_modif_prod_y+(date_modif_prod_m-1)/12+date_modif_prod_d/365)) %>%
  mutate(date_renewal_num = ifelse(date_renewal_d == 31 & date_renewal_m == 12,
                               date_renewal_y+(date_renewal_m-1)/12+(date_renewal_d-1)/365,
                               date_renewal_y+(date_renewal_m-1)/12+date_renewal_d/365)) %>%
  mutate(dist_end_activ = date_end_num-date_activ_num) %>%
  mutate(dist_modif_activ = date_modif_prod_num-date_activ_num) %>%
  mutate(dist_renewal_activ = date_renewal_num-date_activ_num) %>%
  mutate(dist_end_modif = date_end_num-date_modif_prod_num) %>%
  mutate(dist_end_renewal = date_end_num-date_renewal_num) %>%
  mutate(dist_renewal_modif = date_renewal_num-date_modif_prod_num) 
  

df_date_new <- df_date_feat %>%
  select(date_activ_num,date_end_num,date_modif_prod_num,date_renewal_num,dist_end_activ,dist_modif_activ,
         dist_renewal_activ,dist_end_modif,dist_end_renewal,dist_renewal_modif) 


######### Categorical features analysis (whole and split dataset) #########################
         
df_cat <- df[ ,(names(df) %in% cat)]

# recode categorical features

levels(df_cat$channel_sales)
levels(df_cat$channel_sales)[levels(df_cat$channel_sales) == "" ] <- "A"
levels(df_cat$channel_sales)[levels(df_cat$channel_sales) == "epumfxlbckeskwekxbiuasklxalciiuu" ] <- "B"
levels(df_cat$channel_sales)[levels(df_cat$channel_sales) == "ewpakwlliwisiwduibdlfmalxowmwpci" ] <- "C"
levels(df_cat$channel_sales)[levels(df_cat$channel_sales) == "fixdbufsefwooaasfcxdxadsiekoceaa" ] <- "D"
levels(df_cat$channel_sales)[levels(df_cat$channel_sales) == "foosdfpfkusacimwkcsosbicdxkicaua" ] <- "F"
levels(df_cat$channel_sales)[levels(df_cat$channel_sales) == "lmkebamcaaclubfxadlmueccxoimlema" ] <- "G"
levels(df_cat$channel_sales)[levels(df_cat$channel_sales) == "sddiedcslfslkckwlfkdpoeeailfpeds" ] <- "H"
levels(df_cat$channel_sales)[levels(df_cat$channel_sales) == "usilxuppasemubllopkaafesmlibmsdf" ] <- "I"

levels(df_cat$origin_up)
levels(df_cat$origin_up)[levels(df_cat$origin_up) == "" ] <- "AA"
levels(df_cat$origin_up)[levels(df_cat$origin_up) == "ewxeelcelemmiwuafmddpobolfuxioce" ] <- "BB"
levels(df_cat$origin_up)[levels(df_cat$origin_up) == "kamkkxfxxuwbdslkwifmmcsiusiuosws" ] <- "CC"
levels(df_cat$origin_up)[levels(df_cat$origin_up) == "ldkssxwpmemidmecebumciepifcamkci" ] <- "DD"
levels(df_cat$origin_up)[levels(df_cat$origin_up) == "lxidpiddsbxsbosboudacockeimpuepw" ] <- "FF"
levels(df_cat$origin_up)[levels(df_cat$origin_up) == "usapbepcfoloekilkwsdiboslwaxobdp" ] <- "GG"
levels(df_cat$origin_up)[levels(df_cat$origin_up) == "aabpopmuoobccoxasfsksebxoxffdcxs" ] <- "HH"

# Analysis for categorical features (barplot, univariate analysis, bivariate analysis)
df_cat_tr <- df_cat[tr ,(names(df_cat) %in% cat)]
df_cat_te <- df_cat[te ,(names(df_cat) %in% cat)]

for(i in 1:length(df_cat)) {
  counts <- table(df_cat[,i])
  name <- names(df_cat)[i]
  barplot(counts, main=name, col='green')
}

for(i in 1:length(df_cat)){
  freq_tr=table(df_cat_tr[,i])
  percentage_tr=prop.table(table(df_cat_tr[,i]))*100
  freq_te=table(df_cat_te[,i])
  percentage_te=prop.table(table(df_cat_te[,i]))*100
  cat(names(df_cat_tr)[i],sep = "\n")
  print(cbind(freq_tr,percentage_tr,freq_te,percentage_te))
  print(chisq.test(table(y,df_cat_tr[,i])))
  counts <- table(y,df_cat_tr[,i])
  barplot(counts, col=c('blue','green','red','purple','orange'), 
          main="Distribution by churn and categorical features")
}

################### feature engineering: one-hot encoding categorical features ######################

dmy <- dummyVars("~.", data = df_cat,fullRank = F)
df_cat_num <- data.frame(predict(dmy, newdata = df_cat))

################## Time series on prices ###########################################################

# check missing values and replace of them
mi_summary <- function(data_frame){
  mi_summary<-c()
  for (col in colnames(data_frame)){
    mi_summary <- c(mi_summary,mean(is.na(data_frame[,col])*100))
  }
  mi_summary_new <- mi_summary[mi_summary>0]
  mi_summary_cols <- colnames(data_frame)[mi_summary>0]
  mi_summary <- data.frame('col_name' = mi_summary_cols, 'perc_missing' = mi_summary_new)
  mi_summary <- mi_summary[order(mi_summary[,2], decreasing = TRUE), ]
  mi_summary[,2] <- round(mi_summary[,2],6)
  rownames(mi_summary) <- NULL
  return(mi_summary)
}
missing_summary <- mi_summary(ts)
missing_summary


# input missing values with median for numerical columns 
for (col in missing_summary$col_name){
    ts[is.na(ts[,col]),col] <- median(as.numeric(ts[,col]), na.rm = TRUE)
}

# plot time seires prices
ts_ <- ts %>%
  group_by(id) %>%
  summarise_all(funs(mean)) %>%
  select(-price_date)

price_p1_var = ts(data = ts_$price_p1_var, start = c(2015,1), end=c(2015,12), frequency = 12)
price_p2_var = ts(data = ts_$price_p2_var, start = c(2015,1), end=c(2015,12), frequency = 12)
price_p3_var = ts(data = ts_$price_p3_var, start = c(2015,1), end=c(2015,12), frequency = 12)
price_p1_fix = ts(data = ts_$price_p1_fix, start = c(2015,1), end=c(2015,12), frequency = 12)
price_p2_fix = ts(data = ts_$price_p2_fix, start = c(2015,1), end=c(2015,12), frequency = 12)
price_p3_fix = ts(data = ts_$price_p3_fix, start = c(2015,1), end=c(2015,12), frequency = 12)

par(mfrow=c(2,3))
plot(price_p1_var, main='price of energy for the 1st period')
plot(price_p2_var, main='price of energy for the 2nd period')
plot(price_p3_var, main='price of energy for the 3rd period')
plot(price_p1_fix, main='price of power for the 1st period')
plot(price_p2_fix, main='price of power for the 2nd period')
plot(price_p3_fix, main='price of power for the 3rd period')

# feature engineering on prices
ts$price_date <- ymd(ts$price_date)
ts_n <- ts %>%
  mutate(price_date_y = as.numeric(format(price_date, format = "%Y"))) %>%
  select(-price_date)


fn <- funs(mean, max, min, .args = list(na.rm = TRUE))

ts_riep <- ts_n %>%
  group_by(id,price_date_y) %>% 
  summarise_all(fn) 


################################ Merging all sub datasets ##########################################

df_merge <- cbind(df_num_,df_cat_num,df_date_new)
df_all <- df_merge %>%
  left_join(ts_riep, by ="id") %>%
  select(-price_date_y) %>%
  select(-id)


################################ Pre-processing and feature engineering ##########################

for(i in 1:length(df_all)){
  df_all[,i] <- as.numeric(df_all[,i])
}

# Collinearity
comboInfo <- findLinearCombos(df_all)
comboInfo
df_all2 <- df_all[,-comboInfo$remove]

# near zero variance
nzv <- nearZeroVar(df_all2, saveMetrics= TRUE)
nzv[nzv$nzv,][1:15,]
nzv <- nearZeroVar(df_all2)
df_all3 <- df_all2[, -nzv]

# Correlation
cor <- cor(df_all3,use="complete.obs",method = "spearman")
summary(cor[upper.tri(cor)])
par(mfrow=c(1,1))
corrplot(cor, type="lower", tl.col = "black", diag=FALSE, method="number")
#write.xlsx(cor,'cgnal_corr.xlsx')

drops_corr <- c('forecast_cons_year','imp_cons','margin_net_pow_ele','has_gas.f','date_activ_num','date_renewal_num','price_p1_var_mean','price_p2_var_mean',
               'price_p3_var_mean','price_p1_fix_mean','price_p2_fix_mean','price_p3_fix_mean','price_p1_var_max','price_p2_var_max','price_p1_fix_max',
               'price_p2_fix_max','price_p3_fix_max','price_p1_var_min','price_p2_var_min','price_p1_fix_min','price_p2_fix_min','price_p3_fix_min')
dataset <- df_all3[ , !(names(df_all3) %in% drops_corr)]
cor2 <- cor(dataset,use="complete.obs",method = "spearman")
summary(cor2[upper.tri(cor2)])
highcor <- sum(abs(cor2[upper.tri(cor2)]) > .75)
highcor


# Input PCA features
set.seed(2019)
PCA = preProcess(dataset, method=c("center","scale", "pca"))
PC = predict(PCA, dataset)
ds <- cbind(PC[,1:5],dataset)

# download the file to tune and fit the model
write.csv(ds,'data.csv', row.names=FALSE)

# calculate the pre-process parameters from the dataset
set.seed(2019)
preprocessParams <- preProcess(ds, method=c("center", "scale"))
# transform the dataset using the parameters
transformed <- predict(preprocessParams, ds)


# Sample of the dataset
train_transformed <- cbind.data.frame(transformed[tr,],y)
test_trensformed <- transformed[te,]
levels(train_transformed$y) <- make.names(levels(factor(train_transformed$y)))

# Split dataset
# create a list of 80% of the rows in the train dataset to use for training
set.seed(2019)
validation_index <- createDataPartition(train_transformed$y, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- train_transformed[-validation_index,]
# use the remaining 80% of data to training and testing the models
training <- train_transformed[validation_index,]

###################################### Evaluations of algorithms #######################################
# Run algorithms using 5-fold cross validation
control <- trainControl(method="cv", number=5,classProbs = TRUE, summaryFunction = twoClassSummary)
metric <- "ROC"


# GLM
set.seed(2019)
fit.glm <- train(y~., data=training, method="glm", family=binomial(link='logit'), metric=metric, trControl=control)
print(fit.glm)
plot(varImp(fit.glm),15, main = 'GLM feature selection')


# CART
set.seed(2019)
fit.cart <- train(y~., data=training, method="rpart", metric=metric, trControl=control)
print(fit.cart)
plot(varImp(fit.cart),15, main = 'CART feature selection')


# RANDOM FOREST
set.seed(2019)
fit.rf <- train(y~., data=training, method="rf", metric=metric, trControl=control)
print(fit.rf)
plot(varImp(fit.rf),15, main='Random Forest feature selection')

# NNET
set.seed(2019)
fit.nnet <- train(y~., data=training, method="nnet", metric=metric, trControl=control)
print(fit.nnet)
plot(varImp(fit.nnet),15, main = 'Neural Network feature selection')

# GBM
set.seed(2019)
fit.gbm <- train(y~., data=training, method="gbm", metric=metric, trControl=control, verbose=F)
print(fit.gbm)
par(mar = c(4, 11, 1, 1))
summary(fit.gbm, cBars=15, las=2, plotit=T, main = 'GBM feature selection')

# Comparision of algorithms
results <- resamples(list(glm=fit.glm, cart=fit.cart, rf=fit.rf, nnet=fit.nnet, gbm=fit.gbm))
summary(results)
par(mar = c(4, 11, 1, 1))
dotplot(results, main = 'ROC results from algorithms')

# predictions
par(mfrow=c(3,2))
set.seed(2019)
prediction.glm<-predict(fit.glm,newdata=validation,type="raw")
matrix.glm<-confusionMatrix(prediction.glm,validation$y)
matrix.glm
ctable.glm <- table(prediction.glm, validation$y)
fourfoldplot(ctable.glm, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "GLM Confusion Matrix")

set.seed(2019)
prediction.cart<-predict(fit.cart,newdata=validation,type="raw")
matrix.cart<-confusionMatrix(prediction.cart,validation$y)
matrix.cart
ctable.cart <- table(prediction.glm, validation$y)
fourfoldplot(ctable.cart, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "CART Confusion Matrix")


set.seed(2019)
prediction.rf<-predict(fit.rf,newdata=validation,type="raw")
matrix.rf<-confusionMatrix(prediction.rf,validation$y)
matrix.rf
ctable.rf <- table(prediction.rf, validation$y)
fourfoldplot(ctable.rf, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "RF Confusion Matrix")

set.seed(2019)
prediction.nnet<-predict(fit.nnet,newdata=validation,type="raw")
matrix.nnet<-confusionMatrix(prediction.nnet,validation$y)
matrix.nnet
ctable.nnet <- table(prediction.nnet, validation$y)
fourfoldplot(ctable.nnet, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "NNET Confusion Matrix")

set.seed(2019)
prediction.gbm<-predict(fit.gbm,newdata=validation,type="raw")
matrix.gbm<-confusionMatrix(prediction.gbm,validation$y)
matrix.gbm
ctable.gbm <- table(prediction.gbm, validation$y)
fourfoldplot(ctable.gbm, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "GBM Confusion Matrix")


################################### EDA end #########################################################
