##############################################################################
#Feature engineering with PCA
##############################################################################

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

# scatterplot
pairs(x_PCA_train_test,col=c("red","blue","yellow","green"),pch=20)

# attach the new features respectively
data_h2o <- h2o.cbind(train_test_h2o, x_PCA_train_test)

##############################################################################
#Feature engineering with Autoencoder
##############################################################################

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

# get new featueres columns of the train data 
train_test_aec <- h2o.deepfeatures(autoencoder, data_h2o[,-1], layer = 2) %>% as.data.frame()

# scatterplot
pairs(train_test_aec,col=c("red","blue","yellow","green"),pch=20)

data_frame_aec_h2o <- as.h2o(train_test_aec)
data_new_h2o <- h2o.cbind(data_h2o,data_frame_aec_h2o)
data <- as.data.table(data_new_h2o) %>%
  select(-TARGET) %>%
  data.matrix()

h2o.shutdown()
write.csv(data, '.../Documents/Kaggle/Home Credit Default Risk/data_frame_pca_aec2.csv', quote=F, na="", row.names=F)
