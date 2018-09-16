#--------------------------------------------------------------
cat("Preparing workspace...\n")

library(tidyverse)
library(data.table)

#-------------------------------------------------------------

cat("Loading data...\n")

T1 <- fread(".../Documents/Kaggle/Home Credit Default Risk/blend/my_blend/WEIGHT_AVERAGE_RANK_sub1.csv",header = T, showProgress = F)
T2 <- fread(".../Documents/Kaggle/Home Credit Default Risk/blend/my_blend/Blends1_0.798.csv",header = T, showProgress = F)
T3 <- fread(".../Documents/Kaggle/Home Credit Default Risk/blend/my_blend/tidy_xgb_0.796.csv",header = T, showProgress = F)
T4 <- fread(".../Documents/Kaggle/Home Credit Default Risk/blend/my_blend/sub_xgb_lgb_0.793.csv",header = T, showProgress = F)
T5 <- fread(".../Documents/Kaggle/Home Credit Default Risk/blend/my_blend/sub_xgb2_0.792.csv",header = T, showProgress = F)

#--------------------------------------------------------------

cat("handling dataframe...\n")

data_frame <- T1 %>%
  bind_cols(T2[,2]) %>%
  rename(TARGET2=TARGET1) %>%
  bind_cols(T3[,2]) %>%
  rename(TARGET3=TARGET1) %>%
  bind_cols(T4[,2]) %>%
  rename(TARGET4=TARGET1) %>%
  bind_cols(T5[,2]) %>%
  rename(TARGET5=TARGET1) %>%
  rename(TARGET1=TARGET)
head(data_frame)   

#-------------------------------------------------------

cat("Blending...\n")

blend_sub <- data_frame %>%
  select(SK_ID_CURR) %>%
  mutate(TARGET=0.6*data_frame$TARGET1+0.2*data_frame$TARGET2+0.1*data_frame$TARGET3+0.05*(data_frame$TARGET4+data_frame$TARGET5))
head(blend_sub)   

#--------------------------------------------------------------

cat("Submission...\n")

write.csv(blend_sub, 'my_blend_sub_2.csv', quote=F, na="", row.names=F)


#scoring: 0.800

