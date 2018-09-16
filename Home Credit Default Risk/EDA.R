#-----------------------------------------

#################################################################################################
#Exploratory Data Analysis
#################################################################################################

cat("EDA...\n")

#TARGET Variable Analysis

train %>%
  group_by(TARGET) %>%
  summarise(Count = n()/nrow(train)*100) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(TARGET = reorder(TARGET,Count)) %>%
  
  ggplot(aes(x = TARGET,y = Count, fill=TARGET)) +
  geom_bar(stat="identity")+
  geom_text(aes(x = TARGET, y = 1, label = paste0("( ",round(Count,2)," %)",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'white',
            fontface = 'bold') +
  labs(x = 'TARGET', 
       y = 'Percentage', 
       title = 'TARGET and Count',
       subtitle = '0 - No Problem in Payment ; 1 - Problem in Payment') +
  theme_bw()

#Amount Credit Distribution

train %>%
  group_by(AMT_CREDIT) %>%
  ggplot(aes(x = AMT_CREDIT)) +
  geom_histogram(bins=50, fill="steelblue") +
  labs(x= 'Amount Credit',y = 'Count', title = paste("Distribution of", ' Amount Credit ')) +
  theme_bw()

#Loans per Gender

train %>%
  group_by(CODE_GENDER) %>%
  summarise(Count = n()/nrow(train)*100) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(TARGET = reorder(CODE_GENDER,Count)) %>%
  
  ggplot(aes(x = CODE_GENDER,y = Count, fill=CODE_GENDER)) +
  geom_bar(stat="identity")+
  geom_text(aes(x = CODE_GENDER, y = 1, label = paste0("( ",round(Count,2)," %)",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'white',
            fontface = 'bold') +
  labs(x = 'GENDER', 
       y = 'Percentage', 
       title = 'Loans per Gender') +
  theme_bw()


#Family Status for the applicants

train %>%
  group_by(NAME_FAMILY_STATUS) %>%
  summarise(Count = n()/nrow(train)*100) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(TARGET = reorder(NAME_FAMILY_STATUS,Count)) %>%
  #mutate(TARGET = as.factor(TARGET)) %>%
  
  ggplot(aes(x = NAME_FAMILY_STATUS,y = Count, fill=NAME_FAMILY_STATUS)) +
  geom_bar(stat="identity")+
  geom_text(aes(x = NAME_FAMILY_STATUS, y = 1, label = paste0("( ",round(Count,2)," %)",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'white',
            fontface = 'bold') +
  labs(x = 'NAME_FAMILY_STATUS', 
       y = 'Percentage', 
       title = 'Family Status for the applicants') +
  theme_bw()


#Income Sources for the applicants

train %>%
  group_by(NAME_INCOME_TYPE) %>%
  summarise(Count = n()/nrow(train)*100) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(TARGET = reorder(NAME_INCOME_TYPE,Count)) %>%
  
  ggplot(aes(x = NAME_INCOME_TYPE,y = Count, fill=NAME_INCOME_TYPE)) +
  geom_bar(stat="identity")+
  geom_text(aes(x = NAME_INCOME_TYPE, y = 1, label = paste0("( ",round(Count,2)," %)",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'white',
            fontface = 'bold') +
  labs(x = 'NAME_INCOME_TYPE', 
       y = 'Percentage', 
       title = 'Income Sources for the applicants') +
  theme_bw()



#---------------------------
