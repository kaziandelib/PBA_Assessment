# Data Preprocessing
# Importing libraries
install.packages("tidyverse")
install.packages("reshape2")
install.packages("mice") # package for categorical & numeric imputation
install.packages("ggplot2")
install.packages("tidyr")
install.packages("lubridate")
install.packages("ConfusionTableR")
library(pacman)
library(reshape2)
library(mice) # package for categorical & numeric imputation
library(ggplot2)
library(caTools)
library(caret)
library(dplyr)
library(corrplot)
library(lubridate)
library(ConfusionTableR)

#Functions 
dropColumns <- function(data, columns_to_drop) {
  data[, !(names(data) %in% columns_to_drop), drop = FALSE]
}

remove_value <- function(data, value_to_remove) {
  filtered_data <- data[data != value_to_remove]
  return(filtered_data)
}


remove_na_from_column <- function(data, column_name) {
  # Check if the specified column exists in the data frame
  if (!(column_name %in% names(data))) {
    stop(paste("Column", column_name, "not found in the data frame."))
  }
  
  # Remove rows with NA values in the specified column
  data <- data[complete.cases(data[, column_name]), ]
  
  return(data)
}


# Function to find and drop outliers in a column of a dataframe
remove_outliers_and_plot <- function(dataframe, column_name) {
 # par(mfrow = c(1, 2))  # Set up a 1x2 grid for plots
  
  # Box plot before removing outliers
  boxplot(dataframe[[column_name]], main = paste("Box plot of", column_name, "before removing outliers"))
  
  # Identify outliers using the IQR method
  Q1 <- quantile(dataframe[[column_name]], 0.25)
  Q3 <- quantile(dataframe[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Identify and drop outliers in the dataframe
  dataframe <- dataframe[!(dataframe[[column_name]] < lower_bound | dataframe[[column_name]] > upper_bound), , drop = FALSE]
  
  # Box plot after removing outliers
  boxplot(dataframe[[column_name]], main = paste("Box plot of", column_name, "after removing outliers"))
  
  # Return the modified dataframe without outliers
  return(dataframe)
}



# Impoting the dataset
dataset <- read.csv("superstore_data.csv")
data_org <- read.csv("superstore_data.csv")
# To get the first six values
head(dataset)
# To check the dimensions of the data
dim(dataset)
# To get the column names
names(dataset)
# To get a summary of the values
summary(dataset)

# Tibble
dataset <- tibble(dataset)
head(dataset)

# To calculate which columns have missing values
sapply(dataset, function(x) sum(is.na(x)))
# To drop columns with the missing values 
dataset<-remove_na_from_column(dataset,"Income")
# To calculate which columns have missing values after dropping
sapply(dataset, function(x) sum(is.na(x)))


# Working with each column
# ID
# To drop ID here (id is irrelevant to the model)
dataset<-dropColumns(dataset,"Id")
head(dataset)


# Birth Year
n_distinct(dataset$Year_Birth)
year <- unique(dataset$Year_Birth)# Look at the unique years and assigns them to c
year
year[order(year)] # Lists the unique years in order
ggplot(data = dataset, aes(x=Year_Birth)) + geom_histogram(bins = 50, fill="palegreen", col="blue")
filter(dataset, Year_Birth==1893)
filter(dataset, Year_Birth==1899)
filter(dataset, Year_Birth==1900)
# Code to drop 1893, 1899, 1900
dataset<-dataset[!(dataset$Year_Birth==1893 | dataset$Year_Birth==1899 | dataset$Year_Birth==1900),]
ggplot(data = dataset, aes(x=Year_Birth)) + geom_histogram(bins = 50, fill="palegreen", col="blue")


#Education
n_distinct(dataset$Education)
unique(dataset$Education)
table(dataset$Education)
ggplot(data = dataset, aes(x=factor(Education))) + geom_bar()
ggplot(data = dataset, aes(x=factor(Complain), fill=factor(Education))) + geom_bar(position = "fill")
# Encoding the categorical values
dataset <- mutate(dataset, TwoNCycle = ifelse(Education =="2n Cycle",1,0))
dataset <- mutate(dataset, Basic = ifelse(Education =="Basic",1,0))
dataset <- mutate(dataset, Graduation = ifelse(Education =="Graduation",1,0))
dataset <- mutate(dataset, Master = ifelse(Education =="Master",1,0))
dataset <- mutate(dataset, PhD = ifelse(Education =="PhD",1,0))


#Marital Status
n_distinct(dataset$Marital_Status)
unique(dataset$Marital_Status)
table(dataset$Marital_Status)
ggplot(data = dataset, aes(x=factor(Marital_Status))) + geom_bar()
filter(dataset, Marital_Status=="YOLO")
filter(dataset, Marital_Status=="Alone")
filter(dataset, Marital_Status=="Absurd")
# Write code to drop the joke responses
dataset<-dataset[!(dataset$Marital_Status=="YOLO" | dataset$Marital_Status=="Absurd"| dataset$Marital_Status=="Alone"),]
table(dataset$Marital_Status)
ggplot(data = dataset, aes(x=factor(Marital_Status))) + geom_bar()
ggplot(data = dataset, aes(x=factor(Response), fill=factor(Marital_Status))) + geom_bar(position = "fill")
# One Hot Encoding the Martial Status
dataset <- mutate(dataset, Divorced = ifelse(Marital_Status =="Divorced",1,0))
dataset <- mutate(dataset, Married = ifelse(Marital_Status =="Married",1,0))
dataset <- mutate(dataset, Single = ifelse(Marital_Status =="Single",1,0))
dataset <- mutate(dataset, Together = ifelse(Marital_Status =="Together",1,0))
dataset <- mutate(dataset, Widow = ifelse(Marital_Status =="Widow",1,0))


# Income:
ggplot(data = dataset, aes(x=Income)) + geom_histogram(bins = 75, fill="palegreen", col="blue")
n_distinct(dataset$Income)
dim(dataset)
ggplot(data = dataset, aes(x=factor(Response), y=Income)) + geom_boxplot()
dataset <- remove_outliers_and_plot(dataset,"Income")
dim(dataset)
ggplot(data = dataset, aes(x=factor(Response), y=Income)) + geom_boxplot()

# Kidhome
n_distinct(dataset$Kidhome)
unique(dataset$Kidhome)
ggplot(data = dataset, aes(x=Kidhome, fill=factor(Response))) + geom_bar(position = "fill")


# Teenhome
n_distinct(dataset$Teenhome)
unique(dataset$Teenhome)
ggplot(data = dataset, aes(x=Teenhome, fill=factor(Response))) + geom_bar(position = "fill")


# Dt_Customer
dataset$Dt_Customer <- mdy(dataset$Dt_Customer)
ggplot(data = dataset, aes(x=Dt_Customer, fill=factor(Response))) + geom_boxplot()
ggplot(data = dataset, aes(x=Dt_Customer, fill=factor(Response))) + geom_bar()

#Recency
n_distinct(dataset$Recency)
unique(dataset$Recency)
ggplot(data = dataset, aes(x=Recency)) + geom_histogram(bins = 75, fill="palegreen", col="blue")
ggplot(data = dataset, aes(x=factor(Response), y=Recency)) + geom_boxplot()

#MntWines
n_distinct(dataset$MntWines)
ggplot(data = dataset, aes(x=MntWines)) + geom_histogram(bins = 50, fill="palegreen", col="blue")
ggplot(data = dataset, aes(x=factor(Response), y=MntWines)) + geom_boxplot()


#MntFruits
n_distinct(dataset$MntFruits)
ggplot(data = dataset, aes(x=MntFruits)) + geom_histogram(bins = 50, fill="palegreen", col="blue")
ggplot(data = dataset, aes(x=factor(Response), y=MntFruits)) + geom_boxplot() 


#MntMeatProducts
n_distinct(dataset$MntMeatProducts)
ggplot(data = dataset, aes(x=MntMeatProducts)) + geom_histogram(bins = 50, fill="palegreen", col="blue")
ggplot(data = dataset, aes(x=factor(Response), y=MntMeatProducts)) + geom_boxplot()


#MntFishProducts
n_distinct(dataset$MntFishProducts)
ggplot(data = dataset, aes(x=MntFishProducts)) + geom_histogram(bins = 50, fill="palegreen", col="blue")
ggplot(data = dataset, aes(x=factor(Response), y=MntFishProducts)) + geom_boxplot()


#MntSweetProducts
n_distinct(dataset$MntSweetProducts)
ggplot(data = dataset, aes(x=MntSweetProducts)) + geom_histogram(bins = 50, fill="palegreen", col="blue")
ggplot(data = dataset, aes(x=factor(Response), y=MntSweetProducts)) + geom_boxplot()


#MntGoldProds
n_distinct(dataset$MntGoldProds)
ggplot(data = dataset, aes(x=MntGoldProds)) + geom_histogram(bins = 50, fill="palegreen", col="blue")
ggplot(data = dataset, aes(x=factor(Response), y=MntGoldProds)) + geom_boxplot()

#NumDealsPurchases
n_distinct(dataset$NumDealsPurchases)
ggplot(data = dataset, aes(x=NumDealsPurchases)) + geom_histogram(bins = 50, fill="palegreen", col="blue")
ggplot(data = dataset, aes(x=factor(Response), y=NumDealsPurchases)) + geom_boxplot()


#NumWebPurchases
n_distinct(dataset$NumWebPurchases)
ggplot(data = dataset, aes(x=NumWebPurchases)) + geom_histogram(bins = 50, fill="palegreen", col="blue")
ggplot(data = dataset, aes(x=factor(Response), y=NumWebPurchases)) + geom_boxplot()

#NumCatalogPurchases
n_distinct(dataset$NumCatalogPurchases)
ggplot(data = dataset, aes(x=NumCatalogPurchases)) + geom_histogram(bins = 50, fill="palegreen", col="blue")
ggplot(data = dataset, aes(x=factor(Response), y=NumCatalogPurchases)) + geom_boxplot()


#NumStorePurchases
n_distinct(dataset$NumStorePurchases)
ggplot(data = dataset, aes(x=NumStorePurchases)) + geom_histogram(bins = 50, fill="palegreen", col="blue")
ggplot(data = dataset, aes(x=factor(Response), y=NumStorePurchases)) + geom_boxplot()


#NumWebVisitsMonth
n_distinct(dataset$NumWebVisitsMonth)
ggplot(data = dataset, aes(x=NumWebVisitsMonth)) + geom_histogram(bins = 50, fill="palegreen", col="blue")
ggplot(data = dataset, aes(x=factor(Response), y=NumWebVisitsMonth)) + geom_boxplot()

#Response
n_distinct(dataset$Response)
unique(dataset$Response)
table(dataset$Response)
barplot(table(dataset$Response))

#Complain
n_distinct(dataset$Complain)
unique(dataset$Complain)
barplot(table(dataset$Complain))
ggplot(data = dataset, aes(x=factor(Complain), fill=factor(Response))) + geom_bar(position = "fill")


# Data Imbalance
dataset$Response <- factor(dataset$Response, level = c(0, 1), labels = c(0, 1))
target_column_index <- match("Response", names(dataset))
new_upsampled_data <- upSample(dataset[-target_column_index], dataset$Response, yname = "Response")
table(new_upsampled_data$Response)
ggplot(data = new_upsampled_data, aes(Response)) + geom_bar()

ggplot(data = new_upsampled_data, aes(x=Response, y=MntWines)) + geom_boxplot()


new_upsampled_data<-dropColumns(new_upsampled_data,"Education")
new_upsampled_data<-dropColumns(new_upsampled_data,"Marital_Status")
dim(new_upsampled_data)

# New Upsampled dataset with outliers removed
new_upsampled_data_no_outliers <- remove_outliers_and_plot(new_upsampled_data,"MntWines")
new_upsampled_data_no_outliers <- remove_outliers_and_plot(new_upsampled_data,"MntFruits")
new_upsampled_data_no_outliers <- remove_outliers_and_plot(new_upsampled_data,"MntMeatProducts")
new_upsampled_data_no_outliers <- remove_outliers_and_plot(new_upsampled_data,"MntFishProducts")
new_upsampled_data_no_outliers <- remove_outliers_and_plot(new_upsampled_data,"MntSweetProducts")
new_upsampled_data_no_outliers <- remove_outliers_and_plot(new_upsampled_data,"MntGoldProds")
new_upsampled_data_no_outliers <- remove_outliers_and_plot(new_upsampled_data,"NumDealsPurchases")
new_upsampled_data_no_outliers <- remove_outliers_and_plot(new_upsampled_data,"NumWebPurchases")
new_upsampled_data_no_outliers <- remove_outliers_and_plot(new_upsampled_data,"NumCatalogPurchases")
new_upsampled_data_no_outliers <- remove_outliers_and_plot(new_upsampled_data,"NumWebVisitsMonth")
dim(new_upsampled_data_no_outliers)

# Feature Selection/Importance
install.packages("randomForest")
library(randomForest)
# Random Forest Model on upsampled data
set.seed(123)
rf_model <- randomForest(Response~., data = new_upsampled_data, ntree = 500)
feature_importance <- importance(rf_model)
sorted_importance <- feature_importance[order(-feature_importance), ]

# Barplot for the first model
barplot_obj <- barplot(sorted_importance, names.arg = names(sorted_importance),
                       main = "Feature Importance",
                       xlab = "Importance",
                       col = "skyblue",
                       las = 2, # Rotates axis labels vertically
                       cex.names = 0.7) # Adjust font size of axis labels



# Add text labels on top of each bar for the first model
text(x = barplot_obj, y = sorted_importance, label = as.integer(sorted_importance), pos = 3, cex = 0.8)


# Random Forest Model on data without outliers
set.seed(123)
rf_model_no_outliers <- randomForest(Response~., data = new_upsampled_data_no_outliers, ntree = 500)
feature_importance_no_outliers <- importance(rf_model_no_outliers)
sorted_importance_no_outliers <- feature_importance_no_outliers[order(-feature_importance_no_outliers), ]



# Barplot for the second model
barplot_obj_no_outliers <- barplot(sorted_importance_no_outliers, names.arg = names(sorted_importance_no_outliers),
                                   main = "Feature Importance (No Outliers)",
                                   xlab = "Importance",
                                   col = "aquamarine",
                                   las = 2, # Rotates axis labels vertically
                                   cex.names = 0.7) # Adjust font size of axis labels



# Add text labels on top of each bar for the second model
text(x = barplot_obj_no_outliers, y = sorted_importance_no_outliers, label = as.integer(sorted_importance_no_outliers), pos = 3, cex = 0.8)

# Choosing a threshold of 40
modelling_data1 <- select(new_upsampled_data_no_outliers, Recency,
                          starts_with("Mnt"), Income, starts_with("Num"), Year_Birth,
                          Teenhome, Dt_Customer, Response)

# Choosing a threshold of 90
modelling_data2 <- select(new_upsampled_data, Recency, MntWines, Income,
                          MntMeatProducts, MntGoldProds, NumCatalogPurchases, NumStorePurchases,
                          Dt_Customer, Response)


# Train-Test Split
set.seed(123)
split <- sample.split(modelling_data1$Response, SplitRatio = 0.70)
modelling_data1_training <- subset(modelling_data1, split==TRUE)
modelling_data1_testing <- subset(modelling_data1, split==FALSE)

set.seed(123)
split2 <- sample.split(modelling_data2$Response, SplitRatio = 0.70)
modelling_data2_training <- subset(modelling_data2, split==TRUE)
modelling_data2_testing <- subset(modelling_data2, split==FALSE)


# Scaling
scaled_modelling_data1_training <- subset(modelling_data1, split==TRUE)
scaled_modelling_data1_testing <- subset(modelling_data1, split==FALSE)
scaled_modelling_data1_training[, 1:15] <- scale(modelling_data1_training[, 1:15])
scaled_modelling_data1_testing[, 1:15] <- scale(modelling_data1_testing[, 1:15])


scaled_modelling_data2_training <- subset(modelling_data2, split==TRUE)
scaled_modelling_data2_testing <- subset(modelling_data2, split==FALSE)
scaled_modelling_data2_training[, 1:7] <- scale(modelling_data2_training[, 1:7])
scaled_modelling_data2_testing[, 1:7] <- scale(modelling_data2_testing[, 1:7])



# Random Forest
library(randomForest)

# Using feature theshold = 40 and no scaling
set.seed(1)
rf_classifier = randomForest(x = modelling_data1_training[-17],
                             y = modelling_data1_training$Response, ntree = 500)
rf_classifier

mtry <- tuneRF(modelling_data1_training[-17], modelling_data1_training$Response, 
               ntreeTry=500, stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

set.seed(1)
rf_classifier = randomForest(x = modelling_data1_training[-17],
                             y = modelling_data1_training$Response, ntree = 60,
                             mtry = best.m,importance=TRUE)
rf_classifier


y_pred_thresh_40_no_scale_rf = predict(rf_classifier, newdata = modelling_data1_testing[-17])
cm = table(modelling_data1_testing$Response, y_pred_thresh_40_no_scale_rf)
cm

ConfusionTableR::binary_visualiseR(train_labels = modelling_data1_testing$Response,
                                   truth_labels= y_pred_thresh_40_no_scale_rf,
                                   class_label1 = "Declined", 
                                   class_label2 = "Accepted",
                                   quadrant_col1 = "aquamarine3", 
                                   quadrant_col2 = "lightblue2",
                                   custom_title = "Gold Membership Confusion Martix", 
                                   text_col= "black")

importance(rf_classifier)
varImp(rf_classifier)
varImpPlot(rf_classifier)


# Using feature theshold = 40 and scaling
set.seed(1)
rf_classifier = randomForest(x = scaled_modelling_data1_training[-17],
                             y = scaled_modelling_data1_training$Response, ntree = 500)
rf_classifier

mtry <- tuneRF(scaled_modelling_data1_training[-17], scaled_modelling_data1_training$Response, 
               ntreeTry=500, stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

set.seed(1)
rf_classifier = randomForest(x = scaled_modelling_data1_training[-17],
                             y = scaled_modelling_data1_training$Response, ntree = 60,
                             mtry = best.m,importance=TRUE)
rf_classifier

y_pred_thresh_40_scaled_rf = predict(rf_classifier, newdata = scaled_modelling_data1_testing[-17])
cm = table(scaled_modelling_data1_testing$Response, y_pred_thresh_40_scaled_rf)
cm

ConfusionTableR::binary_visualiseR(train_labels = scaled_modelling_data1_testing$Response,
                                   truth_labels= y_pred_thresh_40_scaled_rf,
                                   class_label1 = "Declined", 
                                   class_label2 = "Accepted",
                                   quadrant_col1 = "aquamarine3", 
                                   quadrant_col2 = "lightblue2", 
                                   custom_title = "Gold Membership Confusion Martix", 
                                   text_col= "black")

importance(rf_classifier)
varImp(rf_classifier)
varImpPlot(rf_classifier)

# Using feature theshold = 90 and no scaling
set.seed(1)
rf_classifier = randomForest(x = modelling_data2_training[-9],
                             y = modelling_data2_training$Response, ntree = 500)
rf_classifier

mtry <- tuneRF(modelling_data2_training[-9], modelling_data2_training$Response, 
               ntreeTry=500, stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)


set.seed(1)
rf_classifier = randomForest(x = modelling_data2_training[-9],
                             y = modelling_data2_training$Response, ntree = 60,
                             mtry = best.m,importance=TRUE)
rf_classifier

y_pred_thresh_90_no_scale_rf = predict(rf_classifier, newdata = modelling_data2_testing[-9])
cm = table(modelling_data2_testing$Response, y_pred_thresh_90_no_scale_rf)
cm

ConfusionTableR::binary_visualiseR(train_labels = modelling_data2_testing$Response,
                                   truth_labels= y_pred_thresh_90_no_scale_rf,
                                   class_label1 = "Declined", 
                                   class_label2 = "Accepted",
                                   quadrant_col1 = "aquamarine3", 
                                   quadrant_col2 = "lightblue2", 
                                   custom_title = "Gold Membership Confusion Martix", 
                                   text_col= "black")


importance(rf_classifier)
varImp(rf_classifier)
varImpPlot(rf_classifier)

# Using feature theshold = 90 and scaling
set.seed(1)
rf_classifier = randomForest(x = scaled_modelling_data2_training[-9],
                             y = scaled_modelling_data2_training$Response, ntree = 500)
rf_classifier


tuneRF(scaled_modelling_data2_training[-9], scaled_modelling_data2_training$Response, stepFactor = 3)
mtry <- tuneRF(scaled_modelling_data2_training[-9], scaled_modelling_data2_training$Response, 
               ntreeTry=500, stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

set.seed(1)
rf_classifier = randomForest(x = scaled_modelling_data2_training[-9],
                             y = scaled_modelling_data2_training$Response, ntree = 60,
                             mtry = best.m,importance=TRUE)
rf_classifier

y_pred_thresh_90_scaled_rf = predict(rf_classifier, newdata = scaled_modelling_data2_testing[-9], type="class")
cm = table(scaled_modelling_data2_testing$Response, y_pred_thresh_90_scaled_rf)
cm
ConfusionTableR::binary_visualiseR(train_labels = scaled_modelling_data2_testing$Response,
                                   truth_labels= y_pred_thresh_90_scaled_rf,
                                   class_label1 = "Declined", 
                                   class_label2 = "Accepted",
                                   quadrant_col1 = "aquamarine3", 
                                   quadrant_col2 = "lightblue2", 
                                   custom_title = "Gold Membership Confusion Martix", 
                                   text_col= "black")

importance(rf_classifier)
varImp(rf_classifier)
varImpPlot(rf_classifier)


# K-Nearest Neighbor
# Using feature theshold = 40 and no scaling
knnModel <- train(
  Response ~ ., 
  data = modelling_data1_training, 
  method = "knn", 
  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3), 
  tuneGrid = data.frame(k = c(5, 7,9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33)))

knnModel
plot(knnModel)

best_model<- knn3(
  Response ~ .,
  data = modelling_data1_training,
  k = knnModel$bestTune$k)

y_pred_thresh_40_no_scale_knn <- predict(best_model, modelling_data1_testing,type = "class")

cm <- table(modelling_data1_testing$Response, y_pred_thresh_40_no_scale_knn)
cm

ConfusionTableR::binary_visualiseR(train_labels = modelling_data1_testing$Response,
                                   truth_labels= y_pred_thresh_40_no_scale_knn,
                                   class_label1 = "Declined", 
                                   class_label2 = "Accepted",
                                   quadrant_col1 = "aquamarine3", 
                                   quadrant_col2 = "lightblue2", 
                                   custom_title = "Gold Membership Confusion Martix", 
                                   text_col= "black")


# Using feature theshold = 40 and scaling
knnModel <- train(
  Response ~ ., 
  data = scaled_modelling_data1_training, 
  method = "knn", 
  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3), 
  tuneGrid = data.frame(k = c(5, 7,9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33)))

knnModel
plot(knnModel)
print(knnModel$bestTune$k)

set.seed(123)
best_model<- knn3(
  Response ~ .,
  data = scaled_modelling_data1_training,
  k = knnModel$bestTune$k)

y_pred_thresh_40_scaled_knn <- predict(best_model, scaled_modelling_data1_testing, 
                                       type = "class")

cm <- table(scaled_modelling_data1_testing$Response, y_pred_thresh_40_scaled_knn)
cm


ConfusionTableR::binary_visualiseR(train_labels = scaled_modelling_data1_testing$Response,
                                   truth_labels= y_pred_thresh_40_scaled_knn,
                                   class_label1 = "Declined", 
                                   class_label2 = "Accepted",
                                   quadrant_col1 = "aquamarine3", 
                                   quadrant_col2 = "lightblue2", 
                                   custom_title = "Gold Membership Confusion Martix", 
                                   text_col= "black")

# Using feature theshold = 90 and no scaling
knnModel <- train(
  Response ~ ., 
  data = modelling_data2_training, 
  method = "knn", 
  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3), 
  tuneGrid = data.frame(k = c(5, 7,9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33)))

knnModel
plot(knnModel)
print(knnModel$bestTune$k)

set.seed(123)
best_model<- knn3(
  Response ~ .,
  data = modelling_data2_training,
  k = knnModel$bestTune$k)

y_pred_thresh_90_no_scale_knn <- predict(best_model, modelling_data2_testing, 
                                       type = "class")

cm <- table(modelling_data2_testing$Response, y_pred_thresh_90_no_scale_knn)
cm


ConfusionTableR::binary_visualiseR(train_labels = modelling_data2_testing$Response,
                                   truth_labels= y_pred_thresh_90_no_scale_knn,
                                   class_label1 = "Declined", 
                                   class_label2 = "Accepted",
                                   quadrant_col1 = "aquamarine3", 
                                   quadrant_col2 = "lightblue2", 
                                   custom_title = "Gold Membership Confusion Martix", 
                                   text_col= "black")



# Using feature theshold = 90 and scaling
knnModel <- train(
  Response ~ ., 
  data = scaled_modelling_data2_training, 
  method = "knn", 
  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3), 
  tuneGrid = data.frame(k = c(5, 7,9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33)))

knnModel
plot(knnModel)
print(knnModel$bestTune$k)

set.seed(123)
best_model<- knn3(
  Response ~ .,
  data = scaled_modelling_data2_training,
  k = knnModel$bestTune$k)

y_pred_thresh_90_scaled_knn <- predict(best_model, scaled_modelling_data2_testing, 
                                       type = "class")

cm <- table(scaled_modelling_data2_testing$Response, y_pred_thresh_90_scaled_knn)
cm


ConfusionTableR::binary_visualiseR(train_labels = scaled_modelling_data2_testing$Response,
                                   truth_labels= y_pred_thresh_90_scaled_knn,
                                   class_label1 = "Declined", 
                                   class_label2 = "Accepted",
                                   quadrant_col1 = "aquamarine3", 
                                   quadrant_col2 = "lightblue2", 
                                   custom_title = "Gold Membership Confusion Martix", 
                                   text_col= "black")
