library(ggplot2)
#ggplot2 is for plotting of your complex graphs that will help us in relations
library(caTools)
#caTools is our library for fast calculations and basic arithmetic utilities 
library(dplyr)
library(magrittr)
library(randomForest)
#randomForest is most important over here as it will create our decision trees for observations
#data and the majority of outcomes will be taken as the final outcome for an observations.

train <- read.csv("train_v9rqX0R.csv")
test <- read.csv("test_AbJTz2l.csv")

# checking for rows with missing data
nRows <- nrow(train)
completeRows <- sum(complete.cases(train)) #calculating the number of rows with complete data
completeRows/nRows #0.8283468

# Check fat levels in train model
(train$Item_Fat_Content)


# Converting irregulaities in data for getting consistency
train$Item_Fat_Content <- gsub("LF", "lowfat",train$Item_Fat_Content)
train$Item_Fat_Content <- gsub("low fat", "lowfat",train$Item_Fat_Content)
train$Item_Fat_Content <- gsub("Low Fat", "lowfat",train$Item_Fat_Content)
train$Item_Fat_Content <- gsub("reg", "regular",train$Item_Fat_Content)
train$Item_Fat_Content <- as.factor(train$Item_Fat_Content)
# using categorical variables in statistical models differently than continuous variables
# storing data as factors insures that the modeling functions will treat such data correctly
summary(train$Item_Fat_Content)

# Using mean weight to replace the NA or missing value in Item_Weight
mean_weight <- mean(train$Item_Weight[!is.na(train$Item_Weight)])
train$Item_Weight[is.na(train$Item_Weight)] <-mean_weight

#use regression so that we can replace the zeroes in Item_Visibility variable
temp_var_train <- train %>% filter(train$Item_Visibility != 0)
#%>% is pipeline operator it is encoded intor R and it passes one's input to other's output here, we pass the filter into our next command
visibility_model <- lm(train$Item_Visibility ~ train$Item_Weight + train$Item_Fat_Content +
                         train$Item_Type + train$Item_MRP +
                         train$Outlet_Establishment_Year + train$Outlet_Size +
                         train$Outlet_Location_Type + train$Item_Outlet_Sales,
                       data = temp_var_train)
train$Item_Visibility[train$Item_Visibility == 0] <-
  predict(visibility_model,newdata = train[train$Item_Visibility == 0,])
#the above lines are making a linar model for our predictions and take the values on the basis of all the parameters we gave.

#we now check for the missing values and replace them
set.seed(100) # Generate random numbers.
train$Outlet_Size <- as.character(train$Outlet_Size)
Storetypes <- subset(train, Outlet_Size != "")
spl <- sample.split(Storetypes$Outlet_Size, SplitRatio = 0.8283468)
train_outlet <- subset(Storetypes, spl == TRUE)
test_outlet <- subset(Storetypes, spl == FALSE)
#now we will use our random forest library for classification
train_outlet$Outlet_Size <- as.factor(train_outlet$Outlet_Size)
test_outlet$Outlet_Size <- as.factor(test_outlet$Outlet_Size)
## Creating the model
modelForest <- randomForest(Outlet_Size ~.-Item_Outlet_Sales -Item_Identifier,
                           data =  train_outlet,nodesize = 25, ntree = 100)  
#making predictions on test matrix 
PredictForest <- predict(modelForest, newdata = test_outlet)
## This is the mixed or the confusing intersection matrix
table(test_outlet$Outlet_Size, PredictForest)
# Final Classification
train$Outlet_Size <- predict(modelForest, newdata = train)


# Check rows are all filled? Yes they are
nRows <- nrow(train)
nCompRows <- sum(complete.cases(train))
nCompRows/nRows