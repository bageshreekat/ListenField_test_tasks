library(randomForest)
library(caTools)

genom_data <- read.csv("D:/Personal/Job_related/Listenfield/Task_data/genos_train.csv")
label_data <- read.csv("D:/Personal/Job_related/Listenfield/Task_data/phenos.csv")

#Filtering and merging trait1 data into genom data
trait1_indiv <- label_data[,c(1:1,5:5)]
df_trait_markers <- merge(genom_data, trait1_indiv, by= 'ind')
#table(unlist(genom_data[,2:10000]))

head(df_trait_markers[1:10, c(1:1,10000:10002)])
df_trait_markers <- df_trait_markers[,-1]
head(df_trait_markers[1:10, c(1:1,10000:10001)])
#df_trait_markers <- na.omit(df_trait_markers)

###########################################################
## Splitting the data into training and validation 
set.seed(120)
split <- sample.split(df_trait_markers$trait1, SplitRatio = 0.8)

train_set <- subset(df_trait_markers, split == "TRUE") #training data
valid_set <- subset(df_trait_markers, split == "FALSE") #validation data

##########################################################
#Building randomforest model
classifier_RF = randomForest(x = train_set[,-10001],
                             y = train_set$trait1,
                             ntree = 500)

save(classifier_RF,file = "D:/Personal/Job_related/Listenfield/Task_data/genos_forest1.RData")
#classifier_RF <- get(load("D:/Personal/Job_related/Listenfield/Task_data/genos_forest1.RData"))
#classifier_RF

#Prediction on valid data
y_pred_valid <- predict(classifier_RF, newdata = valid_set[,-10001])

####
# Plotting model
plot(classifier_RF)
# Importance plot
head(importance(classifier_RF))
# Variable importance plot
varImpPlot(classifier_RF)

#Mean Squared Error of the model valid
mse_genomRF <- mean((valid_set$trait1 - y_pred_valid)^2) #446.8385
mse_genomRF

#######################################################################
## Prediction of new individual potential for trait1
genom_data_test_ori <- read.csv("D:/Personal/Job_related/Listenfield/Task_data/genos_pred.csv")
genom_data_test <- genom_data_test_ori[,-1]
y_pred_test = predict(classifier_RF, newdata = genom_data_test)
print(y_pred_test)

predicted_traits_RF <- as.data.frame(cbind(genom_data_test_ori$ind, y_pred_test))
names(predicted_traits_RF) <- c("ind", "Predicted_trait1")
#write.csv(predicted_traits_RF, "D:/Personal/Job_related/Listenfield/Scipts/Output/Predicted_traits_1.csv")

####################################################
####################################################
# K fold cross validation 
###################################################
##################################################

library("caret")
library(dplyr)

## Define repeated cross validation with 5 folds and three repeats
repeat_cv <- trainControl(method='repeatedcv', number=5, repeats=1)
repeat_cv
## Set seed for reproducibility
#set.seed(123)
#split <- sample.split(df_trait_markers$trait1, SplitRatio = 0.8)
#training_set <- subset(df_trait_markers, split == "TRUE")
#valid_set <- subset(df_trait_markers, split == "FALSE")

## Train a random forest model
forest <- train(
  trait1~., 
  # Source of data
  data=train_set, 
  # `rf` method for random forest
  method='rf', 
  # Add repeated cross validation as trControl
  trControl=repeat_cv,
  # Accuracy to measure the performance of the model
  metric='RMSE')

## Print out the details about the model
forest$finalModel
forest$results
forest$control

save(forest,file = "D:/Personal/Job_related/Listenfield/Task_data/genos_forest_with_kfold.RData")

## Get variable importance, and turn into a data frame
var_imp <- varImp(forest, scale=FALSE)$importance
var_imp <- data.frame(variables=row.names(var_imp), importance=var_imp$Overall)
var_impo

typeof(var_imp)
var_imp_df_kfold <- as.data.frame(do.call(cbind, var_imp ))
typeof(var_imp$variables)

## Create a plot of variable importance
var_imp %>%
  
  ## Sort the data by importance
  arrange(importance) %>%
  ggplot(aes(x=reorder(variables, importance), y=importance)) + 
  geom_bar(stat='identity') + 
  coord_flip() + 
  xlab('Variables') +
  labs(title='Random forest (k fold) variable importance') + 
  theme_minimal() + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 20), 
  )

######################################
## Generate predictions on valid data
y_pred_valid_k <- predict(
  object=forest, 
  newdata=valid_set[,-10001])
y_pred_valid_k

## Print the accuracy
mse <- mean((valid_set$trait1 - y_pred_valid_k)^2)
cat('MSE on valid data: ', round(mse, 2),  sep='') #MSE on valid data: 429.28

#######################
## Generate predictions on test data
y_pred_test_k <- predict(
  object=forest, 
  newdata=genom_data_test)

print(y_pred_test_k)
predicted_traits_RF_k <- as.data.frame(cbind(genom_data_test_ori$ind, y_pred_test_k))
#write.csv(predicted_traits_RF_k, "D:/Personal/Job_related/Listenfield/Scipts/Output/Predicted_traits_kfold.csv")

predicted_traits_final <- cbind(predicted_traits_RF, predicted_traits_RF_k$y_pred_test_k)
names(predicted_traits_final) <- c("ind", "Predicted_trait1_RF", "Predicted_trait1_kfold")

##FInal predicted trait1 for both methods
#ind Predicted_trait1_RF Predicted_trait1_kfold
#1 Coll0751    104.126008561571        99.725095462229
#2 Coll0752    97.2269814592819        99.162370147309
#3 Coll0753    95.9129548145208       98.3368911310929
#4 Coll0754    93.7461923738718       96.1734900723572

write.csv(predicted_traits_final, "D:/Personal/Job_related/Listenfield/Scipts/Output/Predicted_traits_final.csv")

#################################################################
##########################################P##################
##############################################################