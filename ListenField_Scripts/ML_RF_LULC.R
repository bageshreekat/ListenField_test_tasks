library(sf)
library(dplyr)
library(raster)
library(tmap)
library(caret)
library(rgdal)
library(rasterVis)
library(randomForest)
library(randomForest)

setwd("D:/Personal/Job_related/Listenfield/Task_data/")

## The cloud percentage in each image is less than 3%.
##labels file
bankok_labels = read_sf("D:/Personal/Job_related/Listenfield/Task_data/LandCover_5_classes.geojson")
bankok_labels$geometry
unique(bankok_labels$type)

#Write it as shape file
#st_write(bankok_labels, "training_labels_bankok.shp")

#creating training labels
training_labels_bankok <- sf::read_sf("./training_labels_bankok.shp")
extent(training_labels_bankok)
#training_labels_bankok$type <- as.factor(training_labels_bankok$type)
crs(training_labels_bankok)
unique(training_labels_bankok$type)

### Load Sentinel2 images
sen2_211212_list <- list.files("./Sentinel_Bankok/S2A_MSIL2A_20211212T034141_N0301_R061_T47PPR_20211212T070825/S2A_MSIL2A_20211212T034141_N0301_R061_T47PPR_20211212T070825.SAFE/GRANULE/L2A_T47PPR_A033806_20211212T035217/IMG_DATA/R10m", full.names = T) 
sen2_211222_list <- list.files("./Sentinel_Bankok/S2A_MSIL2A_20211222T034151_N0301_R061_T47PPR_20211222T070757/S2A_MSIL2A_20211222T034151_N0301_R061_T47PPR_20211222T070757.SAFE/GRANULE/L2A_T47PPR_A033949_20211222T034928/IMG_DATA/R10m/", full.names = T) 
sen2_211207_list <- list.files("./Sentinel_Bankok/S2B_MSIL2A_20211207T034129_N0301_R061_T47PPR_20211207T054849/S2B_MSIL2A_20211207T034129_N0301_R061_T47PPR_20211207T054849.SAFE/GRANULE/L2A_T47PPR_A024826_20211207T035342/IMG_DATA/R10m/", full.names = T)
sen2_211227_list <- list.files("./Sentinel_Bankok/S2B_MSIL2A_20211227T034139_N0301_R061_T47PPR_20211227T062428/S2B_MSIL2A_20211227T034139_N0301_R061_T47PPR_20211227T062428.SAFE/GRANULE/L2A_T47PPR_A025112_20211227T035212/IMG_DATA/R10m/", full.names = T)

####training stack of available images
sen2_bankok_training_stack <- raster::stack(raster(sen2_211212_list[[2]]),
                                            raster(sen2_211212_list[[3]]),
                                            raster(sen2_211212_list[[4]]),
                                            raster(sen2_211212_list[[5]]),
                                            
                                            raster(sen2_211222_list[[2]]),
                                            raster(sen2_211222_list[[3]]),
                                            raster(sen2_211222_list[[4]]),
                                            raster(sen2_211222_list[[5]]),
                                            
                                            raster(sen2_211207_list[[2]]),
                                            raster(sen2_211207_list[[3]]),
                                            raster(sen2_211207_list[[4]]),
                                            raster(sen2_211207_list[[5]]),
                                            
                                            raster(sen2_211227_list[[2]]),
                                            raster(sen2_211227_list[[3]]),
                                            raster(sen2_211227_list[[4]]),
                                            raster(sen2_211227_list[[5]]))

extent(sen2_bankok_training_stack)
sen2_bankok_training_stack

#extract the raster values at the various polygon locations
extract_poly_fields <- raster::extract(sen2_bankok_training_stack, 
                                       training_labels_bankok, df=TRUE)

hist(extract_poly_fields$T47PPR_20211212T034141_B02_10m)
hist(extract_poly_fields$ID)
unique(extract_poly_fields$ID)

#write.csv(extract_poly_fields, "D:/Personal/Job_related/Listenfield/Scipts/Output/extracted_poly_fields_16bands.csv")
extract_poly_fields <- read.csv("D:/Personal/Job_related/Listenfield/Scipts/Output/extracted_poly_fields_16bands.csv")
dim(extract_poly_fields)

#Joining extracted data and labels
training_labels_bankok$row_num <- seq.int(nrow(training_labels_bankok)) 
extractMerged_bankok <- dplyr::inner_join(extract_poly_fields,
                                          training_labels_bankok,
                                          by= c("ID"="row_num")) 
typeof(extractMerged_bankok)

extractMerged_bankok$type
extractMerged_bankok$geometry <- NULL
extractMerged_bankok$ID <- NULL
extractMerged_bankok <- as.data.frame(extractMerged_bankok)

############################################################
set.seed(120) 
trainIndex <- caret::createDataPartition(extractMerged_bankok$type,list = FALSE,p=0.7)
trainData <- extractMerged_bankok[trainIndex,]  # 70% for training Data
testData <- extractMerged_bankok[-trainIndex,] # 30% for testing Data
typeof(extractMerged_bankok)

classCount <- trainData %>%
  dplyr::group_by(type) %>% 
  count()

classCount
head(trainData)

### The current machine memory is not sufficient to process all the data
#######Reducing number of samples 
train_data_small_try <- trainData %>% group_by(type) %>% sample_frac(0.25) %>% ungroup
train_data_small_try <- train_data_small_try[,c(2:5,18:18)]
test_data_small_try <- testData[,c(2:5,18:18)]
train_data_small_try$type <- as.character(train_data_small_try$type)
train_data_small_try$type <- as.factor(train_data_small_try$type)

#respVar <- c("type")
#predVar <- c("T47PPR_20211212T034141_B02_10m","T47PPR_20211212T034141_B03_10m",
 #            "T47PPR_20211212T034141_B04_10m", "T47PPR_20211212T034141_B08_10m")

#####################################################################
####################################################
##### Random forest ################################
classifier_RF_sen_bankok = randomForest(type ~ .,
                                        data = train_data_small_try,
                                        ntree = 500)
classCount_try <- train_data_small_try %>%
  dplyr::group_by(type) %>% 
  count()
classCount_try

classifier_RF_sen_bankok
head(test_data_small_try)

### Prediction
y_pred_valid <- predict(classifier_RF_sen_bankok, newdata = test_data_small_try[,-5])

#confusion_mtx <- table(test_data_small_try[, 5], y_pred_valid)
confusion_mtx <- confusionMatrix(y_pred_valid, as.factor(test_data_small_try$type))
confusion_mtx$table
confusion_mtx$overall
confusion_mtx
###############################################################
#### Confusion matrix RF details
#Overall Statistics

#Accuracy : 0.8142          
#95% CI : (0.8129, 0.8154)
#No Information Rate : 0.5494          
#P-Value [Acc > NIR] : < 2.2e-16       
#Kappa : 0.6625          
#Mcnemar's Test P-Value : NA

#y_pred_valid
#           farmland forest  grass   road  urban  water
#farmland    11721    262    101      9  22005   4811
#forest        552   1495      6      0   1865     91
#grass         278      8   1042      1   5418    224
#road           60      0      1     17   1535    249
#urban        5445    304    550     56 201628   8941
#water        3612     64    120     29  16778 105538
##########
#                         Class: farmland Class: forest Class: grass Class: road Class: urban
#Sensitivity                  0.30124      0.372911     0.149476   9.130e-03       0.9295
#Specificity                  0.97205      0.998367     0.997994   9.998e-01       0.7324
#Pos Pred Value               0.54094      0.700891     0.572527   1.518e-01       0.8090
#Neg Pred Value               0.92714      0.993598     0.984913   9.953e-01       0.8949
#Prevalence                   0.09855      0.010154     0.017656   4.716e-03       0.5494
#Detection Rate               0.02969      0.003787     0.002639   4.306e-05       0.5107
#Detection Prevalence         0.05488      0.005403     0.004610   2.837e-04       0.6313
#Balanced Accuracy            0.63665      0.685639     0.573735   5.044e-01       0.8310
                          #Class: water
#Sensitivity                0.8367
#Specificity                0.9467
#Pos Pred Value             0.8806
#Neg Pred Value             0.9251
#Prevalence                 0.3195
#Detection Rate             0.2673
#Detection Prevalence       0.3036
#Balanced Accuracy          0.8917
####################################################################
# Plotting model
plot(classifier_RF_sen_bankok)

# Importance plot
importance(classifier_RF_sen_bankok)

# Variable importance plot
varImpPlot(classifier_RF_sen_bankok)

##########################################################
##########################################################
### Q. 1.2.3 Prediction of January land use

sen2_bankok_jan_list <- list.files("D:/Personal/Job_related/Listenfield/Task_data/Sentinel_Bankok/S2B_MSIL2A_20220106T034129_N0301_R061_T47PPR_20220106T065911/S2B_MSIL2A_20220106T034129_N0301_R061_T47PPR_20220106T065911.SAFE/GRANULE/L2A_T47PPR_A025255_20220106T034832/IMG_DATA/R10m/", full.names = T)

sen2_bankok_jan_stack <- raster::stack(raster(sen2_bankok_jan_list[[2]]),
                                       raster(sen2_bankok_jan_list[[3]]),
                                       raster(sen2_bankok_jan_list[[4]]),
                                       raster(sen2_bankok_jan_list[[5]]))

####### too large 1.9gb
#xy_jan <- xyFromCell(sen2_bankok_jan_stack$T47PPR_20220106T034129_B02_10m,
 #                    1:ncell(sen2_bankok_jan_stack$T47PPR_20220106T034129_B02_10m))
#head(xy_jan)
#try_df <- as.data.frame(sen2_bankok_jan_stack$T47PPR_20220106T034129_B02_10m)
#head(try_df)

########## crop raster stack to reduce the size ###########
extent(training_labels_bankok)
extent(sen2_bankok_jan_stack)
extent_to_extract <- extent(647532, 693541, 1493504, 1550525)

##cropped stack
cropped_jan_stack <- crop(sen2_bankok_jan_stack, extent_to_extract)
plot(sen2_bankok_jan_stack$T47PPR_20220106T034129_B02_10m)
plot(cropped_jan_stack$T47PPR_20220106T034129_B02_10m)

#extract the raster values at the various polygon locations
xy_jan <- xyFromCell(cropped_jan_stack$T47PPR_20220106T034129_B02_10m,
                    1:ncell(cropped_jan_stack$T47PPR_20220106T034129_B02_10m))
head(xy_jan)

extract_jan_fields <- raster::extract(cropped_jan_stack, 
                                      xy_jan,
                                      df=TRUE)
head(extract_jan_fields)

##########################################################
##################################################################
##Prediction for January images 
extract_jan_fields$T47PPR_20220106T034129_B02_10m

## It is needed to temporarily change the column names of the extract_jan_fields to match with the RF model
names(extract_jan_fields)
names(train_data_small_try)

extracted_jan_changed_names <- extract_jan_fields
colnames(extracted_jan_changed_names) <- c("ID",
                                           "T47PPR_20211212T034141_B02_10m",
                                           "T47PPR_20211212T034141_B03_10m",
                                           "T47PPR_20211212T034141_B04_10m",
                                           "T47PPR_20211212T034141_B08_10m")


names(extracted_jan_changed_names)

########### prediction of Jan
jan_sen2_pred <- predict(classifier_RF_sen_bankok, 
                         newdata = extracted_jan_changed_names[,-1])

# Error: cannot allocate vector of size 1.2 Gb

typeof(jan_sen2_pred)
unique(jan_sen2_pred)

##### Due to time constraint and memory limitation, the predicted values for January could not be executed.

#######################################################################
#######################################################################
######## Try k-fold cross validation ##################################
#k fold cross validation
cvControl_sen_bankok <- caret::trainControl(method = 'cv',
                                            number = 5,
                                            savePredictions = TRUE,
                                            verboseIter = FALSE)

train_data_small_try <- as.data.frame(train_data_small_try)
rfModel_sen_bankok <- caret::train(x = train_data_small_try[,-5],
                                   y = train_data_small_try[,5],
                                   method="rf",
                                   metric = "Kappa",
                                   ntree= 500,
                                   trControl= cvControl_sen_bankok)

rfModel_sen_bankok

rfPredict_kfold <- predict(rfModel_sen_bankok,test_data_small_try[,-5])
level(rfPredict_kfold)
rfpredict_kfold_confmatrix <- confusionMatrix(rfPredict_kfold, as.factor(testData$type))
rfpredict_kfold_confmatrix$table
rfpredict_kfold_confmatrix$overall
rfpredict_kfold_confmatrix

#########################################################
#### Confusion matrix statistics ###########################
#k fold cross validation
#Prediction farmland forest  grass   road  urban  water
#farmland    11743    551    278     57   5428   3643
#forest        262   1488      8      0    296     65
#grass         101      3   1046      1    528    119
#road            9      0      1     19     56     25
#urban       22039   1880   5413   1530 201698  16733
#water        4755     87    225    255   8918 105556

#                       Class: farmland Class: forest Class: grass Class: road Class: urban
#Sensitivity                  0.30181      0.371165     0.150050   1.020e-02       0.9298
#Specificity                  0.97202      0.998385     0.998061   9.998e-01       0.7325
#Pos Pred Value               0.54115      0.702218     0.581758   1.727e-01       0.8091
#Neg Pred Value               0.92719      0.993580     0.984924   9.953e-01       0.8954
#Prevalence                   0.09855      0.010154     0.017656   4.716e-03       0.5494
#Detection Rate               0.02974      0.003769     0.002649   4.812e-05       0.5109
#Detection Prevalence         0.05496      0.005367     0.004554   2.786e-04       0.6314
#Balanced Accuracy            0.63692      0.684775     0.574056   5.050e-01       0.8311
#                        Class: water
#Sensitivity                0.8368
#Specificity                0.9470
#Pos Pred Value             0.8811
#Neg Pred Value             0.9252
#Prevalence                 0.3195
#Detection Rate             0.2674
#Detection Prevalence       0.3034
#Balanced Accuracy          0.8919