ico_data_dt_raw <- ico_data

write.csv(ico_data_dt_raw, "ico_data_c50.csv", row.names = FALSE)
ico_data_dt_raw <- read.csv("ico_data_c50.csv", stringsAsFactors = FALSE, encoding = 'UTF-8')

ico_data_dt_raw$distrubutedCoins <- 
  ((ico_data_dt_raw$coinNum * ico_data_dt_raw$distributedPercentage)/100)


#rm(ico_data_DT_raw)

ico_data_dt_raw$continenetAmericas <- NULL
ico_data_dt_raw$continenetAfrica <- NULL
ico_data_dt_raw$continenetAsia <- NULL
ico_data_dt_raw$continenetEurope <- NULL
ico_data_dt_raw$continenetOceania <- NULL
ico_data_dt_raw$pltfrmEthereum <- NULL
ico_data_dt_raw$pltfrmSprtblkchn <- NULL
ico_data_dt_raw$pltfrmStellar <- NULL
ico_data_dt_raw$pltfrmWaves <- NULL
ico_data_dt_raw$pltfrmBtcn <- NULL
ico_data_dt_raw$pltfrmEos <- NULL
ico_data_dt_raw$pltfrmNem <- NULL
ico_data_dt_raw$pltfrmNeo <- NULL
ico_data_dt_raw$pltfrmScrpt <- NULL
ico_data_dt_raw$pltfrmTrn <- NULL
ico_data_dt_raw$pltfrmX11 <- NULL
ico_data_dt_raw$pltfrmOthers <- NULL


ico_data_dt_raw$countryRegion <- NULL

ico_data_dt_raw$platform <- NULL
ico_data_dt_raw$continentRegion <- NULL



ico_data_dt_raw$success <- factor(ico_data_dt_raw$success, levels = c('Y', 'N'), 
                         labels = c("yes", "no"))
str(ico_data_dt_raw)
head(ico_data_dt_raw$success)
md.pattern(ico_data_dt_raw)


#ico_data_dt_raw <- na.omit(ico_data_dt_raw)


write.csv(ico_data_dt_raw, "ico_data_dt_raw.csv", row.names = FALSE)
ico_data_dt_raw <- read.csv("ico_data_dt_raw.csv", encoding = 'UTF-8')

# Normal sampling
set.seed(100)
smp_size <- floor(0.75 * nrow(ico_data_dt_raw))
in_train <- sample(nrow(ico_data_dt_raw), smp_size)
ico_train <- ico_data_dt_raw[in_train, ]
ico_test <- ico_data_dt_raw[-in_train, ]


# Stratified Random sampling (BETA)

library('ggplot2')
library('lattice')
library('caret')
set.seed(100)
in_train <- createDataPartition(ico_data_dt_raw$success, p = 0.75,
                                list = FALSE)
ico_train <- ico_data_dt_raw[in_train, ]
ico_test <- ico_data_dt_raw[-in_train, ]

prop.table(table(ico_data_dt_raw$success))
prop.table(table(ico_train$success))
prop.table(table(ico_test$success))

caret::train  

library(C50)
library(tidyverse)


ico_model <- C5.0(select(ico_train, -success), ico_train$success)
?C5.0

ico_model
summary(ico_model)

library(rpart.plot)

m.rpart <- rpart(success ~ ., data = ico_train)

rpart.plot(m.rpart, digits = 4)
#------------------------------------ Prediction-------------------------------

ico_pred <- predict(ico_model, ico_test)
ico_pred


ico_pred_prob <- predict(ico_model, ico_test, type = 'prob')
ico_pred_prob


library(gmodels)
# be careful when the order of credit_pred and credit_test$default, and the order of the dnn labels
CrossTable(ico_test$success, ico_pred, 
           prop.chisq = TRUE, prop.c = TRUE, prop.r = TRUE,
           dnn = c('actual success', 'predicted success'))

library(caret)
confusionMatrix(ico_pred,ico_test$success,
                positive = "yes")

library(vcd)
kappa_value <- Kappa(table(ico_test$success, ico_pred))
kappa_value

#-------------------------------------------------------------------------------------------------------
# Automated Tuning of the model using caret package
#-------------------------------------------------------------------------------------------------------
library('caret')
set.seed(100)
m <- train(success ~ ., data = ico_data_dt_raw, method = 'C5.0', metric = "Kappa")
m
m$finalModel
summary(m)

p <- predict(m, credit)
table(p, credit$default)

# Tuning the caret package
ctrl <- trainControl(method = "cv", number = 50,
                     selectionFunction = "oneSE")
grid <- expand.grid(.model = "tree",
                    .trials = c(1:50),
                    .winnow = "FALSE")

set.seed(300)
m <- train(success ~ ., data = ico_data_dt_raw, method = 'C5.0', metric = "Kappa",
           trControl = ctrl,
           tuneGrid = grid)
m

md.pattern(ico_data_dt_raw)
#-------------------------------------------------------------------------------------------------------
#Adaptive Boosting
#-------------------------------------------------------------------------------------------------------
set.seed(100)
ico_boost <- C5.0(select(ico_train, -success), ico_train$success, trials = 1, winnow = FALSE)

ico_boost
summary(ico_boost)

ico_boost_pred <- predict(ico_boost, ico_test)
ico_pred


ico_pred_prob <- predict(ico_boost, ico_test, type = 'prob')
ico_pred_prob



library(gmodels)
# be careful when the order of credit_pred and credit_test$default, and the order of the dnn labels
CrossTable(ico_test$success, ico_boost_pred, 
           prop.chisq = TRUE, prop.c = TRUE, prop.r = TRUE,
           dnn = c('actual values', 'predicted values'))

library(caret)
confusionMatrix(ico_boost_pred,ico_test$success,
                positive = "yes")

#------------------------------------------------------------------------------------------------------
#ROC Curve
#-------------------------------------------------------------------------------------------------------

ico_df <- data.frame(pred_class = ico_pred,
                     actuall_class = ico_test$success,
                     pred_prob = ico_pred_prob)
library(ROCR)

# create the prediction object (particularly used by the ROCR package)
pred_object <- prediction(ico_df$pred_prob.yes, ico_test$success)
# calculate the ROC curve 
# check the help document of performance to see the definition of parameters 
roc_NB <- performance(pred_object, measure = "tpr", x.measure = "fpr")

# plot the ROC curve 
# col controls the colour of the line
# lwd controls the width of < the line
plot(roc_NB, main = "ROC curve for ICO Success by models", col = "blue", lwd = 2)
# this line indicates the performance of random guess (50:50)
abline(a = 0, b = 1, lwd = 2, lty = 2)

auc_object_dt <- performance(pred_object, measure = "auc")
auc_dt <- auc_object_knn@y.values[[1]]
auc_dt

#-----------------------------------------------------------------------------------------------------
#Manual 10-fold CV
#------------------------------------------------------------------------------------------------------

K = 10

set.seed(100)
folds <- createFolds(ico_data_dt_raw$success, k = K) #'letter' is the class label column in dataframe 'letters'

folds

# the list used to store result for each fold, so we can compute the average performance later
accuracy_list<-as.numeric()
kappa_list <- as.numeric()


# loop, from fold 1 to fold K:
for(i in 1:K){
  
  # for each fold, we conduct training and testing
  
  # use fold i for testing
  fold_test <- ico_data_dt_raw[folds[[i]],]
  # use the remaining folds for training
  fold_train <- ico_data_dt_raw[-folds[[i]],] 
  
  # train the model
  set.seed(100)
  fold_model_DT <- C5.0(select(fold_train, -success), fold_train$success)
  
  # get predicted class labels for testing data
  fold_predict_class_DT <- predict(fold_model_DT, fold_test)
  
  # calculate the recognition rate for this fold
  agreement <- fold_predict_class_DT == fold_test$success
  prop = prop.table(table(agreement))
  fold_accuracy_DT = prop['TRUE']
  
  # append the result to the accuracy list for each fold
  accuracy_list<- append(accuracy_list,fold_accuracy_DT)
  
  conf_matrix <- confusionMatrix(fold_predict_class_DT,fold_test$success,
                                 positive = "yes")
  
  kappa <- conf_matrix$overall['Kappa']
  
  kappa_list <- append(kappa_list,kappa)
  
}

fold_accuracy_DT
conf_matrix

accuracy_list
# compute the average accuracy over 5 folds
accuracy_average_DT <- mean(accuracy_list)
accuracy_average_DT
sd(accuracy_list) # check the standard deviation for the accuracy for all folds

kappa_list
Kappa_average_DT <- mean(kappa_list)
Kappa_average_DT
