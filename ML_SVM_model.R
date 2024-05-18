ico_data_svm <- ico_data

write.csv(ico_data_svm, "ico_data_svm", row.names = FALSE)
ico_data_svm <- read.csv("ico_data_svm", stringsAsFactors = TRUE, encoding = 'UTF-8')

ico_data_svm$distrubutedCoins <- 
  ((ico_data_svm$coinNum * ico_data_svm$distributedPercentage)/100)


#rm(ico_data_DT_raw)

ico_data_svm$countryRegion <- NULL
ico_data_svm$platform <- NULL
ico_data_svm$continentRegion <- NULL



ico_data_svm$success <- factor(ico_data_svm$success, levels = c('Y', 'N'), 
                                  labels = c("Y", "N"))
str(ico_data_svm)
head(ico_data_svm$success)

# Normal sampling
set.seed(100)
smp_size <- floor(0.75 * nrow(ico_data_svm))
in_train <- sample(nrow(ico_data_svm), smp_size)
ico_train <- ico_data_svm[in_train, ]
ico_test <- ico_data_svm[-in_train, ]


# Stratified Random sampling (BETA)

library('ggplot2')
library('lattice')
library('caret')
set.seed(100)
in_train <- createDataPartition(ico_data_svm$success, p = 0.75,
                                list = FALSE)
ico_train <- ico_data_svm[in_train, ]
ico_test <- ico_data_svm[-in_train, ]

prop.table(table(ico_data_svm$success))
prop.table(table(ico_train$success))
prop.table(table(ico_test$success))

#------------------------------------------------------------------------------------------------------
# SVM Model
#------------------------------------------------------------------------------------------------------

library(kernlab)
library(ggplot2)
library(mice)
# the column 'letter' contains the class label (letter name)
# other columns contain features 
# kernel type: 
# 'vanilladot' - linear; 'rbfdot' - rbf; 'polydot' - ploynomial; 'tanhdot' - Sigmoid 
# C=1 is default 
ico_model <- ksvm(success ~ ., data = ico_train,
                          kernel = "vanilladot", C = 1, prob.model = TRUE)

ico_model <- ksvm(success ~ ., data = ico_train,
                  kernel = "polydot", C = 1, prob.model = TRUE)

?ksvm
# look at basic information about the model
ico_model

## Step 3: Evaluating model performance ----
library(tidyverse)
# predictions on testing dataset
# type: 
# 'response' (default) - predicted class
# 'probabilities' - predicted probability 
ico_pred <- predict(ico_model, select(ico_test, -success))

library(e1071)
# Get the probability estimates
ico_pred_prob_svm <- predict(ico_model, select(ico_test, -success), type = 'prob')

ico_pred

ico_pred_prob_svm

# look only at agreement vs. non-agreement
# construct a vector of TRUE/FALSE indicating correct/incorrect predictions
agreement <- ico_pred == ico_test$success
table(agreement)
prop.table(table(agreement)) # the prop of 'TRUE' is the recognition accuracy 

table(ico_pred, ico_test$success)

library(gmodels)
# be careful when the order of credit_pred and credit_test$default, and the order of the dnn labels
CrossTable(ico_test$success, ico_pred, 
           prop.chisq = TRUE, prop.c = TRUE, prop.r = TRUE,
           dnn = c('actual success', 'predicted success'))

library(caret)
confusionMatrix(ico_pred,ico_test$success,
                positive = "Y")

## Step 4: Improving model performance ----
# The seed number you choose is the starting point used in the generation of a sequence of random numbers, 
# so you'll obtain the same results given the same seed number
# the results can be different due to randomness in the ksvm RBF kernel 
# using set.seed(123) before running the ksvm() function can make sure the results are the same 

library('caret')
set.seed(100)
m <- train(success ~ ., data = ico_data_svm, method = 'svmLinear')
m
m$finalModel
summary(m)

p <- predict(m, credit)
table(p, credit$default)

# Tuning the caret package
ctrl <- trainControl(method = "cv", number = 10,
                     selectionFunction = "best")
grid <- expand.grid(C = seq(1, 10, by = 1))

set.seed(100)
m <- train(success ~ ., data = ico_data_svm, method = 'svmLinear',
           trControl = ctrl,
           tuneGrid = grid)
m


set.seed(100)
ico_model_rbf <- ksvm(success ~ ., data = ico_train, kernel = "rbfdot", C = 1)
ico_model_rbf

ico_pred_rbf <- predict(ico_model_rbf, select(ico_test, -success))

agreement_rbf <- ico_pred_rbf == ico_test$success
table(agreement_rbf)
prop.table(table(agreement_rbf))

library(gmodels)
# be careful when the order of credit_pred and credit_test$default, and the order of the dnn labels
CrossTable(ico_test$success, ico_pred_rbf, 
           prop.chisq = TRUE, prop.c = TRUE, prop.r = TRUE,
           dnn = c('actual success', 'predicted success'))

library(caret)
confusionMatrix(ico_pred_rbf,ico_test$success,
                positive = "Y")

conf_matrix <- confusionMatrix(ico_pred_rbf,ico_test$success,
                positive = "Y")


#------------------Manual Cv Looping------------------------------------------------

K = 10

set.seed(100)
folds <- createFolds(ico_data_svm$success, k = K) #'letter' is the class label column in dataframe 'letters'

folds

# the list used to store result for each fold, so we can compute the average performance later
accuracy_list<-as.numeric()
kappa_list <- as.numeric()
specificity_list <- as.numeric()
sensitivity_list <- as.numeric()


# loop, from fold 1 to fold K:
for(i in 1:K){
  
  # for each fold, we conduct training and testing
  
  # use fold i for testing
  fold_test <- ico_data_svm[folds[[i]],]
  # use the remaining folds for training
  fold_train <- ico_data_svm[-folds[[i]],] 
  
  # train the model
  set.seed(123)
  fold_model_SVM <- ksvm(success ~ ., data = fold_train, kernel = "vanilladot", C = 1)
  
  # get predicted class labels for testing data
  fold_predict_class_SVM <- predict(fold_model_SVM, select(fold_test, -success), type='response')
  
  # calculate the recognition rate for this fold
  agreement <- fold_predict_class_SVM == fold_test$success
  prop = prop.table(table(agreement))
  fold_accuracy_SVM = prop['TRUE']
  
  # append the result to the accuracy list for each fold
  accuracy_list<- append(accuracy_list,fold_accuracy_SVM)
  
  conf_matrix <- confusionMatrix(fold_predict_class_SVM,fold_test$success,
                                 positive = "Y")
  
  kappa <- conf_matrix$overall['Kappa']
  kappa_list <- append(kappa_list,kappa)
  
  sensitivity <- sensitivity(fold_predict_class_SVM, fold_test$success, positive = "Y")
  sensitivity_list <- append(sensitivity_list,sensitivity)
  
  specificity <- specificity(fold_predict_class_SVM, fold_test$success, negative = "N")
  specificity_list <- append(specificity_list,specificity)
  
}

fold_accuracy_SVM

accuracy_list
# compute the average accuracy over 5 folds
accuracy_average_SVM <- mean(accuracy_list)
accuracy_average_SVM
sd(accuracy_list) # check the standard deviation for the accuracy for all folds

kappa_list
Kappa_average_svm <- mean(kappa_list)
Kappa_average_svm

mean(specificity_list)
mean(sensitivity_list)

conf_matrix


#----------------------------------------------------------------------------------------------------
#Roc Curve
#----------------------------------------------------------------------------------------------------

ico_df <- data.frame(pred_class = ico_pred,
                     actuall_class = ico_test$success,
                     pred_prob = ico_pred_prob_svm)
library(ROCR)

# create the prediction object (particularly used by the ROCR package)
pred_object <- prediction(ico_df$pred_prob.Y, ico_test$success)
# calculate the ROC curve 
# check the help document of performance to see the definition of parameters 
roc_NB <- performance(pred_object, measure = "tpr", x.measure = "fpr")

# plot the ROC curve 
# col controls the colour of the line
# lwd controls the width of < the line
plot(roc_NB, main = "ROC curve for ICO Success", col = "purple", lwd = 2)
# this line indicates the performance of random guess (50:50)
abline(a = 0, b = 1, lwd = 2, lty = 2)

auc_object_svm <- performance(pred_object, measure = "auc")
auc_svm <- auc_object_svm@y.values[[1]]
auc_svm

auc_object_svm
