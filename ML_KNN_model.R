ico_data_knn <- ico_data

write.csv(ico_data_knn, "ico_data_knn", row.names = FALSE)
ico_data_knn <- read.csv("ico_data_knn", stringsAsFactors = TRUE, encoding = 'UTF-8')

ico_data_knn$distrubutedCoins <- 
  ((ico_data_knn$coinNum * ico_data_knn$distributedPercentage)/100)


#rm(ico_data_DT_raw)

ico_data_knn$countryRegion <- NULL
ico_data_knn$platform <- NULL
ico_data_knn$continentRegion <- NULL


str(ico_data_knn)
ico_data_knn$success <- factor(ico_data_knn$success, levels = c('Y', 'N'), 
                               labels = c("Y", "N"))

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

ico_data_knn_n <- as.data.frame(lapply(ico_data_knn[2:28], normalize))


str(ico_data_knn)
str(ico_data_knn_n)
head(ico_data_svm$success)

# Normal sampling
set.seed(100)
smp_size <- floor(0.75 * nrow(ico_data_knn_n))
in_train <- sample(nrow(ico_data_knn_n), smp_size)
ico_train <- ico_data_knn_n[in_train, ]
ico_test <- ico_data_knn_n[-in_train, ]

ico_train_labels <- ico_data_knn[in_train, 1]
ico_test_labels <- ico_data_knn[-in_train, 1]

ico_test_labels


# Stratified Random sampling (BETA)

library('ggplot2')
library('lattice')
library('caret')
set.seed(100)
in_train <- createDataPartition(ico_data_svm$success, p = 0.75,
                                list = FALSE)
ico_train <- ico_data_knn_n[in_train, ]
ico_test <- ico_data_knn_n[-in_train, ]
ico_train_labels <- ico_data_knn[in_train, 1]
ico_test_labels <- ico_data_knn[-in_train, 1]

prop.table(table(ico_data_knn$success))
prop.table(table(ico_train_labels$success))
prop.table(table(ico_test_labels$success))
#------------------------------------------------------------------------------------------------------
# Knn Model
#------------------------------------------------------------------------------------------------------

library(class)

K = 45

ico_pred <- knn(train = ico_train, test = ico_test, cl = ico_train_labels, k=45)

ico_pred_prob_knn <- knn(train = ico_train, test = ico_test, cl = ico_train_labels, k=45, prob = TRUE)


library(gmodels)
# be careful when the order of credit_pred and credit_test$default, and the order of the dnn labels
CrossTable(ico_test_labels, ico_pred, 
           prop.chisq = TRUE, prop.c = TRUE, prop.r = TRUE,
           dnn = c('actual success', 'predicted success'))


library(caret)
confusionMatrix(ico_pred,ico_test_labels,
                positive = "Y")
#---------------------------------------------------------------------------------------
# To Improve Performance
#----------------------------------------------------------------------------------------


library('caret')
set.seed(100)
m <- train(success ~ ., data = ico_data_knn, method = 'knn')
m

# Tuning the caret package
ctrl <- trainControl(method = "cv", number = 10,
                     selectionFunction = "oneSE")
grid <- expand.grid(k = 1:100)

set.seed(100)
m <- train(success ~ ., data = ico_data_knn, method = 'knn',
           trControl = ctrl,
           tuneGrid = grid)
m

#---------------------------------------------------------------------------------------------------
#Manual Cv Sampling
#---------------------------------------------------------------------------------------------------

K = 10

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

ico_data_knn_n <- as.data.frame(lapply(ico_data_knn[2:28], normalize))

set.seed(100)
folds <- createFolds(ico_data_knn$success, k = K)



# the list used to store result for each fold, so we can compute the average performance later
accuracy_list<-as.numeric()
kappa_list <- as.numeric()
specificity_list <- as.numeric()
sensitivity_list <- as.numeric()


# loop, from fold 1 to fold K:
for(i in 1:K){
  
  # for each fold, we conduct training and testing
  
  # use fold i for testing
  fold_test <- ico_data_knn_n[folds[[i]],]
  # use the remaining folds for training
  fold_train <- ico_data_knn_n[-folds[[i]],] 
  
  fold_test_labels <- ico_data_knn[folds[[i]], 1]
  fold_train_labels <- ico_data_knn[-folds[[i]], 1]
  
  # train the model
  set.seed(123)
  fold_predict_class_knn <- knn(train = fold_train, test = fold_test, cl = fold_train_labels, k=99)
  
  
  # calculate the recognition rate for this fold
  agreement <- fold_predict_class_knn == fold_test_labels
  prop = prop.table(table(agreement))
  fold_accuracy_knn = prop['TRUE']
  
  # append the result to the accuracy list for each fold
  accuracy_list<- append(accuracy_list,fold_accuracy_knn)
  
  conf_matrix <- confusionMatrix(fold_predict_class_knn,fold_test_labels,
                                 positive = "Y")
  
  kappa <- conf_matrix$overall['Kappa']
  
  kappa_list <- append(kappa_list,kappa)
  
  sensitivity <- sensitivity(fold_predict_class_knn, fold_test_labels, positive = "Y")
  sensitivity_list <- append(sensitivity_list,sensitivity)
  
  specificity <- specificity(fold_predict_class_knn, fold_test_labels, negative = "N")
  specificity_list <- append(specificity_list,specificity)
  
}

fold_accuracy_knn

accuracy_list
# compute the average accuracy over 5 folds
accuracy_average_knn <- mean(accuracy_list)
accuracy_average_knn
sd(accuracy_list) # check the standard deviation for the accuracy for all folds

kappa_list
Kappa_average_knn <- mean(kappa_list)
Kappa_average_knn


mean(specificity_list)
mean(sensitivity_list)

#-----------------------------------------------------------------------------------------------------
# ROC Curve
#-----------------------------------------------------------------------------------------------------

#Removing values from probs
KNN_Prob <- attributes(ico_pred_prob_knn)$prob

ico_df <- data.frame(pred_class = ico_pred,
                     actuall_class = ico_test_labels, prob_Yes = round((1-KNN_Prob),4),
                     pred_prob = round(KNN_Prob,4))

pred_object <- prediction(ico_df$prob_Yes, ico_test_labels)
# calculate the ROC curve 
# check the help document of performance to see the definition of parameters 
roc_NB <- performance(pred_object, measure = "tpr", x.measure = "fpr")

# plot the ROC curve 
# col controls the colour of the line
# lwd controls the width of < the line
plot(roc_NB, main = "ROC curve for ICO Success", col = "orange", lwd = 2, add = TRUE)
# this line indicates the performance of random guess (50:50)
abline(a = 0, b = 1, lwd = 2, lty = 2)

auc_object_knn <- performance(pred_object, measure = "auc")
auc_knn <- auc_object_knn@y.values[[1]]
auc_knn

legend("bottomright", c("DT","SVM", "KNN"), lwd=1, col = c("blue", "purple", "orange"))
rm(legend)
