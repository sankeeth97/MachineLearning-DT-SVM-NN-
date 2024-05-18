ico_randm_frst <- read.csv("ico_data.csv", stringsAsFactors = FALSE, encoding = 'UTF-8')

ico_randm_frst$continenetAmericas <- NULL
ico_randm_frst$continenetAfrica <- NULL
ico_randm_frst$continenetAsia <- NULL
ico_randm_frst$continenetEurope <- NULL
ico_randm_frst$continenetOceania <- NULL
ico_randm_frst$pltfrmEthereum <- NULL
ico_randm_frst$pltfrmSprtblkchn <- NULL
ico_randm_frst$pltfrmStellar <- NULL
ico_randm_frst$pltfrmWaves <- NULL
ico_randm_frst$pltfrmBtcn <- NULL
ico_randm_frst$pltfrmEos <- NULL
ico_randm_frst$pltfrmNem <- NULL
ico_randm_frst$pltfrmNeo <- NULL
ico_randm_frst$pltfrmScrpt <- NULL
ico_randm_frst$pltfrmTrn <- NULL
ico_randm_frst$pltfrmX11 <- NULL
ico_randm_frst$pltfrmOthers <- NULL

ico_randm_frst$countryRegion <- NULL


str(ico_randm_frst)

ico_randm_frst$success <- factor(ico_randm_frst$success, levels = c('Y', 'N'), 
                                  labels = c("yes", "no"))

ico_randm_frst <- na.omit(ico_randm_frst)

ico_randm_frst$hasVideo <- factor(ico_randm_frst$hasVideo, levels = c('0', '1'))
ico_randm_frst$hasGithub <- factor(ico_randm_frst$hasGithub, levels = c('0', '1'))
ico_randm_frst$hasReddit <- factor(ico_randm_frst$hasReddit, levels = c('0', '1'))
ico_randm_frst$minInvestment <- factor(ico_randm_frst$minInvestment, levels = c('0', '1'))

#--------------------------------------------------------------------------------------------------------
# Stratified Sampling
#--------------------------------------------------------------------------------------------------------

set.seed(12345)
in_train <- createDataPartition(ico_randm_frst$success, p = 0.75,
                                list = FALSE)
ico_train <- ico_randm_frst[in_train, ]
ico_test <- ico_randm_frst[-in_train, ]

prop.table(table(ico_randm_frst$success))
prop.table(table(ico_train$success))
prop.table(table(ico_test$success))

#--------------------------------------------------------------------------------------------------------
# Random Forest
#--------------------------------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(randomForest)

ico_rf_model <- randomForest(success ~ ., data=ico_train, ntree=10)
ico_rf_predict <- predict(ico_rf_model, ico_test)

library(rpart.plot)

m.rpart <- rpart(success ~ ., data = ico_train)

rpart.plot(m.rpart, digits = 4)

ico_pred_prob <- predict(ico_rf_model, ico_test, type = 'prob')
ico_pred_prob

ico_df <- data.frame(pred_class = ico_rf_predict,
                     actuall_class = ico_test$success,
                     pred_prob = ico_pred_prob)

ico_df

library(gmodels)
# be careful when the order of credit_pred and credit_test$default, and the order of the dnn labels
CrossTable(ico_test$success, ico_rf_predict, 
           prop.chisq = TRUE, prop.c = TRUE, prop.r = TRUE,
           dnn = c('actual success', 'predicted success'))

library(caret)
confusionMatrix(ico_rf_predict,ico_test$success,
                positive = "yes")

#------------------------------------------------------------------------------------------------------
# Tuning using caret package
#-------------------------------------------------------------------------------------------------------

set.seed(300)
ico_RF_crt <- train(success ~ ., data = ico_randm_frst, method = 'rf')

# Tuning the caret package
ctrl <- trainControl(method = "cv", number = 100,
                     selectionFunction = "oneSE")

set.seed(300)
ico_RF_crt <- train(success ~ ., data = ico_randm_frst, method = 'rf', trControl = ctrl)

ico_RF_crt
summary(ico_RF_crt)