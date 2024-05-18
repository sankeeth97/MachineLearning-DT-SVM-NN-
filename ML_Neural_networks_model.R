ico_data_nn <- ico_data

write.csv(ico_data_nn, "ico_data_nn", row.names = FALSE)
ico_data_svm <- read.csv("ico_data_nn", stringsAsFactors = TRUE, encoding = 'UTF-8')

ico_data_nn$distrubutedCoins <- 
  ((ico_data_nn$coinNum * ico_data_nn$distributedPercentage)/100)


#rm(ico_data_DT_raw)

ico_data_nn$countryRegion <- NULL
ico_data_nn$platform <- NULL
ico_data_nn$continentRegion <- NULL



ico_data_nn$success <- factor(ico_data_nn$success, levels = c('Y', 'N'), 
                               labels = c("Y", "N"))
str(ico_data_nn)
head(ico_data_nn$success)

# Normal sampling
set.seed(100)
smp_size <- floor(0.75 * nrow(ico_data_nn))
in_train <- sample(nrow(ico_data_nn), smp_size)
ico_train <- ico_data_nn[in_train, ]
ico_test <- ico_data_nn[-in_train, ]


# Stratified Random sampling (BETA)

library('ggplot2')
library('lattice')
library('caret')
set.seed(100)
in_train <- createDataPartition(ico_data_svm$success, p = 0.75,
                                list = FALSE)
ico_train <- ico_data_nn[in_train, ]
ico_test <- ico_data_nn[-in_train, ]

prop.table(table(ico_data_nn$success))
prop.table(table(ico_train$success))
prop.table(table(ico_test$success))

# custom normalization function
# define a function to normalise data into 0-1 range
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# apply normalization to entire data frame
ico_norm <- as.data.frame(lapply(select(ico_train, -success), normalize)) # 'normalize' is the name of function we defined

ico_norm$success <- ico_train$success

ico_norm <- ico_norm %>% relocate(success, .before = hasVideo)

str(ico_norm)

prop.table(table(ico_data_nn$success))
prop.table(table(ico_norm$success))
prop.table(table(ico_test$success))

#------------------------------------------------------------------------------------------------------
# Neural Network Model
#------------------------------------------------------------------------------------------------------

library(neuralnet)

model = neuralnet(
  success ~ .,
  data=ico_train,
  linear.output = FALSE
)

# Normalised Model

model = neuralnet(
  success ~ .,
  data=ico_norm,
  hidden = c(28:2),
  linear.output = FALSE
)


#The output of a neural network's neuron is always numerical and will range between (-1:+1) or (-inf:+inf)
# or (0:1), so when using neural networks for classification, we have to find the output neuron with the 
# highest value, highest value is an indication that the testing data has the highest probability of 
# being classified as that particular output neuron. Observe the below example carefully.

plot(model,rep = "best")

pred <- predict(model, select(ico_test, -success))

pred

labels <- c("Y", "N")
prediction_label <- data.frame(max.col(pred)) %>%     
  mutate(pred=labels[max.col.pred.]) %>%
  select(2) %>%
  unlist()

labels
prediction_label

prediction_label <- factor(prediction_label, levels = c('Y', 'N'), 
                              labels = c("Y", "N"))

library(gmodels)
# be careful when the order of credit_pred and credit_test$default, and the order of the dnn labels
CrossTable(ico_test$success, prediction_label, 
           prop.chisq = TRUE, prop.c = TRUE, prop.r = TRUE,
           dnn = c('actual success', 'predicted success'))

library(caret)
confusionMatrix(prediction_label,ico_test$success,
                positive = "Y")

check = ico_test$sucess == prediction_label
accuracy = (sum(check)/nrow(ico_test))*100
accuracy

check

#improving Model performance

library(lattice)
library(caret)

ico_model_imp <- train(success ~ ., data = ico_data_nn, method = 'neuralnet')


library(keras)
library(tensorflow)

install.packages('tensorflow')

model <- keras_model_sequential() %>%
  layer_dense(units = 8, activation = "relu", input_shape = c(4)) %>%
  layer_dense(units = 3, activation = "softmax")

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_adam(),
  metrics = "accuracy"
)
