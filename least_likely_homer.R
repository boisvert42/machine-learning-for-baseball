# Read the data
my_csv <- 'hr_data.csv'
data_raw <- read.csv(my_csv)
# Make training and test sets
library(caret)
inTrain <- createDataPartition(data_raw$HR,p=0.7,list=FALSE)
training <- data_raw[inTrain,]
testing <- data_raw[-inTrain,]
# rpart == decision tree
method <- 'rpart'
# train the model
modelFit <- train(HR ~ ., method=method, data=training)
# Show the decision tree
library(rattle)
fancyRpartPlot(modelFit$finalModel)

# Use boosting instead
method <- 'gbm' # boosting
modelFit <- train(HR ~ ., method=method, data=training)
# How did this work on the test set?
predicted <- predict(modelFit,newdata=testing)
# Accuracy, precision, recall, F1 score
accuracy <- sum(predicted == testing$HR)/length(predicted)
precision <- posPredValue(predicted,testing$HR)
recall <- sensitivity(predicted,testing$HR)
F1 <- (2 * precision * recall)/(precision + recall)

print(accuracy) # 0.973
print(precision) # 0.792
print(recall) # 0.657
print(F1) # 0.718