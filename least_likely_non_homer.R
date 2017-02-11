# Read the data
my_csv <- 'data.csv'
data_raw <- read.csv(my_csv)
# Convert some to numeric
data_raw$hit_speed <- as.numeric(as.character(data_raw$hit_speed))
data_raw$hit_angle <- as.numeric(as.character(data_raw$hit_angle))
# Add in horizontal angle (thanks to Bill Petti)
horiz_angle <- function(df) {
angle <- with(df, round(tan((hc_x-128)/(208-hc_y))*180/pi*.75,1))
angle
}
data_raw$hor_angle <- horiz_angle(data_raw)
# Remove NULLs
data_raw <- na.omit(data_raw)
# Re-index
rownames(data_raw) <- NULL

# Make training and test sets
cols <- c('HR','hit_speed','hit_angle','hor_angle','home_team')
library(caret)
inTrain <- createDataPartition(data_raw$HR,p=0.7,list=FALSE)
training <- data_raw[inTrain,cols]
testing <- data_raw[-inTrain,cols]
# gbm == boosting
method <- 'gbm'
# train the model
ctrl <- trainControl(method = “repeatedcv”,number = 5, repeats = 5)
modelFit <- train(HR ~ ., method=method, data=training, trControl=ctrl)
# How did this work on the test set?
predicted <- predict(modelFit,newdata=testing)
# Accuracy, precision, recall, F1 score
accuracy <- sum(predicted == testing$HR)/length(predicted)
precision <- posPredValue(predicted,testing$HR)
recall <- sensitivity(predicted,testing$HR)
F1 <- (2 * precision * recall)/(precision + recall)

print(accuracy) # 0.973
print(precision) # 0.811
print(recall) # 0.726
print(F1) # 0.766