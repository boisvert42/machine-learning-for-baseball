# Load the "iris" data set
data(iris)
# Load the caret package
library('caret')
# Take 70% of the data set to build the model
inTrain <- createDataPartition(iris$Species,p=0.7,list=FALSE)
# Create the training set
training <- iris[inTrain,]
# Build the model (random forest)
model <- train(Species~.,data=training,method='rf')

# Create the test set to evaluate the model
# Note that "-inTrain" with the minus sign pulls everything NOT in the training set
testing <- iris[-inTrain,]
# Run the model on the test set
predicted <- predict(model,newdata=testing)
# Determine the model accuracy
accuracy <- sum(predicted == testing$Species)/length(predicted)
# Print the model accuracy
print(accuracy)

# Bonus: print lots of information about the model performance on the test set
confusionMatrix(predicted, testing$Species)
