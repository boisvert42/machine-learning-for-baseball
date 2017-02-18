# Download the custom leaderboard from https://goo.gl/m35EVD
# Save it as "leaderboard.csv"
pitcherData <- read.csv('leaderboard.csv',fileEncoding = "UTF-8-BOM")

# Take a look at the data
str(pitcherData)

# If, say, the WAR column were a factor variable but should have been numeric,
# you could clean it up like this
# We don't need this now but it's a useful thing to know
#pitcherData$WAR <- as.numeric(as.character(pitcherData$WAR))

# Run the correlations
cor(subset(pitcherData,select=-c(Season,Name,Team,playerid)))

# Get set up to build the model
goodColumns <- c('W','L','IP','ER','HR','BB','SO','WAR')
library(caret)
inTrain <- createDataPartition(pitcherData$WAR,p=0.7,list=FALSE)
training <- pitcherData[inTrain,goodColumns]
testing <- pitcherData[-inTrain,goodColumns]

# Choose your favorite method to run here!
method = 'lm' # linear regression
#method = 'glm' # logistic regression (not great here because we are not doing classification)
#method = 'rpart' # decision trees (ditto)
#method = 'gbm' # boosting
#method = 'rf' # random forest

# Use cross-validation to better tune the model
ctrl <- trainControl(method = 'repeatedcv',number = 10, repeats = 10)
modelFit <- train(WAR ~ ., method=method, data=training, trControl=ctrl)

# Was this any good?
summary(modelFit)

# Try it just with the most significant columns
model2 <- train(WAR ~ IP + HR + BB + SO, method=method, data=training, trControl=ctrl)
# Check the model output
summary(model2)
# Apply to test set
predicted2 <- predict(model2,newdata=testing)
# R-squared
cor(testing$WAR,predicted2)^2 # 0.9108492
# Plot the predicted values vs. actuals
plot(testing$WAR,predicted2)

