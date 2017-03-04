# Thank you https://stat.ethz.ch/pipermail/r-help/2010-October/255593.html
# Get the CSVs we're interested in
filenames <- list.files(path = "statcast_data", full.names=TRUE)
# Combine them into a single data.frame
data_raw <- do.call("rbind", lapply(filenames, read.csv, header = TRUE))

# We want "hit_speed", "hit_angle", and "events"
data <- data_raw[,c("hit_speed","hit_angle","events")]

# Look at the data
str(data)

# We need to massage the data before we can use it
# Note that "hit_speed" and "hit_angle" are factors; we have to change that
data$hit_speed <- as.numeric(as.character(data$hit_speed))
data$hit_angle <- as.numeric(as.character(data$hit_angle))

# We got some "NA"s doing the above.  Let's get rid of them.
data <- na.omit(data)


# Let's look at the values in the "events" column
unique(data$events)

# Unfortunately there are a lot of events we can't classify one way or the other
# We'll have to keep just the ones we can
good_columns <- c('Pop Out','Bunt Pop Out','Flyout','Sac Fly','Bunt Groundout','Groundout','Grounded Into DP','Lineout','Home Run')
data <- data[data$events %in% good_columns,]

# Now replace as appropriate to match Tango's chart
# Helpful tip via http://stackoverflow.com/a/20585527
library(plyr)
data$events <- revalue(data$events, c("Pop Out"="Pop",
      "Bunt Pop Out"="Pop","Flyout"="Fly","Sac Fly"="Fly",
      "Bunt Groundout"="GB","Groundout"="GB","Grounded Into DP"="GB",
      "Lineout"="Liner","Home Run"="HR"))
# Take another look to be sure
unique(data$events)
# The data looks good except there are too many levels.  Let's re-factor
data$events <- factor(data$events)

# Re-index to be sure
rownames(data) <- NULL

# Make 100% sure!
str(data)

# Wow, can we finally do some machine learning now?
library(caret)
inTrain <- createDataPartition(data$events,p=0.7,list=FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]

method <- 'rf' # random forest again? sure
# train the model
ctrl <- trainControl(method = 'repeatedcv', number = 5, repeats = 5)
modelFit <- train(events ~ ., method=method, data=training, trControl=ctrl)

# Run the model on the test set
predicted <- predict(modelFit,newdata=testing)
# Check out the confusion matrix
confusionMatrix(predicted, testing$events)

# Plot this!
#install.packages('plotly')
library(plotly)
# Exit velocities from 40 to 120
x <- seq(40,120,by=1)
# Hit angles from 10 to 50
y <- seq(10,50,by=1)
# Make a data frame of the relevant x and y values
plotDF <- data.frame(expand.grid(x,y))
# Add the correct column names
colnames(plotDF) <- c('hit_speed','hit_angle')
# Add the classification
plotPredictions <- predict(modelFit,newdata=plotDF)
plotDF$pred <- plotPredictions

p <- plot_ly(data=plotDF, x=~hit_speed, y = ~hit_angle, color=~pred, type="scatter", mode="markers") %>%
    layout(title = "Exit Velocity + Launch Angle = WIN")
p


# Optional: export to HTML
htmlwidgets::saveWidget(p, "tangoplot.html")
