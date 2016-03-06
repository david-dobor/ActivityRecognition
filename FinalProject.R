library(caret)
library(rpart) # Regressive Partitioning and Regression trees
library(rpart.plot) # Decision Tree plot
library(randomForest)

# setting the overall seed for reproduceability
set.seed(1234)


train <- read.csv("./pml-training.csv", head=TRUE, sep=",", na.strings=c("NA","#DIV/0!","")) 
test <- read.csv("./pml-testing.csv", head=TRUE, sep=",", na.strings=c("NA","#DIV/0!",""))  

dim(train)
dim(test)

#########

# Get rid of columns consisting of missing values only
train<-train[,colSums(is.na(train)) == 0]
test <-test[,colSums(is.na(test)) == 0]

# These variables are irrelevant to the analysis that follows:
# user_name, raw_timestamp_part_1, raw_timestamp_part_,2 cvtd_timestamp, new_window, and  num_window (columns 1 to 7). 
# We therefore delete these variables:
train   <-train[,-c(1:7)]
test <-test[,-c(1:7)]

# we are left with following datasets:
dim(train)
dim(test)
head(train)
head(test)

######## Partitionin the Training Set
# We now partition the traing data into two subsets to perform cross-validation. 
# The split will be 75% for training / 25% for validation
trainIndeces <- createDataPartition(y=train$classe, p=0.75, list=FALSE)
subTraining <- train[trainIndeces, ] 
subTesting <- train[-trainIndeces, ]
dim(subTraining)
dim(subTesting)
head(subTraining)
head(subTesting)

# Frequency of the Labels Plot
# We can take a look at the data. The 5 levels of `classe` variables in `subTraining` are distributed as follows:

plot(subTraining$classe, col="lightsteelblue3", main="Class Label Distribution in the subTraining Data", xlab="Class Labels", ylab="Frequency")

##################################
# Predicting Using Decision Tree #
##################################

mod.DecisionTree <- rpart(classe ~ ., data=subTraining, method="class")
prediction1 <- predict(mod.DecisionTree, subTesting, type = "class")
rpart.plot(model1, main="Classification Tree", extra=100, under=TRUE, faclen=0)

# Let's see how good the prediction is:
confusionMatrix(prediction1, subTesting$classe)

# We see that the accuracy is actually horrible here, so we try some other method.
# (We could try to prune or otherwise fine-tune the tree for better results, but we try another method first.)


#################################
# A Better Model: Random Forest #
#################################

mod.randForest <- randomForest(classe ~. , data=subTraining, method="class")
prediction2 <- predict(mod.randForest, subTesting, type = "class")

# Let's see how good the prediction is:
confusionMatrix(prediction2, subTesting$classe)

# So the Random Forest has an excellent accuracy. 

##################################
#        Final Prediction        #
##################################

# Predict outcome labels on the original Testing data set
predictfinal <- predict(mod.randForest, test, type="class")
predictfinal











