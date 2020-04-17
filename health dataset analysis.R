attach(diabetes)
library(ggsci)
library(plotly)
head(diabetes)
tail(diabetes)
str(diabetes)
summary(diabetes)
diabetes$Outcome<-factor(diabetes$Outcome)
class(diabetes$Outcome)
p1<-ggplot(diabetes,aes(x=Age,y=Pregnancies,col=Outcome))+geom_point()+geom_smooth(method="loess", se=T)+facet_grid(.~Outcome)
ggplotly(p1)
p2<-ggplot(diabetes,aes(x=Age,y=Pregnancies))+geom_boxplot(aes(fill=Outcome))+facet_wrap(Outcome~.)
ggplotly(p2)
p3<-ggplot(diabetes,aes(x=Pregnancies))+geom_density(aes(fill=Outcome),alpha=0.6)+
  geom_vline(aes(xintercept=mean(Pregnancies)),
             color="blue", linetype="dashed", size=1)+facet_grid(.~Outcome)+scale_fill_aaas()
ggplotly(p3)
p4<-ggplot(diabetes,aes(x=Age, y=Pregnancies, size=Glucose, fill=BloodPressure))+geom_point(alpha=0.2)+
  facet_grid(.~Outcome)+geom_jitter(width = 0.4)+scale_x_continuous(limits = c(18, 80))+scale_fill_material("red")
ggplotly(p4)  
library(caret)
# Create the training and test datasets
set.seed(100)
hci<-diabetes

# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(hci$Outcome, p=0.8, list=FALSE) # Data partition for dividing the dataset into training and testing data set. This is useful for cross validation

# Step 2: Create the training  dataset
trainData <- hci[trainRowNumbers,]

# Step 3: Create the test dataset
testData <- hci[-trainRowNumbers,]

# Store X and Y for later use.
x = trainData[, 1:8]
y=trainData$Outcome

xt= testData[, 1:8]
yt=testData$Outcome
preProcess_range_modeltr <- preProcess(trainData, method='range')
preProcess_range_modelts <- preProcess(testData, method='range')

trainData <- predict(preProcess_range_modeltr, newdata = trainData)
testData <- predict(preProcess_range_modelts, newdata = testData)

# Append the Y variable
trainData$Outcome <- y
testData$Outcome<-yt
levels(trainData$Outcome) <- c("Class0", "Class1") # Convert binary outcome into character for caret package
levels(testData$Outcome) <- c("Class0", "Class1")
fitControl <- trainControl(
  method = 'cv',                   # k-fold cross validation
  number = 5,                      # number of folds
  savePredictions = 'final',       # saves predictions for optimal tuning parameter
  classProbs = T,                  # should class probabilities be returned
  summaryFunction=twoClassSummary  # results summary function
) 
# Step 1: Tune hyper parameters by setting tuneLength
set.seed(100)
model1 = train(Outcome ~ ., data=trainData, method='lda', tuneLength = 5, metric='ROC', trControl = fitControl)

model2 = train(Outcome ~ ., data=trainData, method='knn', tuneLength=2, trControl = fitControl)#KNN Model
model3 = train(Outcome ~ ., data=trainData, method='svmRadial', tuneLength=2, trControl = fitControl)#SVM
model4 = train(Outcome ~ ., data=trainData, method='rpart', tuneLength=2, trControl = fitControl)#RandomForest
model5 = train(Outcome ~ ., data=trainData, method='adaboost', tuneLength=2, trControl = fitControl) # Adaboost

# Compare model performances using resample()
models_compare <- resamples(list(LDA=model1,KNN=model2,SVM=model3,RF=model4, ADA=model5))

# Summary of the models performances
summary(models_compare)
# Draw box plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_compare, scales=scales)
# Step 2: Predict on testData and Compute the confusion matrix
# Using LDA Model
predicted <- predict(model1, testData[,1:8])
confusionMatrix(reference = testData$Outcome, data = predicted, mode='everything')
  