
library(stats)
library(caTools)
library(Amelia)
library(caret)
library(dplyr)

# set your working directory

# read the telecom dataset input file
telecomDataframe <- read.csv(file="Telecom.csv")



# print the structure of the dataframe
print(str(telecomDataframe))

# check for the NA values 
any(is.na(telecomDataframe))

# visualize the missing values using the missing map from the Amelia package
missmap(telecomDataframe,col=c("yellow","red"))

fivenum(telecomDataframe$tenure)
# create new column "tenure_interval" from the tenure column
group_tenure <- function(tenure){
  if (tenure >= 0 && tenure <= 5){
    return('0-5 Month')
  }else if(tenure > 5 && tenure <= 10){
    return('6-10 Month')
  }else if (tenure > 10 && tenure <= 15){
    return('11-15 Month')
  }else if (tenure > 15 && tenure <=20){
    return('16-20 Month')
  }else if (tenure > 20 && tenure <=25){
    return('21-25 Month')
  }else if (tenure > 25 && tenure <= 30){
    return('26-30 Month')
  }else if (tenure > 30 && tenure <=35){
    return('31-35 Month')
  }else if (tenure > 35 && tenure <=40){
    return('36-40 Month')
  }else if (tenure > 40 && tenure <=45) {
    return('41-45 Month')
  }else if (tenure > 45 && tenure <=50) {
    return('46-50 Month')
  }else if (tenure > 50 && tenure <=55) {
    return('51-55 Month')
  }else if(tenure > 55 && tenure <= 60) {
    return('56-60 Month')
  } else if(tenure > 60 && tenure <=65) {
    return('61-66 Month')
  }else if(tenure > 65 && tenure <=70)  {
    return('67-70 Month')
  } else if(tenure > 70){
    return('70+')
  }
}

# apply group_tenure function on each row of dataframe
telecomDataframe$tenure_interval <- sapply(telecomDataframe$tenure,group_tenure)
telecomDataframe$tenure_interval <- as.factor(telecomDataframe$tenure_interval)

# Ignore the variables with more levels while predicting the model
# Columns "customerID" and "tenure" having more levels
telecomDataframe <- select(telecomDataframe,-customerID,-tenure)

lapply(telecomDataframe, class)
# The value of the following columns affecting the model and introducing the NA value for "No phone service" and  and "No internet service" need to cleanup the data for these columns MultipleLine,OnlineSecurity,OnlineBackup,DeviceProtection,TechSupport,StreamingTV,StreamingMovies
telecomDataframe$MultipleLines <- as.character(telecomDataframe$MultipleLines)
telecomDataframe$OnlineSecurity <- as.character(telecomDataframe$OnlineSecurity)
telecomDataframe$OnlineBackup <- as.character(telecomDataframe$OnlineBackup)
telecomDataframe$DeviceProtection <- as.character(telecomDataframe$DeviceProtection)
telecomDataframe$TechSupport <- as.character(telecomDataframe$TechSupport)
telecomDataframe$StreamingTV <- as.character(telecomDataframe$StreamingTV)
telecomDataframe$StreamingMovies <- as.character(telecomDataframe$StreamingMovies)
telecomDataframe$InternetService <- as.character(telecomDataframe$InternetService)

# convert factor variables into character variables before changing the values
sapply(telecomDataframe, unique)
# unique(telecomDataframe$MultipleLines)
# unique(telecomDataframe$OnlineSecurity)
# unique(telecomDataframe$OnlineBackup)
# unique(telecomDataframe$DeviceProtection)
# unique(telecomDataframe$TechSupport)
# unique(telecomDataframe$StreamingTV)
# unique(telecomDataframe$StreamingMovies)

telecomDataframe$MultipleLines <-gsub("No phone service","No",telecomDataframe$MultipleLines)
telecomDataframe$OnlineSecurity <-gsub("No internet service","No",telecomDataframe$OnlineSecurity)
telecomDataframe$OnlineBackup <-gsub("No internet service","No",telecomDataframe$OnlineBackup)
telecomDataframe$DeviceProtection <-gsub("No internet service","No",telecomDataframe$DeviceProtection)
telecomDataframe$TechSupport <-gsub("No internet service","No",telecomDataframe$TechSupport)
telecomDataframe$StreamingTV <-gsub("No internet service","No",telecomDataframe$StreamingTV)
telecomDataframe$StreamingMovies <-gsub("No internet service","No",telecomDataframe$StreamingMovies)
telecomDataframe$InternetService <-gsub("Fiber optic","Fiber_optic",telecomDataframe$InternetService)


# converting character variables into factor variables
telecomDataframe$MultipleLines <- as.factor(telecomDataframe$MultipleLines)
telecomDataframe$OnlineSecurity <- as.factor(telecomDataframe$OnlineSecurity)
telecomDataframe$OnlineBackup <- as.factor(telecomDataframe$OnlineBackup)
telecomDataframe$DeviceProtection <- as.factor(telecomDataframe$DeviceProtection)
telecomDataframe$TechSupport <- as.factor(telecomDataframe$TechSupport)
telecomDataframe$StreamingTV <- as.factor(telecomDataframe$StreamingTV)
telecomDataframe$StreamingMovies <- as.factor(telecomDataframe$StreamingMovies)
telecomDataframe$InternetService <- as.factor(telecomDataframe$InternetService)

# check the number of NA rows if it is relatively small in number then ignore those rows from the analysis
any(is.na(telecomDataframe))
telecomDataframe <- na.omit(telecomDataframe)

# set the seed it will output same output when ever the model is executed
set.seed(123)

#spliting train and test
 rows <- sample(nrow(telecomDataframe))
 telecomDataframe <- telecomDataframe[rows,]
 
 split <- round(nrow(telecomDataframe)*.70)
 trainData <- telecomDataframe[1:split,]
 testData <- telecomDataframe[(split+1):nrow(telecomDataframe),]
 nrow(trainData)/nrow(telecomDataframe)
 
# train glm with custom trainControl 
myControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = TRUE
)

##Model
model <- train(Churn~., data=telecomDataframe,method="glm",metric="ROC",
                trControl=myControl)

pred.glmModel <- as.vector(predict(model, newdata=testData, 
                                        type="prob")[,"Yes"])

#ROC Curve
colAUC(pred.glmModel,testData$Churn,plotROC = TRUE)

###Model2
model2 <- train(Churn ~., data=telecomDataframe,method="glmnet",metric="ROC",
                tuneGrid=expand.grid(alpha=0:1,lambda=seq(0.10/10)),trControl=myControl)


pred.glmnetModel <- as.vector(predict(model2, newdata=testData, 
                                   type="prob")[,"Yes"])


colAUC(pred.glmnetModel,testData$Churn,plotROC=TRUE)


models <- list(glm=model,glmnet=model2)
models <- resamples(models)

models

##BWplot
bwplot(models,metric = "ROC")

##dotplot
dotplot(models,metric = "ROC")
##density plot
densityplot(models,metric = "ROC")
#scatter plot
xyplot(models,metric = "ROC")
  
  
  
  
  
  
  
#Result
f.results <- ifelse(pred.glmModel > 0.5,1,0)

#Converting testData churn into character to convert replace them
testData$Churn <- as.character(testData$Churn)
testData$Churn[testData$Churn=="No"] <- "0"
testData$Churn[testData$Churn=="Yes"] <- "1"

#Misclassification error
misClasificationError <- mean(f.results!=testData$Churn)
print(misClasificationError)


# calculating the accuracy rate
accuracyRate <- 1-misClasificationError
print(accuracyRate)

#Confusion matrix
confusionMatrix(f.results,testData$Churn)

# cbinding actual results with the predicted results
results <- cbind(f.results,testData$Churn)
colnames(results) <- c("predicted","actual")
results <- as.data.frame(results)
print(results)


