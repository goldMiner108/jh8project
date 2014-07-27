library(caret)

trainingUrl <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
destTraining <- "./pml-training.csv"
testingUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
destTesting <- "./pml-testing.csv"

#get training data
download.file(trainingUrl, destTraining)
datTraining <- read.csv(destTraining)
#datTraining2<- read.csv(destTraining, na.strings = c("NA",""))

#get testing data
download.file(testingUrl, destTesting)
datTesting <- read.csv(destTesting)

#clean up NA data
#datTraining2 <- datTraining[, which(as.numeric(colSums(is.na(datTraining)))==0)]

#column numbers of NA and blank
#3:5 12:36, 50:59, 69:83, 87:101, 103:112, 125:139, 141:150
#p<-c(1:2, 6:11, 37:49, 60:68, 84:86, 102, 113:124, 140, 151:160)
p <- c(46:48,84:86,122:124,160)
datTraining <- datTraining[,p]
datTesting <- datTesting[,p]

inTrain <- createDataPartition(y=datTraining$classe, p=0.25,list=FALSE)
training <- datTraining[inTrain,]
testing <- datTraining[-inTrain,]
x <- createDataPartition(y=testing$classe, p=0.25,list=FALSE)
testing <- testing[x,]

modFit <- train(classe ~.,data=training,method="rf",prox=TRUE)
modFit

#pca
#modelFit <- train(training$classe ~., method='glm', preProcess="pca", data=training)
#confusionMatrix(testing1$classe, predict(modelFit, testing))

pred <- predict(modFit, testing);
testing$predRight <- pred==testing$classe
table(pred,testing$classe)
confusionMatrix(testing$classe, predict(modFit, testing))
#20% training 87% accuracy
#10% training 81% accuracy

#test 20 cases
pred <- predict(modFit, datTesting);
