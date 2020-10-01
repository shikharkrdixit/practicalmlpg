library(caret)
library(rpart); library(rpart.plot)
library(rattle)
library(randomForest)

download.file(url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
              destfile = "./trainingfile.csv")
training <- read.csv("./trainingfile.csv",na.strings=c("NA","#DIV/0!", ""))
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",destfile = "./testfile.csv")
test <- read.csv("./testfile.csv",na.strings=c("NA","#DIV/0!", ""))

names(training)

training <- training[,colSums(is.na(training))==0]
test <- test[,colSums(is.na(test))==0]
dim(training)
training <- training[,-c(1:7)]
test <- test[,-c(1:7)]

traintraining <- createDataPartition(y = training$classe,p = 0.75,list = FALSE)
testtraining <- training[-traintraining,]
traintraining <- training[traintraining,]

dim(testtraining)
dim(traintraining)
plot(as.factor(traintraining$classe),col = "dark green",
     xlab="Levels",ylab = "Counts",main="The Presence of Each level in the Data-Set",ylim = c(-1,5000))
traintraining$classe <- factor(traintraining$classe)

#Decision Tree
dsc <- rpart(classe~.,data = traintraining,method="class",control = rpart.control(minsplit = 4959,maxdepth = 9))
fancyRpartPlot(dsc)
predictionsA <- predict(dsc,testtraining, type = "class")
confusionMatrix(predictionsA,testtraining$classe)

#Random Forest
rnf <- randomForest(classe~.,data = traintraining)
predictionsB1 <- predict(rnf,testtraining, type = "class")
testtraining$classe <- as.factor(testtraining$classe)
confusionMatrix(predictionsB1,testtraining$classe)
plot(rnf)



finalpred <- predict(rnf,test,type="class")
finalpred
