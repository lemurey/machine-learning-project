setwd('C:/Users/Lee/Desktop/sharedwithvm/datasciencespec/regression project')

library(caret);library(ggplot2);library(randomForest)

if (!file.exists('pml-training.csv')){
     fileurl <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
     download.file(fileurl,'pml-training.csv')
}

if (!file.exists('pml-testing.csv')){
     fileurl <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'
     download.file(fileurl,'pml-testing.csv')
}

training <- read.csv('pml-training.csv',na.strings=c('NA','','#DIV/0!'))
testing <- read.csv('pml-testing.csv')

set.seed(565)
intrain <- createDataPartition(y=training[,160],p=0.7,list=FALSE)
train <- training[intrain,]
test <- training[-intrain,]

howmanyna <- sapply(train,function(x) {sum(is.na(x))})
table(howmanyna)
dim(train)


#remove columns of mostly NA values

colNA <- names(howmanyna[howmanyna>=13451])
train <- train[,!names(train) %in% colNA]
test <- test[,!names(test) %in% colNA]
dim(train)

str(train)

train <- train[,-c(1:7)]
test <- test[,-c(1:7)]

set.seed(565)
system.time(model <- train(classe~.,data=train,method='rf'))

varImp(model)

implevels <- c('roll_belt','pitch_forearm','yaw_belt','magnet_dumbbell_y',
               'pitch_belt', 'magnet_dumbbell_z','roll_forearm',
               'accel_dumbbell_y','roll_dumbbell','magnet_dumbbell_x',
               'accel_forearm_x','magnet_belt_z','magnet_forearm_z',
               'total_accel_dumbbell','accel_belt_z','magnet_belt_y',
               'accel_dumbbell_z','gyros_belt_z','yaw_arm','yaw_dumbbell')
implevels <- c(implevels,'classe')
train2 <- train[,names(train)%in%implevels]
set.seed(787)
model2 <- train(classe~.,data=train2,method='rf')


predict <- predict(model,test)
confusionMatrix(predict,test$classe)

predict2 <- predict(model2,test)
confusionMatrix(predict2,test$classe)


#apply row removals to testing data
testing <- testing[,!names(testing) %in% colNA]
testing <- testing[,-c(1:7)]

predictfinal <- predict(model,testing)
predictfinal2 <- predict(model2,testing)

#write files for submission

pml_write_files(predictfinal)


pml_write_files = function(x){
     n = length(x)
     for(i in 1:n){
          filename = paste0("problem_id_",i,".txt")
          write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
     }
}









