setwd('C:/Users/Lee/Desktop/sharedwithvm/datasciencespec/ML project')

library(caret);library(ggplot2);library(randomForest)

if (!file.exists('pml-training.csv')){
     fileurl <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
     download.file(fileurl,'pml-training.csv')
}

if (!file.exists('pml-testing.csv')){
     fileurl <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'
     download.file(fileurl,'pml-testing.csv')
}

data <- read.csv('pml-training.csv',na.strings=c('NA','','#DIV/0!'))
testing <- read.csv('pml-testing.csv',na.string=c('NA','','#DIV/0!'))




howmanyna <- sapply(data,function(x) sum(is.na(x)))
table(howmanyna)

colremove <- names(howmanyna[howmanyna>=19216])
training <- data[,!(names(data) %in% colremove)] 

names(training[1:7])

training <- training[,-c(1:7)]

rawdataindices <- grep(x=names(training),pattern='_x|_y|_z')
avedataindices <- setdiff(1:53,rawdataindices)

set.seed(121)
traininds <- createDataPartition(y=training$classe,p=0.5,list=FALSE)
train <- training[traininds,]
rest <- training[-traininds,]
set.seed(565)
cvinds <- createDataPartition(y=rest$classe[],p=0.5,list=FALSE)
test <- rest[-cvinds,]
cv <- rest[cvinds,]


trainraw <- train[,c(rawdataindices,53)]
trainave <- train[,avedataindices]


ctrlpar <- trainControl(method='cv',number=3,repeats=1,seeds=list(c(121L,565L,787L),
                                                                  c(323L,454L,898L),
                                                                  c(212L,656L,878L),
                                                                  369L))

modelraw <- train(classe~.,data=trainraw,method='rf',trControl=ctrlpar)
elapsedraw <- 147.75
modelave <- train(classe~.,data=trainave,method='rf',trControl=ctrlpar)
elapsedave <- 69.04
modelboth <- train(classe~.,data=train,method='rf',trControl=ctrlpar)
elapsedboth <- 197.47


predictraw <- predict(modelraw,cv)
predictave <- predict(modelave,cv)
predictboth <- predict(modelboth,cv)

confusionMatrix(predictraw,cv$classe)
confusionMatrix(predictave,cv$classe)
confusionMatrix(predictboth,cv$classe)


predictf <- predict(modelboth,test)
confusionMatrix(predictf,test$classe)


