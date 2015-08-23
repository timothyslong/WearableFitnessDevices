library(doSNOW)
library(caret)
library(rpart)
library(rattle)
source('corrPlotter.R')

cl <- makeCluster(8)
registerDoSNOW(cl)

training.all <- read.csv('pml-training.csv', na.strings='#DIV/0!')
training.all <- training.all[, !names(training.all) %in% c('X', 'user_name','raw_timestamp_part_1','raw_timestamp_part_2', 'cvtd_timestamp')]

set.seed(12345)
inTrain <- createDataPartition(training.all$classe, p=0.7, list=F)
training <- training.all[inTrain,]  
testing <- training.all[-inTrain,]

nzvs <- nearZeroVar(x=training, saveMetrics = F, allowParallel = T)
training <- training[, -nzvs]
testing <- testing[, -nzvs]

corrs <- cor(training.nums, use = 'pairwise')
corrPlot(corrs)

# Identify the numeric fields so we can exclude non-numerics from preProcessing
numFields <- sapply(names(training), function(a){is.numeric(training[, a])})
# There are many div0 columns that I have loaded as NA.  Not sure if they're div0 correctly or not.  Choosing to impute values using k nearest neighbors.
pp <- preProcess(x=training[, numFields], method = c('knnImpute'))
training.x <- predict(pp, training[, numFields])
testing.x <- predict(pp, testing[, numFields])

training.y <- training$classe
testing.y <- testing$classe

#### TREE ####
tree.trainControl <- trainControl(method='cv', number=10, repeats=20, classProbs=F)
tree.tuneGrid <- expand.grid(cp=(1:40)/100000)
tree.fit <- train(y=training.y, 
             x=training.x,
             method='rpart',
             trControl = tree.trainControl,
             tuneGrid = tree.tuneGrid
            )
tree.fit
plot(tree.fit, main='Training Curve for RPart Decision Trees')
fancyRpartPlot(tree.fit, main='Decision Tree Predictions of Classe')


tree.predictions <- predict(tree.fit, testing.x)
confusionMatrix(tree.predictions, testing.y)


#### Gradient Boosting Machine ####
gbm.trainControl <- trainControl(method='cv', number=10, repeats=1)
#3 depth, 50 trees => 93% accuracy
# 4, 100 => 98.6%
gbm.tuneGrid <- expand.grid(n.trees=c(100,250,500,750), interaction.depth=c(3,5,7,9), shrinkage=c(0.1), n.minobsinnode=50)
gbm.fit <- train(y=training.y, 
                 x=training.x,
                 method='gbm',
                 trControl = gbm.trainControl,
                 tuneGrid = gbm.tuneGrid
              )
gbm.fit
plot(gbm.fit)
plot(varImp(gbm.fit), main='GBM Fit')

gbm.predictions <- predict(gbm.fit, testing.x)
confusionMatrix(gbm.predictions, testing.y)


#### Random Forest ####
rf.trainControl <- trainControl(method='cv', number=10, repeats=2)
rf.tuneGrid <-  expand.grid(mtry = c(2), ntree=c(100)) # model selection parameter in this case mtry.

rf.fit <- train(y=training.y, 
                 x=training.x,
                 method='rf',
                 trControl = rf.trainControl,
                # tuneGrid = rf.tuneGrid,
                 ntree=1000
)
rf.fit
plot(rf.fit)
plot(varImp(rf.fit))



rf.predictions <- predict(rf.fit, testing.x)
confusionMatrix(rf.predictions, testing.y)


#### Holdout Set Evaluation ####
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictions)