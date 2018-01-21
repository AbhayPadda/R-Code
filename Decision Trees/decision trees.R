library(xtable)
library(rpart)
library(mlbench)
data('Ionosphere')
str(Ionosphere)


v <- Ionosphere$Class

table(v)

set.seed(522)

Ionosphere[, 'train'] <- ifelse(runif(nrow(Ionosphere)) < 0.75, 1, 0)

trainSet <- Ionosphere[Ionosphere$train == 1,]
testSet <- Ionosphere[Ionosphere$train == 0, ]

trainColNum <- grep('train', names(trainSet))

trainSet <- trainSet[, -trainColNum]
testSet <- testSet[, -trainColNum]

typeColNum <- grep('Class', names(Ionosphere))
rpart_model <-rpart(Class~.,data=trainSet, method='class')

rpart_model <- rpart(Class~., data=trainSet, method = 'class')

plot(rpart_model)
text(rpart_model)

rpart_predict <-  predict(rpart_model,testSet[,-typeColNum],type='class')

mn1 <- mean(rpart_predict==testSet$Class)
mn1

table(pred=rpart_predict,true=testSet$Class)


## Pruning the decision tree
printcp(rpart_model)

opt  <-  which.min(rpart_model$cptable[,'xerror'])

cp <-  rpart_model$cptable[opt, 'CP']
pruned_model <-  prune(rpart_model,cp)
plot(pruned_model);text(pruned_model)


rpart_pruned_predict <- predict(pruned_model, testSet[, -typeColNum], type='class')
mn2 <- mean(rpart_pruned_predict==testSet$Class)
mn2

table(pred=rpart_pruned_predict,true=testSet$Class)
