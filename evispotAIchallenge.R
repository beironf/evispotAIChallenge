#trainData2 <- read.csv("/home/simon/Programming/evispotAIChallenge/data/training_data2.csv", header = T, na.strings=" ")
#testData2 <- read.csv("/home/simon/Programming/evispotAIChallenge/data/test_data2.csv", header = T, na.strings=" ")
#testData2 <- testData2[,-11]

######################################
#### Predict with random forest
######################################

library(randomForest)
rf.object <- randomForest(x = trainData4[,-1], y = trainData4[,1],
                   xtest = testData4[,-1], ytest = testData4[,1],
                   proximity = F)
#library(h2o)
#localH2O <- h2o.init()
#train <- h2o.importFile(path = normalizePath("/home/simon/Programming/evispotAIChallenge/data/training_data2.csv"))
#test <- h2o.importFile(path = normalizePath("/home/simon/Programming/evispotAIChallenge/data/test_data2.csv"))
#
#rf.h2o.object <- h2o.randomForest(y = 'KEYWORD',
#                                  training_frame = df,
#                                  validation_frame = test)
#pred <- predict(rf.h2o.object, test)


#######################################
#### Naive bayes?
#######################################

library(e1071)
nb.object <- naiveBayes(KEYWORD~., data = trainData2)
nb.pred <- predict(nb.object, testData2)
table(nb.pred, testData2$KEYWORD)
sum(diag(table(nb.pred, testData2$KEYWORD))) / dim(testData2)[1]

#######################################
#### SOM
#######################################
library(kohonen)
som_model <- supersom(as.matrix(trainData3[,-1]),
                 grid=somgrid(10,10), 
                 rlen=100, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE)

#######################################
#### Clustering based methods. can we find clusters?
#######################################


#######################################
#### kNN
#######################################
library(class)
knn_object_10 <- knn(train =trainData3[,-1], 
                  test = testData3[,-1], 
                  cl = trainData3[,1],
                  k=10)
