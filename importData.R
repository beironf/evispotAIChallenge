dir <- getSrcDirectory(function(x) {x})
trainData <- read.csv(paste(dir,"/data/training_data.csv",sep=""), header = T, na.strings=" ")
testData <- read.csv(paste(dir,"/data/test_data.csv",sep=""), header = T, na.strings=" ")
testData <- testData[,-11]
