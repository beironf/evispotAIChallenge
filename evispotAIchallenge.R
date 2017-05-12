trainData <- read.csv("/home/simon/Programming/evispotChallenge/data/training_data.csv", header = T)
testData <- read.csv("/home/simon/Programming/evispotChallenge/data/test_data.csv", header = T)
testData <- testData[,-11]

#Substitute bad characters in town names
trainData$MRCH_CITY <- gsub("å","", trainData$MRCH_CITY)
trainData$MRCH_CITY <- gsub("ä","", trainData$MRCH_CITY)
trainData$MRCH_CITY <- gsub("ö","", trainData$MRCH_CITY)
trainData$MRCH_CITY <- gsub("a","", trainData$MRCH_CITY)
trainData$MRCH_CITY <- gsub("e","", trainData$MRCH_CITY)
trainData$MRCH_CITY <- gsub("o","", trainData$MRCH_CITY)
trainData$MRCH_CITY <- gsub(" ","", trainData$MRCH_CITY)
trainData$MRCH_CITY <- lapply(trainData$MRCH_CITY, toupper)

#Function to check what is the plurality factor in a set
#Can return multiple choices if there is a tie (optional)
MaxTable <- function(InVec, mult = FALSE) {
  if (!is.factor(InVec)) InVec <- factor(InVec)
  A <- tabulate(InVec)
  if (isTRUE(mult)) {
    levels(InVec)[A == max(A)]
  } 
  else levels(InVec)[which.max(A)]
}

#Check hometown/homecountry (majority of purchases made there) for each person
IN_HOME_TOWN <- matrix(0, nrow = dim(trainData)[1], 1)
IN_HOME_COUNTRY <- matrix(0, nrow = dim(trainData)[1], 1)
for(ID in unique(trainData$Key_ENGNO)){
  ii <- which(trainData$Key_ENGNO == ID) #index vector
  homeTown <- MaxTable(trainData$MRCH_CITY[ii]) #decide hometown
  homeCountry <- MaxTable(trainData$MRCH_CTRY[ii])
  #loop though and check what transactions were actually made in hometown/country
  for(i in ii){ 
    if (trainData$MRCH_CITY[i] == homeTown) IN_HOME_TOWN[i] <- 1
    if (trainData$MRCH_CTRY[i] == homeCountry) IN_HOME_COUNTRY[i] <- 1
  }
}

#
