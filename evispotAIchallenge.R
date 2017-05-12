trainData <- read.csv("/home/simon/Programming/evispotAIChallenge/data/training_data.csv", header = T, na.strings=" ")
testData <- read.csv("/home/simon/Programming/evispotAIChallenge/data/test_data.csv", header = T, na.strings=" ")
testData <- testData[,-11]

#Substitute bad characters in town names
trainData$MRCH_CITY <- gsub("å|ä|ö|Å|Ä|Ö|A|E|O|a|e|o| ","", as.character(trainData$MRCH_CITY))
trainData$MRCH_CITY <- as.factor(sapply(trainData$MRCH_CITY, toupper))

#Remove NA and substitute for NO_TOWN
trainData$MRCH_CITY[which(is.na(trainData$MRCH_CITY))] <- "NO_TOWN"

#Function to check what is the plurality factor in a  set
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
  print(ID)
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

rm(list = ls())
test_data <- read.csv('test_data.csv')
training_data <- read.csv('training_data.csv')

# First transaction 4/1/2016
# Last transaction 3/31/2017

date <- vector(mode = "numeric", length = length(training_data$DATE))
for (i in 1:length(training_data$DATE)) {
  if (substr(training_data$DATE[i], 1,2) == '4/') {
    ifelse(substr(training_data$DATE[i], 4,4) == '/', date[i] <- as.numeric(substr(training_data$DATE[i],3,3)), date[i] <- as.numeric(substr(training_data$DATE[i],3,4))) 
  } else if (substr(training_data$DATE[i], 1,2) == '5/') {
    ifelse(substr(training_data$DATE[i], 4,4) == '/', date[i] <- 30 + as.numeric(substr(training_data$DATE[i],3,3)), date[i] <- 30 + as.numeric(substr(training_data$DATE[i],3,4))) 
  } else if (substr(training_data$DATE[i], 1,2) == '6/') {
    ifelse(substr(training_data$DATE[i], 4,4) == '/', date[i] <- 61 + as.numeric(substr(training_data$DATE[i],3,3)), date[i] <- 61 + as.numeric(substr(training_data$DATE[i],3,4))) 
  } else if (substr(training_data$DATE[i], 1,2) == '7/') {
    ifelse(substr(training_data$DATE[i], 4,4) == '/', date[i] <- 91 + as.numeric(substr(training_data$DATE[i],3,3)), date[i] <- 91 + as.numeric(substr(training_data$DATE[i],3,4))) 
  } else if (substr(training_data$DATE[i], 1,2) == '8/') {
    ifelse(substr(training_data$DATE[i], 4,4) == '/', date[i] <- 122 + as.numeric(substr(training_data$DATE[i],3,3)), date[i] <- 122 + as.numeric(substr(training_data$DATE[i],3,4))) 
  } else if (substr(training_data$DATE[i], 1,2) == '9/') {
    ifelse(substr(training_data$DATE[i], 4,4) == '/', date[i] <- 153 + as.numeric(substr(training_data$DATE[i],3,3)), date[i] <- 153 + as.numeric(substr(training_data$DATE[i],3,4))) 
  } else if (substr(training_data$DATE[i], 1,2) == '10') {
    ifelse(substr(training_data$DATE[i], 5,5) == '/', date[i] <- 183 + as.numeric(substr(training_data$DATE[i],4,4)), date[i] <- 183 + as.numeric(substr(training_data$DATE[i],4,5))) 
  } else if (substr(training_data$DATE[i], 1,2) == '11') {
    ifelse(substr(training_data$DATE[i], 5,5) == '/', date[i] <- 214 + as.numeric(substr(training_data$DATE[i],4,4)), date[i] <- 214 + as.numeric(substr(training_data$DATE[i],4,5))) 
  } else if (substr(training_data$DATE[i], 1,2) == '12') {
    ifelse(substr(training_data$DATE[i], 5,5) == '/', date[i] <- 244 + as.numeric(substr(training_data$DATE[i],4,4)), date[i] <- 244 + as.numeric(substr(training_data$DATE[i],4,5))) 
  } else if (substr(training_data$DATE[i], 1,2) == '1/') {
    ifelse(substr(training_data$DATE[i], 4,4) == '/', date[i] <- 275 + as.numeric(substr(training_data$DATE[i],3,3)), date[i] <- 275 + as.numeric(substr(training_data$DATE[i],3,4))) 
  } else if (substr(training_data$DATE[i], 1,2) == '2/') {
    ifelse(substr(training_data$DATE[i], 4,4) == '/', date[i] <- 306 + as.numeric(substr(training_data$DATE[i],3,3)), date[i] <- 306 + as.numeric(substr(training_data$DATE[i],3,4))) 
  } else if (substr(training_data$DATE[i], 1,2) == '3/') {
    ifelse(substr(training_data$DATE[i], 4,4) == '/', date[i] <- 334 + as.numeric(substr(training_data$DATE[i],3,3)), date[i] <- 334 + as.numeric(substr(training_data$DATE[i],3,4))) 
  }
}

weekday <- vector("character", length = length(date))
for (i in 1:length(weekday)) {
  ifelse(date[i]%%7 == 0, weekday[i] <- 'thursday', weekday[i] <- weekday[i])
  ifelse(date[i]%%7 == 1, weekday[i] <- 'friday', weekday[i] <- weekday[i])
  ifelse(date[i]%%7 == 2, weekday[i] <- 'saturday', weekday[i] <- weekday[i])
  ifelse(date[i]%%7 == 3, weekday[i] <- 'sunday', weekday[i] <- weekday[i])
  ifelse(date[i]%%7 == 4, weekday[i] <- 'monday', weekday[i] <- weekday[i])
  ifelse(date[i]%%7 == 5, weekday[i] <- 'tuesday', weekday[i] <- weekday[i])
  ifelse(date[i]%%7 == 5, weekday[i] <- 'wednesday', weekday[i] <- weekday[i])
}


month <- vector(mode = "numeric", length = length(training_data$DATE))
for (i in 1:length(month)) {
  ifelse(substr(training_data$DATE[i], 2,2) == '/', month[i] <- as.numeric(substr(training_data$DATE[i], 1,1)), month[i] <- as.numeric(substr(training_data$DATE[i], 1,2)))
}

training_data <- cbind(date, month, weekday, training_data)


paydays <- vector(mode = "numeric", length(month))
payday[1] <- training_data$date[which(training_data$DATE == '1/25/2017')][1]
payday[2] <- training_data$date[which(training_data$DATE == '1/25/2017')][1]


