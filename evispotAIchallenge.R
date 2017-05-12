#trainData <- read.csv("/home/simon/Programming/evispotAIChallenge/data/training_data.csv", header = T, na.strings=" ")
#testData <- read.csv("/home/simon/Programming/evispotAIChallenge/data/test_data.csv", header = T, na.strings=" ")
#testData <- testData[,-11]

#Substitute bad characters in town names
trainData$MRCH_CITY <- gsub("å|ä|ö|Å|Ä|Ö|A|E|O|a|e|o| ","", as.character(trainData$MRCH_CITY))
trainData$MRCH_CITY[which(is.na(trainData$MRCH_CITY))] <- "NO_TOWN"
trainData$MRCH_CITY <- as.factor(sapply(trainData$MRCH_CITY, toupper))

#############################
#### Fix missing values in SEX, CITY, BIRTH_YEAR
##############################


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

################################
#Check hometown/homecountry (majority of purchases made there) for each person
################################

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

##############################
### What weekday?
##############################

# First transaction 4/1/2016
# Last transaction 3/31/2017

day <- vector(mode = "numeric", length = length(trainData$DATE))
for (i in 1:length(trainData$DATE)) {
  if (substr(trainData$DATE[i], 1,2) == '4/') {
    ifelse(substr(trainData$DATE[i], 4,4) == '/', day[i] <- as.numeric(substr(trainData$DATE[i],3,3)), day[i] <- as.numeric(substr(trainData$DATE[i],3,4))) 
  } else if (substr(trainData$DATE[i], 1,2) == '5/') {
    ifelse(substr(trainData$DATE[i], 4,4) == '/', day[i] <- 30 + as.numeric(substr(trainData$DATE[i],3,3)), day[i] <- 30 + as.numeric(substr(trainData$DATE[i],3,4))) 
  } else if (substr(trainData$DATE[i], 1,2) == '6/') {
    ifelse(substr(trainData$DATE[i], 4,4) == '/', day[i] <- 61 + as.numeric(substr(trainData$DATE[i],3,3)), day[i] <- 61 + as.numeric(substr(trainData$DATE[i],3,4))) 
  } else if (substr(trainData$DATE[i], 1,2) == '7/') {
    ifelse(substr(trainData$DATE[i], 4,4) == '/', day[i] <- 91 + as.numeric(substr(trainData$DATE[i],3,3)), day[i] <- 91 + as.numeric(substr(trainData$DATE[i],3,4))) 
  } else if (substr(trainData$DATE[i], 1,2) == '8/') {
    ifelse(substr(trainData$DATE[i], 4,4) == '/', day[i] <- 122 + as.numeric(substr(trainData$DATE[i],3,3)), day[i] <- 122 + as.numeric(substr(trainData$DATE[i],3,4))) 
  } else if (substr(trainData$DATE[i], 1,2) == '9/') {
    ifelse(substr(trainData$DATE[i], 4,4) == '/', day[i] <- 153 + as.numeric(substr(trainData$DATE[i],3,3)), day[i] <- 153 + as.numeric(substr(trainData$DATE[i],3,4))) 
  } else if (substr(trainData$DATE[i], 1,2) == '10') {
    ifelse(substr(trainData$DATE[i], 5,5) == '/', day[i] <- 183 + as.numeric(substr(trainData$DATE[i],4,4)), day[i] <- 183 + as.numeric(substr(trainData$DATE[i],4,5))) 
  } else if (substr(trainData$DATE[i], 1,2) == '11') {
    ifelse(substr(trainData$DATE[i], 5,5) == '/', day[i] <- 214 + as.numeric(substr(trainData$DATE[i],4,4)), day[i] <- 214 + as.numeric(substr(trainData$DATE[i],4,5))) 
  } else if (substr(trainData$DATE[i], 1,2) == '12') {
    ifelse(substr(trainData$DATE[i], 5,5) == '/', day[i] <- 244 + as.numeric(substr(trainData$DATE[i],4,4)), day[i] <- 244 + as.numeric(substr(trainData$DATE[i],4,5))) 
  } else if (substr(trainData$DATE[i], 1,2) == '1/') {
    ifelse(substr(trainData$DATE[i], 4,4) == '/', day[i] <- 275 + as.numeric(substr(trainData$DATE[i],3,3)), day[i] <- 275 + as.numeric(substr(trainData$DATE[i],3,4))) 
  } else if (substr(trainData$DATE[i], 1,2) == '2/') {
    ifelse(substr(trainData$DATE[i], 4,4) == '/', day[i] <- 306 + as.numeric(substr(trainData$DATE[i],3,3)), day[i] <- 306 + as.numeric(substr(trainData$DATE[i],3,4))) 
  } else if (substr(trainData$DATE[i], 1,2) == '3/') {
    ifelse(substr(trainData$DATE[i], 4,4) == '/', day[i] <- 334 + as.numeric(substr(trainData$DATE[i],3,3)), day[i] <- 334 + as.numeric(substr(trainData$DATE[i],3,4))) 
  }
}

weekday <- vector("character", length = length(day))
for (i in 1:length(weekday)) {
  ifelse(day[i]%%7 == 0, weekday[i] <- 'thursday', weekday[i] <- weekday[i])
  ifelse(day[i]%%7 == 1, weekday[i] <- 'friday', weekday[i] <- weekday[i])
  ifelse(day[i]%%7 == 2, weekday[i] <- 'saturday', weekday[i] <- weekday[i])
  ifelse(day[i]%%7 == 3, weekday[i] <- 'sunday', weekday[i] <- weekday[i])
  ifelse(day[i]%%7 == 4, weekday[i] <- 'monday', weekday[i] <- weekday[i])
  ifelse(day[i]%%7 == 5, weekday[i] <- 'tuesday', weekday[i] <- weekday[i])
  ifelse(day[i]%%7 == 6, weekday[i] <- 'wednesday', weekday[i] <- weekday[i])
}


month <- vector(mode = "numeric", length = length(trainData$DATE))
for (i in 1:length(month)) {
  ifelse(substr(trainData$DATE[i], 2,2) == '/', month[i] <- as.numeric(substr(trainData$DATE[i], 1,1)), month[i] <- as.numeric(substr(trainData$DATE[i], 1,2)))
}

trainData <- cbind(day, month, weekday, trainData)

#####################################
#### Days since last salary?
#####################################

paydays <- vector(mode = "numeric", length = 13)
paydays[1] <- trainData$day[which(trainData$DATE == '1/25/2017')][1]
paydays[2] <- trainData$day[which(trainData$DATE == '2/24/2017')][1]
paydays[3] <- trainData$day[which(trainData$DATE == '3/24/2017')][1]
paydays[4] <- -7
paydays[5] <- trainData$day[which(trainData$DATE == '4/25/2016')][1]
paydays[6] <- trainData$day[which(trainData$DATE == '5/25/2016')][1]
paydays[7] <- trainData$day[which(trainData$DATE == '6/24/2016')][1]
paydays[8] <- trainData$day[which(trainData$DATE == '7/25/2016')][1]
paydays[9] <- trainData$day[which(trainData$DATE == '8/25/2016')][1]
paydays[10] <- trainData$day[which(trainData$DATE == '9/23/2016')][1]
paydays[11] <- trainData$day[which(trainData$DATE == '10/25/2016')][1]
paydays[12] <- trainData$day[which(trainData$DATE == '11/25/2016')][1]
paydays[13] <- trainData$day[which(trainData$DATE == '12/23/2016')][1]

sincePayday <- vector(mode = "numeric", length = length(month))
for (i in 1:length(sincePayday)) {
  sincePayday[i] <- 1000
}
for (i in 1:length(sincePayday)) {
  for (j in 1:13) {
    ifelse(day[i]-paydays[j] >= 0 && day[i]-paydays[j] < sincePayday[i], sincePayday[i] <- day[i]-paydays[j], sincePayday[i] <- sincePayday[i])
  }
}
trainData2 <- cbind(sincePayday, trainData)


####################################
#### Any PUR96 within last week? (exchange of money?)
####################################

RECENT_EXCHANGE <- matrix(0, nrow = dim(trainData)[1], ncol = 1)
for(ID in unique(trainData$Key_ENGNO)){
  
}

#####################################
#### ATM transaction?
####################################

#check for PUR94 and even 100 numbers.



########################################
###### Check for phone numbers in city data
########################################

