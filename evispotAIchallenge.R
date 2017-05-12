#trainData <- read.csv("/home/simon/Programming/evispotAIChallenge/data/training_data.csv", header = T, na.strings=" ")
#testData <- read.csv("/home/simon/Programming/evispotAIChallenge/data/test_data.csv", header = T, na.strings=" ")
#testData <- testData[,-11]

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

#########################################
#### Substitute bad characters in town names
#########################################
#for training data
trainData$MRCH_CITY <- gsub("å|ä|ö|Å|Ä|Ö|A|E|O|a|e|o| ","", as.character(trainData$MRCH_CITY))
trainData$MRCH_CITY[which(is.na(trainData$MRCH_CITY))] <- "NO_TOWN"
trainData$MRCH_CITY <- as.factor(sapply(trainData$MRCH_CITY, toupper))

#for test data
testData$MRCH_CITY <- gsub("å|ä|ö|Å|Ä|Ö|A|E|O|a|e|o| ","", as.character(testData$MRCH_CITY))
testData$MRCH_CITY[which(is.na(testData$MRCH_CITY))] <- "NO_TOWN"
testData$MRCH_CITY <- as.factor(sapply(testData$MRCH_CITY, toupper))


#############################
#### Fix missing values in SEX, BIRTH_YEAR
##############################
#training
majoritySex <- MaxTable(trainData$SEX)
meanBirthYear <- round(mean(trainData$BIRTH_YEAR[!is.na(trainData$BIRTH_YEAR)]))
trainData$SEX[which(is.na(trainData$SEX))] <- majoritySex
trainData$BIRTH_YEAR[which(is.na(trainData$BIRTH_YEAR))] <- meanBirthYear

#test
majoritySex <- MaxTable(testData$SEX)
meanBirthYear <- round(mean(testData$BIRTH_YEAR[!is.na(testData$BIRTH_YEAR)]))
testData$SEX[which(is.na(testData$SEX))] <- majoritySex
testData$BIRTH_YEAR[which(is.na(testData$BIRTH_YEAR))] <- meanBirthYear

################################
#Check hometown/homecountry (majority of purchases made there) for each person
################################
#Also updates "NO_TOWN" to hometown (best guess for missing value)

#training data
IN_HOME_TOWN <- matrix(0, nrow = dim(trainData)[1], 1)
IN_HOME_COUNTRY <- matrix(0, nrow = dim(trainData)[1], 1)
for(ID in unique(trainData$Key_ENGNO)){
  ii <- which(trainData$Key_ENGNO == ID) #index vector
  homeTown <- MaxTable(trainData$MRCH_CITY[ii]) #decide hometown
  homeCountry <- MaxTable(trainData$MRCH_CTRY[ii])
  #loop though and check what transactions were actually made in hometown/country
  for(i in ii){ 
    if (trainData$MRCH_CITY[i] == "NO_TOWN") trainData$MRCH_CITY[i] <- homeTown
    if (trainData$MRCH_CITY[i] == homeTown) IN_HOME_TOWN[i] <- 1
    if (trainData$MRCH_CTRY[i] == homeCountry) IN_HOME_COUNTRY[i] <- 1
  }
}

trainData$IN_HOME_COUNTRY <- IN_HOME_COUNTRY 
trainData$IN_HOME_TOWN <- IN_HOME_TOWN

#test data
IN_HOME_TOWN <- matrix(0, nrow = dim(testData)[1], 1)
IN_HOME_COUNTRY <- matrix(0, nrow = dim(testData)[1], 1)
for(ID in unique(testData$Key_ENGNO)){
  ii <- which(testData$Key_ENGNO == ID) #index vector
  homeTown <- MaxTable(testData$MRCH_CITY[ii]) #decide hometown
  homeCountry <- MaxTable(testData$MRCH_CTRY[ii])
  #loop though and check what transactions were actually made in hometown/country
  for(i in ii){ 
    if (testData$MRCH_CITY[i] == "NO_TOWN") testData$MRCH_CITY[i] <- homeTown
    if (testData$MRCH_CITY[i] == homeTown) IN_HOME_TOWN[i] <- 1
    if (testData$MRCH_CTRY[i] == homeCountry) IN_HOME_COUNTRY[i] <- 1
  }
}

testData$IN_HOME_COUNTRY <- IN_HOME_COUNTRY 
testData$IN_HOME_TOWN <- IN_HOME_TOWN

##############################
### What weekday?
##############################

#Training data

# First transaction 4/1/2016
# Last transaction 3/31/2017
monthChar <- c('1/','2/','3/','4/','5/','6/','7/','8/','9/','10','11','12')
indexNr <- c(4,4,4,4,4,4,4,4,4,5,5,5)
startOfMonth <- c(275,306,334,0,30,61,91,122,153,183,214,244)
day <- vector(mode = "numeric", length = length(trainData$DATE))
for (i in 1:length(trainData$DATE)) {
  for (j in 1:length(monthChar)) {
    if (substr(trainData$DATE[i], 1,2) == monthChar[j]) {
      ifelse(substr(trainData$DATE[i], indexNr[j],indexNr[j]) == '/', day[i] <- startOfMonth[j] + as.numeric(substr(trainData$DATE[i],indexNr[j]-1,indexNr[j]-1)), day[i] <- startOfMonth[j] + as.numeric(substr(trainData$DATE[i],indexNr[j]-1,indexNr[j])))
    }
  }
}

dayString <- c('thursday','friday','saturday','sunday','monday','tuesday','wednesday')
weekday <- vector("character", length = length(day))
for (i in 1:length(weekday)) {
  for (j in 1:length(dayString)) {
    if (day[i]%%7 == j-1) weekday[i] <- dayString[j]
  }
}

month <- vector(mode = "numeric", length = length(trainData$DATE))
for (i in 1:length(month)) {
  ifelse(substr(trainData$DATE[i], 2,2) == '/', month[i] <- as.numeric(substr(trainData$DATE[i], 1,1)), month[i] <- as.numeric(substr(trainData$DATE[i], 1,2)))
}

trainData <- cbind(day, month, weekday, trainData)


#Test data

# First transaction 4/1/2016
# Last transaction 3/31/2017
monthChar <- c('1/','2/','3/','4/','5/','6/','7/','8/','9/','10','11','12')
indexNr <- c(4,4,4,4,4,4,4,4,4,5,5,5)
startOfMonth <- c(275,306,334,0,30,61,91,122,153,183,214,244)
day <- vector(mode = "numeric", length = length(testData$DATE))
for (i in 1:length(testData$DATE)) {
  for (j in 1:length(monthChar)) {
    if (substr(testData$DATE[i], 1,2) == monthChar[j]) {
      ifelse(substr(testData$DATE[i], indexNr[j],indexNr[j]) == '/', day[i] <- startOfMonth[j] + as.numeric(substr(testData$DATE[i],indexNr[j]-1,indexNr[j]-1)), day[i] <- startOfMonth[j] + as.numeric(substr(testData$DATE[i],indexNr[j]-1,indexNr[j])))
    }
  }
}

dayString <- c('thursday','friday','saturday','sunday','monday','tuesday','wednesday')
weekday <- vector("character", length = length(day))
for (i in 1:length(weekday)) {
  for (j in 1:length(dayString)) {
    if (day[i]%%7 == j-1) weekday[i] <- dayString[j]
  }
}

month <- vector(mode = "numeric", length = length(testData$DATE))
for (i in 1:length(month)) {
  ifelse(substr(testData$DATE[i], 2,2) == '/', month[i] <- as.numeric(substr(testData$DATE[i], 1,1)), month[i] <- as.numeric(substr(testData$DATE[i], 1,2)))
}

testData <- cbind(day, month, weekday, testData)


#####################################
#### Days since last salary?
#####################################

#training data
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

sincePayday <- vector(mode = "numeric", length = dim(trainData)[1])
for (i in 1:length(sincePayday)) {
  sincePayday[i] <- 1000
}
for (i in 1:length(sincePayday)) {
  for (j in 1:13) {
    ifelse(day[i]-paydays[j] >= 0 && day[i]-paydays[j] < sincePayday[i], sincePayday[i] <- day[i]-paydays[j], sincePayday[i] <- sincePayday[i])
  }
}
trainData <- cbind(sincePayday, trainData)

#testing data
paydays <- vector(mode = "numeric", length = 13)
paydays[1] <- testData$day[which(testData$DATE == '1/25/2017')][1]
paydays[2] <- testData$day[which(testData$DATE == '2/24/2017')][1]
paydays[3] <- testData$day[which(testData$DATE == '3/24/2017')][1]
paydays[4] <- -7
paydays[5] <- testData$day[which(testData$DATE == '4/25/2016')][1]
paydays[6] <- testData$day[which(testData$DATE == '5/25/2016')][1]
paydays[7] <- testData$day[which(testData$DATE == '6/24/2016')][1]
paydays[8] <- testData$day[which(testData$DATE == '7/25/2016')][1]
paydays[9] <- testData$day[which(testData$DATE == '8/25/2016')][1]
paydays[10] <- testData$day[which(testData$DATE == '9/23/2016')][1]
paydays[11] <- testData$day[which(testData$DATE == '10/25/2016')][1]
paydays[12] <- testData$day[which(testData$DATE == '11/25/2016')][1]
paydays[13] <- testData$day[which(testData$DATE == '12/23/2016')][1]

sincePayday <- vector(mode = "numeric", length = dim(testData)[1])
for (i in 1:length(sincePayday)) {
  sincePayday[i] <- 1000
}
for (i in 1:length(sincePayday)) {
  for (j in 1:13) {
    ifelse(day[i]-paydays[j] >= 0 && day[i]-paydays[j] < sincePayday[i], sincePayday[i] <- day[i]-paydays[j], sincePayday[i] <- sincePayday[i])
  }
}
testData <- cbind(sincePayday, testData)

####################################
#### Any PUR96 within last week? (exchange of money?)
####################################
# But only like 6 occurences out of 5*10^5, so perhaps not interesting?


####################################
#### ATM transaction?
####################################

#check for PUR94 and even 100 numbers.

#training data
temp <- as.numeric(levels(trainData$TRANS_AMO))[trainData$TRANS_AMO]
evenPUR94 <- vector(mode = "logical", length = length(month))
for (i in 1:length(month)) {
  ifelse(trainData$TRANSTYP_CODE[i] == 'PUR94' && temp[i]%%100 == 0, evenPUR94[i] <- TRUE, evenPUR94[i] <- FALSE) 
}
trainData <- cbind(evenPUR94, trainData)


#testing data
temp <- as.numeric(levels(testData$TRANS_AMO))[testData$TRANS_AMO]
evenPUR94 <- vector(mode = "logical", length = length(month))
for (i in 1:length(month)) {
  ifelse(testData$TRANSTYP_CODE[i] == 'PUR94' && temp[i]%%100 == 0, evenPUR94[i] <- TRUE, evenPUR94[i] <- FALSE) 
}
testData <- cbind(evenPUR94, testData)


########################################
###### Check for phone numbers in city data
########################################

#training data
PHONE_PAYMENT <- matrix(0, nrow = dim(trainData)[1], ncol = 1)
for (i in 1:length(trainData$MRCH_CITY)) {
  if (substr(trainData$MRCH_CITY[i], 1, 1) == '+') PHONE_PAYMENT[i] <- 1
}



#testing data
PHONE_PAYMENT <- matrix(0, nrow = dim(testData)[1], ncol = 1)
for (i in 1:length(testData$MRCH_CITY)) {
  if (substr(testData$MRCH_CITY[i], 1, 1) == '+') PHONE_PAYMENT[i] <- 1
}

##########################################
#### Build data set with only relevant variables
##########################################

#training data
# Change , to . in TRANS_AMO
trainData$TRANS_AMO <- gsub(",",".", as.character(trainData$TRANS_AMO))

trainData2 <- data.frame(trainData$KEYWORD, trainData$sincePayday, trainData$month, 
                    trainData$weekday, trainData$BIRTH_YEAR, trainData$SEX,
                    as.numeric(trainData$TRANS_AMO), trainData$TRANSTYP_CODE, trainData$IN_HOME_COUNTRY,
                    trainData$IN_HOME_TOWN)
names(trainData2) <- c("KEYWORD", 'SINCE_PAY_DAY', 'MONTH', 'WEEKDAY', 
                       'BIRTH_YEAR', 'SEX', 'TRANS_AMO', 'TRANSTYP_CODE',
                       'IN_HOME_COUNTRY', 'IN_HOME_TOWN')

#testing data
# Change , to . in TRANS_AMO
testData$TRANS_AMO <- gsub(",",".", as.character(testData$TRANS_AMO))

testData2 <- data.frame(testData$KEYWORD, testData$sincePayday, testData$month, 
                         testData$weekday, testData$BIRTH_YEAR, testData$SEX,
                         as.numeric(testData$TRANS_AMO), testData$TRANSTYP_CODE, testData$IN_HOME_COUNTRY,
                         testData$IN_HOME_TOWN)
names(testData2) <- c("KEYWORD", 'SINCE_PAY_DAY', 'MONTH', 'WEEKDAY', 
                       'BIRTH_YEAR', 'SEX', 'TRANS_AMO', 'TRANSTYP_CODE',
                       'IN_HOME_COUNTRY', 'IN_HOME_TOWN')


######################################
#### Predict with random forest
######################################

library(randomForest)
rf.object <- randomForest(x = trainData2[,-1], y = trainData2[,1],
                   xtest = testData2[,-1], ytest = testData2[,1],
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
