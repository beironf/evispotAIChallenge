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

#######################################
#### Fix TRANS_AMO from levels to numeric
#######################################
trainData$TRANS_AMO <- gsub(",",".", as.character(trainData$TRANS_AMO))
testData$TRANS_AMO <- gsub(",",".", as.character(testData$TRANS_AMO))

trainData$TRANS_AMO <- as.numeric(trainData$TRANS_AMO)
testData$TRANS_AMO <- as.numeric(testData$TRANS_AMO)

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
#### Check hometown/homecountry (majority of purchases made there) for each person
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
library(lubridate)

#Training data

# First transaction 4/1/2016
# Last transaction 3/31/2017
trainData$DATE <- mdy(trainData$DATE)
day <- as.numeric(difftime(trainData$DATE,min(trainData$DATE)-1,units="days"))
weekday <- wday(trainData$DATE,label=T)
weekday <- factor(weekday,ordered=F)
month <- month(trainData$DATE,label=T)
month <- factor(month,ordered=F)

trainData$day <- day
trainData$month <- month
trainData$weekday <- weekday

#Test data

# First transaction 4/1/2016
# Last transaction 3/31/2017
testData$DATE <- mdy(testData$DATE)
day <- as.numeric(difftime(testData$DATE,min(testData$DATE)-1,units="days"))
weekday <- wday(testData$DATE,label=T)
weekday <- factor(weekday,ordered=F)
month <- month(testData$DATE,label=T)
month <- factor(month,ordered=F)

testData$day <- day
testData$month <- month
testData$weekday <- weekday

#####################################
#### Days since last salary?
#####################################

#training data
day <- trainData$day
paydays <- sort(mdy(c('1/25/2017','2/24/2017','3/24/2017','3/25/2016','4/25/2016','5/25/2016','6/24/2016','7/25/2016','8/25/2016','9/23/2016','10/25/2016','11/25/2016','12/23/2016')))
paydays <- as.numeric(difftime(paydays,min(trainData$DATE)-1,units="days"))
sincePayday <- day-paydays[1]
for (i in 2:length(paydays)) {
  sincePayday <- cbind(sincePayday,day-paydays[i])
}
sincePayday[which(sincePayday < 0)] <- -1000
sincePayday <- apply(abs(sincePayday),1,min)

trainData$sincePayday <- sincePayday

#testing data
day <- testData$day
paydays <- sort(mdy(c('1/25/2017','2/24/2017','3/24/2017','3/25/2016','4/25/2016','5/25/2016','6/24/2016','7/25/2016','8/25/2016','9/23/2016','10/25/2016','11/25/2016','12/23/2016')))
paydays <- as.numeric(difftime(paydays,min(testData$DATE)-1,units="days"))
sincePayday <- day-paydays[1]
for (i in 2:length(paydays)) {
  sincePayday <- cbind(sincePayday,day-paydays[i])
}
sincePayday[which(sincePayday < 0)] <- -1000
sincePayday <- apply(abs(sincePayday),1,min)

testData$sincePayday <- sincePayday

####################################
#### Any PUR96 within last week? (exchange of money?)
####################################
# But only like 6 occurences out of 5*10^5, so perhaps not interesting?


####################################
#### ATM transaction?
####################################

#check for PUR94 and even 100 numbers.

#training data
evenPUR94 <- vector(mode = "numeric", length = dim(trainData)[1])
for (i in 1:length(month)) {
  ifelse(trainData$TRANSTYP_CODE[i] == 'PUR94' && trainData$TRANS_AMO[i]%%100 == 0, evenPUR94[i] <- 1, evenPUR94[i] <- 0) 
}
trainData$EVEN_WITHDRAWAL <- evenPUR94


#testing data
evenPUR94 <- vector(mode = "numeric", length = dim(testData)[1])
for (i in 1:length(month)) {
  ifelse(testData$TRANSTYP_CODE[i] == 'PUR94' && testData$TRANS_AMO[i]%%100 == 0, evenPUR94[i] <- 1, evenPUR94[i] <- 0) 
}
testData$EVEN_WITHDRAWAL <- evenPUR94


########################################
###### Check for phone numbers in city data
########################################

#training data
PHONE_PAYMENT <- matrix(0, nrow = dim(trainData)[1], ncol = 1)
for (i in 1:length(trainData$MRCH_CITY)) {
  if (substr(trainData$MRCH_CITY[i], 1, 1) == '+') PHONE_PAYMENT[i] <- 1
}
trainData$PHONE_PAYMENT <- PHONE_PAYMENT



#testing data
PHONE_PAYMENT <- matrix(0, nrow = dim(testData)[1], ncol = 1)
for (i in 1:length(testData$MRCH_CITY)) {
  if (substr(testData$MRCH_CITY[i], 1, 1) == '+') PHONE_PAYMENT[i] <- 1
}
testData$PHONE_PAYMENT <- PHONE_PAYMENT
##########################################
#### Build data set with only relevant variables
##########################################
#training data

trainData2 <- data.frame(trainData$KEYWORD, trainData$sincePayday, trainData$month, 
                    trainData$weekday, trainData$BIRTH_YEAR, trainData$SEX,
                    trainData$TRANS_AMO, trainData$TRANSTYP_CODE, trainData$IN_HOME_COUNTRY,
                    trainData$IN_HOME_TOWN, trainData$PHONE_PAYMENT, trainData$EVEN_WITHDRAWAL)
names(trainData2) <- c("KEYWORD", 'SINCE_PAY_DAY', 'MONTH', 'WEEKDAY', 
                       'BIRTH_YEAR', 'SEX', 'TRANS_AMO', 'TRANSTYP_CODE',
                       'IN_HOME_COUNTRY', 'IN_HOME_TOWN', 'PHONE_PAYMENT',
                       'EVEN_WITHDRAWAL')
#write.csv(trainData2, '/home/simon/Programming/evispotAIChallenge/data/training_data2.csv')

#testing data

testData2 <- data.frame(testData$KEYWORD, testData$sincePayday, testData$month, 
                         testData$weekday, testData$BIRTH_YEAR, testData$SEX,
                         testData$TRANS_AMO, testData$TRANSTYP_CODE, testData$IN_HOME_COUNTRY,
                         testData$IN_HOME_TOWN, testData$PHONE_PAYMENT, testData$EVEN_WITHDRAWAL)
names(testData2) <- c("KEYWORD", 'SINCE_PAY_DAY', 'MONTH', 'WEEKDAY', 
                       'BIRTH_YEAR', 'SEX', 'TRANS_AMO', 'TRANSTYP_CODE',
                       'IN_HOME_COUNTRY', 'IN_HOME_TOWN', 'PHONE_PAYMENT',
                      'EVEN_WITHDRAWAL')
#write.csv(testData2, '/home/simon/Programming/evispotAIChallenge/data/test_data2.csv')

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


#######################################
#### Naive bayes?
#######################################

library(e1071)
nb.object <- naiveBayes(KEYWORD~., data = trainData2)
nb.pred <- predict(nb.object, testData2)


#######################################
#### SOM
#######################################


#######################################
#### Clustering based methods. can we find clusters?
#######################################
