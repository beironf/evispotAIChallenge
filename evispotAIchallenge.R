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
trainData$MRCH_CITY <- gsub("å|ä|ö|Å|Ä|Ö|A|E|O|a|e|o| ","", as.character(trainData$MRCH_CITY))
trainData$MRCH_CITY[which(is.na(trainData$MRCH_CITY))] <- "NO_TOWN"
trainData$MRCH_CITY <- as.factor(sapply(trainData$MRCH_CITY, toupper))

#############################
#### Fix missing values in SEX, CITY
##############################
majoritySex <- MaxTable(trainData$SEX)
meanBirthYear <- round(mean(trainData$BIRTH_YEAR[!is.na(trainData$BIRTH_YEAR)]))
trainData$SEX[which(is.na(trainData$SEX))] <- majoritySex
trainData$BIRTH_YEAR[which(is.na(trainData$BIRTH_YEAR))] <- meanBirthYear


################################
#Check hometown/homecountry (majority of purchases made there) for each person
################################
#Also updates "NO_TOWN" to hometown (best guess for missing value)

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


##############################
### What weekday?
##############################


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
trainData <- cbind(sincePayday, trainData)


####################################
#### Any PUR96 within last week? (exchange of money?)
####################################
# But only like 6 occurences out of 5*10^5, so perhaps not interesting?


#####################################
#### ATM transaction?
####################################

#check for PUR94 and even 100 numbers.

temp <- as.numeric(levels(trainData$TRANS_AMO))[trainData$TRANS_AMO]
evenPUR94 <- vector(mode = "logical", length = length(month))
for (i in 1:length(month)) {
  ifelse(trainData$TRANSTYP_CODE[i] == 'PUR94' && temp[i]%%100 == 0, evenPUR94[i] <- TRUE, evenPUR94[i] <- FALSE) 
}
trainData <- cbind(evenPUR94, trainData)


########################################
###### Check for phone numbers in city data
########################################
PHONE_PAYMENT <- matrix(0, nrow = dim(trainData)[1], ncol = 1)
for (i in 1:length(trainData$MRCH_CITY)) {
  if (substr(trainData$MRCH_CITY[i], 1, 1) == '+') PHONE_PAYMENT[i] <- 1
}

##########################################
#### Build data set with only relevant variables
##########################################

trainData2 <- data.frame(trainData$KEYWORD, trainData$sincePayday, trainData$month, 
                    trainData$weekday, trainData$BIRTH_YEAR, trainData$SEX,
                    as.numeric(levels(trainData$TRANS_AMO))[trainData$TRANS_AMO], trainData$TRANSTYP_CODE, trainData$IN_HOME_COUNTRY,
                    trainData$IN_HOME_TOWN)
names(trainData2) <- c("KEYWORD", 'SINCE_PAY_DAY', 'MONTH', 'WEEKDAY', 
                       'BIRTH_YEAR', 'SEX', 'TRANS_AMO', 'TRANSTYP_CODE',
                       'IN_HOME_COUNTRY', 'IN_HOME_TOWN')

######################################
#### Predict with random forest
######################################

library(randomForest)
rf <- randomForest(KEYWORD ~ . , data = trainData2)
