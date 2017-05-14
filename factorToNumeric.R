#Fix training data to numeric

keyword_numeric <- vector(mode = 'numeric', length = dim(trainData2)[1])
count = 1
KEYWORDVEC <- c("Restaurants_bars_cafes", "internet_telephony", "grocery_store", 
                "health", "Electronics", "clothing_accessories", "hobbies", "traveling",
                "transportation_gas_parking", "home", "alcohol", "atm", "casino", "other")

for(keyword in KEYWORDVEC){
  print(count)
  print(keyword)
  keyword_numeric[which(trainData2$KEYWORD == keyword)] <- count
  count <- count + 1
}

#weekday_numeric <- vector(mode = 'numeric', length = dim(trainData2)[1])
#count = 1
#for(weekday in c('monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday')){
#  print(count)
#  print(weekday)
#  weekday_numeric[which(trainData2$WEEKDAY == weekday)] <- count
#  count <- count + 1
#}

sex_numeric <- vector(mode = 'numeric', length = dim(trainData2)[1])
sex_numeric[which(trainData2$SEX == 'MAN')] <- 0
sex_numeric[which(trainData2$SEX == 'KVINNA')] <- 1

#transtyp_code <- vector(mode = 'numeric', length = dim(trainData2)[1])
#count = 1
#for(transtyp in c('PUR90', 'PUR91', 'PUR92', 'PUR93', 'PUR94', 'PUR96')){
#  print(count)
#  print(transtyp)
#  transtyp_code[which(trainData2$TRANSTYP_CODE == transtyp)] <- count
#  count <- count + 1
#}

trainData3 <- trainData2
trainData3$SEX <- sex_numeric
#trainData3$WEEKDAY <- weekday_numeric
#trainData3$KEYWORD <- keyword_numeric
#trainData3$TRANSTYP_CODE <- transtyp_code


#Fix test data to numeric

keyword_numeric <- vector(mode = 'numeric', length = dim(testData2)[1])
count = 1
for(keyword in KEYWORDVEC){
  print(count)
  print(keyword)
  keyword_numeric[which(testData2$KEYWORD == keyword)] <- count
  count <- count + 1
}

#weekday_numeric <- vector(mode = 'numeric', length = dim(testData2)[1])
#count = 1
#for(weekday in c('monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday')){
#  print(count)
#  print(weekday)
#  weekday_numeric[which(testData2$WEEKDAY == weekday)] <- count
#  count <- count + 1
#}

sex_numeric <- vector(mode = 'numeric', length = dim(testData2)[1])
sex_numeric[which(testData2$SEX == 'MAN')] <- 0
sex_numeric[which(testData2$SEX == 'KVINNA')] <- 1

#transtyp_code <- vector(mode = 'numeric', length = dim(testData2)[1])
#count = 1
#for(transtyp in c('PUR90', 'PUR91', 'PUR92', 'PUR93', 'PUR94', 'PUR96')){
#  print(count)
#  print(transtyp)
#  transtyp_code[which(testData2$TRANSTYP_CODE == transtyp)] <- count
#  count <- count + 1
#}

testData3 <- testData2
testData3$SEX <- sex_numeric
#testData3$WEEKDAY <- weekday_numeric
#testData3$KEYWORD <- keyword_numeric
#testData3$TRANSTYP_CODE <- transtyp_code

write.csv(trainData3, '/home/simon/Programming/evispotAIChallenge/data/training_data3.csv')
write.csv(testData3, '/home/simon/Programming/evispotAIChallenge/data/test_data3.csv')
