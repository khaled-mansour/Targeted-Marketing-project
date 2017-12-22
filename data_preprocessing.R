#################################################################################################################
############################################# 1. DATA PREPROCESSING AND VISUALIZATION ###########################
#################################################################################################################
#### 1. Importing data ( traindata is known , testdata is class) and Analysis #######
traindata <- read.csv("~/assignment_BADS_WS1617_known.csv",na.strings=c("NA","NaN",""," "))
testdata <- read.csv("~/assignment_BADS_WS1617_class.csv",na.strings=c("NA","NaN",""," "))

sapply(traindata, function(x) {sum(is.na(x))}) 
sapply(testdata, function(x) {sum(is.na(x))}) 

M <- as.data.frame(sapply(traindata, function(x) {sum(is.na(x))}))
#remark: treat those missing values

#Using package "Amelia" to visualize missing values Vs observed values
install.packages("Amelia")
library (Amelia)
missmap(traindata, main = "Missing values vs observed values in Train Data")
missmap(testdata, main = "Missing values vs observed values in Test Data")

summary(traindata)

str(traindata)
# remark: traindata is 80% of the whole observations (51884+12971), testdata will be 20% 
install.packages("lubridate")
library (lubridate)
install.packages("ggplot2")
library("ggplot2")
############################
##### 2. Data Cleaning #####
############################
# first step is creating the return_customer column in testdata in order to combine both datasets for cleaning
testdata$return_customer = NA 
comdata = rbind(traindata,testdata) 


# check missing values using same sapply function as before but on the new data
sapply(comdata, function(x) {sum(is.na(x))})
library (Amelia)
missmap(comdata, main = "Missing values vs observed values in Comdata")

##2.1.1wrong type variables##
# the below variables are in wrong type
#title 
#newsletter 
#model 
#delivery 
#coupon 
#goods_value 
#giftwrapping 
#referrer 
#points_redeemed 
#cost_shipping 
#return_customer 
# Judging from the Data Dictionary , these variables should be in factor type
# order_date and account_creation_date should be in date type.

#form_of_address 
table(comdata$form_of_address)
# There are missing values
# What are missing values similar to? Mr, Mrs, Company
# Change it to Unknown
head(comdata$form_of_address)
comdata$form_of_address = as.character(comdata$form_of_address) 
comdata[is.na(traindata$form_of_address),]$form_of_address = "Unknown"
comdata$form_of_address = as.factor(comdata$form_of_address)
table(comdata$form_of_address)
# Create subset Group
Unknown = subset(comdata,comdata$form_of_address=="Unknown")
Mr = subset(comdata,comdata$form_of_address=="Mr")
Mrs = subset(comdata,comdata$form_of_address=="Mrs")
Company = subset(comdata,comdata$form_of_address=="Company")
# finding out to which "form_of_address" is the most similar to
# (1.1) return customers
return_customer_foa= table(comdata$return_customer,comdata$form_of_address)
prop.table(return_customer_foa,2)
# (1.2) email_domain
email_domain_foa = table(comdata$email_domain,comdata$form_of_address)
prop.table(email_domain_foa,2)
# (1.3) Newsletter
newsletter_foa = table(comdata$newsletter,comdata$form_of_address)
prop.table(newsletter_foa,2)
# (1.4) payment
payment_foa = table(comdata$payment,comdata$form_of_address)
prop.table(payment_foa,2)
# (1.5) Coupon
coupon_foa = table(comdata$coupon,comdata$form_of_address)
prop.table(coupon_foa,2)
# (1.6) goods_value
goods_value_foa = table(comdata$goods_value,comdata$form_of_address)
prop.table(goods_value_foa,2)
### Result: It seems like Unknown share more similarity with Mr and Mrs, Not Company. distribute Mr and Mrs randomly
## Change it by tran and test data separately
head(testdata$form_of_address)
testdata$form_of_address = as.character(testdata$form_of_address) 
testdata[is.na(testdata$form_of_address),]$form_of_address = "Unknown"
testdata$form_of_address = as.factor(testdata$form_of_address)
table(testdata$form_of_address)

head(traindata$form_of_address)
traindata$form_of_address = as.character(traindata$form_of_address) 
traindata[is.na(traindata$form_of_address),]$form_of_address = "Unknown"
traindata$form_of_address = as.factor(traindata$form_of_address)
table(traindata$form_of_address)

Unknown_train = subset(traindata,traindata$form_of_address=="Unknown")
Unknown_train[1:4120,3] <- c("Mr")
Unknown_train[4121:6866,3] <- c("Mrs")
Unknown_test = subset(testdata,testdata$form_of_address=="Unknown")
Unknown_test[1:990,3] <- c("Mr")
Unknown_test[991:1650,3] <- c("Mrs")

testdata$form_of_address[testdata$form_of_address == "Unknown"] <- NA
testdata[is.na(testdata$form_of_address),]$form_of_address = Unknown_test$form_of_address
traindata$form_of_address[traindata$form_of_address == "Unknown"] <- NA
traindata[is.na(traindata$form_of_address),]$form_of_address = Unknown_train$form_of_address

comdata <- rbind(traindata,testdata)
comdata$form_of_address <- droplevels(comdata$form_of_address)
comdata$form_of_address = as.factor(comdata$form_of_address)
table(comdata$form_of_address)

# Modify type
comdata[,c(4,7,8,10,13,15,17,18,19,20,38)] <- lapply(comdata[,c(4,7,8,10,13,15,17,18,19,20,38)],as.factor)
str(comdata)
# modify order_date/account_creation_date/delivery_estimated and delivery_actual into "Date" type

comdata[,c("order_date","account_creation_date", "deliverydate_estimated", "deliverydate_actual")] <- lapply(comdata[,c("order_date","account_creation_date", "deliverydate_estimated", "deliverydate_actual")], as.Date, "%Y/%m/%d")




#time_interval_for_first_order = order_date - account_creation_date
comdata$time_interval_for_first_order = as.numeric(difftime(comdata$order_date, comdata$account_creation_date, units = "days"))
head(comdata$time_interval_for_first_order,5)
table(comdata$time_interval_for_first_order)
# 57925 observations take the value zero 
57925/nrow(comdata)
#89.3% of all orders are done in the same day as account creation day
# Replace missing values in account creation date by matching order date
comdata$account_creation_date = as.character(comdata$account_creation_date)
comdata$order_date = as.character(comdata$order_date)
index = which(is.na(comdata$account_creation_date))
comdata$account_creation_date[index] = comdata$order_date[index]
table(comdata$account_creation_date)
comdata$account_creation_date = as.Date(comdata$account_creation_date)
comdata$order_date = as.Date(comdata$order_date)
boxplot(comdata$order_date) 
table(comdata$order_date)


#title
comdata$title <- as.factor(comdata$title)
table(comdata$title)
ggplot(comdata, aes(x = title)) + geom_histogram(aes(y=..count../sum(..count..)), stat = "count", bins = 50)


#email_domain
table(comdata$email_domain)


#account_creation_date	
comdata$account_creation_date <- as.Date(comdata$account_creation_date)
hist(comdata$account_creation_date, breaks = 30)
ggplot(comdata, aes(x = account_creation_date)) + geom_histogram(aes(y = ..density..), bins = 50) + geom_density()
boxplot(comdata$account_creation_date) 
table(comdata$account_creation_date)

#newsletter
comdata$newsletter <- as.factor(comdata$newsletter)
table(comdata$newsletter)
ggplot(comdata, aes(x = newsletter)) + geom_histogram(aes(y=..count../sum(..count..)), stat = "count", bins = 50)


#model
comdata$model <- as.factor(comdata$model)
table(comdata$model)
ggplot(comdata, aes(x = model)) + geom_histogram(aes(y=..count../sum(..count..)), stat = "count", bins = 50)


#payment
comdata$payment= as.factor(comdata$payment)
table(comdata$payment)
ggplot(comdata, aes(x = payment)) + geom_histogram(aes(y=..count../sum(..count..)), stat = "count", bins = 50)

#delivery 
comdata$delivery= as.factor(comdata$delivery)
table(comdata$delivery)
ggplot(comdata, aes(x = delievery)) + geom_histogram(aes(y=..count../sum(..count..)), stat = "count", bins = 50)

#postcode_invoice
comdata$postcode_invoice[comdata$postcode_invoice=="??"]<-"44"
comdata$postcode_invoice[comdata$postcode_invoice=="0"]<-"44"
comdata$postcode_invoice[comdata$postcode_invoice=="1"]<-"01"
comdata$postcode_invoice[comdata$postcode_invoice=="2"]<-"02"
comdata$postcode_invoice[comdata$postcode_invoice=="3"]<-"03"
comdata$postcode_invoice[comdata$postcode_invoice=="4"]<-"04"
comdata$postcode_invoice[comdata$postcode_invoice=="6"]<-"06"
comdata$postcode_invoice[comdata$postcode_invoice=="7"]<-"07"
comdata$postcode_invoice[comdata$postcode_invoice=="8"]<-"08"
comdata$postcode_invoice[comdata$postcode_invoice=="9"]<-"09"
comdata$postcode_invoice <- as.factor(comdata$postcode_invoice)
comdata$postcode_invoice <- droplevels(comdata$postcode_invoice)
table(comdata$postcode_invoice)
ggplot(comdata, aes(x = postcode_invoice)) + geom_histogram(stat = "count", bins = 50) 
table(comdata$postcode_invoice)

#postcode_delivery
sum(is.na(comdata$postcode_delivery))/nrow(comdata)
#95% of the data in postcode_delivery is missing. A variable with excessive missing values can be deleted
comdata$postcode_delivery <- NULL

#coupon
comdata$coupon <- as.factor(comdata$coupon)
table(comdata$coupon)
ggplot(comdata, aes(x = coupon)) + geom_histogram(aes(y=..count../sum(..count..)), stat = "count", bins = 50)

#advertising_code
# make them into binary 0 <- no ad code / 1 <- with ad code
comdata$advertising_code=ifelse(is.na(comdata$advertising_code),0,1)
comdata$advertising_code <- as.factor(comdata$advertising_code)
table(comdata$advertising_code)
ggplot(comdata, aes(x = advertising_code)) + geom_histogram(aes(y=..count../sum(..count..)), stat = "count", bins = 50)

#goods_value
comdata$goods_value <- as.numeric(comdata$goods_value)
class(comdata$goods_value)
qplot(comdata$goods_value)
ggplot(comdata, aes(x = goods_value)) + geom_histogram(aes(y=..count../sum(..count..)), stat = "count", bins = 50)
table(comdata$goods_value)
# most goods value purchased are "4" with more than 25%


##giftwrapping
comdata$giftwrapping <- as.factor(comdata$giftwrapping)
table(comdata$giftwrapping)
ggplot(comdata, aes(x = giftwrapping)) + geom_histogram(aes(y=..count../sum(..count..)), stat = "count", bins = 50)

##referrer
comdata$giftwrapping <- as.factor(comdata$giftwrapping)
table(comdata$referrer)
ggplot(comdata, aes(x = referrer)) + geom_histogram(aes(y=..count../sum(..count..)), stat = "count", bins = 50)

##points_redeemed	Khaled
table(comdata$points_redeemed)
# All observations take the value 0 and therefore this variable has no effect and should be deleted
comdata$points_redeemed <- NULL
par(mar=c(1,1,1,1))

#cost_shipping
comdata$cost_shipping <- as.factor(comdata$cost_shipping)
table(comdata$cost_shipping)
ggplot(comdata, aes(x = cost_shipping)) + geom_histogram(aes(y=..count../sum(..count..)), stat = "count", bins = 50)

##deliverydate_estimated	Khaled
##deliverydate_actual	Khaled

#Once thes columns are modified into date type we can calculate the estimated delivery period , the actual delivery period and how long does it take from a customer to make his first order after creating the account
#1 estimated_delivery_duration = deliverydate_estimated - order_date
comdata$estimated_delivery_duration = difftime(comdata$deliverydate_estimated, comdata$order_date, units = "days")
head(comdata$estimated_delivery_duration)
class(comdata$estimated_delivery_duration)
# estimated_delivery_duration is in "difftime" type and need to be converted to numeric
comdata$estimated_delivery_duration <- as.numeric(comdata$estimated_delivery_duration)
class(comdata$estimated_delivery_duration)
#2 actual_delivery_duration = deliverydate_actual - order_date
comdata$actual_delivery_duration = as.numeric(difftime(comdata$deliverydate_actual, comdata$order_date, units = "days"))
head(comdata$actual_delivery_duration,3)
comdata$actual_delivery_duration <- as.numeric(comdata$actual_delivery_duration)
#3 time_interval_for_first_order = order_date - account_creation_date
comdata$time_interval_for_first_order = as.numeric(difftime(comdata$order_date, comdata$account_creation_date, units = "days"))
head(comdata$time_interval_for_first_order,5)
table(comdata$time_interval_for_first_order)
comdata$time_interval_for_first_order <- as.numeric(comdata$time_interval_for_first_order)

##2.1.2 Wrong Value Variables
#wrong value "2010/01/01" in deliverydate_estimated 
nbr_wrong_deliverydate_estimated = nrow(comdata[comdata$deliverydate_estimated == "2010/01/01",] )
print(nbr_wrong_deliverydate_estimated)
#23 observations are wrong
table(year(comdata$deliverydate_estimated))
#in total 73 wrong observations belonging to year "2010" and 14 wrong observations to year "4746"
#Change 2010 dates to 2014 since according order date in 2014 or end december 2013
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "2010/01/01"] = "2014/01/01"
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "2010/01/04"] = "2014/01/04"
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "2010/01/05"] = "2014/01/05"
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "2010/01/07"] = "2014/01/07"
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "2010/01/08"] = "2014/01/08"
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "2010/01/11"] = "2014/01/11"
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "2010/01/12"] = "2014/01/12"
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "2010/01/13"] = "2014/01/13"
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "2010/01/15"] = "2014/01/15"
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "2010/03/09"] = "2014/03/09"
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "2010/01/18"] = "2014/01/18"
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "2010/01/19"] = "2014/01/19"
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "2010/01/21"] = "2014/01/21"
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "2010/01/22"] = "2014/01/22"
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "2010/01/26"] = "2014/01/26"
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "2010/02/04"] = "2014/02/04"
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "2010/02/12"] = "2014/02/12"
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "2010/02/17"] = "2014/02/17"
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "2010/02/22"] = "2014/02/22"
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "2010/02/23"] = "2014/02/23"
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "2010/02/24"] = "2014/02/24"
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "2010/02/26"] = "2014/02/26"
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "2010/03/04"] = "2014/03/04"
table(year(comdata$deliverydate_estimated))
#4870 observations in 2014, 59971 in 2013 and 14 in "4746"
comdata$deliverydate_estimated[15658]
#Replace date values with year '4746' with NA
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "4746/07/23"] <- NA
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "4746/11/26"] <- NA
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "4746/06/13"] <- NA
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "4746/11/15"] <- NA
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "4746/05/08"] <- NA
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "4746/05/15"] <- NA
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "4746/11/15"] <- NA
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "4746/10/01"] <- NA
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "4746/11/18"] <- NA
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "4746/11/05"] <- NA
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "4746/10/24"] <- NA
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "4746/03/12"] <- NA
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "4746/06/25"] <- NA
comdata$deliverydate_estimated[comdata$deliverydate_estimated == "4746/07/03"] <- NA
comdata$deliverydate_estimated[15658]
table(year(comdata$deliverydate_estimated))
# calculate estimated delivery duration after correcting wrong value
comdata$estimated_delivery_duration = difftime(comdata$deliverydate_estimated, comdata$order_date, units = "days")
comdata$estimated_delivery_duration <- as.numeric(comdata$estimated_delivery_duration)
table(comdata$estimated_delivery_duration)
# check outliers 
library(ggplot2)
ggplot(comdata, aes(x = estimated_delivery_duration)) + geom_histogram(aes(y = ..density..), bins = 50) + geom_density()
hist(comdata$estimated_delivery_duration)
boxplot(comdata$estimated_delivery_duration)
summary(comdata$estimated_delivery_duration)
# some values are wrong in deliverydate_estimate since estimated delivery duration is more than 365 days which means more than a year
# number of wrong observations
nrow(comdata[comdata$estimated_delivery_duration > 365,])
#3544 wrong observations so replace wrong value in estimate delivery duration with NA
comdata$estimated_delivery_duration[comdata$estimated_delivery_duration>365] <- NA
table(comdata$estimated_delivery_duration)
sum(is.na(comdata$estimated_delivery_duration))/nrow(comdata)
#5% missing in estimated delivery estimation
#Finding the mode and median of estimated delivery
Mode <- function(x) {
  ux <- na.omit(unique(x) )
  tab <- tabulate(match(x, ux)); ux[tab == max(tab) ]
}
Mode(comdata$estimated_delivery_duration)
# 2 days is the mode thus adjusting NA values with the mode
medianWithoutNA<-function(x) {
  median(x[which(!is.na(x))])
}
medianWithoutNA(comdata$estimated_delivery_duration)
# median is equal to 2 days as well
# Replace missing values in estimated_delivery_duration with the median
comdata$estimated_delivery_duration[is.na(comdata$estimated_delivery_duration)] <- 2
summary(comdata$estimated_delivery_duration)
hist(comdata$estimated_delivery_duration)
#replace and adjust wrong deliverydate_estimated accordingly
comdata$deliverydate_estimated <- comdata$order_date + days(comdata$estimated_delivery_duration)
table(comdata$deliverydate_estimated)
## wrong variables in deliverydate_actual " 00/00/0000 " are considered as NAs since data is loaded
summary(comdata$deliverydate_actual)
sum(is.na(comdata$deliverydate_actual))
sum(is.na(comdata$deliverydate_actual))/nrow(comdata)
#10827 observation missing and this represents 16.7% of all observations
#Imputing missing values with median from actual_delivery_duration and then adjusting actual delivery dates 
# checking outliers 
ggplot(comdata, aes(x = actual_delivery_duration)) + geom_histogram(aes(y = ..density..), bins = 50) + geom_density()
hist(comdata$actual_delivery_duration)
boxplot(comdata$actual_delivery_duration)
summary(comdata$actual_delivery_duration)
# no outliers detected however 10937 actual delivery duration observations are more than 365 days 
nrow(comdata[comdata$actual_delivery_duration > 365,])
#Replace NA in actual_delivery_duration by 0
comdata$actual_delivery_duration[is.na(comdata$actual_delivery_duration)] <- 0
# it is assumed to be a typo error for duration more than 365 and therefore substract 365
for (i in 1:nrow(comdata)) {
  if (comdata$actual_delivery_duration[i] > 365) {
    comdata$actual_delivery_duration[i] <- comdata$actual_delivery_duration[i] - 365
  } 
}


summary(comdata$actual_delivery_duration)
# adjust NA in deliverydate_actual according to actual delivery duration
comdata$deliverydate_actual <- comdata$order_date + days(comdata$actual_delivery_duration)
table(comdata$deliverydate_actual)
plot(comdata$deliverydate_actual)
hist(comdata$deliverydate_actual, breaks = 30)

### statistical analysis
train <- comdata[c(1:51884),]
test <- comdata[-c(1:51884),]
deliv.return <- train[train$return_customer==1, "actual_delivery_duration"]
avg.return <-mean(deliv.return)
deliv.churn <- train[train$return_customer==0, "actual_delivery_duration"]
avg.churn <-mean(deliv.churn)
#box plot
boxplot(deliv.return, deliv.churn, 
        ylab="actual delivery duration",
        names=c("return","churn"),
        main="actual delivery distribution of orders")
# Check who is more probably to return
if (avg.return<avg.churn) {
  print("Returning customers have shorter delivery duration")
} else if (avg.return>avg.churn) {
  print("Returning customers have longer delivery duration")
} else {
  print("Actually, there is no difference in the delivery duration")  
}
# Work out the difference
sprintf("The difference between the average delivery duration of returning and non returning customers is %.2f",avg.churn-avg.return,2)
# Perform Welch test
test.result <-t.test(deliv.return,deliv.churn)
test.result 
if (test.result$p.value < 0.05) {
  print("Observed delivery difference is significant")
} else {
  print("Observed delivery difference is not significant")
}
## result show that actual delivery duration difference is not significance but importance of this variable will be more checked with WoE

##weight	
comdata$weight<- NULL

#order date 
#making order date into factors of 12 levels (considering only months and years, drop the date)
comdata$order_date <- as.Date(comdata$order_date)
comdata$order_date<- format(comdata$order_date, format="%m/%y")
comdata$order_date <- as.factor(comdata$order_date)
table(comdata$order_date)
ggplot(comdata, aes(x = order_date)) + geom_histogram(aes(y=..count../sum(..count..)), stat = "count", bins = 50)

##remitted_items

# Check for Outliers
ggplot(comdata, aes(x = remitted_items)) + geom_histogram(aes(y = ..density..), bins = 50) + geom_density()
boxplot(comdata$remitted_items)
summary(comdata$remitted_items)

# Truncate values above 1.5*IQR 
# Find the quartile values and the inter-quantile-range IQR
lower.quartile <- as.numeric(summary(comdata$remitted_items)[2])
upper.quartile <- as.numeric(summary(comdata$remitted_items)[5])
IQR <- upper.quartile - lower.quartile

# Calculate upper bound value
upper.bound <- upper.quartile + 1.5*IQR
message("Upper bound on remitted_items is ", upper.bound )

## In the result, it shows that no one remitted items, which makes bias in our outcome so we do not truncate outliers.
comdata$remitted_items <- as.numeric(comdata$remitted_items)


##canceled_items

# Check for Outliers
ggplot(comdata, aes(x = canceled_items)) + geom_histogram(aes(y = ..density..), bins = 50) + geom_density()
boxplot(comdata$canceled_items)
summary(comdata$canceled_items)

# Truncate values above 1.5*IQR 
# Find the quartile values and the inter-quantile-range IQR
lower.quartile <- as.numeric(summary(comdata$canceled_items)[2])
upper.quartile <- as.numeric(summary(comdata$canceled_items)[5])
IQR <- upper.quartile - lower.quartile

# Calculate upper bound value
upper.bound <- upper.quartile + 1.5*IQR
message("Upper bound on canceled_items is ", upper.bound )

## In the result, it shows that no one canceled items, which makes bias in our outcome so we do not truncate outliers.

comdata$canceled_items <- as.numeric(comdata$canceled_items)

##used_items

# Check for Outliers
ggplot(comdata, aes(x = used_items)) + geom_histogram(aes(y = ..density..), bins = 50) + geom_density()
boxplot(comdata$used_items)
summary(comdata$used_items)

# Truncate values above 1.5*IQR 
# Find the quartile values and the inter-quantile-range IQR
lower.quartile <- as.numeric(summary(comdata$used_items)[2])
upper.quartile <- as.numeric(summary(comdata$used_items)[5])
IQR <- upper.quartile - lower.quartile

# Calculate upper bound value
upper.bound <- upper.quartile + 1.5*IQR
message("Upper bound on used_items is ", upper.bound )

## In the result, it shows that no one used_items, which makes bias in our outcome so we do not truncate outliers.

comdata$used_items <- as.numeric(comdata$used_items)

##allbook_count
#Group item books because they are similar characteristic (books, real delivery): book_count, schoolbook_count, audiobook_count, paperback_count
comdata$allbook_count=rowSums(comdata[,c("book_count","schoolbook_count","audiobook_count","paperback_count")],na.rm=TRUE)
summary(comdata$allbook_count)

# Outliers in allbook_count

# Check for Outliers
ggplot(comdata, aes(x = allbook_count)) + geom_histogram(aes(y = ..density..), bins = 50) + geom_density()
boxplot(comdata$allbook_count)
summary(comdata$allbook_count)

# Truncate values above 1.5*IQR 
# Find the quartile values and the inter-quantile-range IQR
lower.quartile <- as.numeric(summary(comdata$allbook_count)[2])
upper.quartile <- as.numeric(summary(comdata$allbook_count)[5])
IQR <- upper.quartile - lower.quartile

# Calculate upper bound value
upper.bound <- upper.quartile + 1.5*IQR
message("Upper bound on allbook count is ", upper.bound )

# logical indexing to identify outliers and replace  with upper bound
comdata$allbook_count[comdata$allbook_count > upper.bound ] <- upper.bound
table(comdata$allbook_count)

comdata$allbook_count <- as.numeric(comdata$allbook_count)

##downloadable_count
# Group items that are downloadable: ebook_count, audiobook_download_count, film_count, musical_count
comdata$downloadable_count=rowSums(comdata[,c("ebook_count", "audiobook_download_count", "film_count", "musical_count")],na.rm=TRUE)
summary(comdata$downloadable_count)

comdata$downloadable_count <- as.numeric(comdata$downloadable_count)

##hardware_count	
summary(comdata$hardware_count)

comdata$hardware_count <- as.numeric(comdata$hardware_count)

##imported_count	
summary(comdata$imported_count)

comdata$imported_count <- as.numeric(comdata$imported_count)

##other_count	
summary(comdata$other_count)

comdata$other_count <- as.numeric(comdata$other_count)

## Columns that should be deleted because they are grouped already
#book_count
comdata$book_count<- NULL
#paperback_count
comdata$paperback_count<- NULL
#schoolbook_count	
comdata$schoolbook_count	<- NULL
#ebook_count	
comdata$ebook_count	<- NULL
#audiobook_count	
comdata$audiobook_count<- NULL
#audiobook_download_count
comdata$audiobook_download_count<- NULL
#film_count	
comdata$film_count<- NULL
#musical_count	
comdata$musical_count	<- NULL

# Deleted Variables
comdata$deliverydate_estimated <- NULL
comdata$deliverydate_actual <- NULL
comdata$item_count <-NULL
comdata$account_creation_date <-NULL
comdata$postcode_delivery <- NULL


##total_item_count
comdata[,c("allbook_count","downloadable_count","hardware_count","imported_count","other_count")] <- lapply(comdata[,c("allbook_count","downloadable_count","hardware_count","imported_count","other_count")],as.numeric)
comdata$total_item_count=rowSums(comdata[,c("allbook_count","downloadable_count","hardware_count","imported_count","other_count")],na.rm=TRUE)
summary(comdata$total_item_count)
comdata$total_item_count <- as.numeric(comdata$total_item_count)

#All the numeric variables statistical analysis
ggplot(comdata, aes(x = total_item_count)) + geom_histogram(aes(y = ..density..), bins = 50) + geom_density()
hist(comdata$total_item_count)
boxplot(comdata$total_item_count)

ggplot(comdata, aes(x = actual_delivery_duration)) + geom_histogram(aes(y = ..density..), bins = 50) + geom_density()
hist(comdata$actual_delivery_duration)
boxplot(comdata$actual_delivery_duration)

ggplot(comdata, aes(x = estimated_delivery_duration)) + geom_histogram(aes(y = ..density..), bins = 50) + geom_density()
hist(comdata$estimated_delivery_duration)
boxplot(comdata$estimated_delivery_duration)

ggplot(comdata, aes(x = time_interval_for_first_order)) + geom_histogram(aes(y = ..density..), bins = 50) + geom_density()
hist(comdata$time_interval_for_first_order)
boxplot(comdata$time_interval_for_first_order)

ggplot(comdata, aes(x = other_count)) + geom_histogram(aes(y = ..density..), bins = 50) + geom_density()
hist(comdata$other_count)
boxplot(comdata$other_count)

ggplot(comdata, aes(x = imported_count)) + geom_histogram(aes(y = ..density..), bins = 50) + geom_density()
hist(comdata$imported_count)
boxplot(comdata$imported_count)

ggplot(comdata, aes(x = hardware_count)) + geom_histogram(aes(y = ..density..), bins = 50) + geom_density()
hist(comdata$hardware_count)
boxplot(comdata$hardware_count)

ggplot(comdata, aes(x = downloadable_count)) + geom_histogram(aes(y = ..density..), bins = 50) + geom_density()
hist(comdata$downloadable_count)
boxplot(comdata$downloadable_count)

ggplot(comdata, aes(x = allbook_count)) + geom_histogram(aes(y = ..density..), bins = 50) + geom_density()
hist(comdata$allbook_count)
boxplot(comdata$allbook_count)

ggplot(comdata, aes(x = used_items)) + geom_histogram(aes(y = ..density..), bins = 50) + geom_density()
hist(comdata$used_items)
boxplot(comdata$used_items)

ggplot(comdata, aes(x = canceled_items)) + geom_histogram(aes(y = ..density..), bins = 50) + geom_density()
hist(comdata$canceled_items)
boxplot(comdata$canceled_items)

ggplot(comdata, aes(x = remitted_items)) + geom_histogram(aes(y = ..density..), bins = 50) + geom_density()
hist(comdata$remitted_items)
boxplot(comdata$remitted_items)

ggplot(comdata, aes(x = goods_value)) + geom_histogram(aes(y = ..density..), bins = 50) + geom_density()
hist(comdata$goods_value)
boxplot(comdata$goods_value)

####Save the cleaned version of final data again
write.csv(comdata,file='~/Desktop/clean_comdata.csv')
comdata <- read.csv("/Users/MARZOUK/Desktop/clean_comdata.csv",na.strings=c("NA","NaN",""," "))
