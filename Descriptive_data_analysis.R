######################################################################
################# Descirptive data analysis  #########################
######################################################################
  #check whether imported_count is a critical variable
  m = table(traindata$return_customer,traindata$imported_count)
m2 <- m
m2[] <- sprintf("%.0f%%",round(prop.table(m,2)*100, 3))
m2
  repurchase = factor(traindata$return_customer)
install.packages('qplot')
library(qplot)
k = qplot(x =factor(traindata$form_of_address),
          y=traindata$imported_count, 
          data=traindata,
          geom=c("jitter"),
          color=repurchase)
k+xlab("Form of Address")+ylab("Imported Count")
#most imported goods are purchased by company

repurchase = factor(traindata$return_customer)
k = qplot(x =factor(traindata$goods_value),
          y=traindata$imported_count, 
          data=traindata,
          geom=c("jitter"),
          color=repurchase)
k+xlab("Goods Value")+ylab("Imported Count")
#most imported goods are expensive

repurchase = factor(traindata$return_customer)
k = qplot(x =factor(traindata$canceled_items),
          y=traindata$imported_count, 
          data=traindata,
          geom=c("jitter"),
          color=repurchase)
k+xlab("Canceled Items")+ylab("Imported Count")
# not much pattern

#check whether cost_shipping is a critical variable
m = table(traindata$return_customer,traindata$cost_shipping)
m2 <- m
m2[] <- sprintf("%.0f%%",round(prop.table(m,2)*100, 3))
m2

# check whether giftwrapping is a critical variable
m = table(traindata$return_customer,traindata$giftwrapping)
m2 <- m
m2[] <- sprintf("%.0f%%",round(prop.table(m,2)*100, 3))
m2

#check whether goods_value is a critical variable
m = table(traindata$return_customer,traindata$goods_value)
m2 <- m
m2[] <- sprintf("%.0f%%",round(prop.table(m,2)*100, 3))
m2

#check whether referrer is a critical variable
m = table(traindata$return_customer,traindata$referrer)
m2 <- m
m2[] <- sprintf("%.0f%%",round(prop.table(m,2)*100, 3))
m2

# check whether model is a critical variable
m = table(traindata$return_customer,traindata$model)
m2 <- m
m2[] <- sprintf("%.0f%%",round(prop.table(m,2)*100, 3))
m2

#higher model higher repurchase

# check whether email_domain is a critical variable
m = table(traindata$return_customer,traindata$email_domain)
m2 <- m
m2[] <- sprintf("%.0f%%",round(prop.table(m,2)*100, 3))
m2

#results:   not much pattern
#business of e-comerence--- sell book/music  
plot(colSums(comdata[,as.numeric(20:22&27&28)]),xlab=names(comdata[,as.numeric(20:22&27&28)]))
#sell books, paperback,schoolbook,audiobook(suppose no deliver),impoted_count
summary(comdata)
prop.table(table(traindata$return_customer,traindata$form_of_address),2)


#business of e-comerence--- B2B(9% in all customers)&B2C(91% in all customers)
#B2C: 55% Mr and 36% Mrs(na ignored)
m = table(comdata$form_of_address)
m2 <- m
m2[] <- sprintf("%.0f%%",round(prop.table(m)*100, 3))
m2


round(sum(Mr[,27:37])/(sum(Mr[,27:37])+sum(Mrs[,27:37])+sum(Company[,27:37])+sum(Unknown[,27:37])),2)
round(sum(Mrs[,27:37])/(sum(Mr[,27:37])+sum(Mrs[,27:37])+sum(Company[,27:37])+sum(Unknown[,27:37])),2)
round(sum(Company[,27:37])/(sum(Mr[,27:37])+sum(Mrs[,27:37])+sum(Company[,27:37])+sum(Unknown[,27:37])),2)
round(sum(Unknown[,27:37])/(sum(Mr[,27:37])+sum(Mrs[,27:37])+sum(Company[,27:37])+sum(Unknown[,27:37])),2)

#B2B:9%
#B2C: 50% Mr, 28% Mrs, 9% Company, 28% Unknown
#these 28% are either Mr or Mrs, but do not matter that much in prediciting

#because:  repurchase by add_form (na. ignored)
#           Company    Mr       Mrs
#      0 0.8614972 0.8190603 0.8244053
#      1 0.1385028 0.1809397 0.1755947

str(data)
install.packages('ggplot2')
library(ggplot2)
#form_of_address and item_account
k = qplot(x =factor(traindata$form_of_address),
          y=traindata$item_count, 
          data=traindata,
          geom=c("jitter"),
          color=factor(traindata$return_customer))
k

#model and weight
k = qplot(x =factor(traindata$model),
          y=traindata$weight, 
          data=traindata,
          geom=c("jitter"),
          color=factor(traindata$return_customer))
k

#form_of_address and weight
k = qplot(x =factor(traindata$form_of_address),
          y=traindata$weight, 
          data=traindata,
          geom=c("jitter"),
          color=factor(traindata$return_customer))
k

###################################################################################
## cluster analysis (K-means)
###################################################################################

traindata <- comdata[c(1:51884),]

# cluster numeric variables: "allbook_count", "downloadable_count", "other_count", "hardware_count", "imported_count" into 5 groups
cluster.object <- kmeans(traindata[,c("allbook_count", "downloadable_count", "other_count", "hardware_count", "imported_count")], centers = 5, iter.max = 50, nstart = 25)

# See the Result
str(cluster.object)
cluster.object
cluster.object$cluster
cluster.object$centers
cluster.object$totss
cluster.object$withinss
cluster.object$tot.withinss
cluster.object$betweenss
cluster.object$size
cluster.object$iter
cluster.object$ifault

# See the results in relation to dependent variable (ruturn_customer)
table(traindata$return_customer, cluster.object$cluster)

#Plot the result
plot(traindata[c("allbook_count", "downloadable_count", "other_count", "hardware_count", "imported_count")], col=cluster.object$cluster)
plot(traindata[c("allbook_count", "downloadable_count", "other_count", "hardware_count", "imported_count")], col=traindata$return_customer)


###################################################################################
## Correlation between all variables
###################################################################################

traindata <- comdata[c(1:51884),]
# Check which variables in data frame are numeric
lapply(traindata, class)
traindata_numeric<-sapply(traindata,is.numeric)

# Correlation of them
traindata_correlation<-cor(traindata[,traindata_numeric])
print(traindata_correlation)

# Visualize with package "corrplot"
install.packages("corrplot")
library("corrplot")
corrplot(traindata_correlation)

###################################################################################
## data visualization for all variables 
###################################################################################
#using Hmisc
library(Hmisc)
hist.data.frame(comdata)
#Using Psych
install.packages("psych")
library(psych)
multi.hist(comdata[,sapply(comdata, is.numeric)])
#Using ggplot
d <- melt(comdata)
ggplot(d,aes(x = value), bins = 50) + facet_wrap(~variable,scales = "free_x") + geom_histogram()
