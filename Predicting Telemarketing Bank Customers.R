######### Predicting Telemarketing Bank Customers ################
##################### James To ###################################

#Package installation
install.packages("tidyr")
install.packages("ggplot2")
install.packages("mice")
install.packages("DMwR")
install.packages("caret")
install.packages("C50")
install.packages("pROC")
install.packages("e1071")
install.packages("glmnet")
install.packages("rbind")

#Reading data
train <- read.csv("Marketing_Train.csv",
                  sep=",", header=TRUE)
summary(train)

#Make into df
train <- as.data.frame(train)

#Making unknowns into NA
train[train == "unknown"] <- NA
sum(is.na(train$default))

#Making data frame into numeric
numtrain <- transform(train, id = as.numeric(id),
                      age = as.numeric(age),
                      job = as.numeric(job), 
                      marital = as.numeric(marital),
                      education = as.numeric(education),
                      default = as.numeric(default),
                      housing = as.numeric(housing),
                      loan = as.numeric(loan),
                      contact = as.numeric(contact),
                      year = as.numeric(year),
                      month = as.numeric(month),
                      day_of_week = as.numeric(day_of_week),
                      campaign = as.numeric(campaign),
                      pdays = as.numeric(pdays),
                      previous = as.numeric(previous),
                      poutcome = as.numeric(poutcome),
                      emp.var.rate = as.numeric(emp.var.rate),
                      cons.price.idx = as.numeric(cons.price.idx),
                      cons.conf.idx = as.numeric(cons.conf.idx),
                      euribor3m = as.numeric(euribor3m),
                      nr.employed = as.numeric(nr.employed),
                      y = as.numeric(y))

cortrain <- cor(numtrain, use = "pairwise.complete.obs")

#Histograms
library(tidyr)
library(ggplot2)
numtrain %>% gather() %>% head()
ggplot(gather(numtrain), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

#Remove ID, and default because too many unknowns
train <- train[-c(1,6)]

#Remove 2008
train <- train[!train$year == "2008",]

#Impute data first then make dummycodes
library(mice)
imptrain <- mice(train, m=1, maxit=20, 
                 seed = 500, method="pmm")

comptrain <- mice::complete(imptrain, 1)

#DummyCodes for all
unique(comptrain$job) 
comptrain[,"job1"] <- ifelse(comptrain$job == "admin.", 1, 0)
comptrain[,"job2"] <- ifelse(comptrain$job == "blue-collar", 1, 0)
comptrain[,"job3"] <- ifelse(comptrain$job == "technician", 1, 0)
comptrain[,"job4"] <- ifelse(comptrain$job == "services", 1, 0)
comptrain[,"job5"] <- ifelse(comptrain$job == "management", 1, 0)
comptrain[,"job6"] <- ifelse(comptrain$job == "retired", 1, 0)
comptrain[,"job7"] <- ifelse(comptrain$job == "unemployed", 1, 0)
comptrain[,"job8"] <- ifelse(comptrain$job == "self-employed", 1, 0)
comptrain[,"job9"] <- ifelse(comptrain$job == "entrepreneur", 1, 0)
comptrain[,"job10"] <- ifelse(comptrain$job == "student", 1, 0)
comptrain[,"job11"] <- ifelse(comptrain$job == "housemaid", 1, 0)

unique(comptrain$marital) 
comptrain[,"marital1"] <- ifelse(comptrain$marital == "divorced", 1,0)
comptrain[,"marital2"] <- ifelse(comptrain$marital == "married", 1,0)
comptrain[,"marital3"] <- ifelse(comptrain$marital == "single", 1,0)

unique(comptrain$education) 
comptrain[,"education1"] <- ifelse(comptrain$education == "university.degree", 1, 0)
comptrain[,"education2"] <- ifelse(comptrain$education == "high.school", 1, 0)
comptrain[,"education3"] <- ifelse(comptrain$education == "basic.9y", 1, 0)
comptrain[,"education4"] <- ifelse(comptrain$education == "professional.course", 1, 0)
comptrain[,"education5"] <- ifelse(comptrain$education == "basic.4y", 1, 0)
comptrain[,"education6"] <- ifelse(comptrain$education == "basic.6y", 1, 0)
comptrain[,"education7"] <- ifelse(comptrain$education == "illiterate", 1, 0)

unique(comptrain$year)
comptrain[,"year1"] <- ifelse(comptrain$year == "2009", 1, 0)
comptrain[,"year2"] <- ifelse(comptrain$year == "2010", 1, 0)

unique(comptrain$housing)
comptrain[,"housing1"] <- ifelse(comptrain$housing == "yes", 1, 0) #binary yes no only need yes = 1

unique(comptrain$loan)
comptrain[,"loan1"] <- ifelse(comptrain$loan == "yes", 1, 0)

unique(comptrain$contact)
comptrain[,"contact1"] <- ifelse(comptrain$contact == "telephone", 1, 0)
comptrain[,"contact2"] <- ifelse(comptrain$contact == "cellular", 1, 0)

unique(comptrain$month)
comptrain[,"month1"] <- ifelse(comptrain$month == "may", 1,0)
comptrain[,"month2"] <- ifelse(comptrain$month == "jun", 1,0)
comptrain[,"month3"] <- ifelse(comptrain$month == "jul", 1,0)
comptrain[,"month4"] <- ifelse(comptrain$month == "aug", 1,0)
comptrain[,"month5"] <- ifelse(comptrain$month == "sep", 1,0)
comptrain[,"month6"] <- ifelse(comptrain$month == "oct", 1,0)
comptrain[,"month7"] <- ifelse(comptrain$month == "nov", 1,0)
comptrain[,"month8"] <- ifelse(comptrain$month == "dec", 1,0)
comptrain[,"month9"] <- ifelse(comptrain$month == "mar", 1,0)
comptrain[,"month10"] <- ifelse(comptrain$month == "apr", 1,0)

unique(comptrain$day_of_week)
comptrain[,"day1"] <- ifelse(comptrain$day_of_week == "mon", 1, 0)
comptrain[,"day2"] <- ifelse(comptrain$day_of_week == "tue", 1, 0)
comptrain[,"day3"] <- ifelse(comptrain$day_of_week == "wed", 1, 0)
comptrain[,"day4"] <- ifelse(comptrain$day_of_week == "thu", 1, 0)
comptrain[,"day5"] <- ifelse(comptrain$day_of_week == "fri", 1, 0)

unique(comptrain$poutcome)
comptrain[,"poutcome1"] <- ifelse(comptrain$poutcome == "nonexistent", 1, 0)
comptrain[,"poutcome2"] <- ifelse(comptrain$poutcome == "failure", 1, 0)
comptrain[,"poutcome3"] <- ifelse(comptrain$poutcome == "success", 1, 0)

comptrain[,"y1"] <- factor(ifelse(comptrain$y == "yes", 1, 0))

View(comptrain)

#name the dum var is now clean train
cleantrain <- comptrain[-c(2:10,14,20)]
summary(cleantrain)

#Telemarketing is dealing with human emotions so use a high partition
#Data partition

#Organic method
ind <- sample(2, nrow(cleantrain), replace=TRUE, prob = c(0.8, 0.2))
trtrain <- cleantrain[ind==1,]
tetrain <- cleantrain[ind==2,]

#SMOTE
library(DMwR)
upsamtrain <- SMOTE(y1~., data = trtrain, k = 10, perc.over = 400, perc.under = 125)
typeof(upsamtrain)
summary(as.numeric(upsamtrain$y1))
View(upsamtrain)

#### MODELS
# Decision Trees
library(C50)
dtfit <- C5.0(upsamtrain[1:54], upsamtrain$y1)
dtfit

summary(dtfit)
plot(dtfit)

dtpred <- predict(dtfit, tetrain)

table(dtpred,tetrain$y1)
prop.table(table(dtpred,tetrain$y1))

1-mean(dtpred != tetrain$y1) #Accuracy

library(pROC)
dtauc <- roc(as.numeric(tetrain$y1), 
             as.numeric(dtpred))
dtauc
# Heavily biased so accuracy will always be high

trialsdtfit <- C5.0(upsamtrain[-c(54)], upsamtrain$y1, trials=30)
trialsdtfit

trialsdtpred <- predict(trialsdtfit, tetrain)

trialsdtauc <- roc(as.numeric(tetrain$y1), 
                   as.numeric(trialsdtpred))
trialsdtauc

# Too many false-positives
# Marketing strategy and you don't want over ambitious claims

#Logistic Regression
cols <- names(upsamtrain)
form <- cols[!cols%in%"y1"]
form <- paste(form, collapse = '+')
form <- paste('y1~', form)

lrfit <- glm(formula = form, data=upsamtrain, family = binomial)
# lrfit <- step(lrfit, direction = "backward")

lrcoef <- as.data.frame(t(lrfit$coefficients))
summary(lrcoef)

lrprob <- predict(lrfit, tetrain, type = "response")
lrpred <- ifelse(lrprob > 0.5, 1,0)

table(lrpred, tetrain$y1)

1-mean(lrpred != tetrain$y1)

lrauc <- roc(as.numeric(tetrain$y1), 
             as.numeric(lrpred))
lrauc

# Logistic Regression with CV
# Using the Caret package

library(caret)
cvControl <- trainControl(method="repeatedcv", number=5, repeats = 5)

lr2fit <- train(y1 ~ .,
                method="glm",
                data= upsamtrain,
                family=binomial,
                trControl=cvControl)
lr2pred <- predict(lr2fit, newdata= tetrain, type="raw")

table(lr2pred, tetrain$y1) #Same result

1-mean(lr2pred != tetrain$y1)

lr2auc <- roc(as.numeric(tetrain$y1), 
              as.numeric(lr2pred))
lr2auc

# Removing bad predictors
library(caret)
lrvarimp <- varImp(lrfit)
ordlrvarimp <- lrvarimp[order(lrvarimp$Overall), , drop=FALSE]
ordlrvarimp
plot(lrvarimp)

colnames(upsamtrain)

library(caret)
lrRM1fit <- lr2fit <- train(y1 ~ age + campaign + pdays + previous + emp.var.rate + cons.price.idx +
                              cons.conf.idx + euribor3m + nr.employed + job1 + job2 + job3 + job4 + job5 + job6          
                            + job8  +  job9 + job11 + marital1 + marital2 + marital3 + education1   
                            + education2 + education3 + education4 + education6  +   education7 + year1 + year2  
                            + loan1 + contact1 +  contact2 + month1 + month2+  month3 + month4 + month5 + month6+ month7 + month8
                            + month9 + month10  + day2 + day3  + day4 +  day5          
                            + poutcome1 + poutcome2 + poutcome3,
                            data = upsamtrain, method="glm")
View(upsamtrain)

lrRM1pred <- predict(lrRM1fit, newdata= tetrain, type="raw")

table(lrRM1pred, tetrain$y1)

1-mean(lrRM1pred != tetrain$y1)

library(pROC)
lrRM1auc <- roc(as.numeric(tetrain$y1), 
                as.numeric(lrRM1pred))
lrRM1auc

# FEATURE SELECTION

upsamtrain[] <- lapply(upsamtrain, function(x) as.numeric(as.character(x)))


# Support Vector Machine Linear
library(e1071)
svmLfit <- svm(y1 ~., kernel = "linear", 
               data = upsamtrain)
svmLpred <- predict(svmLfit, tetrain)
svmLrep <-  rep(0, length(svmLpred))
svmLrep[svmLpred > 0.5] <- 1

table(svmLrep, tetrain$y1)
prop.table(table(svmLrep, tetrain$y1))

1-mean(svmLrep != tetrain$y1)

svmLauc <- roc(as.numeric(tetrain$y1), 
               as.numeric(svmLrep)) #AUC
svmLauc

# Support Vectore Machine Radial

svmRfit <- svm(y1 ~., kernel = "radial", 
               data = upsamtrain,probability = TRUE)
svmRpred <- predict(svmRfit,tetrain)
svmRrep <-  rep(0, length(svmRpred))
svmRrep[svmRpred > 0.5] <- 1

table(svmRrep, tetrain$y1)
prop.table(table(svmRrep, tetrain$y1))

svmRauc <- roc(as.numeric(tetrain$y1), 
               as.numeric(svmRrep))
svmRauc

1-mean(svmRrep != tetrain$y1)

# gbm
# upSamptrain.tr$Class<-ifelse(upSamptrain.tr$Class==0,'no','yes')
# upSamptrain.tr$Class<-as.factor(upSamptrain.tr$Class)

library(caret)

upsamtrain[] <- lapply(upsamtrain, function(x) as.numeric(as.character(x)))
notlisttrain <- as.data.frame(matrix(unlist(upsamtrain), nrow=length(upsamtrain), byrow=T))
typeof(notlisttrain)
Class <- notlisttrain$y
splitnotlisttrain<- notlisttrain[-c(55)]

gbmControl <- trainControl(method='cv', number=5, returnResamp='none',classProbs = TRUE)

upSamptrain.tr$Class <- as.numeric(as.character(upSamptrain.tr))

gbmfit <- train(Class~., data=splitnotlisttrain,
                method="gbm",
                trControl = gbmControl,
                metric="ROC",
                preProc = c("BoxCox"),na.action=na.pass)

gbmpred <-predict(gbmfit, data = tetrain, type='raw')

table(gbmpred, tetrain$y1)

gbmauc <- roc(as.numeric(tetrain$y), 
              as.numeric(gbmpred))
gbmauc

# Logistic Regression with Lasso
library(glmnet)

# Splitting independent and dependent variables  
# Finding smallest lambda
lascv<- cv.glmnet(notlisttrain, y1, alpha=1, family="binomial")
lasfit <- glmnet(notlisttrain, y1, family="binomial", 
                 alpha=1, lambda=lascv$lambda.min)



############# Second Method ####################

# Packages
install.packages("mice")
install.packages("missForest")
install.packages("VIM")
install.packages("caret")
install.packages("e1071")
install.packages("randomForest")
install.packages("pROC")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("ROSE")
install.packages("ROCR")

library(mice)
library(dplyr)
library(missForest)
library(VIM)
library(caret)
library(e1071)
library(randomForest)
library(pROC)
library(ROSE)
library(ROCR)

# Setting up
traindata <- read.csv('Marketing_Train.csv')
testdata <- read.csv('Marketing_Eval.csv')



# Cleaning training set
str(traindata)

traindata[traindata == "unknown"] <- NA
testdata[testdata == "unknown"] <- NA

# For unknowns: is.na & !is.na
sapply(traindata, function(x) sum(is.na(x))) #What data is missing?
table(traindata$default)

# For visuals:
trainvisual <- aggr(traindata, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(traindata), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))

sapply(traindata, function(x) sum(is.na(x))) #What data is missing?
table(traindata$default) #Unknown is 8570

#### REMOVE DEFAULT
## 8570 NA in default out of 39780. Only 12 Yes after data imputed. USELESS.
traindata <- traindata[-c(6)]

# Categorical to numeric
require(dplyr)
numtrain <- traindata %>%
  mutate(y = ifelse(y == "no",0,1))
numtrain$y <- as.factor(numtrain$y)

require(dplyr)
numtrain <- numtrain %>%
  mutate(housing = ifelse(housing == "no",0,1))

require(dplyr)
numtrain <- numtrain %>%
  mutate(loan = ifelse(loan == "no",0,1))

fact.marital <- as.numeric(as.factor(numtrain$marital))
subtract.1 = 1 #N/b: divorce = 0, married = 1, single = 2
fact.marital <- fact.marital - subtract.1
numtrain$marital <- fact.marital

subtract.1 = 1 

fact.education <- as.numeric(as.factor(numtrain$education))
fact.education <- fact.education - subtract.1
numtrain$education <- fact.education 
# N/b: basic4y = 0, basic6y = 1, basic9y = 2, highschool = 3, illiterate = 4
# professional course = 5, university degree = 6 

numtrain$job <- as.numeric(as.factor(numtrain$job)) - subtract.1
table(numtrain$job)

# Job: 0 - admin, 1 - bluecollar, 2 - entrepreneur, 3 - housemaid, 4 - management, 5 - retired,
### 6 - self-employed, 7 - services, 8 - student, 9 - technician, 10 - unemployed

numtrain$contact <- as.numeric(as.factor(numtrain$contact)) - subtract.1

numtrain$month <- as.numeric(as.factor(numtrain$month)) - subtract.1

numtrain$day_of_week <- as.numeric(as.factor(numtrain$day_of_week)) - subtract.1

numtrain$poutcome <- as.numeric(as.factor(numtrain$poutcome)) - subtract.1

numtrain <- as.data.frame(unclass(numtrain))


# Imputing data
imputed.train <- mice(numtrain, m=1, maxit=20, 
                      seed = 500, method="pmm")

comptrain <- mice::complete(imputed.train, 1)

# Remove GFC from data set
# Delete all data from 2008
numtrain <- comptrain[!comptrain$year 
                      == "2008",]

##########################################################################
# Create validation set within training data
part.train.no.08 <- createDataPartition(numtrain$y, 
                                        times = 1, p = 0.8, 
                                        list = FALSE)

# Training data within traindata
train.tr <- numtrain[part.train.no.08,]
# Testing data within testdata
train.te <- numtrain[-part.train.no.08,]


# Deal with imbalance in data
prop.table(table(numtrain$y))
barplot(prop.table(table(numtrain$y)),
        col= rainbow(2),
        ylim= c(0,1),
        main= "y Distribution")

# Shows that 79% are no, and 21% are yes
table(train.tr$y)
prop.table(table(train.tr$y))

# Upsampling YES'
upSamptrain.tr <- upSample(x = train.tr[, -ncol(train.tr)],
                           y = train.tr$y)      
table(upSamptrain.tr$Class)
# Remove from train and test: ID

upSamptrain.tr <- upSamptrain.tr[-c(1)]
noIDtrain.te <- train.te[-c(1)]

# GBM
upSamptrain.tr$Class<-ifelse(upSamptrain.tr$Class==0,'no','yes')
upSamptrain.tr$Class<-as.factor(upSamptrain.tr$Class)
gbmControl <- trainControl(method='cv', number=5, returnResamp='none',classProbs = TRUE)
gbmfit <- train(Class~., data=upSamptrain.tr, 
                method='gbm', 
                trControl=gbmControl,  
                metric = "ROC",
                preProc = c("center", "scale"),na.action = na.pass)


gbmpred <-predict(gbmfit, newdata = noIDtrain.te)


gbmpred <- factor(ifelse(gbmpred == "yes",1,0))

length(as.factor(noIDtrain.te$y))
length(gbmpred)

table(as.factor(gbmpred), noIDtrain.te$y)

confusionMatrix()

levels(gbmpred)
levels(as.factor(gbmho))
levels(noIDtrain.te$y)

gbmho<-ifelse(noIDtrain.te$y==0,'no','yes')

gbmauc <- roc(as.numeric(noIDtrain.te$y), 
              as.numeric(gbmpred))
gbmauc

plot(gbmauc, colour = "blue", main = "ROC Curve")

1-mean(gbmpred != noIDtrain.te$y)

rocgbmpred <- prediction(gbmpred, noIDtrain.te$y)
gbmeval <- performance(rocgbmpred, "acc")
plot(gbmeval)

gbmroccurve <- performance(rocgbmpred, "tpr", "fpr")
plot(gbmroccurve)

summary(gbmfit)
# LASSO
library(glmnet)
las <- model.matrix(Class ~ .,upSamptrain.tr)

cv.las <- cv.glmnet(las, upSamptrain.tr$Class, alpha=1, family='binomial', type.measure = 'mse')

plot(cv.las)



lambda <- cv.las$lambda.1se
coef(cv.las,s=lambda)

las.train <- model.matrix(Class~.,upSamptrain.tr)
lasso.prob <- predict(cv.las,newx = las.train,s=lambda,type='response')

las.predict <- rep(0,nrow(upSamptrain.tr))
las.predict[lasso.prob>.5] <- 1

conf.las.train <- table(pred=las.predict,true=upSamptrain.tr$Class)
conf.las.train
train.error.las <- (conf.las.train[2,1]+conf.las.train[1,2])/sum(conf.las.train)


las.test <- model.matrix(y~.,noIDtrain.te)
lasso.prob <- predict(cv.las,newx = las.test,s=lambda,type='response')

summary(las.test)

las.predict <- rep(0,nrow(noIDtrain.te))
las.predict[lasso.prob>.5] <- 1

summary(las.predict)

conf.las.test <- table(pred=las.predict,true=noIDtrain.te$y)
conf.las.test
test.error.las <- 1-mean(las.predict==noIDtrain.te$y)

lambda

auc.las <- roc(as.numeric(noIDtrain.te$y), 
               as.numeric(las.predict))

auc.las

1-mean(las.predict != noIDtrain.te$y)

roclaspred <- prediction(lasso.prob, noIDtrain.te$y)
laseval <- performance(roclaspred, "acc")
plot(laseval)

roc.curve <- performance(roclaspred, "tpr", "fpr")
plot(roc.curve, main = "ROC Curve", color = "red")

###Polynomial
svp.fit <- svm(as.factor(Class) ~., kernel = "polynomial", 
               data = upSamptrain.tr)
svp.pred <- predict(svp.fit, noIDtrain.te)

svp <- table(svp.pred, noIDtrain.te$y)
prop.table(svp)

1-mean(svp.pred != noIDtrain.te$y)

# Polynomial
svp.fit <- svm(as.factor(Class) ~., kernel = "polynomial", 
               data = upSamptrain.tr,
               probability = TRUE)

auc.svp <- roc(as.numeric(noIDtrain.te$y), 
               as.numeric(svp.pred))
auc.svp

# MAKE LOGREG GREAT AGAIN
# Remove: job, housing, loan bc they have too high p-values
# changing data

# RM1
rmJoHoLotrain.tr <- upSamptrain.tr[-c(2,5,6)]
rmJoHoLotrain.te <- noIDtrain.te[-c(2,5,6)]

lrRM1.fit <- glm(Class ~ . , 
                 family='binomial', 
                 data = rmJoHoLotrain.tr)

lrRM1.pred.tr <- predict(lrRM1.fit, newdata = rmJoHoLotrain.tr,
                         type="response")
lrRM1.rep.tr <- rep(0, length(lrRM1.pred.tr))
lrRM1.rep.tr[lrRM1.pred.tr > 0.5] <- 1

lrRM1.pred.te <- predict(lrRM1.fit, 
                         newdata=rmJoHoLotrain.te, 
                         type ="response")

lrRM1.rep.te <- rep(0, length(lrRM1.pred.te))
lrRM1.rep.te[lrRM1.pred.te > 0.5] <- 1


1-mean(lrRM1.rep.te != rmJoHoLotrain.te$y)


# AUC Log
auc.lrRM1.fit <- roc(as.numeric(rmJoHoLotrain.te$y), 
                     as.numeric(lrRM1.pred.te))
auc.lrRM1.fit

# With CV
cvControl <- trainControl(method="repeatedcv", number=5, repeats = 5)

lrRM1.fit.cv <- train(Class ~ .,
                      method="glm",
                      data = rmJoHoLotrain.tr,
                      family=binomial(),
                      trControl=cvControl)

lrRM1.fit.cv.pred <- predict(lrRM1.fit.cv, 
                             newdata = rmJoHoLotrain.te, 
                             type ="raw")

table(lrRM1.fit.cv.pred, rmJoHoLotrain.te$y)

auc.lrRM1.fit.cv <- roc(as.numeric(rmJoHoLotrain.te$y), 
                        as.numeric(lrRM1.fit.cv.pred))
auc.lrRM1.fit.cv


############VISUALISATION SCRIPT###################

### Graphs Script ###
library(zoo)
trainGraphdata <- read.csv("Marketing_Train.csv")
View(trainGraphdata)

graphTrend.sum <- data.frame(trainGraphdata$year,trainGraphdata$y)
graphTrend.sum

data <- transform(graphTrend.sum,month=as.numeric(format(as.Date(Date),"%Y %m")))
bymonth <- aggregate(cbind(graphTrend.sum$y == "yes")~Date,
                     data=graphTrend.sum,FUN=sum)

# importance plot
varImpPlot()


library(ggplot2)

ggplot(trainGraphdata, aes(x = age)) +
  geom_histogram(fill="skyblue", color="black") +
  theme_bw() + ggtitle("Age Histogram")

install.packages("reshape2")

byyear <- aggregate(cbind(graphTrend.sum$y == "yes")~year,
                    data=graphTrend.sum,FUN=sum)
nobyyear <- aggregate(cbind(graphTrend.sum$y == "no")~year,
                      data=graphTrend.sum,FUN=sum)
View(byyear)
View(nobyyear)

df <- as.data.frame(c(byyear, nobyyear))
View(df)

ggplot(df, aes(x=year, y=c(V1,V1.1)), fill = supp) +
  geom_bar(stat="identity")

p4 <- ggplot() + geom_bar(aes(y = y, x = year, fill = factor(y)), data = trainGraphdata,
                          stat="identity") + ggtitle("Yes vs No") + ylab("count") + geom_bar(colour="black")
p4

sum.yes <- sum(trainGraphdata$y == "yes")
sum.yes[trainGraphdata$year == "2008"]


ggplot(trainGraphdata, aes(x = age)) +
  geom_histogram(fill="skyblue", color="black") +
  theme_bw()

ggplot(bymonth, aes(x=Date, y=V1, group=1)) +
  geom_line(color = "blue") +
  geom_point() + ylab("Sum of Yes") + ggtitle("Sum of Yes by Date")


# FIND PEAK (highest accuracy model)
#########################################################################
# LAST PART

testdata[testdata == "unknown"] <- NA

numtrain <- transform(testdata, id = as.numeric(id),
                      age = as.numeric(age),
                      job = as.numeric(job), 
                      marital = as.numeric(marital),
                      education = as.numeric(education),
                      default = as.numeric(default),
                      housing = as.numeric(housing),
                      loan = as.numeric(loan),
                      contact = as.numeric(contact),
                      year = as.numeric(year),
                      month = as.numeric(month),
                      day_of_week = as.numeric(day_of_week),
                      campaign = as.numeric(campaign),
                      pdays = as.numeric(pdays),
                      previous = as.numeric(previous),
                      poutcome = as.numeric(poutcome),
                      emp.var.rate = as.numeric(emp.var.rate),
                      cons.price.idx = as.numeric(cons.price.idx),
                      cons.conf.idx = as.numeric(cons.conf.idx),
                      euribor3m = as.numeric(euribor3m),
                      nr.employed = as.numeric(nr.employed))

xdnumtrain <- numtrain[-c(6)]

imptest <- mice(numtrain, m = 1, maxit=20, seed=500)

comptest <-mice::complete(imptest, 1)

testfit <- predict(gbmfit, comptest, type ="prob")
testdata <- cbind(testdata, testfit$yes)

orderedtest <- testdata[order(-testdata$'testfit$yes'),]

final <- as.matrix(orderedtest[1:500,1])
colnames(final) [1] <- "id"
View(final)

write.csv(final,"select_clients.csv", row.names=FALSE)

