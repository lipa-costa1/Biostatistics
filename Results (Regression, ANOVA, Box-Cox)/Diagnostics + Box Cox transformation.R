########################Diagnosis#################
library(readr)
library(car)
data_no_g <- read_csv("dataset-finale-3-no-gender-no-covid.csv")
#dataset without gender
data_no_g<-na.omit(data_no_g)

data_no_g<-data_no_g[-406,]
data_no_g<-data_no_g[-403,]
data_no_g<-data_no_g[-400,]
data_no_g<-data_no_g[-397,]

#Data train and test
which(data_no_g$Period=='2020-02-01') #453
#data.test is the last month we have available data
########without box cox transformations
data.train_1<-data_no_g[1:452,]
data.test_1 <- data_no_g[453:464,]
data.train_1$Month<-as.factor(data.train_1$Month)
data.test_1$Month<-as.factor(data.test_1$Month)
data.train_1$Region<-as.factor(data.train_1$Region)
data.train_1$Age.Range<-as.factor(data.train_1$Age.Range)

########with box cox transformations
library(forecast)
lambda_1 <- BoxCox.lambda(data.train_1$Hospitalizations_100000)
data.train_1.box.cox<-data.train_1
data.train_1.box.cox$Hospitalizations_100000 <- (data.train_1$Hospitalizations_100000^(lambda_1) - 1)/lambda_1
data.test_1.box.cox<-data.test_1
data.test_1.box.cox$Hospitalizations_100000 <- (data.test_1$Hospitalizations_100000^(lambda_1) - 1)/lambda_1


#####################################Model stepwise everything WITHOUT MONTH

##########################Age 1
model.null <- lm(Hospitalizations_100000~ 1,data=na.omit(data.train_1[data.train_1$Age.Range=="1",-c(1,3,4,18,19,21,20,6,10,11)]))
model <- lm(Hospitalizations_100000~.,data=na.omit(data.train_1[data.train_1$Age.Range=="1",-c(1,3,4,18,19,21,20,6,10,11)]))
m1_1<-step(model.null,scope = list(upper=model),direction="both",data=na.omit(data.train_1[data.train_1$Age.Range=="1",-c(1,3,4,18,19,21,20,6,10,11)]))  
summary(m1_1) #0.6669 

#########################Age 5

model.null <- lm(Hospitalizations_100000~ 1,data=na.omit(data.train_1[data.train_1$Age.Range=="5",-c(1,3,4,18,19,21, 20, 5,6, 10, 11, 13:16)]))
model <- lm(Hospitalizations_100000~.,data=na.omit(data.train_1[data.train_1$Age.Range=="5",-c(1,3,4,18,19,21, 20, 5,6, 10, 11, 13:16)])) 
m1_5<-step(model.null,scope = list(upper=model),direction="both",data=na.omit(data.train_1[data.train_1$Age.Range=="5",-c(1,3,4,18,19,21, 20, 5,6, 10, 11, 13:16)])) 
summary(m1_5) #0.5151

########################Age 6

model.null <- lm(Hospitalizations_100000~ 1,data=na.omit(data.train_1[data.train_1$Age.Range=="6",-c(1,3,4,18,19,21, 20, 5, 7, 13, 14, 9, 10)]))
model <- lm(Hospitalizations_100000~.,data=na.omit(data.train_1[data.train_1$Age.Range=="6",-c(1,3,4,18,19,21, 20, 5, 7, 13, 14, 9, 10)]))
m1_6<-step(model.null,scope = list(upper=model),direction="both",data=na.omit(data.train_1[data.train_1$Age.Range=="6",-c(1,3,4,18,19,21, 20, 5, 7, 13, 14, 9, 10)]))
summary(m1_6)  #0.5017 


################WITH BOX COX TRANSFORMATION

##########################Age 1
model.null <- lm(Hospitalizations_100000~ 1,data=na.omit(data.train_1.box.cox[data.train_1.box.cox$Age.Range=="1",-c(1,3,4,18,19,21,20,6,10,11)]))
model <- lm(Hospitalizations_100000~.,data=na.omit(data.train_1.box.cox[data.train_1.box.cox$Age.Range=="1",-c(1,3,4,18,19,21,20,6,10,11)]))
m1_1.box.cox<-step(model.null,scope = list(upper=model),direction="both",data=na.omit(data.train_1.box.cox[data.train_1.box.cox$Age.Range=="1",-c(1,3,4,18,19,21,20,6,10,11)]))  
summary(m1_1.box.cox) #0.5935

#########################Age 5

model.null <- lm(Hospitalizations_100000~ 1,data=na.omit(data.train_1.box.cox[data.train_1.box.cox$Age.Range=="5",-c(1,3,4,18,19,21, 20, 5,6, 10, 11, 13:16)]))
model <- lm(Hospitalizations_100000~.,data=na.omit(data.train_1.box.cox[data.train_1.box.cox$Age.Range=="5",-c(1,3,4,18,19,21, 20, 5,6, 10, 11, 13:16)])) 
m1_5.box.cox<-step(model.null,scope = list(upper=model),direction="both",data=na.omit(data.train_1.box.cox[data.train_1.box.cox$Age.Range=="5",-c(1,3,4,18,19,21, 20, 5,6, 10, 11, 13:16)])) 
summary(m1_5.box.cox) #0.5168 

########################Age 6

model.null <- lm(Hospitalizations_100000~ 1,data=na.omit(data.train_1.box.cox[data.train_1.box.cox$Age.Range=="6",-c(1,3,4,18,19,21, 20, 5, 7, 13, 14, 9, 10)]))
model <- lm(Hospitalizations_100000~.,data=na.omit(data.train_1.box.cox[data.train_1.box.cox$Age.Range=="6",-c(1,3,4,18,19,21, 20, 5, 7, 13, 14, 9, 10)]))
m1_6.box.cox<-step(model.null,scope = list(upper=model),direction="both",data=na.omit(data.train_1.box.cox[data.train_1.box.cox$Age.Range=="6",-c(1,3,4,18,19,21, 20, 5, 7, 13, 14, 9, 10)]))
summary(m1_6.box.cox)  #0.55 



################### DATASET WITH NAS FILLED

data_no_g <- read_csv("dataset-finale-3-no-gender-no-covid.csv")

##Delete Outliers 

#data_no_g$`PM10 max` 642 647 652 657 OUTLIER
#delete row 642 647 652 657

data_no_g<-data_no_g[-642,]
data_no_g<-data_no_g[-647,]
data_no_g<-data_no_g[-652,]
data_no_g<-data_no_g[-657,]

############# Fill NAs ########################
colSums(is.na(data_no_g)) # only have NAs for numeric variables

# fill the NA values for each column

for (col in colnames(data_no_g)[6:17]) {
  
  for (region in unique(data_no_g$Region)) {
    
    data_no_g[[col]][data_no_g$Region == region & is.na(data_no_g[[col]]) == TRUE] = median(data_no_g[[col]][data_no_g$Region == region], na.rm = TRUE) 
  }
}
################################################

# confirm that we don't have NAs
colSums(is.na(data_no_g)) # CO for Algarve region has no values, for all periods of time

data_no_g<-na.omit(data_no_g) #because of algarve

#Data train and test
which(data_no_g$Period=='2020-02-01') #589
#data.test is the last month we have available data
data.train_2<-data_no_g[1:588,]
data.test_2 <- data_no_g[589:604,]
data.train_2$Month<-as.factor(data.train_2$Month)
data.test_2$Month<-as.factor(data.test_2$Month)
data.train_2$Region<-as.factor(data.train_2$Region)
data.train_2$Age.Range<-as.factor(data.train_2$Age.Range)

########with box cox transformations
lambda <- BoxCox.lambda(data.train_2$Hospitalizations_100000)
data.train_2.box.cox<-data.train_2
data.train_2.box.cox$Hospitalizations_100000 <- (data.train_2$Hospitalizations_100000^(lambda) - 1)/lambda
data.test_2.box.cox<-data.test_2
data.test_2.box.cox$Hospitalizations_100000 <- (data.test_2$Hospitalizations_100000^(lambda) - 1)/lambda



##############################Model stepwise everything WITHOUT MONTH

##########################Age 1
model.null <- lm(Hospitalizations_100000~ 1,data=na.omit(data.train_2[data.train_2$Age.Range=="1",-c(1,3,4,18:21)]))
model <- lm(Hospitalizations_100000~.,data=na.omit(data.train_2[data.train_2$Age.Range=="1",-c(1,3,4,18:21)]))
m2_1<-step(model.null,scope = list(upper=model),direction="both",data=na.omit(data.train_2[data.train_2$Age.Range=="1",-c(1,3,4,18:21)]))
summary(m2_1) #0.6838 


#########################Age 5
model.null <- lm(Hospitalizations_100000~ 1,data=na.omit(data.train_2[data.train_2$Age.Range=="5",-c(1,3,4,18:21)]))
model <- lm(Hospitalizations_100000~.,data=na.omit(data.train_2[data.train_2$Age.Range=="5",-c(1,3,4,18:21)]))
m2_5<-step(model.null,scope = list(upper=model),direction="both",data=na.omit(data.train_2[data.train_2$Age.Range=="5",-c(1,3,4,18:21)]))
summary(m2_5)  #0.537  


########################Age 6
model.null <- lm(Hospitalizations_100000~ 1,data=na.omit(data.train_2[data.train_2$Age.Range=="6",-c(1,3,4,18:21)]))
model <- lm(Hospitalizations_100000~.,data=na.omit(data.train_2[data.train_2$Age.Range=="6",-c(1,3,4,18:21)]))
m2_6<-step(model.null,scope = list(upper=model),direction="both",data=na.omit(data.train_2[data.train_2$Age.Range=="6",-c(1,3,4,18:21)]))
summary(m2_6)  #0.5934 

#####################################With box cox transformation

##########################Age 1
model.null <- lm(Hospitalizations_100000~ 1,data=na.omit(data.train_2.box.cox[data.train_2.box.cox$Age.Range=="1",-c(1,3,4,18:21)]))
model <- lm(Hospitalizations_100000~.,data=na.omit(data.train_2.box.cox[data.train_2.box.cox$Age.Range=="1",-c(1,3,4,18:21)]))
m2_1.box.cox<-step(model.null,scope = list(upper=model),direction="both",data=na.omit(data.train_2.box.cox[data.train_2.box.cox$Age.Range=="1",-c(1,3,4,18:21)]))
summary(m2_1.box.cox) #0.6599 


#########################Age 5
model.null <- lm(Hospitalizations_100000~ 1,data=na.omit(data.train_2.box.cox[data.train_2.box.cox$Age.Range=="5",-c(1,3,4,18:21)]))
model <- lm(Hospitalizations_100000~.,data=na.omit(data.train_2.box.cox[data.train_2.box.cox$Age.Range=="5",-c(1,3,4,18:21)]))
m2_5.box.cox<-step(model.null,scope = list(upper=model),direction="both",data=na.omit(data.train_2.box.cox[data.train_2.box.cox$Age.Range=="5",-c(1,3,4,18:21)]))
summary(m2_5.box.cox)  #0.5612  


########################Age 6
model.null <- lm(Hospitalizations_100000~ 1,data=na.omit(data.train_2.box.cox[data.train_2.box.cox$Age.Range=="6",-c(1,3,4,18:21)]))
model <- lm(Hospitalizations_100000~.,data=na.omit(data.train_2.box.cox[data.train_2.box.cox$Age.Range=="6",-c(1,3,4,18:21)]))
m2_6.box.cox<-step(model.null,scope = list(upper=model),direction="both",data=na.omit(data.train_2.box.cox[data.train_2.box.cox$Age.Range=="6",-c(1,3,4,18:21)]))
summary(m2_6.box.cox)  #0.6339   





#0.6669 #0.5151 #0.5017 STEPWISE WITHOUT GENDERS (m1_1, m1_5, m1_6)

#0.6838 #0.537 #0.5934  STEWPWISE WITH NAS FILLED WITHOUT GENDERS (m2_1, m2_5, m2_6)

########With box cox

#0.5935 #0.5168 #0.55 STEPWISE WITHOUT GENDERS (m1_1.box.cox, m1_5.box.cox, m1_6.box.cox)

#0.6599 #0.5612 #0.6339 STEWPWISE WITH NAS FILLED WITHOUT GENDERS (m2_1.box.cox, m2_5.box.cox, m2_6.box.cox)

################################### Diagnostics #######################################
library(IMTest)
library(nortest)
library(QuantPsyc)
library(jtools)
library(olsrr)
library(asbio)

#Summaries
summary(m1_1)
summary(m1_5)
summary(m1_6)
summary(m2_1)
summary(m2_5)
summary(m2_6)

#AIC, BIC, CP, PRESS
lm.select(list(m1_1))
lm.select(list(m1_5))
lm.select(list(m1_6))
lm.select(list(m2_1))
lm.select(list(m2_5))
lm.select(list(m2_6))

###Heterogeneity of variance

#Breusch-Pagan #H0- the residuals have constant variance
bptest(m1_1)
bptest(m1_5)
bptest(m1_6) 
bptest(m2_1)
bptest(m2_5)
bptest(m2_6)

#The residuals variance is not constant
#Apply box cox transformation to response variable


############Normality of residuals

# Histogram
hist(m1_1$residuals)
hist(m1_5$residuals)
hist(m1_6$residuals)
hist(m2_1$residuals)
hist(m2_5$residuals)
hist(m2_6$residuals)

# Shapiro Test #H0-residuals are normal
shapiro.test(m1_1$residuals)
shapiro.test(m1_5$residuals)
shapiro.test(m1_6$residuals)
shapiro.test(m2_1$residuals)
shapiro.test(m2_5$residuals)
shapiro.test(m2_6$residuals)

#we have normality for everything

###################Residuals autocorrelation

# Durbin Watson Test #H0-autocorrelation between the residuals is zero


plot(m1_1$residuals,xlab="Index", ylab="Residuals",col=rgb(0.5,0.8,1),pch=16)
plot(m1_5$residuals,xlab="Index", ylab="Residuals", col=rgb(0.5,0.8,1),pch=16)
plot(m1_6$residuals,xlab="Index", ylab="Residuals", col=rgb(0.5,0.8,1),pch=16)
plot(m2_1$residuals,xlab="Index", ylab="Residuals", col=rgb(0.5,0.8,1),pch=16)
plot(m2_5$residuals,xlab="Index", ylab="Residuals", col=rgb(0.5,0.8,1),pch=16)
plot(m2_6$residuals,xlab="Index", ylab="Residuals", col=rgb(0.5,0.8,1),pch=16)


durbinWatsonTest(m1_1)
durbinWatsonTest(m1_5)
durbinWatsonTest(m1_6)
durbinWatsonTest(m2_1)
durbinWatsonTest(m2_5)
durbinWatsonTest(m2_6)




######## Box cox transformation

#Breusch-Pagan #H0 - the residuals have constant variance
bptest(m1_1.box.cox)
bptest(m1_5.box.cox)
bptest(m1_6.box.cox) 

bptest(m2_1.box.cox)
bptest(m2_5.box.cox)
bptest(m2_6.box.cox)

#It improved but for some ages we still have non constant variance. 
#Model 1 presents constant residuals variance for age 5,6 while model 2 
#does not present any constant variance

############Normality of residuals for only model 1

# Histogram
hist(m1_1.box.cox$residuals)
hist(m1_5.box.cox$residuals)
hist(m1_6.box.cox$residuals)

# Shapiro Test
shapiro.test(m1_1.box.cox$residuals)
shapiro.test(m1_5.box.cox$residuals)
shapiro.test(m1_6.box.cox$residuals)
#we dont have normality in age 5


###################Residuals autocorrelation

# Durbin Watson Test #H0-autocorrelation between the residuals is zero


plot(m1_1.box.cox$residuals,xlab="Index", ylab="Residuals",col=rgb(0.5,0.8,1),pch=16)
plot(m1_5.box.cox$residuals,xlab="Index", ylab="Residuals", col=rgb(0.5,0.8,1),pch=16)
plot(m1_6.box.cox$residuals,xlab="Index", ylab="Residuals", col=rgb(0.5,0.8,1),pch=16)


durbinWatsonTest(m1_1.box.cox)
durbinWatsonTest(m1_5.box.cox)
durbinWatsonTest(m1_6.box.cox)

##### Multicolinearidade

vif(m1_1.box.cox) 
vif(m1_5.box.cox)
vif(m1_6.box.cox)

##nao há problemas de multicolineriedade

#### Prediction : February 2020 ####
library(ggplot2)
previsao.y_1 <- predict(m1_1.box.cox,data.test_1.box.cox[data.test_1.box.cox$Age.Range==1,])
summary(sqrt((previsao.y_1 - data.test_1.box.cox[data.test_1.box.cox$Age.Range==1,]$Hospitalizations_100000)^2))
d_1<-data.frame(region= data.test_1.box.cox[data.test_1.box.cox$Age.Range==1,]$Region,previsao.y_1, y=data.test_1.box.cox[data.test_1.box.cox$Age.Range==1,]$Hospitalizations_100000)
d_1$rel_err <- abs( (d_1$previsao.y_1 - d_1$y) / d_1$y)
d_1$Rev_transf_pred <- (d_1$previsao.y_1*lambda_1 + 1)^(1/lambda_1)
d_1$real_y <- data.test_1[data.test_1$Age.Range==1,]$Hospitalizations_100000

previsao.y_2 <- predict(m1_5.box.cox,data.test_1.box.cox[data.test_1.box.cox$Age.Range==5,])
summary(sqrt((previsao.y_2 - data.test_1.box.cox[data.test_1.box.cox$Age.Range==5,]$Hospitalizations_100000)^2))
d_2<-data.frame(region= data.test_1.box.cox[data.test_1.box.cox$Age.Range==5,]$Region,previsao.y_2, y=data.test_1.box.cox[data.test_1.box.cox$Age.Range==5,]$Hospitalizations_100000)
d_2$rel_err <- abs( (d_2$previsao.y_2 - d_2$y) / d_2$y)
d_2$Rev_transf_pred <- (d_2$previsao.y_2*lambda_1 + 1)^(1/lambda_1)
d_2$real_y <- data.test_1[data.test_1$Age.Range==5,]$Hospitalizations_100000

previsao.y_3 <- predict(m1_6.box.cox,data.test_1.box.cox[data.test_1.box.cox$Age.Range==6,])
summary(sqrt((previsao.y_3 - data.test_1.box.cox[data.test_1.box.cox$Age.Range==6,]$Hospitalizations_100000)^2))
d_3<-data.frame(region= data.test_1.box.cox[data.test_1.box.cox$Age.Range==6,]$Region,previsao.y_3, y=data.test_1.box.cox[data.test_1.box.cox$Age.Range==6,]$Hospitalizations_100000)
d_3$rel_err <- abs( (d_3$previsao.y_3 - d_3$y) / d_3$y)
d_3$Rev_transf_pred <- (d_3$previsao.y_3*lambda_1 + 1)^(1/lambda_1)
d_3$real_y <- data.test_1[data.test_1$Age.Range==6,]$Hospitalizations_100000

############################SCENARIOS###################################


#data covid

#data covid box cox transformation

data_y <- read_csv("dataset-finale-3-no-gender-with-covid.csv")
which(data_y$Period=='2020-03-01') #761
data_covid<-data_y[761:1198,]
data_covid<-na.omit(data_covid)

data_covid.box.cox<-data_covid
data_covid.box.cox$Hospitalizations_100000 <- (data_covid.box.cox$Hospitalizations_100000^(lambda_1) - 1)/lambda_1

previsao.y_1 <- predict(m1_1.box.cox,data_covid.box.cox[data_covid.box.cox$Age.Range==1,])
summary(sqrt((previsao.y_1 - data_covid.box.cox[data_covid.box.cox$Age.Range==1,]$Hospitalizations_100000)^2))
d_1<-data.frame(region= data_covid.box.cox[data_covid.box.cox$Age.Range==1,]$Region,previsao.y_1, y=data_covid.box.cox[data_covid.box.cox$Age.Range==1,]$Hospitalizations_100000)
d_1$Rev_transf_pred <- (d_1$previsao.y_1*lambda_1 + 1)^(1/lambda_1)
d_1$real_y <- data_covid.box.cox[data_covid.box.cox$Age.Range==1,]$Hospitalizations_100000

previsao.y_2 <- predict(m1_5.box.cox,data_covid.box.cox[data_covid.box.cox$Age.Range==5,])
summary(sqrt((previsao.y_2 - data_covid.box.cox[data_covid.box.cox$Age.Range==5,]$Hospitalizations_100000)^2))
d_2<-data.frame(region= data_covid.box.cox[data_covid.box.cox$Age.Range==5,]$Region,previsao.y_2, y=data_covid.box.cox[data_covid.box.cox$Age.Range==5,]$Hospitalizations_100000)
d_2$rel_err <- abs( (d_2$previsao.y_2 - d_2$y) / d_2$y)
d_2$Rev_transf_pred <- (d_2$previsao.y_2*lambda_1 + 1)^(1/lambda_1)
d_2$real_y <- data_covid.box.cox[data_covid.box.cox$Age.Range==5,]$Hospitalizations_100000

previsao.y_3 <- predict(m1_6.box.cox,data_covid.box.cox[data_covid.box.cox$Age.Range==6,])
summary(sqrt((previsao.y_3 - data_covid.box.cox[data_covid.box.cox$Age.Range==6,]$Hospitalizations_100000)^2))
d_3<-data.frame(region= data_covid.box.cox[data_covid.box.cox$Age.Range==6,]$Region,previsao.y_3, y=data_covid.box.cox[data_covid.box.cox$Age.Range==6,]$Hospitalizations_100000)
d_3$rel_err <- abs( (d_3$previsao.y_3 - d_3$y) / d_3$y)
d_3$Rev_transf_pred <- (d_3$previsao.y_3*lambda_1 + 1)^(1/lambda_1)
d_3$real_y <- data_covid.box.cox[data_covid.box.cox$Age.Range==6,]$Hospitalizations_100000

#par 2x2

ggplot(d_3[d_3$region=="Centro",c(4,5)], aes(colour = supp)) +
  geom_line()
