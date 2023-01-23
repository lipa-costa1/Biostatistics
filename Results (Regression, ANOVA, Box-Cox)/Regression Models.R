library(readr)
library(psych)
library(ggplot2)
library(GGally)
library(leaps)
library(carData)
library(car)
library(MASS)
library(calibrate)
library(purrr)
data_no_g <- read_csv("dataset-finale-3-no-gender-no-covid.csv")
#dataset without gender

data_no_g<-na.omit(data_no_g)

plotting_curves<- function(x,y, tit){
  plot(x,y,col=rgb(0.4,0.4,0.8,0.6),pch=16 , cex=1.3) 
  title(tit)
  # fit linear line through data
  model <- lm(y ~poly(x, 1, raw=TRUE))
  myPredict <- predict( model ) 
  ix <- sort(x,index.return=T)$ix
  lines(x[ix], myPredict[ix], col=2, lwd=2 )  
  
  # fit polynom of second degree through data
  model <- lm(y ~poly(x, 2, raw=TRUE))
  myPredict <- predict( model ) 
  ix <- sort(x,index.return=T)$ix
  lines(x[ix], myPredict[ix], col=2, lwd=2 ) 
  
  # fit log function through data
  tryCatch({
  fm <- nls(y ~ c0 * log(c1 * x + 1), start = list(c0 = mean(y), c1 = 1))
  pred <- predict(fm)
  
  ix <- sort(x,index.return=T)$ix
  lines(x[ix], pred[ix], col = "green", lwd=2)
  },
  error=function(cond) {
    return(NA)
  })
  return(TRUE)
}



hosps <- c(17)
age_col <- c(3)

################################ MAIN ########################################

#data_no_g$`PM10 max`[406] [397] [400] [403] OUTLIER
#delete row 406, 397, 400, 403

data_no_g<-data_no_g[-406,]
data_no_g<-data_no_g[-403,]
data_no_g<-data_no_g[-400,]
data_no_g<-data_no_g[-397,]

par(mfrow=c(4,6))
for (age in c(1,5,6,7)){
  datas <- data_no_g[which(data_no_g[,age_col]==age),]
datas <- na.omit(datas)
y <- datas$Hospitalizations_100000
x <-  datas$`CO max` # DEFINE POLLUTION HERE
pol <- "CO max"
# red lines are 1, 2 degree polynomials, green line is log function
tit <- paste(pol, "Age", as.character(age))
plotting_curves(x,y, tit)
}




######### RESULTS ########
# L = linear, 2 = 2 degree poly, log = log fct
# u=up, d=down or c=constant
# POLLUTANT   AGE 1 # Age 5 # Age 6 # Age 7
# PM10 Mean     Lu    Lc     Lc       Lc
#      Max   # TODO Remove the outlier
# O3   Mean     2u    Lu      Lu      Lc
#      Max    
# NOx  Mean     




###########################################################################

#Data train and test
which(data_no_g$Period=='2020-02-01') #453
#data.test is the last month we have available data
data.train<-data_no_g[1:452,]
data.test <- data_no_g[453:464,]
data.train$Month<-as.factor(data.train$Month)
data.test$Month<-as.factor(data.test$Month)
data.train$Region<-as.factor(data.train$Region)
data.train$Age.Range<-as.factor(data.train$Age.Range)


#4 main assumptions for linear regression:

#Independence of observations (aka no autocorrelation)

#Use the cor() function to test the relationship between your independent variables and make sure they aren't too highly correlated.
#Por idade, quais são as variáveis correlacionadas entre si?

#Linearity. The relationship between the independent and dependent variable must be linear.
#Scaterplot

#eliminar variáveis correlacionadas:

library(corrplot)
corrplot(cor(data.train[data.train$Age.Range==1,c(5:17)]), method = 'number',type = 'upper', diag = TRUE)

#correlations between variables more than 80% will be excluded.
#correlations under 20% between variables and y will be excluded.

#AGE 1
#exclude: PM10 max, NOX max, NO mean

#AGE 5
#keep: 03 mean, 03 max, NOX mean, NO max

#AGE 6
#exclude: PM10 mean, O3 mean, PM2.5 mean, PM2.5 max, NOX mean, NOX max



###########################Model all variables############

##########################Age 1
##Linear model
model <- lm(Hospitalizations_100000~.,data=na.omit(data.train[data.train$Age.Range=="1",-c(1,3,4,18,19,21)]))
summary(model)

#########################Age 5
##Linear model
model <- lm(Hospitalizations_100000~.,data=na.omit(data.train[data.train$Age.Range=="5",-c(1,3,4,18,19,21)]))
summary(model)


########################Age 6
##Linear model
model <- lm(Hospitalizations_100000~.,data=na.omit(data.train[data.train$Age.Range=="6",-c(1,3,4,18,19,21)]))
summary(model)


#################################A presença dos meses nas variáveis
#faz com que não seja dada importância aos poluentes#######################

#Ver se por mês há diferença de hospitalizações

plot(data.train[data.train$Age.Range=="1",]$Month, data.train[data.train$Age.Range=="1",]$Hospitalizations_100000)

#É possível ver que as hospitalizações dependem do mês que estamos


#####################################Model for correlations WITHOUT MONTH###########

##########################Age 1
##Linear model
model <- lm(Hospitalizations_100000~.,data=na.omit(data.train[data.train$Age.Range=="1",-c(1,3,4,18,19,21,20,6,10,11)])) #0.6653 
summary(model)
#exclude: PM10 max (6), NOX max (10), NO mean (11)
## O3 max has a negative influence on hospitalizations because of the chemical reactions.
#03 mean* (0.0874085), O3 max*** (-0.0747667)(negative), PM2.5 max** (0.0727589), CO mean*** (0.0277495 )

#########################Age 5
##Linear model
model <- lm(Hospitalizations_100000~.,data=na.omit(data.train[data.train$Age.Range=="5",-c(1,3,4,18,19,21, 20, 5,6, 10, 11, 13:16)])) #0.5111 
summary(model)
#keep: 03 mean, 03 max, NOX mean, NO max

#Region LVT and Madeira
#O3 mean* (0.0262086), NOX as NO2 mean. (0.0172856)

 
########################Age 6
##Linear model
model <- lm(Hospitalizations_100000~.,data=na.omit(data.train[data.train$Age.Range=="6",-c(1,3,4,18,19,21, 20, 5, 7, 13, 14, 9, 10)])) #0.4956 
summary(model)
#exclude: PM10 mean (5), O3 mean (7), PM2.5 mean (13), PM2.5 max (14), NOX mean (9), NOX max (10)

#Region Madeira
#O3 max* (-0.0215735)(negative), CO mean** (0.0163058)

######################Model for correlations + transformations WITHOUT MONTH####### (THE MAX VARIABLES ARE NOT TRANSFORMED)


#######################Age 1
##Linear model with tranformations on variables
model <- lm(Hospitalizations_100000~`PM10 mean`+log(`O3 mean`)+log(`NOX as NO2 mean`)+`PM2.5 mean`+`CO mean`+Region+log(`O3 max`)+`NO max`+`PM2.5 max`+`CO max`,data=data.train[data.train$Age.Range=="1",]) #0.6569
summary(model)

#log(`O3 mean`)** (5.966e+00), `CO mean`*** (2.454e-02), log(`O3 max`)***(-1.018e+01), `PM2.5 max`*** (7.816e-02)


#########################Age 5
##Linear model with tranformations on variables
model <- lm(Hospitalizations_100000~`O3 mean`+log(`NOX as NO2 mean`)+Region+`O3 max`+log(`NO max`),data=data.train[data.train$Age.Range=="5",])  #0.5315 
summary(model)

#`O3 mean`**  (0.029567), log(`NOX as NO2 mean`)** (1.461445)

#########################Age 6
##Linear model with tranformations on variables
model <- lm(Hospitalizations_100000~log(`NO mean`)+log(`CO mean`)+Region+`O3 max`+log(`NO max`)+log(`CO max`)+`PM10 max`,data=data.train[data.train$Age.Range=="6",]) #0.5164 
summary(model)

#log(`NO max`)* (1.599647), `O3 max`. (-0.019450), log(`CO mean`)** (3.707825)

#Region Madeira. A madeira é uma ilha, por isso secalhar afeta as hospitalizações
#positivamente porque (...)


#####################################Model stepwise everything WITHOUT MONTH

##fazer qqplot para hospitalizações por regiao, por idade.
plot(data.train[data.train$Age.Range=="6",]$Region, data.train[data.train$Age.Range=="6",]$Hospitalizations_100000)
##ver que a regiao influencia as hospitalizações


##########################Age 1
model.null <- lm(Hospitalizations_100000~ 1,data=na.omit(data.train[data.train$Age.Range=="1",-c(1,3,4,18,19,21,20,6,10,11)]))
model <- lm(Hospitalizations_100000~.,data=na.omit(data.train[data.train$Age.Range=="1",-c(1,3,4,18,19,21,20,6,10,11)]))
m1_1<-step(model.null,scope = list(upper=model),direction="both",data=na.omit(data.train[data.train$Age.Range=="1",-c(1,3,4,18,19,21,20,6,10,11)]))  
summary(m1_1) #0.6669 
#`CO mean`*** (0.037099), `PM2.5 max`*** (0.069606), `O3 max`*** (-0.076236), `NOX as NO2 mean`*** (-0.058693), `O3 mean`*  (0.064723)



#########################Age 5

model.null <- lm(Hospitalizations_100000~ 1,data=na.omit(data.train[data.train$Age.Range=="5",-c(1,3,4,18,19,21, 20, 5,6, 10, 11, 13:16)]))
model <- lm(Hospitalizations_100000~.,data=na.omit(data.train[data.train$Age.Range=="5",-c(1,3,4,18,19,21, 20, 5,6, 10, 11, 13:16)])) 
m1_5<-step(model.null,scope = list(upper=model),direction="both",data=na.omit(data.train[data.train$Age.Range=="5",-c(1,3,4,18,19,21, 20, 5,6, 10, 11, 13:16)])) 
summary(m1_5) #0.5151

#Region LVT and Madeira
#O3 mean* (0.026568), NOX as NO2 mean** (0.019029 )


########################Age 6

model.null <- lm(Hospitalizations_100000~ 1,data=na.omit(data.train[data.train$Age.Range=="6",-c(1,3,4,18,19,21, 20, 5, 7, 13, 14, 9, 10)]))
model <- lm(Hospitalizations_100000~.,data=na.omit(data.train[data.train$Age.Range=="6",-c(1,3,4,18,19,21, 20, 5, 7, 13, 14, 9, 10)]))
m1_6<-step(model.null,scope = list(upper=model),direction="both",data=na.omit(data.train[data.train$Age.Range=="6",-c(1,3,4,18,19,21, 20, 5, 7, 13, 14, 9, 10)]))
summary(m1_6)  #0.5017 

#Region LVT, Madeira
#O3 max. (-0.017274)(negative), CO mean*** (0.016411)


#####################################Model stepwise + transformations

#######################Age 1

model.null <- lm(Hospitalizations_100000~ 1,data=data.train[data.train$Age.Range=="1",])
model <- lm(Hospitalizations_100000~`PM10 mean`+log(`O3 mean`)+log(`NOX as NO2 mean`)+`PM2.5 mean`+`CO mean`+Region+log(`O3 max`)+`NO max`+`PM2.5 max`+`CO max`,data=data.train[data.train$Age.Range=="1",]) #0.6569
lx<-step(model.null,scope = list(upper=model),direction="both",data=data.train[data.train$Age.Range=="1",])  
summary(lx) #0.66 

#log(`O3 mean`)* (4.262495), `CO mean`*** (0.035628), log(`O3 max`)***(-10.051310), `PM2.5 max`*** (0.089861), log(`NOX as NO2 mean`) (-2.184257) ** 


#########################Age 5

model.null <- lm(Hospitalizations_100000~ 1,data=data.train[data.train$Age.Range=="5",])
model <- lm(Hospitalizations_100000~`O3 mean`+log(`NOX as NO2 mean`)+Region+`O3 max`+log(`NO max`),data=data.train[data.train$Age.Range=="5",])  #0.5315 
lx<-step(model.null,scope = list(upper=model),direction="both",data=data.train[data.train$Age.Range=="5",])  
summary(lx) #0.533

#`O3 mean`*  (0.022544), log(`NOX as NO2 mean`)*** (1.350672)

#########################Age 6

model.null <- lm(Hospitalizations_100000~ 1,data=data.train[data.train$Age.Range=="6",])
model <- lm(Hospitalizations_100000~log(`NO mean`)+log(`CO mean`)+Region+`O3 max`+log(`NO max`)+log(`CO max`)+`PM10 max`,data=data.train[data.train$Age.Range=="6",]) #0.5164 
lx<-step(model.null,scope = list(upper=model),direction="both",data=data.train[data.train$Age.Range=="6",]) 
summary(lx) #0.4619 

#log(`NO mean`)*** (3.0683)

#######################################BEST SUBSETS TECHNIQUE#########################
###Escolha por adjr2

library(leaps)

#########################AGE 1
regfit.full_A1=regsubsets(Hospitalizations_100000~.,data=na.omit(data.train[data.train$Age.Range=="1",-c(1,3,4,18,19,21,20,6,10,11)]) ,nvmax=21,really.big=TRUE, method=c("exhaustive"))
reg.summary_A1=summary (regfit.full_A1)
best.fit_A1=reg.summary_A1$which[which.max(reg.summary_A1$adjr2),]
#RegionNorte, `O3 mean`, `O3 max`, `NOX as NO2 mean`, `PM2.5 max`, `CO mean`, `CO max`. #0.6759139
m.best.fit_A1<-lm(Hospitalizations_100000~.,data=data.train[data.train$Age.Range=="1",c(17,2,7,8,9,14,15,16)])

#########################AGE 5
regfit.full_A5=regsubsets(Hospitalizations_100000~.,data=na.omit(data.train[data.train$Age.Range=="5",-c(1,3,4,18,19,21, 20, 5,6, 10, 11, 13:16)]) ,nvmax=21,really.big=TRUE, method=c("exhaustive"))
reg.summary_A5=summary (regfit.full_A5)
best.fit_A5=reg.summary_A5$which[which.max(reg.summary_A5$adjr2),]
#RegionLVT, RegionMadeira, `O3 mean`, `O3 max`,`NOX as NO2 mean`  #0.5172925
m.best.fit_A5<-lm(Hospitalizations_100000~.,data=data.train[data.train$Age.Range=="5",c(17,2,7,8,9)]) 

#########################AGE 6
regfit.full_A6=regsubsets(Hospitalizations_100000~.,data=na.omit(data.train[data.train$Age.Range=="6",-c(1,3,4,18,19,21, 20, 5, 7, 13, 14, 9, 10)]) ,nvmax=21,really.big=TRUE, method=c("exhaustive"))
reg.summary_A6=summary (regfit.full_A6)
best.fit_A6=reg.summary_A6$which[which.max(reg.summary_A6$adjr2),]
#RegionLVT, RegionMadeira, `PM10 max`, `O3 max`, `CO mean` #0.5072599
m.best.fit_A6<-lm(Hospitalizations_100000~.,data=data.train[data.train$Age.Range=="6",c(17,2,6,8,15)]) 

#0.6653 #0.5111 #0.4956  MODEL CORRELATION 
#0.6569 #0.5315 #0.5164 MODEL CORRELATION + TRANSFORMATIONS
#0.6669 #0.5151 #0.5017 STEPWISE WITHOUT TRANSF
#0.66  #0.533  #0.4619  STEPWISE WITH CORR + TRANSF
#0.6759139 #0.5172925 #0.5072599 BESTSUBSETS WITHOUT TRANSF


#We will use the stepwise technique to do for GENDER WITH NA.OMIT
#We will use the stepwise technique to do for NA FILLED WITHOUT GENDER


##########################EVERYTHING THE SAME BUT WITH GENDER#############

#dataset with gender
data_with_g <- read_csv("dataset-finale-3-with-gender-no-covid.csv")

data_with_g <- na.omit(data_with_g)

#data_no_g$`PM10 max`[406] [397] [400] [403] OUTLIER
#delete row  793 796 799 932 802 805 808 811 814

data_with_g<-data_with_g[-814,]
data_with_g<-data_with_g[-811,]
data_with_g<-data_with_g[-808,]
data_with_g<-data_with_g[-805,]
data_with_g<-data_with_g[-802,]
data_with_g<-data_with_g[-799,]
data_with_g<-data_with_g[-796,]
data_with_g<-data_with_g[-793,]

#Data train and test
which(data_with_g$Period=='2020-02-01') #905
#data.test is the last month we have available data
data.train<-data_with_g[1:904,]
data.test <- data_with_g[905:928,]
data.train$Month<-as.factor(data.train$Month)
data.test$Month<-as.factor(data.test$Month)
data.train$Region<-as.factor(data.train$Region)
data.train$Age.Range<-as.factor(data.train$Age.Range)
data.train$Gender<-as.factor(data.train$Gender)


#####################################Model stepwise everything WITHOUT MONTH

##########################Age 1
###Male
model.null <- lm(Hospitalizations_100000~ 1,data=na.omit(data.train[data.train$Age.Range=="1" & data.train$Gender=="M",-c(1,5,19:22)]))
model <- lm(Hospitalizations_100000~.,data=na.omit(data.train[data.train$Age.Range=="1" & data.train$Gender=="M",-c(1,5,19:22)]))
lx<-step(model.null,scope = list(upper=model),direction="both",data=na.omit(data.train[data.train$Age.Range=="1" & data.train$Gender=="M",-c(1,5,19:22)]))
summary(lx) #0.5902 


###Female
model.null <- lm(Hospitalizations_100000~ 1,data=na.omit(data.train[data.train$Age.Range=="1" & data.train$Gender=="F",-c(1,5,19:22)]))
model <- lm(Hospitalizations_100000~.,data=na.omit(data.train[data.train$Age.Range=="1" & data.train$Gender=="F",-c(1,5,19:22)]))
lx<-step(model.null,scope = list(upper=model),direction="both",data=na.omit(data.train[data.train$Age.Range=="1" & data.train$Gender=="F",-c(1,5,19:22)]))
summary(lx) #0.5302  

#########################Age 5
###Male
model.null <- lm(Hospitalizations_100000~ 1,data=na.omit(data.train[data.train$Age.Range=="5" & data.train$Gender=="M",-c(1,5,19:22)]))
model <- lm(Hospitalizations_100000~.,data=na.omit(data.train[data.train$Age.Range=="5" & data.train$Gender=="M",-c(1,5,19:22)]))
lx<-step(model.null,scope = list(upper=model),direction="both",data=na.omit(data.train[data.train$Age.Range=="5" & data.train$Gender=="M",-c(1,5,19:22)]))
summary(lx)  #0.4023 

###Female
model.null <- lm(Hospitalizations_100000~ 1,data=na.omit(data.train[data.train$Age.Range=="5" & data.train$Gender=="F",-c(1,5,19:22)]))
model <- lm(Hospitalizations_100000~.,data=na.omit(data.train[data.train$Age.Range=="5" & data.train$Gender=="F",-c(1,5,19:22)]))
lx<-step(model.null,scope = list(upper=model),direction="both",data=na.omit(data.train[data.train$Age.Range=="5" & data.train$Gender=="F",-c(1,5,19:22)]))
summary(lx)  #0.3404 

########################Age 6
##Male
model.null <- lm(Hospitalizations_100000~ 1,data=na.omit(data.train[data.train$Age.Range=="6" & data.train$Gender=="M",-c(1,5,19:22)]))
model <- lm(Hospitalizations_100000~.,data=na.omit(data.train[data.train$Age.Range=="6" & data.train$Gender=="M",-c(1,5,19:22)]))
lx<-step(model.null,scope = list(upper=model),direction="both",data=na.omit(data.train[data.train$Age.Range=="6" & data.train$Gender=="M",-c(1,5,19:22)]))
summary(lx)  #0.4355 

##Female
model.null <- lm(Hospitalizations_100000~ 1,data=na.omit(data.train[data.train$Age.Range=="6" & data.train$Gender=="F",-c(1,5,19:22)]))
model <- lm(Hospitalizations_100000~.,data=na.omit(data.train[data.train$Age.Range=="6" & data.train$Gender=="F",-c(1,5,19:22)]))
lx<-step(model.null,scope = list(upper=model),direction="both",data=na.omit(data.train[data.train$Age.Range=="6" & data.train$Gender=="F",-c(1,5,19:22)]))
summary(lx)  #0.3947 


#0.6669 #0.5151 #0.5017 STEPWISE WITHOUT GENDERS

### The difference is not a lot, so we will procceed with the dataset with NAS filled
#for without genders.

#However, it is noticeable that the adjr2 for females is slightly lower than males
#one could conclude, therefore, that the air polution the hospitalizations of the
#male population better.


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
data.train<-data_no_g[1:588,]
data.test <- data_no_g[589:604,]
data.train$Month<-as.factor(data.train$Month)
data.test$Month<-as.factor(data.test$Month)
data.train$Region<-as.factor(data.train$Region)
data.train$Age.Range<-as.factor(data.train$Age.Range)


##############################Model stepwise everything WITHOUT MONTH

##########################Age 1
model.null <- lm(Hospitalizations_100000~ 1,data=na.omit(data.train[data.train$Age.Range=="1",-c(1,3,4,18:21)]))
model <- lm(Hospitalizations_100000~.,data=na.omit(data.train[data.train$Age.Range=="1",-c(1,3,4,18:21)]))
m2_1<-step(model.null,scope = list(upper=model),direction="both",data=na.omit(data.train[data.train$Age.Range=="1",-c(1,3,4,18:21)]))
summary(m2_1) #0.6838
 

#########################Age 5
model.null <- lm(Hospitalizations_100000~ 1,data=na.omit(data.train[data.train$Age.Range=="5",-c(1,3,4,18:21)]))
model <- lm(Hospitalizations_100000~.,data=na.omit(data.train[data.train$Age.Range=="5",-c(1,3,4,18:21)]))
m2_5<-step(model.null,scope = list(upper=model),direction="both",data=na.omit(data.train[data.train$Age.Range=="5",-c(1,3,4,18:21)]))
summary(m2_5)  #0.537  


########################Age 6
model.null <- lm(Hospitalizations_100000~ 1,data=na.omit(data.train[data.train$Age.Range=="6",-c(1,3,4,18:21)]))
model <- lm(Hospitalizations_100000~.,data=na.omit(data.train[data.train$Age.Range=="6",-c(1,3,4,18:21)]))
m2_6<-step(model.null,scope = list(upper=model),direction="both",data=na.omit(data.train[data.train$Age.Range=="6",-c(1,3,4,18:21)]))
summary(m2_6)  #0.5934  


#0.6669 #0.5151 #0.5017 STEPWISE WITHOUT GENDERS (m1_1, m1_5, m1_6)

#0.6838 #0.537 #0.5934  STEWPWISE WITH NAS FILLED WITHOUT GENDERS (m2_1, m2_5, m2_6)

