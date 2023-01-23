###### Libraries ####
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
library("openxlsx")
library("car")
library("bcmixed")
library("lmtest")
library("ggpubr")
library("moments")
library("asbio")
library("forecast")

## Data without genders
dat<- read_csv("dataset-finale-3-no-gender-no-covid.csv")

dat <- na.omit(dat)
###### See Data #####
library(psych)
x <- dat[which(dat$Age.Range==1),]
pairs.panels(x[,c(5,7,9,11,13,15,17)],density=TRUE,ellipses=FALSE,smooth=FALSE,lm=TRUE,cex=0.5) 
# most correlated: CO and NOx 

x <- dat[which(dat$Age.Range==5),]
pairs.panels(x[,c(5,7,9,11,13,15,17)],density=TRUE,ellipses=FALSE,smooth=FALSE,lm=TRUE,cex=0.5) # most correlated: NOx
# most correlated: NOx and NO then CO

x <- dat[which(dat$Age.Range==6),]
pairs.panels(x[,c(5,7,9,11,13,15,17)],density=TRUE,ellipses=FALSE,smooth=FALSE,lm=TRUE,cex=0.5) # most correlated: NOx
# most correlated: NO and NOx then CO

x <- dat[which(dat$Age.Range==7),]
pairs.panels(x[,c(5,7,9,11,13,15,17)],density=TRUE,ellipses=FALSE,smooth=FALSE,lm=TRUE,cex=0.5) # most correlated: NOx
# most correlated: NO and NOx then CO

### CONCLUSION: Discard NO since very correlated to NOx and NOx has more 
### correlation for most of the data (Age.Range 1 to 5)
### For ANOVA: Only consider CO and NOx

########### Prepare data ##########

# Set low, medium high pollution level
quants <- quantile(dat$`CO mean`, probs=c(0.33, 0.66))

dat$CO_lvl <- 0
dat$CO_lvl[which(dat$`CO mean`>quants[[1]])] <- 1
dat$CO_lvl[which(dat$`CO mean`>quants[[2]])] <- 2

quants <- quantile(dat$`NOX as NO2 mean`, probs=c(0.33, 0.66))

dat$NOx_lvl <- 0
dat$NOx_lvl[which(dat$`NOX as NO2 mean`>quants[[1]])] <- 1
dat$NOx_lvl[which(dat$`NOX as NO2 mean`>quants[[2]])] <- 2

hist(dat$CO_lvl)
hist(dat$NOx_lvl)
# Now obs all same
COlow_dat <- dat[which(dat$CO_lvl==0),]  # 152 Obs
COmedium_dat <- dat[which(dat$CO_lvl==1),] # 24 obs
COhigh_dat <- dat[which(dat$CO_lvl==2),] # 584 obs

NOxlow_dat <- dat[which(dat$NOx_lvl==0),]  # 712 Obs
NOxmedium_dat <- dat[which(dat$NOx_lvl==1),] # 48 obs
NOxhigh_dat <- dat[which(dat$NOx_lvl==2),] # 0 obs


##########################################################################
############################## ANOVA #########################################
#############################################################################

####### ASSUMPTIONS #########
# See Normality
# CO 
x <- dat[which(dat$CO_lvl==2 & dat$Age.Range ==1),]
hist((x$Hospitalizations_100000))
plot(density((x$Hospitalizations_100000)))
qqPlot((x$Hospitalizations_100000))
shapiro.test(log(x$Hospitalizations_100000))

x <- dat[which(dat$CO_lvl==1 & dat$Age.Range ==1),]
hist(log(x$Hospitalizations_100000))
plot(density(log(x$Hospitalizations_100000)))
qqPlot((x$Hospitalizations_100000))
shapiro.test(log(x$Hospitalizations_100000))

# Homosedacity (?)
for (age in c(1,5,6,7)){
  print(age)
  print("\n")
  print(var(dat[which(dat$CO_lvl==0 & dat$Age.Range==age),17]))
  print(var(dat[which(dat$CO_lvl==1 & dat$Age.Range==age),17]))
  print(var(dat[which(dat$CO_lvl==2 & dat$Age.Range==age),17]))
  print(var(dat[which(dat$NOx_lvl==0 & dat$Age.Range==age),17]))
  print(var(dat[which(dat$NOx_lvl==1 & dat$Age.Range==age),17]))
  print(var(dat[which(dat$NOx_lvl==2 & dat$Age.Range==age),17]))
  print("\n")
}
#### Results: ###
# Age Level  Pollutant  Var
# 1     0       CO  14.93749
# 1     1       CO  6.119996    ## Very different
# 1     2       CO  34.04195
# 1     0       NOx 28.78593
# 1     1       NOx 30.24947    ## similar
# 1     2       NOx NA

# 5     0       CO  0.6447983
# 5     1       CO  2.072735    ## similar
# 5     2       CO  1.970014
# 5     0       NOx 2.283967
# 5     1       NOx 0.4871316   ## somewhat similar
# 5     2       NOx NA
#
# 6     0       CO  5.789706 
# 6     1       CO  12.92746    ## 1 2 similar 
# 6     2       CO  13.14957
# 6     0       NOx 12.27722
# 6     1       NOx 9.895242    ## somehwat similar
# 6     2       NOx NA
#
# 7     0       CO  180.738
# 7     1       CO  89.613      ## very different
# 7     2       CO  308.641
# 7     0       NOx 272.267
# 7     1       NOx 441.445     ## very different
# 7     2       NOx NA




########################################################################
################### KRUSKAL ############################################
########################################################################
xd <-dat[which(dat$Age.Range==1),]
group_by(xd, "CO_lvl")
ggboxplot(xd, x = "CO_lvl", y = "Hospitalizations_100000", 
          color = "CO_lvl", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          ylab = "Hospitalizations", xlab = "CO Levels")

xd <-dat[which(dat$Age.Range==1),]
group_by(xd, "NOx_lvl")
ggboxplot(xd, x = "NOx_lvl", y = "Hospitalizations_100000", 
          color = "NOx_lvl", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          ylab = "Hospitalizations", xlab = "NOx Levels")


xd <-dat[which(dat$Age.Range==1),]
kruskal.test(Hospitalizations_100000 ~ CO_lvl, data = xd) # p-value = 1.18e-08

xd <-dat[which(dat$Age.Range==5),]
kruskal.test(Hospitalizations_100000 ~ CO_lvl, data = xd) #  p-value = 0.0494

xd <-dat[which(dat$Age.Range==6),]
kruskal.test(Hospitalizations_100000 ~ CO_lvl, data = xd) #  p-value = 0.001449



xd <-dat[which(dat$Age.Range==1),]
kruskal.test(Hospitalizations_100000 ~ NOx_lvl, data = xd) # p-value = 0.005112

xd <-dat[which(dat$Age.Range==5),]
kruskal.test(Hospitalizations_100000 ~ NOx_lvl, data = xd) # p-value = 3.985e-10

xd <-dat[which(dat$Age.Range==6),]
kruskal.test(Hospitalizations_100000 ~ NOx_lvl, data = xd) #p-value = 3.381e-11







############################ Same but NAs filled #################################
###############################################################################

dat<- read_csv("dataset-finale-3-no-NA.csv")
