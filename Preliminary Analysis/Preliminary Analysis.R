
# set working directory -> change according to your pc
# setwd("C:/Users/passio/Desktop/2º Semestre 2021-22/Bioest/Projeto")

# read the data
datacovid = read.csv("dataset-finale-3-with-gender-with-covid.csv", encoding = 'UTF-8', check.names = FALSE)

### NAs patterns ####
i = 1
datacovid_NA = list()
for (col in colnames(datacovid)) {
  if ( sum(is.na(datacovid[[col]])) != 0 ) {
    datacovid_NA[[i]] = datacovid[is.na(datacovid[[col]]) == TRUE, ]
    i = i + 1
  }
}

for (i in 1:length(datacovid_NA)) { View(datacovid_NA[[i]])}







#### imputation NAs ####
# see NAs per column/Variable
colSums(is.na(datacovid)) # only have NAs for numeric variables

# fill the NA values for each column

for (col in colnames(datacovid)[6:17]) {
  
  for (region in unique(datacovid$Region)) {
      
      median_value = median(datacovid[[col]][datacovid$Region == region], na.rm = TRUE)
      datacovid[[col]][datacovid$Region == region & is.na(datacovid[[col]]) == TRUE] = median_value 
  }
}

# library(imputeTS)
# for (col in colnames(datacovid)[6:17]) {
# 
#   for (region in unique(datacovid$Region)) {
#       
#       if (col != 17 & region != "Algarve") {
#       datacovid[[col]][datacovid$Region == region] = na_seadec(datacovid[[col]][datacovid$Region == region], algorithm = "interpolation", find_frequency = TRUE) }
#     
#   }
# 
# }


# confirm that we don't have NAs
colSums(is.na(datacovid)) # CO for Algarve region has no values, for all periods of time

write.csv(data_final, 
          "dataset-finale-3-no-NA.csv", 
          row.names = FALSE, col.names = TRUE, fileEncoding = "UTF-8", sep = ",", dec = ".")





#### boxplots of the pollutants ####

#### MEAN ####
png("Boxplots_pollutants_mean.png", width = 3*3, height = 3*2, res = 300, units = 'in')
par(mfrow=c(2,3))

boxplot(datacovid$`PM10 mean`, main = "PM10 mean ", col = 'blue', cex.main=1.5, cex.names=1.5, cex.axis=1.5, cex.lab=1.5)# we keep the 2 values out of the box because they are reasonable 
grid(nx = NA, ny = NULL)
boxplot(datacovid$`O3 mean`, main = "O3 mean ", col = 'green', cex.main=1.5, cex.names=1.5, cex.axis=1.5, cex.lab=1.5)# keep them for the same reasons of the PM10 mean
grid(nx = NA, ny = NULL)
boxplot(datacovid$`NOX as NO2 mean`,main = "NOX as NO2 mean ", col = 'red', cex.main=1.5, cex.names=1.5, cex.axis=1.5, cex.lab=1.5)# keep them for the same reasons of the PM10 mean
grid(nx = NA, ny = NULL)
boxplot(datacovid$`NO mean`,main = "NO mean ", col = 'yellow', cex.main=1.5, cex.names=1.5, cex.axis=1.5, cex.lab=1.5)#keep them for the same reasons of the PM10 mean
grid(nx = NA, ny = NULL)
boxplot(datacovid$`PM2.5 mean`,main = "PM2.5 mean ", col = 'orange', cex.main=1.5, cex.names=1.5, cex.axis=1.5, cex.lab=1.5)# keep them for the same reasons of the PM10 mean
grid(nx = NA, ny = NULL)
boxplot(datacovid$`CO mean`,main = "CO mean ", col = 'purple', cex.main=1.5, cex.names=1.5, cex.axis=1.5, cex.lab=1.5)# keep them for the same reasons of the PM10 mean
grid(nx = NA, ny = NULL)

dev.off()

#### MAX ####
png("Boxplots_pollutants_max.png", width = 3*3, height = 3*2, res = 300, units = 'in')
par(mfrow=c(2,3))

boxplot(datacovid$`PM10 max`, main = "PM10 max ", col = 'blue', cex.main=1.5, cex.names=1.5, cex.axis=1.5, cex.lab=1.5)# we keep the 2 values out of the box because they are reasonable 
grid(nx = NA, ny = NULL)
boxplot(datacovid$`O3 max`, main = "O3 max ", col = 'green', cex.main=1.5, cex.names=1.5, cex.axis=1.5, cex.lab=1.5)# keep them for the same reasons of the PM10 max
grid(nx = NA, ny = NULL)
boxplot(datacovid$`NOX as NO2 max`,main = "NOX as NO2 max ", col = 'red', cex.main=1.5, cex.names=1.5, cex.axis=1.5, cex.lab=1.5)# keep them for the same reasons of the PM10 max
grid(nx = NA, ny = NULL)
boxplot(datacovid$`NO max`,main = "NO max ", col = 'yellow', cex.main=1.5, cex.names=1.5, cex.axis=1.5, cex.lab=1.5)#keep them for the same reasons of the PM10 max
grid(nx = NA, ny = NULL)
boxplot(datacovid$`PM2.5 max`,main = "PM2.5 max ", col = 'orange', cex.main=1.5, cex.names=1.5, cex.axis=1.5, cex.lab=1.5)# keep them for the same reasons of the PM10 max
grid(nx = NA, ny = NULL)
boxplot(datacovid$`CO max`,main = "CO max ", col = 'purple', cex.main=1.5, cex.names=1.5, cex.axis=1.5, cex.lab=1.5)# keep them for the same reasons of the PM10 mean
grid(nx = NA, ny = NULL)

dev.off()







#### pollutant levels over time ####
# the age range and gender we choose is irrelevant because levels are the same
# and we have no NAs 

#### MEAN ####
time <- seq.POSIXt(from = as.POSIXct("2017-01-01",format="%Y-%m-%d",tz="WET"), length.out = 60, by = "1 month")

pollutants_means = c(colnames(datacovid)[6],
                         colnames(datacovid)[8],
                         colnames(datacovid)[10],
                         colnames(datacovid)[12],
                         colnames(datacovid)[14],
                         colnames(datacovid)[16])

png("Behavior_time_pollutants_mean.png", width = 4*3, height = 3*2, res = 300, units = 'in')
par(mfrow=c(2,3))


for (pol in pollutants_means) {
  
  pollutant_data_all_regions = datacovid[[pol]][datacovid$Age.Range == 1 & datacovid$Gender == 'F']
  plot(time, datacovid[datacovid$Region=="Norte" & datacovid$Age.Range == 1 & datacovid$Gender == 'F', 6], type = "l", ylim=c(0,max(pollutant_data_all_regions, na.rm = TRUE)), xlab="Year", ylab="Air pollution level (ug/m3)", col="white", main = pol, cex.main=1.5, cex.names=1.5, cex.axis=1.5, cex.lab=1.5)
  rect(as.POSIXct("2020-03-01", format="%Y-%m-%d",tz="WET"), 0, as.POSIXct("2021-02-01", format="%Y-%m-%d",tz="WET"), max(pollutant_data_all_regions, na.rm = TRUE), col="#F0F0F0", border="#f0f0f0")
  if (pol == "NOX as NO2 mean") {legend(x ="topright", legend=c("Norte", "LVT", "Centro", "Algarve", "Madeira"), col = c("#fa481b","#eb7be2","#3f58fc", "#02ded7", "#279e03"), lty=c(1,1,1,1,1)) }
  if (pol != "CO mean") {
    grid(col = 'gray')
    print(pol)
    
    lines(time,datacovid[[pol]][datacovid$Region=="Norte" & datacovid$Age.Range == 1 & datacovid$Gender == 'F'], col="#fa481b")
    lines(time,datacovid[[pol]][datacovid$Region=="LVT" & datacovid$Age.Range == 1 & datacovid$Gender == 'F'], col="#eb7be2")
    lines(time,datacovid[[pol]][datacovid$Region=="Centro" & datacovid$Age.Range == 1 & datacovid$Gender == 'F'], col="#3f58fc")
    lines(time,datacovid[[pol]][datacovid$Region=="Algarve" & datacovid$Age.Range == 1 & datacovid$Gender == 'F'], col="#02ded7")
    lines(time,datacovid[[pol]][datacovid$Region=="Madeira" & datacovid$Age.Range == 7 & datacovid$Gender == 'F'], col="#279e03")
  }
  else {
    grid(col = 'gray')
    print(pol)
    
    lines(time,datacovid[[pol]][datacovid$Region=="Norte" & datacovid$Age.Range == 1 & datacovid$Gender == 'F'], col="#fa481b")
    lines(time,datacovid[[pol]][datacovid$Region=="LVT" & datacovid$Age.Range == 1 & datacovid$Gender == 'F'], col="#eb7be2")
    lines(time,datacovid[[pol]][datacovid$Region=="Centro" & datacovid$Age.Range == 1 & datacovid$Gender == 'F'], col="#3f58fc")
    lines(time,datacovid[[pol]][datacovid$Region=="Madeira" & datacovid$Age.Range == 7 & datacovid$Gender == 'F'], col="#279e03")
  }
}

dev.off()

#### MAX ####
pollutants_max = c(colnames(datacovid)[7],
                     colnames(datacovid)[9],
                     colnames(datacovid)[11],
                     colnames(datacovid)[13],
                     colnames(datacovid)[15],
                     colnames(datacovid)[17])

png("Behavior_time_pollutants_max.png", width = 4*3, height = 3*2, res = 300, units = 'in')
par(mfrow=c(2,3))


for (pol in pollutants_max) {
  
  pollutant_data_all_regions = datacovid[[pol]][datacovid$Age.Range == 1 & datacovid$Gender == 'F']
  plot(time, datacovid[datacovid$Region=="Norte" & datacovid$Age.Range == 1 & datacovid$Gender == 'F', 6], type = "l", ylim=c(0,max(pollutant_data_all_regions, na.rm = TRUE)), xlab="Year", ylab="Air pollution level (ug/m3)", col="white", main = pol, cex.main=1.5, cex.names=1.5, cex.axis=1.5, cex.lab=1.5)
  rect(as.POSIXct("2020-03-01", format="%Y-%m-%d",tz="WET"), 0, as.POSIXct("2021-02-01", format="%Y-%m-%d",tz="WET"), max(pollutant_data_all_regions, na.rm = TRUE), col="#F0F0F0", border="#f0f0f0")
  if (pol == "NOX as NO2 max") {legend(x ="topright", legend=c("Norte", "LVT", "Centro", "Algarve", "Madeira"), col = c("#fa481b","#eb7be2","#3f58fc", "#02ded7", "#279e03"), lty=c(1,1,1,1,1)) }
  if (pol != "CO mean") {
    grid(col = 'gray')
    print(pol)
    
    lines(time,datacovid[[pol]][datacovid$Region=="Norte" & datacovid$Age.Range == 1 & datacovid$Gender == 'F'], col="#fa481b")
    lines(time,datacovid[[pol]][datacovid$Region=="LVT" & datacovid$Age.Range == 1 & datacovid$Gender == 'F'], col="#eb7be2")
    lines(time,datacovid[[pol]][datacovid$Region=="Centro" & datacovid$Age.Range == 1 & datacovid$Gender == 'F'], col="#3f58fc")
    lines(time,datacovid[[pol]][datacovid$Region=="Algarve" & datacovid$Age.Range == 1 & datacovid$Gender == 'F'], col="#02ded7")
    lines(time,datacovid[[pol]][datacovid$Region=="Madeira" & datacovid$Age.Range == 7 & datacovid$Gender == 'F'], col="#279e03")
  }
  else {
    grid(col = 'gray')
    print(pol)
    
    lines(time,datacovid[[pol]][datacovid$Region=="Norte" & datacovid$Age.Range == 1 & datacovid$Gender == 'F'], col="#fa481b")
    lines(time,datacovid[[pol]][datacovid$Region=="LVT" & datacovid$Age.Range == 1 & datacovid$Gender == 'F'], col="#eb7be2")
    lines(time,datacovid[[pol]][datacovid$Region=="Centro" & datacovid$Age.Range == 1 & datacovid$Gender == 'F'], col="#3f58fc")
    lines(time,datacovid[[pol]][datacovid$Region=="Madeira" & datacovid$Age.Range == 7 & datacovid$Gender == 'F'], col="#279e03")
  }
}

dev.off()







#### boxplots of the hospt. blablabla (the dataset from SNS) ####
png("Boxplots_hospt_2.png", width = 3*2, height = 3*2, res = 300, units = 'in')
par(mfrow=c(1,1))

boxplot(datacovid$`Duration of Hospitalizations Mean`,main = "Duration of Hospitalizations Mean Boxplot", col = 'lightblue', cex.main=0.8, cex.names=1.5, cex.axis=1.5, cex.lab=1.5)# has outliers but we need to keep them because the duration is important as it is in days 
grid(nx = NA, ny = NULL)
boxplot(datacovid$Hospitalizations_100000,main = "Hospitalizations_100000 Boxplot", col = 'lightcoral', cex.main=1.0, cex.names=1.5, cex.axis=1.5, cex.lab=1.5)#keep them for the same reasons of the PM10 mean
grid(nx = NA, ny = NULL)
boxplot(datacovid$Ambulatory_100000,main = "Ambulatory_100000 Boxplot", col = 'lightgreen', cex.main=1.0, cex.names=1.5, cex.axis=1.5, cex.lab=1.5)#keep them for the same reasons of the PM10 mean
grid(nx = NA, ny = NULL)
boxplot(datacovid$Deaths_100000,main = "Deaths_100000 Boxplot", col = 'lightyellow', cex.main=1.0, cex.names=1.5, cex.axis=1.5, cex.lab=1.5)#keep them for the same reasons of the PM10 mean
grid(nx = NA, ny = NULL)

dev.off()






#### hospt. variables over time ####
# since they depend on age range and gender, lets go to other dataset (no gender)

names = c(colnames(datacovid)[5],
          colnames(datacovid)[18],
          colnames(datacovid)[19],
          colnames(datacovid)[20])

colors = c("#02ded7","#3f58fc","#eb7be2","#279e03","#fa481b")
titles = c('Duration', 'Hospitalization','Ambulatory','Deaths')
ytitles = c('Duration Hospitalizations (days)', 'Hospitalization per 100 000 habitants', 'Ambulatory per 100 000 habitants', 'Deaths per 100 000 habitants')

for (i in unique(datacovid$Age.Range)) {
  
  for (j in unique(datacovid$Gender)) {
    
    name_file = paste("Behavior_time_hospt_AGE-RAGE-",i,"_GENDER-",j,".png",sep = "")
    png(name_file, width = 4*2, height = 3*2, res = 300, units = 'in')
    par(mfrow=c(2,2))
    
    aux1 = 1
    for (name in names) {
      
      hospt_data_all_regions = datacovid[[name]][datacovid$Age.Range == i & datacovid$Gender == j]
      plot(time, datacovid[[name]][datacovid$Region=="Norte" & datacovid$Age.Range == i & datacovid$Gender == j], type = "l", ylim=c(0,max(hospt_data_all_regions, na.rm = TRUE)), xlab="Year", ylab=ytitles[aux1], col="white", main = titles[aux1], cex.main=1.5, cex.names=1.5, cex.axis=1.5, cex.lab=0.8)
      rect(as.POSIXct("2020-03-01", format="%Y-%m-%d",tz="WET"), 0, as.POSIXct("2021-02-01", format="%Y-%m-%d",tz="WET"), max(hospt_data_all_regions, na.rm = TRUE), col="#F0F0F0", border="#f0f0f0")
      if (name == 'Hospitalizations_100000') {legend(x="topright", legend=c("Norte", "LVT", "Centro", "Algarve", "Madeira"), col = c("#fa481b","#eb7be2","#3f58fc", "#02ded7", "#279e03"), lty=c(1,1,1,1,1), cex = 0.7)}
      grid()
      
      print(name)
      
      aux2 = 1
      for (region in unique(datacovid$Region)) {
        data = datacovid[[name]][datacovid$Region==region & datacovid$Age.Range == i & datacovid$Gender == j]
        median_value = median(datacovid[[name]][datacovid$Region==region & datacovid$Age.Range == i & datacovid$Gender == j])
        while (length(data) != 60) { data[length(data) + 1] = median_value  }
        lines(time, data, col=colors[aux2])
        aux2 = aux2 + 1
      }
      aux1 = aux1 + 1
    }
    
    dev.off()
    
  }
  
}






























#### correlation values ####
data_corr = datacovid[c(5:20)]

data_corr$`CO mean`[is.na(data_corr$`CO mean`)] = median(data_corr$`CO mean`, na.rm = TRUE)
data_corr$`CO max`[is.na(data_corr$`CO max`)] = median(data_corr$`CO max`, na.rm = TRUE)

cor(data_corr, method = "spearman")
library(corrplot)
png("Correlation.png", width = 12, height = 12, res = 300, units = 'in')
corrplot(cor(data_corr), method = 'number',type = 'upper', diag = TRUE)
dev.off()


### scatterplots ####
data_scat = datacovid
data_scat$`CO mean`[is.na(data_scat$`CO mean`)] = median(data_scat$`CO mean`, na.rm = TRUE)
data_scat$`CO max`[is.na(data_scat$`CO max`)] = median(data_scat$`CO max`, na.rm = TRUE)
### MEAN ####
data_scat = data_scat[c(2,5:6,8,10,12,14,16,18,19,20)]
data_scat = data_scat[c(2:11)]
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 19)
}

png("Scatterplot Matrix mean.png", width = 10*2, height = 10*2, res = 300, units = 'in')
# Create the plots
pairs(data_scat, upper.panel = panel.cor, lower.panel = upper.panel)
dev.off()

### MAX ####
data_scat = data_scat[c(5,7,9,11,13,15,17,18,19,20)]
png("Scatterplot Matrix max.png", width = 10*2, height = 10*2, res = 300, units = 'in')
# Create the plots
pairs(data_scat, upper.panel = panel.cor, lower.panel = upper.panel)
dev.off()
### correlation plots ####
library(psych)
data_scat = datacovid
data_scat$`CO mean`[is.na(data_scat$`CO mean`)] = median(data_scat$`CO mean`, na.rm = TRUE)
data_scat$`CO max`[is.na(data_scat$`CO max`)] = median(data_scat$`CO max`, na.rm = TRUE)
### MEAN ####
data_scat = data_scat[c(2,5:6,8,10,12,14,16,18,19,20)]
data_scat = data_scat[c(2:11)]
png("Correlation Plot Matrix mean.png", width = 10*2, height = 10*2, res = 300, units = 'in')
pairs.panels(data_scat,density=TRUE,ellipses=FALSE,smooth=FALSE,lm=TRUE,cex=0.5)
dev.off()
### MAX ####
data_scat = datacovid
data_scat$`CO mean`[is.na(data_scat$`CO mean`)] = median(data_scat$`CO mean`, na.rm = TRUE)
data_scat$`CO max`[is.na(data_scat$`CO max`)] = median(data_scat$`CO max`, na.rm = TRUE)
data_scat = data_scat[c(5,7,9,11,13,15,17,18,19,20)]
png("Correlation Plot Matrix max.png", width = 10*2, height = 10*2, res = 300, units = 'in')
pairs.panels(data_scat,density=TRUE,ellipses=FALSE,smooth=FALSE,lm=TRUE,cex=0.5)
dev.off()







### correlation without covid per age range ####
### without gender ####
data1 = read.csv('dataset-finale-3-no-gender.csv', encoding = 'UTF-8', check.names = FALSE)

colSums(is.na(data1)) # only have NAs for numeric variables

# fill the NA values for each column

for (col in colnames(data1)[5:16]) {
  
  for (region in unique(data1$Region)) {
    
    data1[[col]][data1$Region == region & is.na(data1[[col]]) == TRUE] = median(data1[[col]][data1$Region == region], na.rm = TRUE) 
  }
}

colSums(is.na(data1)) 

# fill the CO and PM10 mean values
data1$`CO mean`[is.na(data1$`CO mean`)] = median(data1$`CO mean`, na.rm = TRUE)
data1$`CO max`[is.na(data1$`CO max`)] = median(data1$`CO max`, na.rm = TRUE)

colSums(is.na(data1)) # no NAs

library(corrplot)
for (age in unique(data1$Age.Range)) {
  data_corr = data1[data1$Age.Range == age, c(4:19)]
  name_file = paste("Correlation_no-covid_no-gender_AGE-RANGE-", age, ".png", sep = '')
  png(name_file, width = 12, height = 12, res = 300, units = 'in')
  corrplot(cor(data_corr), method = 'number',type = 'upper', diag = TRUE)
  dev.off()
  
}

### with gender ####
data1 = read.csv('dataset-finale-3-no-covid.csv', encoding = 'UTF-8', check.names = FALSE)

colSums(is.na(data1)) # only have NAs for numeric variables

# fill the NA values for each column

for (col in colnames(data1)[6:17]) {
  
  for (region in unique(data1$Region)) {
    
    data1[[col]][data1$Region == region & is.na(data1[[col]]) == TRUE] = median(data1[[col]][data1$Region == region], na.rm = TRUE) 
  }
}

colSums(is.na(data1)) 

# fill the CO and PM10 mean values
data1$`CO mean`[is.na(data1$`CO mean`)] = median(data1$`CO mean`, na.rm = TRUE)
data1$`CO max`[is.na(data1$`CO max`)] = median(data1$`CO max`, na.rm = TRUE)

colSums(is.na(data1)) # no NAs

library(corrplot)
for (age in unique(data1$Age.Range)) {
  for (gender in unique(data1$Gender)) {
    data_corr = data1[data1$Age.Range == age & data1$Gender == gender, c(5:20)]
    name_file = paste("Correlation_no-covid_AGE-RANGE-", age,"_GENDER-",gender,".png", sep = '')
    png(name_file, width = 12, height = 12, res = 300, units = 'in')
    corrplot(cor(data_corr), method = 'number',type = 'upper', diag = TRUE)
    dev.off()
  }
}  
































### hospt. variables over time NO GENDER ####
# read the data
datacovid = read.csv("dataset-finale-3-no-gender-with-covid.csv", encoding = 'UTF-8', check.names = FALSE)

# imputation NAs 
# see NAs per column/Variable
colSums(is.na(datacovid)) # only have NAs for numeric variables

# fill the NA values for each column

for (col in colnames(datacovid)[5:16]) {
  
  for (region in unique(datacovid$Region)) {
    
    median_value = median(datacovid[[col]][datacovid$Region == region], na.rm = TRUE)
    datacovid[[col]][datacovid$Region == region & is.na(datacovid[[col]]) == TRUE] = median_value 
  }
}



names = c(colnames(datacovid)[4],
          colnames(datacovid)[17],
          colnames(datacovid)[18],
          colnames(datacovid)[19])

name = 'Hospitalizations_100000'

colors = c("#02ded7","#3f58fc","#eb7be2","#279e03","#fa481b")
titles = c('Hospt. Age [0,25[', 2,3,4,'Hospt. Age [25-45[', 'Hospt. Age [45-65[', 'Hospt. Age [65-120[')
ytitles = c('Hospitalization per 100 000 habitants')

name_file = paste("Behavior_time_hospt_AGE-RAGES",".png",sep = "")
png(name_file, width = 4*2, height = 3*2, res = 300, units = 'in')
par(mfrow=c(2,2))

for (i in unique(datacovid$Age.Range)) {
      
    hospt_data_all_regions = datacovid[[name]][datacovid$Age.Range == i]
    plot(time, datacovid[[name]][datacovid$Region=="Norte" & datacovid$Age.Range == i], type = "l", ylim=c(0,max(hospt_data_all_regions, na.rm = TRUE)), xlab="Year", ylab=ytitles[1], col="white", main = titles[i], cex.main=1.5, cex.names=1.5, cex.axis=1.5, cex.lab=0.8)
    rect(as.POSIXct("2020-03-01", format="%Y-%m-%d",tz="WET"), 0, as.POSIXct("2021-02-01", format="%Y-%m-%d",tz="WET"), max(hospt_data_all_regions, na.rm = TRUE), col="#F0F0F0", border="#f0f0f0")
    if (i == 5) {legend(x="topright", legend=c("Norte", "LVT", "Centro", "Algarve", "Madeira"), col = c("#fa481b","#eb7be2","#3f58fc", "#02ded7", "#279e03"), lty=c(1,1,1,1,1), cex = 0.7)}
    grid()
    
    print(name)
      
    aux2 = 1
    for (region in unique(datacovid$Region)) {
      data = datacovid[[name]][datacovid$Region==region & datacovid$Age.Range == i]
      median_value = median(datacovid[[name]][datacovid$Region==region & datacovid$Age.Range == i])
      while (length(data) != 60) { data[length(data) + 1] = median_value  }
      lines(time, data, col=colors[aux2])
      aux2 = aux2 + 1
    }
}
    
dev.off()
  