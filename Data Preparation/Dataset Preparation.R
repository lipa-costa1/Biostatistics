
# set the working directory with the files
# setwd("C:/Users/passio/Desktop/2º Semestre 2021-22/Bioest/Projeto/Dataset Preparation 2")


# ========================================================================================
#                                  HOSPITALIZATIONS DATASET
# ========================================================================================

# read the data for hospitalizations
dataH <- openxlsx::read.xlsx("morbilidade-e-mortalidade-hospitalar-doenca-respiratoria.xlsx", 
                             colNames = TRUE, sep.names = " ", sheet = 1)
class(dataH)

tokeep = c("Período", 
           "Descrição Capítulo Diagnóstico ICD9CM/ICD10CMPCS", 
           "Região", 
           "Faixa Etária", 
           "Género", 
           "Internamentos",
           "Dias de Internamento",
           "Ambulatório",
           "Óbitos")

dataH = dataH[,tokeep]

# change the names to make it more friendly (and in English for Max)
library(data.table)
new = c("Period",
        "Diagnostic",
        "Health Region",
        "Age Range",
        "Gender",
        "Hospitalizations",
        "Duration of Hospitalizaions",
        "From Ambulance",
        "Deaths")
setnames(dataH, old = names(dataH), new = new)

dataH$Diagnostic[dataH$Diagnostic == "Doenças do aparelho respiratório"] = "Respiratory Disease"
dataH$`Health Region` = gsub("Região de Saúde ", "", dataH$`Health Region`)
dataH$`Health Region` = gsub("dos ", "", dataH$`Health Region`)
dataH$`Health Region` = gsub("do ", "", dataH$`Health Region`)
dataH$`Health Region` = gsub("da ", "", dataH$`Health Region`)

unique(dataH$`Health Region`)
# [1] "Açores"   "Alentejo" "Centro"   "LVT"      "Norte"   
# [6] "Madeira"  "Algarve" 

# get rid of the diagnostic variable -> we all know is Respiratory Disease
dataH = subset(dataH, select=-c(Diagnostic))

# get rid of records from 2022 -> there are only from January (only 5 years)
dataH = dataH[!substr(dataH$Period,1,4) == "2022",]

# renaming to just have 1 name
library(data.table)
new = c("Period", "Region", "Age Range", "Gender", "Hospitalizations", 
        "Duration Hospitalizations", "Ambulatory", "Deaths")
setnames(dataH, old = names(dataH), new = new)

# Unfortunately, in the new air pollution data we don't have açores and alentejo data
# So we dismiss records with that region :(
dataH = dataH[!dataH$Region == 'Açores',]
dataH = dataH[!dataH$Region == 'Alentejo',]



# ========================================================================================
#                                    AIR POLLUTION DATASET
# ========================================================================================

# Each .csv regards data for a certain pollutant and year
# We go for each .csv and fill the hospitalizations dataset accordingly
# we have a .txt with the .csv links for a certain city, with air pollution data

# make function that receives the .txt with .csv data for a city
# and associated region (we must tell the code which it is)
# didn't make a function per se because i want to see the variables pair by pair

# run each line pair and then the code below, IN ORDER

#FEITO
city = "Aveiro" 
region = "Centro"

#FEITO
city = "Braga" 
region =  "Norte"

#FEITO
city = "Coimbra" 
region =  "Centro"

#FEITO
city = "Faro" 
region =  "Algarve"

#FEITO
city = "Funchal" 
region =  "Madeira"

#FEITO
city = "Guimaraes" 
region =  "Norte"

#FEITO
city = "Lisboa" 
region =  "LVT"

#FEITO
city = "Paredes" 
region =  "Norte"

#FEITO
city = "Porto" 
region =  "Norte"

#FEITO
city = "Setubal" 
region =  "LVT"

#FEITO
city = "Sintra" 
region =  "LVT"

#FEITO
city = "Vila Franca de Xira" 
region =  "LVT"
  
### RUN FOR EACH PAIR OF CITY+REGION ###

# file 
filename <- paste(city, ".txt", sep = "")

# read the data for a certain city
datalinks <- read.table(filename)
AQ_data <- lapply(datalinks$V1, read.csv, encoding="UTF-8", check.names = FALSE)

# clean it
tokeep <- c("AirPollutant", 
           "Concentration", 
           "UnitOfMeasurement", 
           "DatetimeBegin",
           "DatetimeEnd")

AQ_data <- lapply(AQ_data, function(x) x <- x[, tokeep])

# see values of Air Pollutant for each data frame
lapply(AQ_data, function(x) unique(x$AirPollutant))
# in the link provided there are only 7 pollutants, so we dont discard any, like before

# see values for Unit of Measurement -> put everything in ug/m3 like before
lapply(AQ_data, function(x) unique(x$UnitOfMeasurement))

# adapt units if necessary
for(i in 1:length(AQ_data)) {
  
  if ('mg/m3' %in% unique(AQ_data[[i]]$UnitOfMeasurement)) { 
    AQ_data[[i]]$Concentration[AQ_data[[i]]$UnitOfMeasurement == 'mg/m3'] = 1000*AQ_data[[i]]$Concentration[AQ_data[[i]]$UnitOfMeasurement == 'mg/m3'] 
  }
  
  if ('ng/m3' %in% unique(AQ_data[[i]]$UnitOfMeasurement)) { 
    AQ_data[[i]]$Concentration[AQ_data[[i]]$UnitOfMeasurement == 'ng/m3'] = 0.001*AQ_data[[i]]$Concentration[AQ_data[[i]]$UnitOfMeasurement == 'ng/m3']
  }

  if ('count' %in% unique(AQ_data[[i]]$UnitOfMeasurement)) {
    AQ_data[[i]] = AQ_data[[i]][!AQ_data[[i]]$UnitOfMeasurement == 'count',]
  }

  if ('ug/m2/day' %in% unique(AQ_data[[i]]$UnitOfMeasurement)) {
    AQ_data[[i]] = AQ_data[[i]][!AQ_data[[i]]$UnitOfMeasurement == 'ug/m2/day',]
  }
  
}

# eliminate column of units
AQ_data <- lapply(AQ_data, function(x) x <- subset(x, select=-c(UnitOfMeasurement)))

# We want a monthly granularity so lets eliminate the hour
for(i in 1:length(AQ_data)){AQ_data[[i]]$DatetimeBegin <- substr(AQ_data[[i]]$DatetimeBegin, 1, 10) } 
for(i in 1:length(AQ_data)){AQ_data[[i]]$DatetimeEnd <- substr(AQ_data[[i]]$DatetimeEnd, 1, 10) } 

# see values for DateTime
# print(lapply(AQ_data, function(x) unique(x$DatetimeEnd)))

# see values of year for each dataset
lapply(AQ_data, function(x) unique(substr(x$DatetimeBegin, 1, 4)))

# create the columns with the pollutants already
# for a given city, there maybe not exist all pollutants
# so, after the first pair, we add the new ones
# This part it's not so efficient, but fuck it
if (city ==  "Aveiro" & region == "Centro") {
  
  pollutants = c()
  for(i in 1:length(AQ_data)) { 
    pollutant = unique(AQ_data[[i]]$AirPollutant)
    if (!pollutant %in% pollutants) {
      pollutants = c(pollutants, pollutant)  
    }
  }
  
  for(pollutant in pollutants) { 
    name_max = paste(pollutant, "max", sep = " ")
    name_sum = paste(pollutant, "sum", sep = " ")
    name_rec = paste(pollutant, "records", sep = " ")
    dataH[name_sum] = NA
    dataH[name_max] = NA
    dataH[name_rec] = NA
  }
}

if (!(city ==  "Aveiro" & region == "Centro")) { 
  new_pollutants = c()
  for(i in 1:length(AQ_data)) { 
    pollutant = unique(AQ_data[[i]]$AirPollutant)
    if (!pollutant %in% pollutants) {
      new_pollutants = c(new_pollutants, pollutant)  
    }
  }
  
  for(newpollutant in new_pollutants) { 
    name_max = paste(newpollutant, "max", sep = " ")
    name_sum = paste(newpollutant, "sum", sep = " ")
    name_rec = paste(pollutant, "records", sep = " ")
    dataH[name_sum] = NA
    dataH[name_max] = NA
    dataH[name_rec] = NA
    pollutants = c(pollutants, newpollutant)
  }
}

# merge the datasets
for(i in 1:length(AQ_data)) {
  
  pollutant = unique(AQ_data[[i]]$AirPollutant)
  yearvalue = substr(AQ_data[[i]]$DatetimeBegin[1], 1, 4)
  
  name_max = paste(pollutant, "max", sep = " ")
  name_sum = paste(pollutant, "sum", sep = " ")
  name_rec = paste(pollutant, "records", sep = " ")
  
  # get the monthly measure and add it to dataH
  # there may be no data for a given month and year
  
  for (month in 1:12) {
    
    if (month < 10) { month = paste("0", month, sep = "") }
    
    year_month = paste(yearvalue, month, sep = "-")
    monthvalues = AQ_data[[i]]$Concentration[substr(AQ_data[[i]]$DatetimeBegin, 1, 7) == year_month]
    
    if (length(monthvalues) != 0 & sum(is.na(monthvalues)) != length(monthvalues)) {
      
      max_value = max(monthvalues, na.rm = TRUE)
      sum_value = sum(monthvalues, na.rm = TRUE)
      records_value = length(monthvalues) - sum(is.na(monthvalues))
      
      # replace the monthly and region pollutant sum level and records

      old_values_sum = dataH[[name_sum]][dataH$Period == year_month & dataH$Region == region]
      old_values_max = dataH[[name_max]][dataH$Period == year_month & dataH$Region == region]
      old_values_records = dataH[[name_rec]][dataH$Period == year_month & dataH$Region == region]
      
      # if the cells have no previous value (first add), we just put the value there
      # put old_values for sum, max or records is equivalent
      if (sum(is.na(old_values_max)) == length(old_values_max)) {
        dataH[[name_sum]][dataH$Period == year_month & dataH$Region == region] <- sum_value
        dataH[[name_max]][dataH$Period == year_month & dataH$Region == region] <- max_value
        dataH[[name_rec]][dataH$Period == year_month & dataH$Region == region] <- records_value
      }
      
      # if the cells have a previous value, we do the new sum and new max and new records
      else {
        old_max = unique(old_values_max)
        old_sum = unique(old_values_sum)
        old_records = unique(old_values_records)
        
        new_max = max(old_max, max_value)
        new_sum = sum(old_sum, sum_value)
        new_records = sum(old_records, records_value)
        
        dataH[[name_sum]][dataH$Period == year_month & dataH$Region == region] <- new_sum
        dataH[[name_max]][dataH$Period == year_month & dataH$Region == region] <- new_max
        dataH[[name_rec]][dataH$Period == year_month & dataH$Region == region] <- new_records
      }

    }
  }
}


## AFTER RUNNING FOR ALL PAIRS CITY+REGION ##

# save data, since it took ages to run it all
save.image(".RDATA")

# do the mean values and change the columns names
# delete the records variable
for (pol in pollutants) {
  
  name_pol_sum = paste(pol, "sum", sep = " ")
  name_pol_rec = paste(pol, "records", sep = " ")
  
  dataH[[name_pol_sum]] = dataH[[name_pol_sum]] / dataH[[name_pol_rec]]
  
  new_name = paste(pol, "mean", sep = " ")
  names(dataH)[names(dataH) == name_pol_sum] <- new_name
  
  dataH = dataH[-c(which(colnames(dataH) == name_pol_rec))]
  
}

colSums(is.na(dataH))

#eliminate the pollutants with a lot of NAs
num = which(colnames(dataH) == "C6H6 mean")
dataH <- dataH[, 1:num-1]

colSums(is.na(dataH))

#eliminate SO2 and NO2 (still a lot of NAs)
dataH = dataH[-c(which(colnames(dataH) == "SO2 mean"))]
dataH = dataH[-c(which(colnames(dataH) == "SO2 max"))]
dataH = dataH[-c(which(colnames(dataH) == "NO2 mean"))]
dataH = dataH[-c(which(colnames(dataH) == "NO2 max"))]

colSums(is.na(dataH))

# ========================================================================================
#                                  PREPARATION FOR RESULTS
# ========================================================================================
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


data_y <- dataH
#View(data_y)

#Turn periods into dates
month <- data_y$Period
data_y$Period <- as.Date(paste(month,"-01",sep=""))

#Remove NA
data_y <- na.omit(data_y)

####Categorize the variables #Region, Age.Range, Gender (2,3,4)
#transformar factors
data_y$Region<-as.factor(data_y$Region) #Algarve Centro LVT Madeira Norte
data_y$Gender<-as.factor(data_y$Gender)

#remove I gender (there are 3)
remove<-which(data_y$Gender=="I") #7198 21061 28453
data_y<-data_y[-c(remove),]

#tranformar Age.Range para numerico por ordem

#onde tem "[0-1[" mete 1.
#onde tem "[1-5[" mete 2.
#onde tem "[5-15[" mete 3.
#onde tem "[15-25[" mete 4.
#onde tem "[25-45[" mete 5.
#onde tem "[45-65[" mete 6.
#onde tem "[65-120[" mete 7.


data_y$'Age Range'[data_y$'Age Range'=="[0-1["] <- 1
data_y$'Age Range'[data_y$'Age Range'=="[1-5["] <- 2
data_y$'Age Range'[data_y$'Age Range'=="[5-15["] <- 3
data_y$'Age Range'[data_y$'Age Range'=="[15-25["] <- 4
data_y$'Age Range'[data_y$'Age Range'=="[25-45["] <- 5
data_y$'Age Range'[data_y$'Age Range'=="[45-65["] <- 6
data_y$'Age Range'[data_y$'Age Range'=="[65-120["] <- 7


#remove the erro value
which(data_y$`Age Range`=="ERRO") #96
data_y<-data_y[-96,]

data_y$'Age Range'<-as.numeric(data_y$'Age Range')

#order by date
data_y<-data_y[order(data_y$Period),]

#View(data_y)

#Initially, we proceeded with a comparison of the
#number of new hospitalizations between 2017 and 2020,
#in order to understand whether or not they were constant in the
#years without COVID-19, and if with the existence
#of COVID-19 their numbers would change.

#variable named sum
#group it by region by month, counts the nr of hospitalizations

library("lubridate")
dates_seq<-seq(ymd('2017-01-01'),ymd('2020-12-01'),by='months')
regions_seq<-c("Algarve", "Centro", "LVT", "Madeira", "Norte")
colClasses = c("Date", "character", "numeric")
col.names = c("Date", "Region", "Sum_Hospitalizations")

df <- read.table(text = "",
                 colClasses = colClasses,
                 col.names = col.names)


for (i in 1:length(dates_seq)){
  for (j in 1:length(regions_seq)){
    sum_region<-sum(data_y$Hospitalizations[which(data_y$Period==dates_seq[i] & data_y$Region==regions_seq[j])])
    rownr<-nrow(df)
    df[rownr + 1,1] <- dates_seq[i]
    df[rownr + 1,2] <- regions_seq[j]
    df[rownr + 1,3] <- sum_region
  }
}

df[which(df$Sum_Hospitalizations==0),3] <- NA


#define data to plot
x <- dates_seq
y1 <- df[which(df$Region=="Algarve"),3]

#plot first line
plot(x, y1, type='l', col='red', xlab='x', ylab='y', ylim = c(0,5106))

listcol<-c("green", "grey", "black", "magenta", "blue")
#add second line to plot
for (i in 2:length(regions_seq)){
  y2 <- df[which(df$Region==regions_seq[i]),3]
  lines(x, y2, col=listcol[i-1])
}

### in the plot we can see that from march 2020 (start of covid)
#the hospitalizations decreased a lot and didnt follow the pattern of the last
#years. We can suppose its because of the ongoing pandemic (doen?as respiratorias sao marcadas como covid)

### we will analyse the data before the pandemic

#data with only date before covid (2020-03-01)
which(data_y$Period=='2020-03-01') #19802
data_new<-data_y[1:19801,]



#########################################################################

data_new<-na.omit(data_new)
data_new$Hospitalizations_100000 <- data_new$Hospitalizations*100000
data_new$Hospitalizations_100000[data_new$Region=="Algarve"]<-(data_new$Hospitalizations_100000[data_new$Region=="Algarve"])/438864
data_new$Hospitalizations_100000[data_new$Region=="Centro"]<-(data_new$Hospitalizations_100000[data_new$Region=="Centro"])/2217000
data_new$Hospitalizations_100000[data_new$Region=="LVT"]<-(data_new$Hospitalizations_100000[data_new$Region=="LVT"])/3500000
data_new$Hospitalizations_100000[data_new$Region=="Madeira"]<-(data_new$Hospitalizations_100000[data_new$Region=="Madeira"])/253945
data_new$Hospitalizations_100000[data_new$Region=="Norte"]<-(data_new$Hospitalizations_100000[data_new$Region=="Norte"])/3573000

dev.off()
plot(data_new$`Age Range`,data_new$Hospitalizations_100000)

summary(data_new[data_new$`Age Range`=="1",]$Hospitalizations_100000)
summary(data_new[data_new$`Age Range`=="2",]$Hospitalizations_100000)
summary(data_new[data_new$`Age Range`=="3",]$Hospitalizations_100000)
summary(data_new[data_new$`Age Range`=="4",]$Hospitalizations_100000)
summary(data_new[data_new$`Age Range`=="5",]$Hospitalizations_100000)
summary(data_new[data_new$`Age Range`=="6",]$Hospitalizations_100000)
summary(data_new[data_new$`Age Range`=="7",]$Hospitalizations_100000)

plot(data_new$Gender,data_new$Hospitalizations_100000)
summary(data_new[data_new$Gender=="M",]$Hospitalizations_100000)
summary(data_new[data_new$Gender=="F",]$Hospitalizations_100000)
##parecem se comportar da mesma forma, então vamos juntar o age.range 1,2,3,4
#e os genders F,M

#para o mesmo periodo, mesma região, mesmo género, juntar o age range(1,2,3,4) e gender(F,M)
#join by duration of hospitalizations mean, sum hospitalizations, sum ambulatory,
#sum deaths.


#nestas linhas vamos juntar os age ranges
data_new$`Age Range`[which(data_new$`Age Range`==1 | data_new$`Age Range`==2 | data_new$`Age Range`==3 | data_new$`Age Range`==4)]<-1
data_new$Gender<-1

##vamos ver os repetidos para depois juntar 
n_occur<-data.frame(table(data_new[,1:4]))
n_occur<-n_occur[n_occur$Freq>=1,]
n_occur['Hospitalizations']<-0
n_occur['Duration of Hospitalizations Mean']<-0
n_occur['Ambulatory']<-0
n_occur['Deaths']<-0
n_occur['PM10 mean']<-0
n_occur['PM10 max']<-0
n_occur['O3 mean']<-0
n_occur['O3 max']<-0
n_occur['NOX as NO2 mean']<-0
n_occur['NOX as NO2 max']<-0
n_occur['NO mean']<-0
n_occur['NO max']<-0
n_occur['PM2.5 mean']<-0
n_occur['PM2.5 max']<-0
n_occur['CO mean']<-0
n_occur['CO max']<-0



for (i in 1:nrow(n_occur)) {
  period<-levels(factor(n_occur[i,1]))
  region<-levels(factor(n_occur[i,2]))
  age<-levels(factor(n_occur[i,3]))
  
  duplicates<-which(data_new$Period==period & data_new$Region==region & data_new$'Age Range'==age)
  hosps<-0
  deaths<-0
  ambulatory<-0
  internamento_list<-list()   #<-vector("list",n_occur$Freq[i])
  
  
  
  last_pos<-0
  for (j in 1:length(duplicates)) {
    pos<-as.numeric(duplicates[j])
    hosps<-hosps+data_new$Hospitalizations[pos]
    deaths<-deaths+data_new$Deaths[pos]
    ambulatory<-ambulatory+data_new$Ambulatory[pos]
    internamento_list[j]<-data_new$'Duration Hospitalizations'[pos]
    
    last_pos<-pos
  }
  
  
  n_occur$'PM10 mean'[i]<-data_new$'PM10 mean'[last_pos]
  data_new$'PM10 mean'[last_pos]
  n_occur$'PM10 max'[i]<-data_new$'PM10 max'[last_pos]
  n_occur$'O3 mean'[i]<-data_new$'O3 mean'[last_pos]
  n_occur$'O3 max'[i]<-data_new$'O3 max'[last_pos]
  n_occur$'NOX as NO2 mean'[i]<-data_new$'NOX as NO2 mean'[last_pos]
  n_occur$'NOX as NO2 max'[i]<-data_new$'NOX as NO2 max'[last_pos]
  n_occur$'NO mean'[i]<-data_new$'NO mean'[last_pos]
  n_occur$'NO max'[i]<-data_new$'NO max'[last_pos]
  n_occur$'PM2.5 mean'[i]<-data_new$'PM2.5 mean'[last_pos]
  n_occur$'PM2.5 max'[i]<-data_new$'PM2.5 max'[last_pos]
  n_occur$'CO mean'[i]<-data_new$'CO mean'[last_pos]
  n_occur$'CO max'[i]<-data_new$'CO max'[last_pos]
  
  n_occur$Hospitalizations[i]<-hosps
  n_occur$Deaths[i]<-deaths
  n_occur$Ambulatory[i]<-ambulatory
  n_occur$'Duration of Hospitalizations Mean'[i]<-mean(unlist(internamento_list))
  
  
}


data_final<-n_occur

####### Adicionar variável hospitalizações por 1000 pessoas ########


#Algarve - 438 864
#Centro - 2 217 000
#LVT - 3 500 000
#Madeira - 253 945
#Norte - 3 573 000


####Hospitalizations
data_final$Hospitalizations_100000 <- data_final$Hospitalizations*100000
data_final$Hospitalizations_100000[data_final$Region=="Algarve"]<-(data_final$Hospitalizations_100000[data_final$Region=="Algarve"])/438864
data_final$Hospitalizations_100000[data_final$Region=="Centro"]<-(data_final$Hospitalizations_100000[data_final$Region=="Centro"])/2217000
data_final$Hospitalizations_100000[data_final$Region=="LVT"]<-(data_final$Hospitalizations_100000[data_final$Region=="LVT"])/3500000
data_final$Hospitalizations_100000[data_final$Region=="Madeira"]<-(data_final$Hospitalizations_100000[data_final$Region=="Madeira"])/253945
data_final$Hospitalizations_100000[data_final$Region=="Norte"]<-(data_final$Hospitalizations_100000[data_final$Region=="Norte"])/3573000

data_final<-data_final[,-c(4,5,6,8,9)]

##################Linear Models############

#order by date
data_final<-data_final[order(data_final$Period),]

#The data seems to depend on the months, not on the years, so we will add a new
#variable called month.

data_final$Month<-month(data_final$Period)

getSeason <- function(DATES) {
  WS <- as.Date("2012-12-01", format = "%Y-%m-%d") # Winter
  SE <- as.Date("2012-4-01",  format = "%Y-%m-%d") # Spring
  SS <- as.Date("2012-07-01",  format = "%Y-%m-%d") # Summer 
  FE <- as.Date("2012-10-01",  format = "%Y-%m-%d") # Fall
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

data_final$Season<-getSeason(data_final$Period)
#Season Winter <- 4
#Season Fall <- 3
#Season Spring <- 2
#Season Summer <- 1
data_final$Season[data_final$Season=="Winter"] <- 4
data_final$Season[data_final$Season=="Fall"] <- 3
data_final$Season[data_final$Season=="Spring"] <- 2
data_final$Season[data_final$Season=="Summer"] <- 1


data_final$Season<-as.numeric(data_final$Season)
data_final$Season<-as.factor(data_final$Season)

# dataH = subset(dataH, select=-c(`Duration of Hospitalizations Mean`))
# dataH = subset(dataH, select=-c(Ambulatory_100000))
# dataH = subset(dataH, select=-c(Deaths_100000))
# dataH = subset(dataH, select=-c(Month))
# dataH = subset(dataH, select=-c(Season))

# save the final dataset
write.csv(data_final, 
          "dataset-finale-3-no-gender-no-covid.csv", 
          row.names = FALSE, col.names = TRUE, fileEncoding = "UTF-8", sep = ",", dec = ".")
























