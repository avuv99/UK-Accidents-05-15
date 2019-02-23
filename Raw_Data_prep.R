rm(list = ls())
library('ggplot2')
#install.packages("psych")
library("psych")
#install.packages("tidyverse")
library("tidyverse")

#setwd("/Users/aohana1/Documents/Study/Semester_A_2018/Data Visualization/ShinyApp")
#save.image(file='Accidents_Vehic.RData')
load('Accidents_Vehic.RData')
# Load data files about UK accidents
#vehic <- read.csv("./Vehicles.csv", header = T)
#acc <- read.csv("./Accidents.csv", header = T) #Load all Accidents file
#Casualties <- read.csv("./Casualties.csv", header = T) #Load all Accidents file

####Removing columns of data which I'm not going to use
acc<- acc[-c(17,18,21:30)]
vehic<- vehic[-c(4:12,18,19,21,22)]
Casualties<- Casualties[-c(9,10,12,13,15)]

####Working on Accident data
#Adding new columns 
acc['Day']=weekdays(as.Date(acc$Date))
acc['Month']=months(as.Date(acc$Date))
acc['Year'] <- as.numeric(format(as.Date(acc$Date, format="%d/%m/%Y"),"%Y"))
acc['Day_Num'] <- format(as.Date(acc$Date, format="%d/%m/%Y"),"%d")
acc['Hour'] <-format(as.POSIXct(acc$Time,format="%H:%M"),"%H")


##Adding the Police category, rename and drop unneccessary columns
Police <- read.csv("./Police_Force.csv",header=T)
acc <-merge(acc,Police, by.x="Police_Force", by.y = "code", all.x=TRUE)
acc <- acc[-c(1)] #drop un needed column
colnames(acc)[colnames(acc)=="label"] <- "Police_Force" #Rename a column

##Updating fatality category
acc$Accident_Severity <- replace(acc$Accident_Severity,acc$Accident_Severity=="1", "Fatal")
acc$Accident_Severity <- replace(acc$Accident_Severity,acc$Accident_Severity=="2", "Serious")
acc$Accident_Severity <- replace(acc$Accident_Severity,acc$Accident_Severity=="3", "Slight")

#adding the Local Authority
local.Authority.District <- read.csv("./Local_Authority_District.csv",header=T)
acc <-merge(acc,local.Authority.District, by.x="Local_Authority_.District.", by.y = "code", all.x=TRUE)
acc <- acc[-c(1)] #drop un needed column
colnames(acc)[colnames(acc)=="label"] <- "Local_Authority_.District" #Rename a column

##Adding the Road type, rename and drop unneccessary columns
Road.Type <- read.csv("./Road_Type.csv",header=T)
acc <-merge(acc,Road.Type, by.x="Road_Type", by.y = "code", all.x=TRUE)
acc <- acc[-c(1)] #drop un needed column
colnames(acc)[colnames(acc)=="label"] <- "Road_Type"

# Updating X1st_Road_Class and X2
acc$X1st_Road_Class <- replace(acc$X1st_Road_Class,acc$X1st_Road_Class=="0", "Not at junction")
acc$X1st_Road_Class <- replace(acc$X1st_Road_Class,acc$X1st_Road_Class=="1", "Motorway")
acc$X1st_Road_Class <- replace(acc$X1st_Road_Class,acc$X1st_Road_Class=="2", "A(M)")
acc$X1st_Road_Class <- replace(acc$X1st_Road_Class,acc$X1st_Road_Class=="3", "A")
acc$X1st_Road_Class <- replace(acc$X1st_Road_Class,acc$X1st_Road_Class=="4", "B")
acc$X1st_Road_Class <- replace(acc$X1st_Road_Class,acc$X1st_Road_Class=="5", "C")
acc$X1st_Road_Class <- replace(acc$X1st_Road_Class,acc$X1st_Road_Class=="6", "Unclassified")

acc$X2nd_Road_Class <- replace(acc$X2nd_Road_Class,acc$X2nd_Road_Class=="0", "Not at junction")
acc$X2nd_Road_Class <- replace(acc$X2nd_Road_Class,acc$X2nd_Road_Class=="1", "Motorway")
acc$X2nd_Road_Class <- replace(acc$X2nd_Road_Class,acc$X2nd_Road_Class=="2", "A(M)")
acc$X2nd_Road_Class <- replace(acc$X2nd_Road_Class,acc$X2nd_Road_Class=="3", "A")
acc$X2nd_Road_Class <- replace(acc$X2nd_Road_Class,acc$X2nd_Road_Class=="4", "B")
acc$X2nd_Road_Class <- replace(acc$X2nd_Road_Class,acc$X2nd_Road_Class=="5", "C")
acc$X2nd_Road_Class <- replace(acc$X2nd_Road_Class,acc$X2nd_Road_Class=="6", "Unclassified")

###########
##Working on Casualty data
##Updating Casualty Class
Casualties$Casualty_Class <- replace(Casualties$Casualty_Class,Casualties$Casualty_Class=="1", "Driver or Rider")
Casualties$Casualty_Class <- replace(Casualties$Casualty_Class,Casualties$Casualty_Class=="2", "Passenger")
Casualties$Casualty_Class <- replace(Casualties$Casualty_Class,Casualties$Casualty_Class=="3", "Pedestrian")

###Working on Vehicle data
####analyzing the Age distribution
summary(vehic$Age_of_Driver)  # Summary for one variable
age <- (vehic$Age_of_Driver)
age<- age[age>=1]
summary(age)
quantile(vehic$Age_of_Driver, probs = c(0, 0.25, 0.5, 0.75, 1),na.rm = TRUE)

hist(age)
table(age) #Count of values
sort(unique(age)) # unique values, sorted
summary(vehic$Age_of_Vehicle)  # Summary for one variable

hist(age, 
     main="Histogram of Age distribution", 
     xlab="Age", 
     #ylim = c(0,60000),
     #axis(side=2, labels = (0,60000)),
     #breaks = 100,
     col="blue")
table(vehic$Age_of_Driver)
AgeHistogram <- ggplot(data = vehic, aes(x = Age_of_Driver)) + 
  geom_histogram(fill ="blue",col="darkblue",binwidth =5)+
  labs(title="Distribution - Age of Driver")+
  labs(x="Age", y="Count")+
  xlim (c(0,100))+
  ylim(c(0,400000))
AgeHistogram

###Age_Band                                                             
hist(vehic$Age_Band_of_Driver, 
     main="Histogram of Age band of driver distribution", 
     xlab="Age_Band", 
     #ylim = c(0,60000),
     #breaks = 100,
     col="blue")

####Gender
table(vehic$Sex_of_Driver)
summary(vehic$Sex_of_Driver)
gender <-vehic$Sex_of_Driver
gender <-subset(gender, gender==1 | gender==2)
table(gender)

###Engine capacity
table(vehic$Engine_Capacity_.CC.)

##Vehicle type
# vic.type <- read.csv("./Vehicle_Type.csv", header = T)
# vehic <-merge(vehic,vic.type, by.x="Vehicle_Type", by.y = "code", all.x=TRUE)
# table(vehic$label)
# vehic <- vehic[-c(1)] #drop un needed column
# colnames(vehic)[colnames(vehic)=="label"] <- "Vehicle_Type" #Rename a column

vehic$Vehicle_Type <- replace(vehic$Vehicle_Type,vehic$Vehicle_Type=="90", "24")
vehic$Vehicle_Type <- replace(vehic$Vehicle_Type,vehic$Vehicle_Type=="97", "25")
vehic$Vehicle_Type <- replace(vehic$Vehicle_Type,vehic$Vehicle_Type=="98", "26")
#vehic$vic.type.ordered <- factor(vehic$Vehicle_Type, levels=vehic$Vehicle_Type)

#ggplot(data=vehic, aes(x=reorder(Vehicle_Type,-table(vehic)[Vehicle_Type])))+geom_bar()
Vic.type.hist <- ggplot(data = vehic, aes(x =Vehicle_Type)) + 
  geom_histogram(fill ="blue",col="darkblue",binwidth =1,stat="count")+
  labs(title="Distribution - Types of Vehicles")+
  labs(x="Type", y="Count")
#theme(axis.text.x = element_text(face="bold", color="#993333", 
#                                size=14, angle=45))
Vic.type.hist

########
#Working on Caualties dta
#calculate the precntile of age casualty 
quantile(Casualties$Age_of_Casualty, probs = c(0, 0.25, 0.5, 0.75, 1),na.rm = TRUE)
AgeHistogram <- ggplot(data = Casualties, aes(x = Age_of_Casualty)) + 
  geom_histogram(fill ="blue",col="darkblue",binwidth =5)+
  labs(title="Distribution - Age of Driver")+
  labs(x="Age", y="Count")+
  xlim (c(0,100))+
  ylim(c(0,400000))
#scale_y_discrete(labels=c("20,000","100,000","400,000"))
AgeHistogram

####Gender of casualty
table(Casualties$Sex_of_Casualty)
gender <-Casualties$Sex_of_Casualty
gender <-subset(gender, gender==1 | gender==2)
table(gender)

###Casualty type analysis
Cas.Type <- read.csv("./Casualty_Type.csv",header = T)

sort(table(Casualties$Casualty_Type))

#####Merging all 3 data files in to one file
acc.vic <-merge(acc,vehic, by.x="ï..Accident_Index", by.y = "ï..Accident_Index", all.x=TRUE)
#sapply(acc.vic, function(x) sum(is.na(x)))

acc.vic.cas <-merge(acc.vic,Casualties, by.x="ï..Accident_Index", by.y = "ï..Accident_Index", all.x=TRUE)
#sapply(acc.vic, function(x) sum(is.na(x)))

###Saving this file to be input for application
write.csv(acc.vic.cas,"Accedint_Vehic_Cas.CSV")
################

