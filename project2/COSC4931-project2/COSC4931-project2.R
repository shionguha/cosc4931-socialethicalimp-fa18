#  Program:               COSC4931_Project2
#  Version:               2.0
#  Note:                  This code is an amalgamation of several individuals' work and should not run be expected to run directly/concisely. 
#  Author:                
#  Date Created:          8 November 2018
#  Date Last Modified:    26 November 2018
#  Purpose:               Clean and analyze data within the city of Boston to illuminate 
#                         biases in pothole locations. 
#  Versioning Plan:       1.0: Data may be opened and viewed
#                         1.1: Clean and merged datasets
#                         1.2: Geographic visualization
#                         1.3: Base statistic visualization
#                         1.4: Multinomial Logistic Regression
#                         2.0: Final project deliverable
#  External Datasets:     "Closed_Pothole_Cases__Boston__MA_.csv"
#                         "commuting.csv"
#                         "housingtenure.csv"
#                         "MedianIncome.csv"
#                         "raceandorethnicity.csv"
#                         "BostonNeighborhoods.csv"
#                         "mergepotholedatacleaned.csv"    
#  Final Dataset:         FILENAME = "finalDF.csv"
#


# SETUP
#
#Package installations if needed
# install.packages("geosphere")
# install.packages("tidyverse")
# install.packages("mice")
# install.packages("dplyr")
# install.packages("maps")
# install.packages("ggmap")
# install.packages("maptools")
# install.packages("doparallel")
# install.packages("foreach")
# install.packages("tidyverse")
# install.packages("sf")
# install.packages("sp")
# install.packages("maptools")
# install.packages("OpenStreetMap")
# install.packages("elevatr")
# install.packages("corrplot")
# install.packages("revgeo")
# install.packages("tidycensus")
# install.packages("viridis")
# install.packages("nnet")
# install.packages ("glm2")
# install.packages("foreign")
# install.packages ("purr")
# install.packages ("mlbench")
# install.packages('e1071', dependencies=TRUE)
# install.packages ("rpart")
# install.packages("rpart.plot")
# install.packages("neuralnet")
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("Rcpp")
# install.packages("devtools")
# install.packages("stringr")
# install.packages("lattice")
# install.packages("glmnet")
# install.packages("glmnetUtils")
# install.packages("ROCR")
# install.packages("gplots")
# install.packages("tidyr")
# install.packages("reshape")
# install.packages("ROCR")
# install.packages("neuralnet")
# install.packages("rpart.plot")
# install.packages("mlbench")
#
#IF ERRORS OCCUR FOR sf PACKAGE
#On Mac OSX
#brew unlink gdal
#brew tap osgeo/osgeo4mac && brew tap --repair
#brew install proj geos udunits
#brew install gdal2 --with-armadillo --with-complete --with-libkml --with-unsupported
#brew link --force gdal2

#On Ubuntu 16.04
#sudo apt-get install python-setuptools python-dev build-essential
#sudo apt install gdal-bin python-gdal python3-gdal
#sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable
# apt-get update
#sudo apt-get install libudunits2-dev libgdal-dev libgeos-dev libproj-dev 

# On a RHL Based Distro
# dnf install proj-devel proj-epsg udunits2-devel geos-devel gdal-devel
# install.packages('udunits2', type = 'source', repo = 'cran.rstudio.com', configure.args='--with-udunits2-include=/usr/include/udunits2')

#Library imports
library(tidyverse)
library(reshape2)
library(geosphere)
library(dplyr)
library(maps)
library(ggmap)
library(mice)
library(sf)
library(sp)
library(maptools)
library(rgdal)
library(OpenStreetMap)
library(scales)
library(lubridate)
library(raster)
library(elevatr)
library(corrplot)
library(revgeo)
library(tidycensus)
library(viridis)
library(gplots)
library(purrr)
library(tidyr)
library(ggplot2)
library(ROCR)
library(glm2)
library(nnet)
library(lattice)
library(foreign)
library(reshape)
library(glmnetUtils)
library(glmnet)
library(stringr)
library(Rcpp)
library(ggplot2)
library(mlbench)
library(lubridate)
library(tibble)
library(e1071)
library(rpart)
library(rpart.plot)
library(neuralnet)
library(ROCR)
library(nnet)

#Section 1 - Data import and initial clean/visualization
potholedata <- read_csv("Closed_pothole_cases.csv")
age<-read_csv("age.csv")
commuting<-read_csv("commuting.csv")
housingtenure<-read_csv("housingtenure.csv")
MedianIncome<-read_csv("MedianIncome.csv")
raceandorethnicity<-read_csv("raceandorethnicity.csv")
BostonNeighborhoods<-read_csv("BostonNeighborhoods.csv")
potholedatamerge<-read_csv("mergepotholedatacleaned.csv")
as_tibble(potholedata)
as_tibble(age)
as_tibble(commuting)
as_tibble(housingtenure)
as_tibble(MedianIncome)
as_tibble(raceandorethnicity)
as_tibble(BostonNeighborhoods)
as_tibble(potholedatamerge)
View(potholedatamerge)
View(potholedata)
View(age)
View(commuting)
View(housingtenure)
View(MedianIncome)
View(raceandorethnicity)
View(BostonNeighborhoods)
potholedatamerge <-mutate(potholedatamerge <- merge(potholedata, age, by ="Neighborhood"))
potholedatamerge <-mutate(potholedatamerge <- merge(potholedatamerge, commuting, by ="Neighborhood"))
potholedatamerge <-mutate(potholedatamerge <- merge(potholedatamerge, housingtenure, by ="Neighborhood"))
potholedatamerge <-mutate(potholedatamerge <- merge(potholedatamerge, MedianIncome, by ="Neighborhood"))
potholedatamerge <-mutate(potholedatamerge <- merge(potholedatamerge, raceandorethnicity, by ="Neighborhood"))
potholedatamerge <-mutate(potholedatamerge <- merge(potholedatamerge, BostonNeighborhoods, by ="Neighborhood"))
as_tibble(potholedatamerge)
View(potholedatamerge)
#changing data/cleaning data
#changing ID to a factor
potholedatamerge$CASE_ENQUIRY_ID <- as.factor(potholedata$CASE_ENQUIRY_ID)
as_tibble(potholedatamerge)
#changing date to mdy
mutate(potholedatamerge, OPEN_DT =mdy_hm(potholedatamerge$OPEN_DT))
potholedatamerge <-mutate(potholedatamerge, OPEN_DT =mdy_hm(potholedatamerge$OPEN_DT))
as_tibble(potholedatamerge)
potholedatamerge <-mutate(potholedatamerge, CLOSED_DT =mdy_hm(potholedatamerge$CLOSED_DT))
as_tibble(potholedatamerge)
View(potholedatamerge)
#changing to factor
potholedatamerge$CASE_STATUS <- as.factor(potholedatamerge$CASE_STATUS)
#handing of NA to none
as_tibble(potholedatamerge)
potholedatamerge$CLOSURE_REASON[is.na(potholedatamerge$CLOSURE_REASON)] <- "none"
View(potholedatamerge)
#chanGing Char to factors
potholedata$CASE_TITLE <- as.factor(potholedata$CASE_TITLE)
potholedata$SUBJECT <- as.factor(potholedata$SUBJECT)
potholedata$REASON <- as.factor(potholedata$REASON)
potholedata$TYPE <- as.factor(potholedata$TYPE)
potholedata$QUEUE <- as.factor(potholedata$QUEUE)
potholedata$Department <- as.factor(potholedata$Department)
potholedata$CLOSURE_REASON <- as.factor(potholedata$CLOSURE_REASON)
as_tibble(potholedata)
#handling NA to none
potholedatamerge$Location[is.na(potholedatamerge$Location)] <- "none"
potholedatamerge$fire_district[is.na(potholedatamerge$fire_district)] <- "none"
potholedatamerge$pwd_district[is.na(potholedatamerge$pwd_district)] <- "none"
potholedatamerge$city_council_district[is.na(potholedatamerge$city_council_district)] <- "none"
potholedatamerge$police_district[is.na(potholedatamerge$police_district)] <- "none"
potholedatamerge$neighborhood[is.na(potholedatamerge$neighborhood)] <- "none"

#changing as factors
potholedata$Location <- as.factor(potholedata$Location)
potholedata$fire_district <- as.factor(potholedata$fire_district)
potholedata$pwd_district <- as.factor(potholedata$pwd_district)
potholedata$city_council_district <- as.factor(potholedata$city_council_district)
potholedata$police_district <- as.factor(potholedata$police_district)
potholedata$neighborhood <- as.factor(potholedata$neighborhood)

#handling NA to none
potholedatamerge$neighborhood_services_district[is.na(potholedatamerge$neighborhood_services_district)] <- "none"
potholedatamerge$ward[is.na(potholedatamerge$ward)] <- "none"
potholedatamerge$precinct[is.na(potholedatamerge$precinct)] <- "none"
potholedatamerge$land_usage[is.na(potholedatamerge$land_usage)] <- "none"
potholedatamerge$LOCATION_STREET_NAME[is.na(potholedatamerge$LOCATION_STREET_NAME)] <- "none"
potholedatamerge$LOCATION_ZIPCODE[is.na(potholedatamerge$LOCATION_ZIPCODE)] <- "none"
as_tibble(potholedata)
View(potholedata)
#changing to factors
potholedata$neighborhood_services_district <- as.factor(potholedata$neighborhood_services_district)
potholedata$ward <- as.factor(potholedata$ward)
potholedata$preceinct <- as.factor(potholedata$precinct)
potholedata$land_usage <- as.factor(potholedata$land_usage)
potholedata$LOCATION_STREET_NAME <- as.factor(potholedata$LOCATION_STREET_NAME)
#changing NA to none
potholedatamerge$Property_Type[is.na(potholedatamerge$Property_Type)] <- "none"
potholedatamerge$Property_ID[is.na(potholedatamerge$Property_ID)] <- "none"
#changing to factors
potholedata$Property_Type <- as.factor(potholedata$Property_Type)
potholedata$Source <- as.factor(potholedata$Source)
potholedata$Geocoded_Location <- as.factor(potholedata$Geocoded_Location)
potholedata$'Location 1' <- as.factor(potholedata$'Location 1')
potholedata$LOCATION_ZIPCODE <- as.factor(potholedata$LOCATION_ZIPCODE)
potholedata$Property_ID <- as.factor(potholedata$Property_ID)
ggplot(data = potholedata) + 
  geom_bar(mapping = aes(x = Source, fill = Source))
ggplot(data = potholedata, mapping = aes(x = Source, y = neighborhood)) + 
  geom_jitter(mapping = aes(color = Source)) 
as_tibble(bostonage)

#changing as a factor
bostonage$Neighborhood <- as.factor(bostonage$Neighborhood)
potholedatatype <- potholedata %>% group_by(neighborhood, Source) %>% summarise(count = n())
ggplot(data = potholedatatype, mapping = aes(x=Source, y = neighborhood))+ geom_tile(mapping = aes(fill=count), colour = "white") + scale_fill_gradient(low = "white", high = "steelblue")
write.csv(potholedatamerge, "mergepotholedata.csv")
write.csv(potholeTrain, "potholeTrain.csv")
write.csv(potholeTest, "potholeTest.csv")
write.csv(dataset, "filename.csv")
potholedatamerge$Worked_at_home <- as.factor(potholedatamerge$Worked_at_home)
potholedatamerge$Car_truck_van <- as.factor(potholedatamerge$Car_truck_van)
potholedatamerge$Drove_alone <- as.factor(potholedatamerge$Drove_alone)
potholedatamerge$Bus_trolley <- as.factor(potholedatamerge$Bus_trolley)
potholedatamerge$Subway_elevated <- as.factor(potholedatamerge$Subway_elevated)
potholedatamerge$Railroad <- as.factor(potholedatamerge$Railroad)
potholedatamerge$Bicycle <- as.factor(potholedatamerge$Bicycle)
potholedatamerge$Walked <- as.factor(potholedatamerge$Walked)
potholedatamerge$Other_Means <- as.factor(potholedatamerge$Other_Means)

#commuting data
commuting$Neighborhood <- as.factor(commuting$Neighborhood)

#plotting each of the commuting types per neighborhood
ggplot(data = commuting, mapping = aes(x = Neighborhood, y = Worked_at_home)) + geom_boxplot()  
as_tibble(commuting)
View(commuting)
commuting <- melt(commuting,id.vars='Neighborhood', measure.vars=c('Worked_at_home','Car_truck_van','Drove_alone','Carpooled','Bus_trolley','Subway_elevated','Railroad', 'Bicycle','Walked','Other_Means'))

#working below
ggplot(data = commuting, mapping = aes(x = variable, y = value)) + geom_boxplot() + labs(title ="Boston Area Means of Transportation",x = "feature", y = "Units")   
commuting$Neighborhood <- as.factor(commuting$Neighborhood)
as_tibble(housingtenure)
housingtenure <- melt(housingtenure,id.vars='Neighborhood', measure.vars=c('Owner_Occupied','Renter_Occupied'))
View(housingtenure)
housingtenure$Neighborhood <- as.factor(housingtenure$Neighborhood)
ggplot(data = housingtenure, mapping = aes(x = variable, y = value)) + geom_boxplot() + labs(title ="Boston Area Housing Occupancy",x = "feature", y = "Units") 
age <- as_tibble(agedf)
age <- melt(age, id.vars='Neighborhood', measure.vars=c('0-9_Year_Olds','10-19_Year_Olds','20-34_Year_Olds', '35-54_Year_Olds','55-64_Year_Olds','65_Years_and_over'))
View(age)
age$Neighborhood <- as.factor(age$Neighborhood)
ggplot(data = age, mapping = aes(x = variable, y = value)) + geom_boxplot() + labs(title ="Boston Area Age Population",x = "feature", y = "Units") 
raceandorethnicity <- melt(raceandorethnicity,id.vars='Neighborhood', measure.vars=c('White','Black_African_American','Hispanic', 'Asian/PI','Other'))
View(raceandorethnicity)
raceandorethnicity$Neighborhood <- as.factor(raceandorethnicity$Neighborhood)
ggplot(data = raceandorethnicity, mapping = aes(x = variable, y = value)) + geom_boxplot() + labs(title ="Boston Ethnicity",x = "feature", y = "Units")
as_tibble(MedianIncome)
MedianIncome$Neighborhood <- as.factor(MedianIncome$Neighborhood)
ggplot(data = MedianIncome, mapping = aes(x = Median_Income, y = Neighborhood)) + geom_boxplot()    
BostonNeighborhoods$Neighborhood <- as.factor(BostonNeighborhoods$Neighborhood)
View(BostonNeighborhoods)
as_tibble(BostonNeighborhoods)
hist(MedianIncome$Median_Income, breaks=30, col="gray", xlab="Income", main="Boston Median Income")
MedianIncome$Neighborhood <- factor(MedianIncome$Neighborhood, levels = MedianIncome$Neighborhood[order(MedianIncome$Median_Income)])
x$name <- factor(x$name, levels = x$name[order(x$val)])
ggplot(data = MedianIncome) +
  geom_bar(mapping = aes(x = Neighborhood, y = Median_Income), stat = "identity") +ggtitle("Boston Neighborhoods Median Income") +coord_flip()
ggplot(corr.m, aes(x = reorder(miRNA, -value), y = value, fill = variable)) +
  geom_bar(stat = "identity")  
ggplot(data = MedianIncome aes(x = reorder(Neighborhood, -Median_Income), y = Income, fill = Neighborhood)) +
  geom_bar(stat = "identity")  
MedianIncome$Neighborhood <- as.factor(MedianIncome$Neighborhood)  
set.seed(12345)
trainIndex <-createDataPartition (potholedatamerge$SOURCE, p = 0.8, list = FALSE, times=1)
potholeTrain <-potholedatamerge [trainIndex,]
potholeTest <-potholedatamerge [-trainIndex,]
glmlog = multinom(SOURCE ~ ., data =potholeTrain)
summary(glmlog)
Oddratio1 = exp(coef(glmlog))
Oddratio1

#predictTrain = predict(glmLog, type="response", newdata=df.train) 
#acctable1<-table(df.train$SOURCE, predictTrain > 0.5)
#acctable1

#Accuracy2=sum(diag(acctable1))/(sum(acctable1))
#Accuracy2

predictTest = predict(glmlog, type="response", newdata=df.test) 
summary(predictTest)
acctable<-table(df.test$SOURCE, predictTest > 0.5)
acctable
Accuracy1=sum(diag(acctable))/(sum(acctable))
Accuracy1

ROCRpred = prediction(predictTest, df.test$SOURCE)
prf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")
plot(prf)
as.numeric(performance(ROCRpred, "auc")@y.values)

library(MASS)
backward=step(glmlog)
summary(backward)
names(dat1)
glmlog1 = glm(SOURCE ~ [write the variable names here that are not significant], data = df.train, family=binomial)
summary(glmlog1)

predictTest1 = predict(backward, type="response", newdata=df.test)
summary(predictTest1)
acc1=table(df.test$SOURCE, predictTest1 > 0.5)
Accuracy=sum(diag(acc1))/(sum(acc1))
Accuracy

ROCRpred1 = prediction(predictTest1, df.test$SOURCE)
prf1 <- performance(ROCRpred1, measure = "tpr", x.measure = "fpr")
plot(prf1)
as.numeric(performance(ROCRpred1, "auc")@y.values)
Oddratio = exp(glmlog1$coefficients)
Oddratio
scaler <- preProcess(potholeTrain,
                     method = c("center", "scale")) 
potholeTrain <-predict(scaler, potholeTrain)
potholeTest <-predict(scaler, potholeTest)
lr <- glmnet(SOURCE ~.,
             data = potholeTrain, 
             family = "multinomial", 
             na.action = na.omit)
predicton <- predict(lr, potholeTest, type = "coefficients", na.action = na.pass, s = 0.01)
summary(potholeTest)
View(potholeTest)
View(potholeTrain)
View(potholedatamerge)
as_tibble(potholedatamerge)
potholedatamerge$NEIGHBORHOOD<- as.factor(potholedatamerge$NEIGHBORHOOD)
potholedatamerge$OPEN_DT <- as.factor(potholedatamerge$OPEN_DT)
potholedatamerge$CLOSED_DT <- as.factor(potholedatamerge$CLOSED_DT)
potholedatamerge$CASE_STATUS <- as.factor(potholedatamerge$CASE_STATUS)
potholedatamerge$CLOSURE_REASON <- as.factor(potholedatamerge$CLOSURE_REASON)
potholedatamerge$CASE_TITLE <- as.factor(potholedatamerge$CASE_TITLE)
potholedatamerge$SUBJECT <- as.factor(potholedatamerge$SUBJECT)
potholedatamerge$REASON <- as.factor(potholedatamerge$REASON)
potholedatamerge$TYPE <- as.factor(potholedatamerge$TYPE)
potholedatamerge$ZIPCODE <- as.factor(potholedatamerge$ZIPCODE)
potholedatamerge$QUEUE <- as.factor(potholedatamerge$QUEUE)
potholedatamerge$Department <- as.factor(potholedatamerge$Department)
potholedatamerge$CLOSURE_REASON <- as.factor(potholedatamerge$CLOSURE_REASON)
potholedata$Department <- as.factor(potholedatamerge$Department)
potholedatamerge$Location <- as.factor(potholedatamerge$Location)
potholedatamerge$fire_district <- as.factor(potholedatamerge$fire_district)
potholedatamerge$pwd_district <- as.factor(potholedatamerge$pwd_district)
potholedatamerge$police_district <- as.factor(potholedatamerge$police_district)
potholedatamerge$ward <- as.factor(potholedatamerge$ward)
potholedatamerge$precinct <- as.factor(potholedatamerge$precinct)
potholedatamerge$land_usage <- as.factor(potholedatamerge$land_usage)
potholedatamerge$LOCATION_STREET_NAME <- as.factor(potholedatamerge$LOCATION_STREET_NAME)
potholedatamerge$LOCATION_ZIPCODE <- as.factor(potholedatamerge$LOCATION_ZIPCODE)
potholedatamerge$Property_Type <- as.factor(potholedatamerge$Property_Type)
potholedatamerge$Source <- as.factor(potholedatamerge$Source)
potholedatamerge$Geocoded_Location <- as.factor(potholedatamerge$Geocoded_Location)
potholedatamerge$'Location 1' <- as.factor(potholedatamerge$'Location 1')
potholedatamerge$LOCATION_ZIPCODE <- as.factor(potholedatamerge$LOCATION_ZIPCODE)
potholedatamerge$Property_ID <- as.factor(potholedatamerge$Property_ID)
potholedatamerge$Name <- as.factor(potholedatamerge$Name)











#SECTION 2: Continued cleaning, geographic plots, & correlations
#Set working directory to location where data is stored; will of course vary per person
setwd("~/Desktop/Fall 2018/COSC 4931 - Data Science Ethics/COSC4931-project2/COSC4931-project2")
#
#IMPORT DATASETS
#
df <- read.csv("secondDF.csv")

#Fix age percentage mixup 
df$X <- NULL
df$X0.9_Year_Olds <- NULL
df$X10.19_Year_Olds <- NULL
df$X20.34_Year_Olds <- NULL
df$X35.54_Year_Olds <- NULL
df$X55.64_Year_Olds <- NULL
df$X65_Years_and_over <- NULL
df$Percent_Population_0.9 <- NULL
df$Percent_Population_10.19 <- NULL
df$Percent_Population_20_34 <- NULL
df$Percent_Population_35.54 <- NULL
df$Percent_Population_55.64 <- NULL
df$Percent_Population_65_and_over <- NULL
agedf <- read.csv("age.csv")
agedf$Percent.of.Population <- NULL
agedf <- agedf[(agedf$Decade == 2010),]
agedf$Decade <- NULL

df <- merge(df, agedf[(agedf$Age.Range=='0-9 years'),], by ="Neighborhood")
df$"AGES_0_9_(#)" <- df$Number.of.People
df$Age.Range <- NULL
df$Number.of.People <- NULL
df <- merge(df, agedf[(agedf$Age.Range=='10-19 years'),], by ="Neighborhood")
df$"AGES_10_19_(#)"<- df$Number.of.People
df$Age.Range <- NULL
df$Number.of.People <- NULL
df <- merge(df, agedf[(agedf$Age.Range=='20-34 years'),], by ="Neighborhood")
df$"AGES_20_34_(#)"<- df$Number.of.People
df$Age.Range <- NULL
df$Number.of.People <- NULL
df <- merge(df, agedf[(agedf$Age.Range=='35-54 years'),], by ="Neighborhood")
df$"AGES_35_54_(#)"<- df$Number.of.People
df$Age.Range <- NULL
df$Number.of.People <- NULL
df <- merge(df, agedf[(agedf$Age.Range=='55-64 years'),], by ="Neighborhood")
df$"AGES_55_64_(#)"<- df$Number.of.People
df$Age.Range <- NULL
df$Number.of.People <- NULL
df <- merge(df, agedf[(agedf$Age.Range=='65 years and over'),], by ="Neighborhood")
df$"AGES_65_UP_(#)"<- df$Number.of.People
df$Age.Range <- NULL
df$Number.of.People <- NULL

#
#CLEAN DATASETS FURTHER
#
df$SOURCE <- df$Source
df$Source <- NULL
df$NEIGHBORHOOD <- df$Neighborhood
df$Neighborhood <- NULL
df$ZIPCODE <- df$LOCATION_ZIPCODE
df$LOCATION_ZIPCODE <- NULL
df$"CASE_OPEN_(Days)" <- difftime(parse_date_time(df$CLOSED_DT, orders="mdy"), parse_date_time(df$OPEN_DT, orders="mdy"), units = "days")
df$CLOSED_DT <- NULL
df$OPEN_DT <- NULL
df$Location.1 <- NULL
df$ward <- NULL
df$neighborhood_services_district <- NULL
df$police_district <- NULL
df$city_council <- NULL
df$pwd_district <- NULL
df$fire_district <- NULL
df$neighborhood_services_district <- NULL
df$ward <- NULL
df$precinct <- NULL
df$land_usage <- NULL
df$LOCATION_STREET_NAME <- NULL
df$Property_Type <- NULL
df$Property_ID <- NULL
df$Neighborhood_ID <- NULL
df$Acres <- NULL
df$SqMiles <- NULL
df$ShapeSTArea <- NULL
df$ShapeSTLength <- NULL
df$neighborhood <- NULL
df$X <- NULL
df$Department <- NULL
df$SUBJECT <- NULL
df$CASE_STATUS <- NULL
df$CASE_ENQUIRY_ID <- NULL
df$CASE_TITLE <- NULL
df$REASON <- NULL
df$QUEUE <- NULL
df$Location <- NULL
df$city_council_district <- NULL
df$Percent_Owner <- NULL
df$Percent_Rented <- NULL
df$Percent_Population_White <- NULL
df$Percent_Population_Black_African_American <- NULL
df$Percent_Population_Hispanic <- NULL
df$Percent_Population_Asian_PI <- NULL
df$Percent_Population_Other <- NULL
df$OBJECTID <- NULL
df$Name <- NULL
df$"REGION_MEDIAN_INCOME_(USD)" <- df$REGION_MEDIAN_INCOME_.USD.
df$REGION_MEDIAN_INCOME_.USD. <- NULL
df$"WORK_TRANSPORT_AT_HOME_(#)" <- df$Worked_at_home
df$"WORK_TRANSPORT_CAR_TRUCK_VAN_(#)" <- df$Car_truck_van
df$"WORK_TRANSPORT_DROVE_ALONE_(#)" <- df$Drove_alone
df$"WORK_TRANSPORT_CARPOOLED_(#)" <- df$Carpooled
df$"WORK_TRANSPORT_BUS_TROLLEY_(#)" <- df$Bus_trolley
df$"WORK_TRANSPORT_SUBWAY_ELEVATED_(#)" <- df$Subway_elevated
df$"WORK_TRANSPORT_RAILROAD_(#)" <- df$Railroad
df$"WORK_TRANSPORT_BICYCLE_(#)" <- df$Bicycle
df$"WORK_TRANSPORT_WALKED_(#)" <- df$Walked
df$"WORK_TRANSPORT_OTHER_(#)" <- df$Other_Means
df$"WORK_TRANSPORT_TOTAL_(#)" <- df$Total_Means
df$Worked_at_home <- NULL
df$Car_truck_van <- NULL
df$Drove_alone <- NULL
df$Carpooled <- NULL
df$Bus_trolley <- NULL
df$Subway_elevated <- NULL
df$Railroad <- NULL
df$Bicycle <- NULL
df$Walked <- NULL
df$Other_Means <- NULL
df$Total_Means <- NULL
df$"HOME_OWNER_(#)" <- df$Owner_Occupied
df$"HOME_RENTER_(#)" <- df$Renter_Occupied
df$"HOME_TOTAL_(#)" <- df$Total
df$"REGION_MEDIAN_INCOME_(USD)" <- df$Median_Income
df$"RACE_WHITE_(#)" <- df$White
df$"RACE_AFRICAN_AMERICAN_(#)" <- df$Black_African_American
df$"RACE_HISPANIC_(#)" <- df$Hispanic
df$"RACE_ASIAN_(#)" <- df$Asian.PI
df$"RACE_OTHER_(#)" <- df$Other
df$"RACE_TOTAL_(#)" <- df$"RACE_OTHER_(#)" + df$"RACE_ASIAN_(#)" + df$"RACE_HISPANIC_(#)" + df$"RACE_AFRICAN_AMERICAN_(#)"+ df$"RACE_WHITE_(#)"
df$"Owner_Occupied" <- NULL
df$"Renter_Occupied" <- NULL
df$Total <- NULL
df$Median_Income <- NULL
df$White <- NULL
df$Black_African_American <- NULL
df$Hispanic <- NULL
df$Asian.PI <- NULL
df$Other <- NULL

#Split geospatial into latitude and longitude
df <- separate(data = df, col = Geocoded_Location, into = c("LATITUDE", "LONGITUDE"), sep = ",")
df$LONGITUDE <- gsub("[()]", "", df$LONGITUDE)
df$LATITUDE <- gsub("[()]", "", df$LATITUDE)
df$Geocoded_Location <- NULL

#Correct datatypes if they have not already been
df$LONGITUDE <- as.numeric(df$LONGITUDE)
df$LATITUDE <- as.numeric(df$LATITUDE)

#For coordinate box: https://boundingbox.klokantech.com; reverse latitude and longitude pairs
#Generate spatial map of Boston for overlay
osm_boston_map <- openmap(c(42.219128, -71.191261),c(42.400914,-70.858829))
#Switch to longlat projection
longLat_boston_map <- openproj(osm_boston_map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", src = "epqs", units="feet")
boston_map <- autoplot(longLat_boston_map)
numRows <- nrow(df)

locations <- data.frame(df$LONGITUDE,df$LATITUDE)
elevationsList <- get_elev_point(locations, prj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", src = "epqs", units="feet")
df$"ELEVATION_(ft)" <- elevationsList$elevation
write.csv(df, "finalDF.csv") #Write a backup, even though it causes formatting problems
cordf <- as.data.frame(df)

#Transform any remaining strings into numberscore(my_data)
cordf$TYPE <- NULL
cordf$NEIGHBORHOOD <- NULL
cordf$SOURCE <- gsub('Citizens Connect App','1',cordf$SOURCE)
cordf$SOURCE <- gsub('City Worker App','2',cordf$SOURCE)
cordf$SOURCE <- gsub('Employee Generated','3',cordf$SOURCE)
cordf$SOURCE <- gsub('Constituent Call','4',cordf$SOURCE)
cordf$SOURCE <- gsub('Self Service','5',cordf$SOURCE)
cordf$ZIPCODE <- gsub('none',NA,cordf$ZIPCODE)

naZips <- is.na(cordf$ZIPCODE)
cordf$ZIPCODE <- NULL




#OPTION 1
#Calculate corelation values
cordf$CLOSURE_REASON <- NULL
cordf$SOURCE <- NULL
cordf <- sapply(cordf, as.numeric)
corrRelation <- cor(cordf)
corrRelation <- as.data.frame(corrRelation)

#plot correlation matrix
corrRelation <- sapply(corrRelation, as.numeric)
averageAbsCorr <- mean(abs(corrRelation))
belowAverage <- colMeans(abs(corrRelation)) < averageAbsCorr
corrRelation <- corrRelation[!belowAverage , !belowAverage]
row.names(corrRelation) <- colnames(corrRelation)
par(mar=c(0, 0, 0, 0)) #Set figure margins
corrplot(corrRelation, type = "upper", tl.col="black", tl.cex=0.6, method = "square", col=colorRampPalette(c("black","white"))(10), order = "FPC")

#OPTION 2

#look at source correlation specifically
#remove rows with city worker app
cordf <- cordf[!(cordf$SOURCE == '2'),]
#Set traditional sources as 1; connect app as 0
cordf$SOURCE <- gsub('1','0',cordf$SOURCE)
cordf$SOURCE <- gsub('3','1',cordf$SOURCE)
cordf$SOURCE <- gsub('4','1',cordf$SOURCE)
cordf$SOURCE <- gsub('5','1',cordf$SOURCE)

#Calculate corelation values
cordf <- sapply(cordf, as.numeric)
corrRelation <- cor(cordf)
corrRelation <- as.data.frame(corrRelation)

par(mar=c(5, 12, 0, 2)) #Set figure margins
xx = barplot(as.data.frame(corrRelation)[!(rownames(corrRelation) == "SOURCE" | rownames(corrRelation) == "CLOSURE_REASON"),]$SOURCE, xlab="Correlation", horiz=TRUE)
axis(2, at=xx, labels=row.names(as.data.frame(corrRelation)[!(rownames(corrRelation) == "SOURCE" | rownames(corrRelation) == "CLOSURE_REASON"),]), tick=FALSE, line=-0.5, cex.axis = 0.6, las = 1)
xx

#Splitting up zipcode for loop requests to segments of 500 or less
#Requests fail after ~1000 are made non-stop
# j<-1
# maxj = length(naZips)
# 
# j <- 1
# while (j < maxj) {
#   for (i in 0:499){
#     if(!is.na(cordf$ZIPCODE[(i+j)])) {
#       print(paste("j:", j, "i:", i))
#       locationInfo <- revgeo(cordf$LONGITUDE[i+j], cordf$LATITUDE[i+j], output="frame")
#       temp <- substring(as.character(locationInfo$zip), 7, 10)
#       cordf$ZIPCODE[(i+j)] <- temp
#       print(paste("code:",cordf$ZIPCODE[(i+j)]))
#       if(j == 100) { #Every 100 requests that are made
#         print(paste("Pausing a moment"))  
#         Sys.sleep(5)#Pause the code to prevent crashing
#       }
#     }
#     }
#   j<-j+500
#   print(paste("Resting"))
#   Sys.sleep(0)#Pause the code longer to prevent crashing
# }

#so revgeo cannot get all of the zip codes...
#"Getting geocode data from Photon: http://photon.komoot.de/reverse?lon=-71.1346&lat=42.3633"


#
#ANALYSIS OF DATASETS
#

#
#PLOT DATASETS
#


#Plot elevations within Boston
boston_map_elevation_heat <- boston_map +
  geom_bin2d(data = df, aes(df$LONGITUDE, df$LATITUDE), size = 0) + 
  stat_bin2d(data = df, aes(df$LONGITUDE, df$LATITUDE, fill = df$ELEVATION, alpha = df$ELEVATION), size = 0, bins = 100, geom = "polygon", show.legend = TRUE, na.rm = TRUE) +
  scale_fill_gradient(low = "purple", high = "red") + 
  scale_alpha(range = c(0, 0.1), guide = FALSE) + 
  labs(title = "Elevations at Pothole Locations in Boston", x = "Longitude", y = "Latitude", fill = "Elevation (ft)")

#Plot all potholes within Boston; the blob
locations_map <- boston_map +
  geom_point(data = df, mapping = aes(df$LONGITUDE, df$LATITUDE)) +
  labs(title = "Pothole Locations in Boston", color = "Pothole", x = "Longitude", y = "Latitude")

#Heatmap of pothole density
density_heatmap <- boston_map +
  geom_density2d(data = df, aes(df$LONGITUDE, df$LATITUDE), size = 0) + 
  stat_density2d(data = df, aes(df$LONGITUDE, df$LATITUDE, fill = ((..level../numRows)*100), alpha = ((..level../numRows)*100)), size = 0, bins = 100, geom = "polygon", show.legend = TRUE, na.rm = TRUE) +
  scale_fill_gradient(low = "purple", high = "red", limits=c(0,5)) + 
  scale_alpha(range = c(0, 0.1), guide = FALSE) + 
  labs(title = "Pothole Locations in Boston", x = "Longitude", y = "Latitude", fill = "Percent")

#Heatmaps by source for pothole density
df_tradVapp <- df
df_tradVapp$SOURCE <- gsub('Employee Generated','Traditional', df_tradVapp$SOURCE)
df_tradVapp$SOURCE <- gsub('Constituent Call','Traditional', df_tradVapp$SOURCE)
df_tradVapp$SOURCE <- gsub('Self Service','Traditional', df_tradVapp$SOURCE)

citdf <- df_tradVapp[(df_tradVapp$SOURCE == 'Citizens Connect App'),]
workdf <- df_tradVapp[(df_tradVapp$SOURCE == 'City Worker App'),]
traddf <- df_tradVapp[(df_tradVapp$SOURCE == 'Traditional'),]

#Individually
density_heatmaps_all <- boston_map +
  geom_density2d(data = df_tradVapp, aes(df_tradVapp$LONGITUDE, df_tradVapp$LATITUDE), size = 0.1) + 
  stat_density2d(data = df_tradVapp, aes(df_tradVapp$LONGITUDE, df_tradVapp$LATITUDE, fill = ((..level../numRows)*100), alpha = ((..level../numRows)*100)), size = 0, bins = 100, geom = "polygon", show.legend = TRUE, na.rm = TRUE) + 
  scale_fill_gradient(low = "purple", high = "red", limits=c(0,5)) +
  scale_alpha(range = c(0, 0.1), guide = FALSE) +
  theme(strip.text = element_text(size=6), text = element_text(size=12), legend.position="bottom") + 
  labs(y='', x='', fill = "Percent")

density_heatmaps_citizen <- boston_map +
  geom_density2d(data = citdf, aes(citdf$LONGITUDE, citdf$LATITUDE), size = 0.1) + 
  stat_density2d(data = citdf, aes(citdf$LONGITUDE, citdf$LATITUDE, fill = ((..level../numRows)*100), alpha = ((..level../numRows)*100)), size = 0, bins = 100, geom = "polygon", show.legend = TRUE, na.rm = TRUE) + 
  scale_fill_gradient(low = "purple", high = "red", limits=c(0,5)) +
  scale_alpha(range = c(0, 0.1), guide = FALSE) +
  theme(strip.text = element_text(size=6), text = element_text(size=12), legend.position="bottom") + 
  labs(y='', x='', fill = "Percent")

density_heatmaps_worker <- boston_map +
  geom_density2d(data = workdf, aes(workdf$LONGITUDE, workdf$LATITUDE), size = 0.1) + 
  stat_density2d(data = workdf, aes(workdf$LONGITUDE, workdf$LATITUDE, fill = ((..level../numRows)*100), alpha = ((..level../numRows)*100)), size = 0, bins = 100, geom = "polygon", show.legend = TRUE, na.rm = TRUE) + 
  scale_fill_gradient(low = "purple", high = "red", limits=c(0,5)) +
  scale_alpha(range = c(0, 0.1), guide = FALSE) +
  theme(strip.text = element_text(size=6), text = element_text(size=12), legend.position="bottom") + 
  labs(y='', x='', fill = "Percent")

density_heatmaps_traditional <- boston_map +
  geom_density2d(data = traddf, aes(traddf$LONGITUDE, traddf$LATITUDE), size = 0.1) + 
  stat_density2d(data = traddf, aes(traddf$LONGITUDE, traddf$LATITUDE, fill = ((..level../numRows)*100), alpha = ((..level../numRows)*100)), size = 0, bins = 100, geom = "polygon", show.legend = TRUE, na.rm = TRUE) + 
  scale_fill_gradient(low = "purple", high = "red", limits=c(0,5)) +
  scale_alpha(range = c(0, 0.1), guide = FALSE) +
  theme(strip.text = element_text(size=6), text = element_text(size=12), legend.position="bottom") + 
  labs(y='', x='', fill = "Percent")

#All together
density_heatmaps_source <- boston_map +
  geom_density2d(data = df_tradVapp, aes(df_tradVapp$LONGITUDE, df_tradVapp$LATITUDE), size = 0.1) + 
  stat_density2d(data = df_tradVapp, aes(df_tradVapp$LONGITUDE, df_tradVapp$LATITUDE, fill = ((..level../numRows)*100), alpha = ((..level../numRows)*100)), size = 0, bins = 100, geom = "polygon", show.legend = TRUE, na.rm = TRUE) + 
  scale_fill_gradient(low = "purple", high = "red", limits=c(0,5)) +
  scale_alpha(range = c(0, 0.1), guide = FALSE) +
  facet_wrap(~SOURCE) + 
  theme(strip.text = element_text(size=6), text = element_text(size=12)) + 
  labs(title = "Pothole Locations in Boston", x = "Longitude", y = "Latitude", fill = "Percent")


#Median Income Heatmap
boston_income_heatmap <- boston_map +
  geom_bin2d(data = df, aes(df$LONGITUDE, df$LATITUDE), size = 0) + 
  stat_bin2d(data = df, aes(df$LONGITUDE, df$LATITUDE, colour = df$REGION_MEDIAN_INCOME_.USD., alpha = 0.8), size = 0, bins = 100, geom = "polygon", show.legend = TRUE, na.rm = TRUE) +
  scale_fill_gradient(low = "purple", high = "red") + 
  scale_alpha(range = c(0, 0.1), guide = FALSE) + 
  labs(title = "Median Income at Pothole Locations in Boston", x = "Longitude", y = "Latitude", fill = "Median Income (USD)")
boston_income_heatmap

#Case Time Heatmap
boston_caseTime_heatmap <- boston_map +
  stat_summary_2d(data = df, aes(df$LONGITUDE, df$LATITUDE, z = (df$CASE_OPEN_.Days./numRows)), bins = 100, show.legend = TRUE, na.rm = TRUE) +
  scale_fill_gradient(low = "purple", high = "red") + scale_alpha(range = c(0, 0.2), guide = FALSE) +
  labs(x = "Longitude", y = "Latitude", fill = "Days Report Open") +
  ggtitle("Days Case Open at Pothole Locations in Boston")
boston_caseTime_heatmap

#Case Time Heatmap
boston_caseTime_heatmap <- boston_map +
  geom_bin2d(data = df, aes(df$LONGITUDE, df$LATITUDE), size = 0) + 
  stat_bin2d(data = df, aes(df$LONGITUDE, df$LATITUDE, fill = df$CASE_OPEN_.Days., alpha = df$CASE_OPEN_.Days.), size = 0, bins = 100, geom = "tile", show.legend = TRUE, na.rm = TRUE) +
  scale_fill_gradient(low = "purple", high = "red") + 
  scale_alpha(range = c(0, 0.1), guide = FALSE) + 
  labs(title = "Days Case Open at Pothole Locations in Boston", x = "Longitude", y = "Latitude", fill = "Days Report Open")
boston_caseTime_heatmap


#Race heatmaps
race_heatmap1 <- boston_map +
  geom_density2d(data = df, aes(df$LONGITUDE, df$LATITUDE), size = 0) + 
  stat_density2d(data = df, aes(df$LONGITUDE, df$LATITUDE, fill = -((..level../numRows)*100), alpha = -((..level../numRows)*100)), size = 0, bins = 100, geom = "polygon", show.legend = TRUE, na.rm = TRUE) +
  scale_fill_gradient(low = "purple", high = "red") + 
  scale_alpha(range = c(0, 0.1), guide = FALSE) + 
  labs(title = "Pothole Locations in Boston", x = "Longitude", y = "Latitude", fill = "Percent")


#Population overlay for normal map
options(tigris_use_cache = TRUE)
census_api_key("a8e943951e3bc53783cd383c6ecd1b762fcb01a3")
populationDensity <- get_acs(state = "MA", geography = "tract", year = 2015, variables = "B19013_001", geometry = TRUE)

png('population.png')
ggplot(populationDensity, aes(fill = estimate, color = estimate)) +
  geom_sf() +
  coord_sf(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", xlim = c(-71.191261, -70.858829), ylim = c(42.219128, 42.400914)) +
  scale_color_gradient(guide=FALSE) +
  scale_fill_gradient(low = "purple", high = "red") + 
  labs(color = "Estimated Population", fill = "Estimated Population") +
  theme_void()
dev.off()

#Save default map for the overlay to go on top of
png('boston_map.png')
plot(boston_map)
dev.off()

geom_map(mapping = NULL, data = test, map = boston_map, inherit.aes = TRUE)
add_osm_objects (boston_map, populationDensity)
install.packages("osmplotr")
library("osmplotr")
bbox <- get_bbox (c(-0.15, 51.5, -0.10, 51.52))
dat_B <- extract_osm_objects (key = 'building', bbox = bbox)
map <- osm_basemap (bbox = bbox, bg = 'gray20')
map <- add_osm_objects (map, dat_B, col = 'gray40')
print_osm_map(osm_boston_map)
osm_boston_map <- openmap(c(42.219128, -71.191261),c(42.400914,-70.858829))

#Switch to longlat projection
longLat_boston_map <- openproj(osm_boston_map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", src = "epqs", units="feet")
boston_map <- autoplot(longLat_boston_map)

plot(populationDensity$geometry, col=populationDensity$estimate, xlim = (-71.191261 -70.858829))
test <- geom_density(data = populationDensity, aes(fill = estimate, color = estimate)) +
  geom_sf() +
  coord_sf(crs = 26914)
raceVariables <- c(White = "P005003",Black = "P005004", Asian = "P005006", Hispanic = "P004003")

#OTHER
sum <- df
sum$CLOSURE_REASON <- NULL
sum$TYPE <- NULL
sum$SOURCE <- NULL
sum$NEIGHBORHOOD <- NULL
sum$ZIPCODE <- NULL

#Confirm general trends in data with simple example
mean(dat1[(dat1$SOURCE == "City Worker App"),]$LONGITUDE) > mean(dat1[(dat1$SOURCE == "Citizens Connect App"),]$LONGITUDE)
mean(dat1[(dat1$SOURCE == "City Worker App"),]$`CASE_OPEN_(Days)`) > mean(dat1[(dat1$SOURCE == "Citizens Connect App"),]$`CASE_OPEN_(Days)`)

#Show the number of reports relative to elevation
ggplot(dat1, aes(`ELEVATION_(ft)`)) +
  geom_histogram()

#Section 3 - Adjusted Multinomial Regression
backupdf <- df
dat1 <- as.data.frame(df)
dat1 <- dat1[complete.cases(dat1), ]
dat1$SOURCE <- gsub('Employee Generated','Traditional',dat1$SOURCE)
dat1$SOURCE <- gsub('Constituent Call','Traditional',dat1$SOURCE)
dat1$SOURCE <- gsub('Self Service','Traditional',dat1$SOURCE)
dat1$ZIPCODE <- gsub('none',NA,dat1$ZIPCODE)
dat1$ZIPCODE <- sapply(dat1$ZIPCODE, as.numeric)
dat1$SOURCE <- as.factor(dat1$SOURCE)
dat1$"NEIGHBORHOOD_" <- dat1$NEIGHBORHOOD
dat1$NEIGHBORHOOD <- NULL
dat1$"NEIGHBORHOOD_" <- as.factor(dat1$"NEIGHBORHOOD_")
dat1$CLOSURE_REASON <- NULL
dat1$TYPE <- NULL
dat1$SOURCE <- relevel(dat1$SOURCE, ref="Traditional")

#Training/Testing split; 70/30
set.seed(123)
ind = sample(2, nrow(dat1), replace = TRUE, prob=c(0.7,0.3))
df2Traditional.train = dat1[ind == 1,]
df2Traditional.test = dat1[ind == 2,]

multlog=multinom(SOURCE ~ ., data=df2Traditional.train, maxit = 1000)
modelSum <- summary(multlog)

# Calculate z-values
zvaluesTraditional <- summary(multlog)$coefficients / summary(multlog)$standard.errors

#Calculate p values
pTraditional<-(1-pnorm(abs(zvaluesTraditional), 0,1))*2
#Calculate risk ratios
riskRatiosTraditional=exp(summary(multlog)$coefficients)

#Test the model
predictTest = predict(multlog, newdata = df2Traditional.test, "probs")
predicted_class<-predict(multlog, df2Traditional.test)
table(predicted_class, df2Traditional.test$SOURCE)
write.csv(table(predicted_class, df2Traditional.test$SOURCE), "confusion_traditional_relative.csv")
mean(as.character(predicted_class) != as.character(df2Traditional.test$SOURCE), na.rm=TRUE)


#Prepare coefficients for plotting
modelCoeff <- as.data.frame(modelSum[["coefficients"]])
tmodelCoeff <- as.data.frame(t(modelCoeff))
tmodelCoeff$`Citizens Connect App` <- sapply(tmodelCoeff$`Citizens Connect App`, as.numeric)
tmodelCoeff$`City Worker App` <- sapply(tmodelCoeff$`City Worker App`, as.numeric)

coeffModelData = melt(data.frame(Citizen_Connect_App=tmodelCoeff$`Citizens Connect App`, 
                                 City_Worker_App=tmodelCoeff$`City Worker App`, 
                                 Variable=row.names(tmodelCoeff)),
                      variable.name="Source")

#Prepare standard error for plotting
modelErr <- as.data.frame(modelSum[["standard.errors"]])
tmodelErr <- as.data.frame(t(modelErr))
tmodelErr$`Citizens Connect App` <- sapply(tmodelErr$`Citizens Connect App`, as.numeric)
tmodelErr$`City Worker App` <- sapply(tmodelErr$`City Worker App`, as.numeric)

errModelData = melt(data.frame(Citizen_Connect_App=tmodelErr$`Citizens Connect App`, 
                               City_Worker_App=tmodelErr$`City Worker App`, 
                               Variable=row.names(tmodelErr)),
                    variable.name="Source")


#Calculate initial error bars before adjusting values for visualization
ymaxLimit = coeffModelData$value + errModelData$value
yminLimit = coeffModelData$value - errModelData$value

#Adjust values for visualization
coeffModelData$value <- sign(coeffModelData$value) * log10(abs(coeffModelData$value))
coeffModelData$"log10_Coefficient" <- coeffModelData$value
coeffModelData$value <- NULL
errModelData$value <- sign(errModelData$value) * log10(abs(errModelData$value))
errModelData$"log10_Coefficient" <- errModelData$value
errModelData$value <- NULL

ymaxLimit <- sign(ymaxLimit) * log10(abs(ymaxLimit))
yminLimit <- sign(yminLimit) * log10(abs(yminLimit))

#Remove any NAN values from datasets and error bar limits prior to visualization
ymaxLimit <- ymaxLimit[!(is.na(coeffModelData$log10_Coefficient))]
yminLimit <- yminLimit[!(is.na(coeffModelData$log10_Coefficient))]
errModelData <- errModelData[!(is.na(coeffModelData$log10_Coefficient)),]
coeffModelData <- coeffModelData[!(is.na(coeffModelData$log10_Coefficient)),]

#Generate initial coefficients plot
modelCoefficientPlot <- ggplot(coeffModelData, aes(Variable, log10_Coefficient, fill=Source)) + 
  scale_fill_grey(start = .1, end = .5) +
  theme_bw() +
  geom_bar(stat="identity", width=0.5, position = position_dodge(width=0.6)) +
  theme(text = element_text(size=15), axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#Set standard error bar sizes
limits <- aes(ymax = ymaxLimit, ymin=yminLimit)

#Add standard error bars to initial plot
#save image at 1200,600
relativeTraditional <- modelCoefficientPlot + geom_bar(stat = "identity", position = position_dodge(width=0.6)) +
  geom_errorbar(limits, position = position_dodge(width=0.6), width = 0.25, colour="red")


tpmodel <- as.data.frame(t(pTraditional))
tpmodel$`Citizens Connect App` <- sapply(tpmodel$`Citizens Connect App`, as.numeric)
tpmodel$`City Worker App` <- sapply(tpmodel$`City Worker App`, as.numeric)

#Prepare p-values for visualizaation
pValueData = melt(data.frame(Citizen_Connect_App=tpmodel$`Citizens Connect App`, 
                               City_Worker_App=tpmodel$`City Worker App`, 
                               Variable=row.names(tpmodel)),
                    variable.name="Source")
#Remove any NA values
pValueData <- pValueData[!(is.na(pValueData$value)),]
pValueData$p_values <- pValueData$value
pValueData$value <- NULL

#Generate initial p-value plot
pValuePlotTraditional <- ggplot(pValueData, aes(Variable, p_values, fill=Source)) + 
  scale_fill_grey(start = .1, end = .5) +
  theme_bw() +
  geom_bar(stat="identity", width=0.5, position = position_dodge(width=0.6)) +
  theme(text = element_text(size=15), axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


triskmodel <- as.data.frame(t(riskRatiosTraditional))
triskmodel$`Citizens Connect App` <- sapply(triskmodel$`Citizens Connect App`, as.numeric)
triskmodel$`City Worker App` <- sapply(triskmodel$`City Worker App`, as.numeric)

#Prepare risk ratios for visualizaation
riskValueData = melt(data.frame(Citizen_Connect_App=triskmodel$`Citizens Connect App`, 
                             City_Worker_App=triskmodel$`City Worker App`, 
                             Variable=row.names(triskmodel)),
                  variable.name="Source")
#Remove any NA values
riskValueData <- riskValueData[!(is.na(riskValueData$value)),]
riskValueData$Risk_Ratio <- riskValueData$value
riskValueData$value <- NULL

#Generate initial risk value plot, truncate values greater than 2
riskValueData$Risk_Ratio[(riskValueData$Risk_Ratio > 2)] <- 2
riskValuePlotTraditional <- ggplot(riskValueData, aes(Variable, Risk_Ratio, fill=Source)) + 
  scale_fill_grey(start = .1, end = .5) +
  theme_bw() +
  geom_bar(stat="identity", width=0.5, position = position_dodge(width=0.6)) +
  theme(text = element_text(size=15), axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
riskValuePlotTraditional

#Perform the regression again, but relative to the worker application
dat1$SOURCE <- relevel(dat1$SOURCE, ref="City Worker App")

#Training/Testing split; 70/30
set.seed(123)
ind = sample(2, nrow(dat1), replace = TRUE, prob=c(0.7,0.3))
df2Worker.train = dat1[ind == 1,]
df2Worker.test = dat1[ind == 2,]

multlog=multinom(SOURCE ~ ., data=df2Worker.train, maxit = 1000)
modelSum <- summary(multlog)

# Calculate z-values
zvaluesTraditional <- summary(multlog)$coefficients / summary(multlog)$standard.errors

#Calculate p values
pTraditional<-(1-pnorm(abs(zvaluesTraditional), 0,1))*2
#Calculate risk ratios
riskRatiosTraditional=exp(summary(multlog)$coefficients)

#Test the model
predictTest = predict(multlog, newdata = df2Worker.test, "probs")
predicted_class<-predict(multlog, df2Worker.test)
table(predicted_class, df2Worker.test$SOURCE)
write.csv(table(predicted_class, df2Worker.test$SOURCE), "confusion_worker_relative.csv")
mean(as.character(predicted_class) != as.character(df2Worker.test$SOURCE), na.rm=TRUE)


#Prepare coefficients for plotting
modelCoeff <- as.data.frame(modelSum[["coefficients"]])
tmodelCoeff <- as.data.frame(t(modelCoeff))
tmodelCoeff$`Citizens Connect App` <- sapply(tmodelCoeff$`Citizens Connect App`, as.numeric)
tmodelCoeff$`Traditional` <- sapply(tmodelCoeff$`Traditional`, as.numeric)

coeffModelData = melt(data.frame(Citizen_Connect_App=tmodelCoeff$`Citizens Connect App`, 
                                 Traditional=tmodelCoeff$`Traditional`, 
                                 Variable=row.names(tmodelCoeff)),
                      variable.name="Source")

#Prepare standard error for plotting
modelErr <- as.data.frame(modelSum[["standard.errors"]])
tmodelErr <- as.data.frame(t(modelErr))
tmodelErr$`Citizens Connect App` <- sapply(tmodelErr$`Citizens Connect App`, as.numeric)
tmodelErr$`Traditional ` <- sapply(tmodelErr$`Traditional`, as.numeric)

errModelData = melt(data.frame(Citizen_Connect_App=tmodelErr$`Citizens Connect App`, 
                               City_Worker_App=tmodelErr$`Traditional`, 
                               Variable=row.names(tmodelErr)),
                    variable.name="Source")


#Calculate initial error bars before adjusting values for visualization
ymaxLimit = coeffModelData$value + errModelData$value
yminLimit = coeffModelData$value - errModelData$value

#Adjust values for visualization
coeffModelData$value <- sign(coeffModelData$value) * log10(abs(coeffModelData$value))
coeffModelData$"log10_Coefficient" <- coeffModelData$value
coeffModelData$value <- NULL
errModelData$value <- sign(errModelData$value) * log10(abs(errModelData$value))
errModelData$"log10_Coefficient" <- errModelData$value
errModelData$value <- NULL

ymaxLimit <- sign(ymaxLimit) * log10(abs(ymaxLimit))
yminLimit <- sign(yminLimit) * log10(abs(yminLimit))

#Remove any NAN values from datasets and error bar limits prior to visualization
ymaxLimit <- ymaxLimit[!(is.na(coeffModelData$log10_Coefficient))]
yminLimit <- yminLimit[!(is.na(coeffModelData$log10_Coefficient))]
errModelData <- errModelData[!(is.na(coeffModelData$log10_Coefficient)),]
coeffModelData <- coeffModelData[!(is.na(coeffModelData$log10_Coefficient)),]

#Generate initial coefficients plot
modelCoefficientPlot <- ggplot(coeffModelData, aes(Variable, log10_Coefficient, fill=Source)) + 
  scale_fill_grey(start = .1, end = .5) +
  theme_bw() +
  geom_bar(stat="identity", width=0.5, position = position_dodge(width=0.6)) +
  theme(text = element_text(size=15), axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#Set standard error bar sizes
limits <- aes(ymax = ymaxLimit, ymin=yminLimit)

#Add standard error bars to initial plot
#save image at 1200,600
relativeWorker <- modelCoefficientPlot + geom_bar(stat = "identity", position = position_dodge(width=0.6)) +
  geom_errorbar(limits, position = position_dodge(width=0.6), width = 0.25, colour="red")

# Calculate z-values
zvaluesWorker <- summary(multlog)$coefficients / summary(multlog)$standard.errors
#Calculate p values
pWorker<-(1-pnorm(abs(zvaluesWorker), 0,1))*2
riskRatiosWorker=exp(summary(multlog)$coefficients)



triskmodel <- as.data.frame(t(riskRatiosWorker))
triskmodel$`Citizens Connect App` <- sapply(triskmodel$`Citizens Connect App`, as.numeric)
triskmodel$`Traditional` <- sapply(triskmodel$`Traditional`, as.numeric)


#Prepare risk ratios for visualizaation
riskValueData = melt(data.frame(Citizen_Connect_App=triskmodel$`Citizens Connect App`, 
                                Traditional=triskmodel$`Traditional`, 
                                Variable=row.names(triskmodel)),
                     variable.name="Source")
#Remove any NA values
riskValueData <- riskValueData[!(is.na(riskValueData$value)),]
riskValueData$Risk_Ratio <- riskValueData$value
riskValueData$value <- NULL

#Generate initial risk value plot, truncate values greater than 2
riskValueData$Risk_Ratio[(riskValueData$Risk_Ratio > 2)] <- 2
riskValuePlotWorker <- ggplot(riskValueData, aes(Variable, Risk_Ratio, fill=Source)) + 
  scale_fill_grey(start = .1, end = .5) +
  theme_bw() +
  geom_bar(stat="identity", width=0.5, position = position_dodge(width=0.6)) +
  theme(text = element_text(size=15), axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
riskValuePlotWorker








#Run the regression split and test on randomized distributions 100 times
iter <- 100
out <- matrix(NA, nrow=iter, ncol=1)
for (i in 1:iter) {
  dat1$SOURCE <- relevel(dat1$SOURCE, ref="City Worker App")
  #Training/Testing split; 70/30
  set.seed(i)
  ind = sample(2, nrow(dat1), replace = TRUE, prob=c(0.7,0.3))
  df2Worker.train = dat1[ind == 1,]
  df2Worker.test = dat1[ind == 2,]
  #Test the model
  predictTest = predict(multlog, newdata = df2Worker.test, "probs")
  predicted_class<-predict(multlog, df2Worker.test)
  out[i] <- mean(as.character(predicted_class) != as.character(df2Worker.test$SOURCE), na.rm=TRUE)
}
#Determine the average error
mean(out)




#Section 4 - Fix figures in line with peer review requests
#Age boxplot
age <-df$`AGES_0_9_(#)`
age <- as.data.frame(age)
age$`0_9_(#)`<-df$`AGES_0_9_(#)`
age$age <- NULL
age$NEIGHBORHOOD <- df$NEIGHBORHOOD
age$`10_19_(#)` <- df$`AGES_10_19_(#)`
age$`20_34_(#)` <- df$`AGES_20_34_(#)`
age$`35_54_(#)` <- df$`AGES_35_54_(#)`
age$`55_64_(#)` <- df$`AGES_55_64_(#)`
age$`65_UP_(#)` <- df$`AGES_65_UP_(#)`
age$NEIGHBORHOOD <- as.factor(age$NEIGHBORHOOD)
ageMelt <- melt(age, id.vars='NEIGHBORHOOD', measure.vars=c('0_9_(#)', '10_19_(#)','20_34_(#)','35_54_(#)','55_64_(#)', '65_UP_(#)'))
ggplot(data = ageMelt, mapping = aes(x = variable, y = value)) + 
  geom_boxplot() + 
  labs(x = "Variable", y = "Instances by Neighborhood") +
  theme(text = element_text(size=10), axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#Race boxplot
race <- df$`RACE_WHITE_(#)`
race <- as.data.frame(race)
race$`WHITE_(#)` <- df$`RACE_WHITE_(#)`
race$`HISPANIC_(#)` <- df$`RACE_HISPANIC_(#)`
race$`AFRICAN_(#)` <- df$`RACE_AFRICAN_AMERICAN_(#)`
race$`ASIAN_(#)` <- df$`RACE_ASIAN_(#)`
race$`OTHER_(#)` <- df$`RACE_OTHER_(#)`
race$race <- NULL
race$NEIGHBORHOOD <- df$NEIGHBORHOOD
race$NEIGHBORHOOD <- as.factor(age$NEIGHBORHOOD)
raceMelt <- melt(race, id.vars='NEIGHBORHOOD', measure.vars=c('WHITE_(#)','HISPANIC_(#)','HISPANIC_(#)','AFRICAN_(#)','ASIAN_(#)','OTHER_(#)'))
ggplot(data = raceMelt, mapping = aes(x = variable, y = value)) + 
  geom_boxplot() + 
  labs(x = "Variable", y = "Instances by Neighborhood") +
  theme(text = element_text(size=10), axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


#Transportation boxplot
trans <- df$`WORK_TRANSPORT_AT_HOME_(#)`
trans <- as.data.frame(trans)
trans$`WORK_AT_HOME_(#)` <- df$`WORK_TRANSPORT_AT_HOME_(#)`
trans$`BICYCLE_(#)` <-  df$`WORK_TRANSPORT_BICYCLE_(#)`
trans$`BUS_TROLLEY_(#)` <- df$`WORK_TRANSPORT_BUS_TROLLEY_(#)`
trans$`CAR_TRUCK_VAN_(#)` <- df$`WORK_TRANSPORT_CAR_TRUCK_VAN_(#)`
trans$`CARPOOLED_(#)` <- df$`WORK_TRANSPORT_CARPOOLED_(#)`
trans$`DROVE_ALONE_(#)` <- df$`WORK_TRANSPORT_DROVE_ALONE_(#)`
trans$`RAILROAD_(#)` <- df$`WORK_TRANSPORT_RAILROAD_(#)`
trans$`SUBWAY_ELEVATED_(#)` <- df$`WORK_TRANSPORT_SUBWAY_ELEVATED_(#)` 
trans$`WALKED_(#)` <- df$`WORK_TRANSPORT_WALKED_(#)`
trans$`OTHER_(#)` <- df$`WORK_TRANSPORT_OTHER_(#)`
trans$trans <- NULL
trans$NEIGHBORHOOD <- df$NEIGHBORHOOD
trans$NEIGHBORHOOD <- as.factor(age$NEIGHBORHOOD)
transMelt <- melt(trans, id.vars='NEIGHBORHOOD', measure.vars=c('WORK_AT_HOME_(#)','BICYCLE_(#)','BUS_TROLLEY_(#)','CAR_TRUCK_VAN_(#)','DROVE_ALONE_(#)','RAILROAD_(#)','SUBWAY_ELEVATED_(#)','WALKED_(#)','OTHER_(#)'))
ggplot(data = transMelt, mapping = aes(x = variable, y = value)) + 
  geom_boxplot() + 
  labs(x = "Variable", y = "Instances by Neighborhood") +
  theme(text = element_text(size=10), axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#Housing boxplot
hous <- df$`HOME_OWNER_(#)`
hous <- as.data.frame(hous)
hous$`OWNER_(#)` <- df$`HOME_OWNER_(#)`
hous$`RENTER_(#)` <- df$`HOME_RENTER_(#)`
hous$hous <- NULL
hous$NEIGHBORHOOD <- df$NEIGHBORHOOD
hous$NEIGHBORHOOD <- as.factor(age$NEIGHBORHOOD)
housMelt <- melt(hous, id.vars='NEIGHBORHOOD', measure.vars=c('OWNER_(#)','RENTER_(#)'))
ggplot(data = housMelt, mapping = aes(x = variable, y = value)) + 
  geom_boxplot() + 
  labs(x = "Variable", y = "Instances by Neighborhood") +
  theme(text = element_text(size=10), axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#Median income bar graph
medianIncome <- df$`REGION_MEDIAN_INCOME_(USD)`
medianIncome <- as.data.frame(medianIncome)
medianIncome$`REGION_MEDIAN_INCOME_(USD)` <- df$`REGION_MEDIAN_INCOME_(USD)`
medianIncome$medianIncome <- NULL
medianIncome$NEIGHBORHOOD <- df$NEIGHBORHOOD
medianIncome$NEIGHBORHOOD <- as.factor(medianIncome$NEIGHBORHOOD)
incomeMelt <- melt(medianIncome, id.vars='NEIGHBORHOOD', measure.vars=c('REGION_MEDIAN_INCOME_(USD)'))
incomeMelt <- unique(incomeMelt)
rownames(incomeMelt) <- incomeMelt$NEIGHBORHOOD
incomeMelt$NEIGHBORHOOD <- NULL
incomeMelt$variable <- NULL
incomeMelt <- as.data.frame(incomeMelt)
incomeMelt$'REGION_MEDIAN_INCOME_(USD)' <- incomeMelt$value
incomeMelt$value <- NULL

par(mar=c(5, 5, 0, 1)) #Set figure margins
xx <- barplot(incomeMelt$`REGION_MEDIAN_INCOME_(USD)`, cex.lab=0.7, xlab="REGION_MEDIAN_INCOME_(USD)", horiz=TRUE)
axis(2, at=xx, labels=rownames(incomeMelt), tick=FALSE, line=-0.5, cex.axis = 0.7, las = 1)
