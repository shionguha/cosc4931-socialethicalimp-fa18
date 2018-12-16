# Social and Ethica Implications of Data 
# Fall 2018 
# Group Project Script  
# By: Eyad Aldawod
# Dataset: HappyDB

# Libraries 
library(ggplot2)

# loading dataset to R
setwd("/happydb/")


#####  Plan  #####

# load demographic.csv
demDat <- read.csv("demographic.csv", header=TRUE)

# clean gender and age (add age pnts to Na's)
# gender 
demDat <- within(demDat, gender[gender == ''] <- 'o')

# age 

demDat$age <- as.character(demDat$age)
#### age problem ###
demDat <- within(demDat, age[age == '60yrs'] <- '60')
demDat <- within(demDat, age[age == 'čá'] <- '')
demDat <- within(demDat, age[age == '233.0'] <- '')
demDat <- within(demDat, age[age == '233'] <- '')
demDat <- within(demDat, age[age == '227'] <- '')
demDat <- within(demDat, age[age == '2'] <- '')
demDat <- within(demDat, age[age == '2.0'] <- '')
demDat <- within(demDat, age[age == '3'] <- '')
demDat <- within(demDat, age[age == '3.0'] <- '')
demDat <- within(demDat, age[age == '4'] <- '')

demDat <- within(demDat, age[age == ''] <- '0')
demDat <- within(demDat, age[age == 'prefer not to say'] <- '5')
demDat$age <- as.numeric(demDat$age)
demDat$age <- as.character(demDat$age)
demDat <- within(demDat, age[age == '0'] <- 'NaN')
demDat <- within(demDat, age[age == '5'] <- 'prefer not to say')

# add new variable called age_category 
demDat$age_category <- demDat$age

# assign values to the age_category based on values from age
demDat <- within(demDat, age_category[age_category == '17' ] <- '17-20')
demDat <- within(demDat, age_category[age_category == '18' ] <- '17-20')
demDat <- within(demDat, age_category[age_category == '19' ] <- '17-20')
demDat <- within(demDat, age_category[age_category == '20' ] <- '17-20')

demDat <- within(demDat, age_category[age_category == '21' ] <- '21-30')
demDat <- within(demDat, age_category[age_category == '22' ] <- '21-30')
demDat <- within(demDat, age_category[age_category == '23' ] <- '21-30')
demDat <- within(demDat, age_category[age_category == '24' ] <- '21-30')
demDat <- within(demDat, age_category[age_category == '25' ] <- '21-30')
demDat <- within(demDat, age_category[age_category == '26' ] <- '21-30')
demDat <- within(demDat, age_category[age_category == '27' ] <- '21-30')
demDat <- within(demDat, age_category[age_category == '28' ] <- '21-30')
demDat <- within(demDat, age_category[age_category == '29' ] <- '21-30')
demDat <- within(demDat, age_category[age_category == '30' ] <- '21-30')

demDat <- within(demDat, age_category[age_category == '31' ] <- '31-40')
demDat <- within(demDat, age_category[age_category == '32' ] <- '31-40')
demDat <- within(demDat, age_category[age_category == '33' ] <- '31-40')
demDat <- within(demDat, age_category[age_category == '34' ] <- '31-40')
demDat <- within(demDat, age_category[age_category == '35' ] <- '31-40')
demDat <- within(demDat, age_category[age_category == '36' ] <- '31-40')
demDat <- within(demDat, age_category[age_category == '37' ] <- '31-40')
demDat <- within(demDat, age_category[age_category == '38' ] <- '31-40')
demDat <- within(demDat, age_category[age_category == '39' ] <- '31-40')
demDat <- within(demDat, age_category[age_category == '40' ] <- '31-40')

demDat <- within(demDat, age_category[age_category == '41' ] <- '41-50')
demDat <- within(demDat, age_category[age_category == '42' ] <- '41-50')
demDat <- within(demDat, age_category[age_category == '43' ] <- '41-50')
demDat <- within(demDat, age_category[age_category == '44' ] <- '41-50')
demDat <- within(demDat, age_category[age_category == '45' ] <- '41-50')
demDat <- within(demDat, age_category[age_category == '46' ] <- '41-50')
demDat <- within(demDat, age_category[age_category == '47' ] <- '41-50')
demDat <- within(demDat, age_category[age_category == '48' ] <- '41-50')
demDat <- within(demDat, age_category[age_category == '49' ] <- '41-50')
demDat <- within(demDat, age_category[age_category == '50' ] <- '41-50')

demDat <- within(demDat, age_category[age_category == '51' ] <- '51-60')
demDat <- within(demDat, age_category[age_category == '52' ] <- '51-60')
demDat <- within(demDat, age_category[age_category == '53' ] <- '51-60')
demDat <- within(demDat, age_category[age_category == '54' ] <- '51-60')
demDat <- within(demDat, age_category[age_category == '55' ] <- '51-60')
demDat <- within(demDat, age_category[age_category == '56' ] <- '51-60')
demDat <- within(demDat, age_category[age_category == '57' ] <- '51-60')
demDat <- within(demDat, age_category[age_category == '58' ] <- '51-60')
demDat <- within(demDat, age_category[age_category == '59' ] <- '51-60')
demDat <- within(demDat, age_category[age_category == '60' ] <- '51-60')

demDat <- within(demDat, age_category[age_category == '61' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '62' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '63' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '64' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '65' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '66' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '67' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '68' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '69' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '70' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '71' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '72' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '73' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '74' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '75' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '76' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '77' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '78' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '79' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '80' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '81' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '82' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '83' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '84' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '85' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '86' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '87' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '88' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '89' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '90' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '91' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '92' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '93' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '94' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '95' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '96' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '97' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '98' ] <- '61+')
demDat <- within(demDat, age_category[age_category == '99' ] <- '61+')
 
# save the changes of demographic to a new .csv file
write.csv(demDat, file = "demographicV1.csv", row.names=FALSE)

# load clean_hm 
clDat <- read.csv("cleaned_hm.csv", header=TRUE)

# remove un-used variables (original_hm, modified, and num_sentance)
clDat$modified <- NULL
clDat$original_hm <- NULL
clDat$num_sentence <- NULL
  
# merged both dataset by wid  
dat <- merge(clDat, demDat, by="wid")

# save the mergerd data to a new .csv file
write.csv(dat, file = "mergedData.csv", row.names=FALSE)

### plots section ###

##### Basic analsys of the dataset ####
# Gender comparisons between the mergedData file and the demograpic file 
png(file='Gender Representation (Contributions).png')
ggplot(dat, aes(gender, position="dodge", fill=gender))  +  geom_bar() + 
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5)) +
  ggtitle("Gender Representation (Contributions)")
dev.off()

png(file='Gender Representation (Participants).png')
ggplot(demDat, aes(gender, position="dodge", fill=gender))  +  geom_bar() + 
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))+
  ggtitle("Gender Representation (Participants)")
dev.off()

# Age comparisons between the mergedData file and the demograpic file 
png(file='Age Representation (Contributions).png')
ggplot(dat, aes(age_category, fill=age_category))  +  geom_bar() + 
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5)) +
  ggtitle("Age Representation (Contributions)")
dev.off()

png(file='Age Representation (Participants).png')
ggplot(demDat, aes(age_category, fill=age_category))  +  geom_bar() + 
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5)) +
  ggtitle("Age Representation (Participants)")
dev.off()

# Parenthood comparisons between the mergedData file and the demograpic file 
png(file='parenthood (Contributors).png')
ggplot(dat, aes(parenthood, fill=parenthood))  +  geom_bar() + 
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5)) +
  ggtitle("parenthood (Contributors)")
dev.off()

png(file='parenthood (Participants).png')
ggplot(demDat, aes(parenthood, fill=parenthood))  +  geom_bar() + 
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5)) +
  ggtitle("parenthood (Participants)")
dev.off()

# Marital comparisons between the mergedData file and the demograpic file 
png(file='marital(Contributors).png')
ggplot(dat, aes(marital, fill=marital))  +  geom_bar() + 
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5)) +
  ggtitle("marital(Contributors)") + 
  theme(axis.text=element_text(size=8), axis.text.x = element_text(angle=45, hjust=1))
dev.off()

png(file='marital (Participants).png')
ggplot(demDat, aes(marital, fill=marital))  +  geom_bar() + 
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))+
  ggtitle("marital (Participants)") + 
  theme(axis.text=element_text(size=8), axis.text.x = element_text(angle=45, hjust=1))
dev.off()

png(file='reflection_period.png')
ggplot(dat, aes(reflection_period, fill=reflection_period))  +  geom_bar() + 
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5)) +
  ggtitle("reflection_period")
dev.off()

png(file='Age_category VS Predicted_category.png')
ggplot(dat, aes(x=age_category,y=predicted_category))  +  
  geom_jitter(size=0.05, alpha=0.15) + ggtitle("Age_category VS Predicted_category")
dev.off()

##### End of The Plan #####
