# ---------------------------------------------------------------------------- #
# Joseph Marotta
# October 17th, 2018
# Dr. Guha
# COSC 4931 - Social and Ethical Implications of Data
# Project 1 
# ---------------------------------------------------------------------------- 

# Importing Packages
# install.packages("tidyverse")
library(reshape2)
library(plyr)
library(ggplot2)
library(tidyverse)

# Importing Dataset
dataSet <- read.csv("C:\\Users\\josep\\Documents\\Datasets\\facebook-fact-check.csv");

# ---------------------------------------------------------------------------- 
# Data Cleaning 

# Removing 'debate' column. No info on it + only 13% have it
dataSet$Debate <- NULL

names(dataSet)[1] <- "FB_ACCOUNT_ID"
names(dataSet)[2] <- "POST_ID"
names(dataSet)[3] <- "CATEGORY"
names(dataSet)[4] <- "PUBLISHER" 
names(dataSet)[5] <- "URL"
names(dataSet)[6] <- "DATE"
names(dataSet)[7] <- "FORMAT"
names(dataSet)[8] <- "RATING" 
names(dataSet)[9] <- "SHARE_COUNT"
names(dataSet)[10] <- "REACTION_COUNT"
names(dataSet)[11] <- "COMMENT_COUNT"

dataSet$SHARE_COUNT <- mapvalues(dataSet$SHARE_COUNT, from = c(NA), to = c(0))
dataSet$REACTION_COUNT <- mapvalues(dataSet$REACTION_COUNT, from = c(NA), to = c(0))
dataSet$COMMENT_COUNT <- mapvalues(dataSet$COMMENT_COUNT, from = c(NA), to = c(0))

dataSet$SHARE_COUNT <- as.numeric(as.character(dataSet$SHARE_COUNT))
dataSet$REACTION_COUNT <- as.numeric(as.character(dataSet$REACTION_COUNT))
dataSet$COMMENT_COUNT <- as.numeric(as.character(dataSet$COMMENT_COUNT))

dataSet$RATING <- mapvalues(dataSet$RATING, from = c("mostly true","mixture of true and false","mostly false","no factual content"), to = c(3, 2, 1, 0))
dataSet$RATING <- as.numeric(as.character(dataSet$RATING))

# OVERALL - NO EXCLUDED CATEGORIES
allPublishers <- c(as.character(unique(dataSet$PUBLISHER)))
allTruthCounts <- c(sum(dataSet$RATING == 3), sum(dataSet$RATING == 2), sum(dataSet$RATING == 1), sum(dataSet$RATING == 0))
allPercents <- c((allTruthCounts[1] / 2282),(allTruthCounts[2] / 2282),(allTruthCounts[3] / 2282),(allTruthCounts[4] / 2282))


# CATEGORICAL ANALYSIS ( MAINSTREAM / LEFT / RIGHT )
# MAINSTREAM
mainNews <- dataSet[dataSet$CATEGORY == "mainstream",]
mainPublishers <- c(as.character(unique(mainNews$PUBLISHER)))
mainTruthCounts <- c(sum(mainNews$RATING == 3), sum(mainNews$RATING == 2), sum(mainNews$RATING == 1), sum(mainNews$RATING == 0))
mainPercents <- c((mainTruthCounts[1] / 1145),(mainTruthCounts[2] / 1145),(mainTruthCounts[3] / 1145),(mainTruthCounts[4] / 1145))


# LEFT
leftNews <- dataSet[dataSet$CATEGORY == "left",];
leftPublishers <- c(as.character(unique(leftNews$PUBLISHER)))
leftTruthCounts <- c(sum(leftNews$RATING == 3), sum(leftNews$RATING == 2), sum(leftNews$RATING == 1), sum(leftNews$RATING == 0))
leftPercents <- c((leftTruthCounts[1] / 471),(leftTruthCounts[2] / 471),(leftTruthCounts[3] / 471),(leftTruthCounts[4] / 471))


# RIGHT
rightNews <- dataSet[dataSet$CATEGORY == "right",];
rightPublishers <- c(as.character(unique(rightNews$PUBLISHER)))
rightTruthCounts <- c(sum(rightNews$RATING == 3), sum(rightNews$RATING == 2), sum(rightNews$RATING == 1), sum(rightNews$RATING == 0))
rightPercents <- c((rightTruthCounts[1] / 666),(rightTruthCounts[2] / 666),(rightTruthCounts[3] / 666),(rightTruthCounts[4] / 666))

# Visualizing Results
Categories = c(rep("All News", 4), rep("Mainstream News", 4), rep("Left News", 4), rep("Right News", 4))
Condition = rep(c("Mostly True", "Mix of True and False", "Mostly False", "No Factual Content"))
truthPercents = c(allPercents, mainPercents, leftPercents, rightPercents)
truthCounts = c(allTruthCounts, mainTruthCounts, leftTruthCounts, rightTruthCounts)
truthFrame = data.frame(Categories,Condition,truthPercents, truthCounts)

# Stacked
ggplot(truthFrame, aes(fill=Condition, y=truthPercents, x=Categories)) + geom_bar( stat="identity")

# Mulitple Regression Analysis
# Response = truthfulness (0-4)
# Predictor = Number of Shares, Likes, Comments
allReactionModel <- lm(dataSet$RATING ~ dataSet$SHARE_COUNT + dataSet$REACTION_COUNT + dataSet$COMMENT_COUNT, data = dataSet)
summary(allReactionModel)
anova(allReactionModel)
coefficients(allReactionModel)

mainReactionModel <- lm(mainNews$RATING ~ mainNews$SHARE_COUNT + mainNews$REACTION_COUNT + mainNews$COMMENT_COUNT, data = mainNews)
summary(mainReactionModel)
anova(mainReactionModel)
coefficients(mainReactionModel)

leftReactionModel <- lm(leftNews$RATING ~ leftNews$SHARE_COUNT + leftNews$REACTION_COUNT + leftNews$COMMENT_COUNT, data = leftNews)
summary(leftReactionModel)
anova(leftReactionModel)
coefficients(leftReactionModel)

rightReactionModel <- lm(rightNews$RATING ~ rightNews$SHARE_COUNT + rightNews$REACTION_COUNT + rightNews$COMMENT_COUNT, data = mainNews)
summary(rightReactionModel)
anova(rightReactionModel)
coefficients(rightReactionModel)

# Response = Number of Shares, Likes, Comments
# Predictor = truthfulness
reverseModel <- lm(dataSet$SHARE_COUNT + dataSet$REACTION_COUNT + dataSet$COMMENT_COUNT ~ dataSet$RATING, data = dataSet)
summary(reverseModel)
anova(reverseModel)
coefficients(reverseModel)
