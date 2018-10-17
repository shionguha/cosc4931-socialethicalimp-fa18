
#=============Section I: Data Importation and Pre-Processing===================#
# Clear Environment and Set Working Directory
rm(list = ls())
setwd("~/Documents/GitHub/cosc4931-socialethicalimp-fa18/project1")

# Import dataset
# https://www.kaggle.com/zynicide/wine-reviews
# Data description --
# country: The country that the wine is from
# description: A few sentences from a sommelier describing the wine's taste, smell, look, feel, etc.
# designation: The vineyard within the winery where the grapes that made the wine are from
# points: The number of points WineEnthusiast rated the wine on a scale of 1-100 (though they say they only post reviews for wines that score >=80)
# price: The cost for a bottle of the wine
# province: The province or state that the wine is from
# region_1: The wine growing area in a province or state (ie Napa)
# region_2: Sometimes there are more specific regions specified within a wine growing area (ie Rutherford inside the Napa Valley), but this value can sometimes be blank
# taster_name: Name of the person who tasted and reviewed the wine
# taster_twitter_handle: Twitter handle for the person who tasted and reviewed the wine
# title: The title of the wine review, which often contains the vintage if you're interested in extracting that feature
# variety: The type of grapes used to make the wine (ie Pinot Noir)
wine_df <- read.csv("winemag-data-130k-v2.csv")

# Only use prices less than 100, prices reachable to the average consumer
wine_df_u100 <- subset(wine_df, price <=100)

# Recode variables, assign the lower counts to "Other"
# Country
sort(table(wine_df_u100$country), decreasing=T)[1:20]
plot(sort(table(wine_df_u100$country), decreasing=T)[1:20])
wine_df_u100$country.1 <- car::recode(wine_df_u100$country, "'France'='France'; 'US'='US'; 
                                              'Italy'='Italy'; 'Spain'='Spain'; 
                                              'Portugal'='Portugal';'Chile'='Chile'; 
                                              'Argentina' = 'Argentina';
                                              else = 'Other'")
table(wine_df_u100$country.1)

#Variety
plot(sort(table(wine_df_u100$variety), decreasing=T)[1:20])
sort(table(wine_df_u100$variety), decreasing=T)[1:20]
wine_df_u100$variety.1 <- car::recode(wine_df_u100$variety, "'Pinot Noir'='Pinot Noir'; 
                                     'Chardonnay'='Chardonnay'; 
                                     'Cabernet Sauvignon'='Cabernet Sauvignon'; 
                                     'Red Blend'='Red Blend'; 
                                     'Bordeaux-style Red Blend'='Bordeaux-style Red Blend';
                                     'Riesling'='Riesling'; 'Sauvignon Blanc'='Sauvignon Blanc'; 
                                     else = 'Other'")
table(wine_df_u100$variety.1)

# Taster
plot(sort(table(wine_df_u100$taster_name), decreasing=T)[1:20])
sort(table(wine_df_u100$taster_name), decreasing = T)
wine_df_u100$taster_name.1 <- car::recode(wine_df_u100$taster_name, "'Roger Voss'='Roger Voss'; 
                                        'Michael Schachner'='Michael Schachner'; 
                                        'Kerin O’Keefe'='Kerin O’Keefe'; 
                                        'Virginie Boone'='Virginie Boone'; 
                                        'Paul Gregutt'='Paul Gregutt';
                                        'Matt Kettman'='Matt Kettman'; 
                                        else = 'Other'")
table(wine_df_u100$taster_name.1)

#Extract Vintage from title, only use usable years, convert to factor for 
# regression interpretability
wine_df_u100$vintage <- as.numeric(readr::parse_number(wine_df_u100$title))
table(wine_df_u100$vintage)
wine_df_u100_vtg <- subset(wine_df_u100, wine_df_u100$vintage >= 1990 & wine_df_u100$vintage < 2018 )
wine_df_u100_vtg$vintage <- as.factor(wine_df_u100_vtg$vintage)
table(wine_df_u100_vtg$vintage)
#==============================================================================#

#=============Section II: Summary Statistics and Visualizations================#
#Check distribution of points
hist(wine_df_u100_vtg$points, breaks = 20)

#calculate summary statistics
summary(wine_df_u100_vtg[,c("points", "price", "country.1", "taster_name.1", 
                            "variety.1","vintage")])

# plot average reviews by category
plot(sort(table(wine_df_u100_vtg$country.1), decreasing = TRUE), 
     ylab = "Count of Reviewed Wines", xlab = "Country", 
     main = "Count of Reviewed Wines by Country")
plot(sort(table(wine_df_u100_vtg$taster_name.1), decreasing = TRUE), 
     ylab = "Count of Reviewed Wines", xlab = "Taster Name", 
     main = "Count of Reviewed Wines by Taster Name")
plot(sort(table(wine_df_u100_vtg$variety.1), decreasing = TRUE), 
     ylab = "Count of Reviewed Wines", xlab = "Variety", 
     main = "Count of Reviewed Wines by Variety")
plot(sort(table(wine_df_u100_vtg$vintage), decreasing = TRUE), 
     ylab = "Count of Reviewed Wines", xlab = "Vintage", 
     main = "Count of Reviewed Wines by Vintage")

#plot average price and average points by country
sum <- merge(aggregate(wine_df_u100_vtg$price~wine_df_u100_vtg$country, FUN=mean),
             aggregate(wine_df_u100_vtg$points~wine_df_u100_vtg$country, FUN=mean))
fit <- glm(sum$`wine_df_u100_vtg$points` ~ sum$`wine_df_u100_vtg$price`)
plot(sum$`wine_df_u100_vtg$price`,sum$`wine_df_u100_vtg$points`,
     xlab = "Average Price", ylab = "Average Points", main = "Country")
abline(fit)
outls <- which(sum$`wine_df_u100_vtg$price` > 40
               | sum$`wine_df_u100_vtg$points` > 90 )
text(sum$`wine_df_u100_vtg$price`[outls], sum$`wine_df_u100_vtg$points`[outls], 
       labels=sum$`wine_df_u100_vtg$country`[outls], cex= 0.7, pos= 2)
outlss <- which(sum$`wine_df_u100_vtg$price` < 10 
                | sum$`wine_df_u100_vtg$points` < 85)
text(sum$`wine_df_u100_vtg$price`[outlss], sum$`wine_df_u100_vtg$points`[outlss], 
     labels=sum$`wine_df_u100_vtg$country`[outlss], cex= 0.7, pos= 4)
View(sum) #create table to discuss datapoints

#plot average price and average points by taster
sum <- merge(aggregate(wine_df_u100_vtg$price~wine_df_u100_vtg$taster_name, FUN=mean),
             aggregate(wine_df_u100_vtg$points~wine_df_u100_vtg$taster_name, FUN=mean))
fit <- glm(sum$`wine_df_u100_vtg$points` ~ sum$`wine_df_u100_vtg$price`)
plot(sum$`wine_df_u100_vtg$price`,sum$`wine_df_u100_vtg$points`,
     xlab = "Average Price", ylab = "Average Points", main = "Taster")
abline(fit)
outls <- which(sum$`wine_df_u100_vtg$price` > 40 
               | sum$`wine_df_u100_vtg$price` < 10 
               | sum$`wine_df_u100_vtg$points` < 86.5 
               | sum$`wine_df_u100_vtg$points` > 90 )
text(sum$`wine_df_u100_vtg$price`[outls], sum$`wine_df_u100_vtg$points`[outls], 
     labels=sum$`wine_df_u100_vtg$taster_name`[outls], cex= 0.7, pos= 2)
View(sum) #create table to discuss datapoints

#plot average price and average points by vintage
sum <- merge(aggregate(wine_df_u100_vtg$price~wine_df_u100_vtg$vintage, FUN=mean),
             aggregate(wine_df_u100_vtg$points~wine_df_u100_vtg$vintage, FUN=mean))
fit <- glm(sum$`wine_df_u100_vtg$points` ~ sum$`wine_df_u100_vtg$price`)
plot(sum$`wine_df_u100_vtg$price`,sum$`wine_df_u100_vtg$points`,
     xlab = "Average Price", ylab = "Average Points", main = "Vintage")
abline(fit)
outls <- which(sum$`wine_df_u100_vtg$price` > 45)
text(sum$`wine_df_u100_vtg$price`[outls], sum$`wine_df_u100_vtg$points`[outls], 
     labels=sum$`wine_df_u100_vtg$vintage`[outls], cex= 0.7, pos= 2)
outlss <- which(sum$`wine_df_u100_vtg$price` < 15)
text(sum$`wine_df_u100_vtg$price`[outlss], sum$`wine_df_u100_vtg$points`[outlss], 
     labels=sum$`wine_df_u100_vtg$vintage`[outlss], cex= 0.7, pos= 4)
View(sum) #create table to discuss datapoints

#plot average price and average points by variety
sum <- merge(aggregate(wine_df_u100_vtg$price~wine_df_u100_vtg$variety, FUN=mean),
             aggregate(wine_df_u100_vtg$points~wine_df_u100_vtg$variety, FUN=mean))
fit <- glm(sum$`wine_df_u100_vtg$points` ~ sum$`wine_df_u100_vtg$price`)
plot(sum$`wine_df_u100_vtg$price`,sum$`wine_df_u100_vtg$points`,
     xlab = "Average Price", ylab = "Average Points", main = "Variety")
abline(fit)
outls <- which(sum$`wine_df_u100_vtg$price` > 60
               & sum$`wine_df_u100_vtg$points` <90)
text(sum$`wine_df_u100_vtg$price`[outls], sum$`wine_df_u100_vtg$points`[outls], 
     labels=sum$`wine_df_u100_vtg$variety`[outls], cex= 0.7, pos= 2)
outlss <- which(sum$`wine_df_u100_vtg$price` < 20
                & sum$`wine_df_u100_vtg$points` < 81)
text(sum$`wine_df_u100_vtg$price`[outlss], sum$`wine_df_u100_vtg$points`[outlss], 
     labels=sum$`wine_df_u100_vtg$variety`[outlss], cex= 0.7, pos= 4)
outlsss <- which(sum$`wine_df_u100_vtg$points` > 95)
text(sum$`wine_df_u100_vtg$price`[outlsss], sum$`wine_df_u100_vtg$points`[outlsss], 
     labels=sum$`wine_df_u100_vtg$variety`[outlsss], cex= 0.7, pos= 4)
outlssss <- which(sum$`wine_df_u100_vtg$points` > 92.5
                  & sum$`wine_df_u100_vtg$price` < 20)
text(sum$`wine_df_u100_vtg$price`[outlssss], sum$`wine_df_u100_vtg$points`[outlssss], 
     labels=sum$`wine_df_u100_vtg$variety`[outlssss], cex= 0.7, pos= 3)
View(sum) #create table to discuss datapoints

# plot $1 bin by average price, dd suspected price break
bins <- tapply(wine_df_u100_vtg$points, 
               cut(wine_df_u100_vtg$price, seq(0, 100, by=1)), mean)
plot(bins, main = "Average Points Calculated in $1 Bins", 
     xlab = "$1 Bins of Price", ylab = "Average Points")
abline(v=20, lty = 3)
#==============================================================================#

#===========================Section III: Regressions===========================#
#linear regression model
lin.mod <- lm(points ~ price + relevel(vintage, ref = "1990") 
              + relevel(country.1, ref = "Other") 
              + relevel(taster_name.1, ref = "Other")
              + relevel(variety.1, ref = "Other")
              , data = wine_df_u100_vtg)
stargazer::stargazer(lin.mod, out = "linmod.html", type = "html")

#test for segmentation 
segmented.mod <- segmented::segmented(lin.mod, seg.Z = ~ price, psi=20)
predict(segmented.mod)
summary(segmented.mod)
segmented::slope(segmented.mod)

#plot the actual segmentation point in bins
bins <- tapply(wine_df_u100_vtg$points, 
               cut(wine_df_u100_vtg$price, seq(0, 100, by=1)), mean)
plot(bins, main = "Average Points Calculated in $1 Bins", 
     xlab = "$1 Bins of Price", ylab = "Average Points")
abline(v=23.805)
#==============================================================================#










