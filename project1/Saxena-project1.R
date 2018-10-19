rawdata <- read.csv("New York State Children in Foster Care annually.csv")

devtools::install_github("UrbanInstitute/urbnmapr")
devtools::install_github("UI-Research/urbnthemes")

install.packages('devtools')
install.packages('ggplot2')
install.packages('scales')

library(scales)
library(tidyverse)
library(urbnthemes)
library(urbnmapr)
library(ggplot2)

#----------------------------------------------------------------------------------------------------
#Functions
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#----------------------------------------------------------------------------------------------------
#Test given data
test1 <- countydata
test2 <- counties

#----------------------------------------------------------------------------------------------------
#Clean data to get correct "county_name" and only consider year "2017"
q <- which(rawdata$Year == "2017")
cleandat <- rawdata[q,]
cleandat$County <- tolower(cleandat$County)
cleandat$County <- trim(cleandat$County)
colnames(cleandat)[1] <- "county_name"

q <- which(counties$state_name == "New York")
NY_counties <- counties[q,]

NY_counties$county_name <- gsub("County", "", NY_counties$county_name)
NY_counties$county_name <- tolower(NY_counties$county_name)
NY_counties$county_name <- trim(NY_counties$county_name)

#Join the data and the counties data
my_data <- left_join(cleandat, NY_counties, by = "county_name") 

#----------------------------------------------------------------------------------------------------
#Create New York State Visualization
cleandat %>% 
  left_join(NY_counties, by = "county_name") %>% 
  filter(state_name =="New York") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = Number.of.Children.Served)) +
  geom_polygon(color = "#ffffff", size = .25) +
  scale_fill_gradientn(labels = scales::number,
                       guide = guide_colorbar(title.position = "top")) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "Number of Children served") +
  theme_urbn_map()

#----------------------------------------------------------------------------------------------------
#WORK ON SECOND DATA SET TO GET FUNDING DETAILS
funding_data <-read.csv("New York State Child Welfare and Community Services Programs.csv")
colnames(funding_data)[6] <- "county_name"
funding_data$county_name <- tolower(funding_data$county_name)
funding_data$county_name <-trim(funding_data$county_name)

library(data.table)
dt <- data.table(funding_data)
dt2 <- dt[,list(sumamount = sum(Funding.Level), freq = .N), by = c("county_name")]

dt2 %>% 
  left_join(NY_counties, by = "county_name") %>% 
  filter(state_name =="New York") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = sumamount)) +
  geom_polygon(color = "#ffffff", size = .25) +
  scale_fill_gradientn(labels = scales::number,
                       guide = guide_colorbar(title.position = "top")) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(), legend.key.width = unit(.5, "in")) +
  labs(fill = "Total Funding received (USD)") +
  theme_urbn_map()

#----------------------------------------------------------------------------------------------------
#Income by County
incomedat <-read.csv("New York State Income by County.csv")
colnames(incomedat)[2] <- "county_name"
incomedat$county_name <- tolower(incomedat$county_name)
incomedat$county_name <-trim(incomedat$county_name)

incomedat$median.family.income <- as.numeric(gsub("[\\$,]", "", incomedat$median.family.income))
incomedat$per.capita.income <- as.numeric(gsub("[\\$,]", "", incomedat$per.capita.income))

incomedat %>% 
  left_join(NY_counties, by = "county_name") %>% 
  filter(state_name =="New York") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = per.capita.income)) +
  geom_polygon(color = "#ffffff", size = .25) +
  scale_fill_gradientn(labels = scales::number,
                       guide = guide_colorbar(title.position = "top")) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "Median Family Income (USD)") +
  theme_urbn_map()

uniqueNY$c <- unique(NY_counties$county_name)


#----------------------------------------------------------------------------------------------------
#TEST CODE
q1 <- which(cleandat$county_name == "albany")
X <- cleandat[q1,]