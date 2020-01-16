library("rgdal")
library("tmap")
library("readxl")
library("dplyr")
library('reshape2')

#Shapefile for data; dataset for county
meck_county <- readOGR(dsn = "qol-data", layer = "NPA_2014_meck")
summary(meck_county)
plot(meck_county)

#Adding additional information
#Income and demographics
meck_data <- read_excel("qol-data/QOL_Data_October_2018.xls", skip = 1, sheet = 3)
#Education
meck_data1 <- read_excel("qol-data/QOL_Data_October_2018.xls", skip = 1, sheet = 4)
#Community Partcipiation/Voter Participation
meck_data2 <- read_excel("qol-data/QOL_Data_October_2018.xls", skip = 1, sheet = 5)
#Population
meck_data3 <- read_excel("qol-data/QOL_Data_October_2018.xls", skip = 1, sheet = 2)
#Healthcare
meck_data4 <- read_excel("qol-data/QOL_Data_October_2018.xls", skip = 1, sheet = 7)

#Voter Participation Target Set
voter_part <- meck_data2[, c(1,10,12,14,16,18)]

#Income shapefile
shp <- merge(meck_county, meck_data, by = "NPA")
#Education shapefile
shp1 <- merge(meck_county, meck_data1, by = "NPA")
#Community Partcipiation/Voter Participation shapefile
shp2 <- merge(meck_county, meck_data2, by = "NPA")
#Population and demographics shapefile
shp3 <- merge(meck_county, meck_data3, by = "NPA")
#Healthcare shapefile
shp4 <- merge(meck_county, meck_data4, by = "NPA")
vote_shp <- merge(meck_county, voter_part, by = "NPA")

#Section of County by NPA ID
#Issues with 122 & 285?
tm_shape(meck_county) +
  tm_text("NPA") +
  tm_borders()

#INCOME 
#Unemployment map
tm_shape(shp) +
  tm_fill("Employment_Rate_2016") +
  tm_borders()

#Household income
tm_shape(shp) +
  tm_fill("Household_Income_2016") +
  tm_borders()

#EDUCATION
#College degrees
tm_shape(shp1) +
  tm_fill("Bachelors_Degree_2016") +
  tm_borders()

#School attendance
shp1@data$Neighborhood_School_Attendance_2016
tm_shape(shp1) +
  tm_fill("Neighborhood_School_Attendance_2016") +
  tm_borders()

#POPULATION & DEMOGRAPHICS
#Percentage of Population- Caucasian
head(shp3@data$White_Population_2016)
tm_shape(shp3) +
  tm_fill("White_Population_2016") +
  tm_borders()

#Percentage of Population- Black
head(shp3@data$Black_Population_2016)
tm_shape(shp3) +
  tm_fill("Black_Population_2016") +
  tm_borders()

#Mean Age of Population
head(shp3@data$Age_of_Residents_2016)
tm_shape(shp3) +
  tm_fill("Age_of_Residents_2016") +
  tm_borders()

#HEALTHCARE
#Grocery proximity
shp4@data$Grocery_Proximity_2017
tm_shape(shp4) +
  tm_fill("Grocery_Proximity_2017") +
  tm_borders()

#Voting Participation Graphics
#Evaluating total percentage
head(vote_shp@data)
vote_shp@data$ACRES <- NULL
vote_shp1 <- vote_shp

#Melt data to wrap
vote_shp1@data <- melt(vote_shp@data, id.vars = "NPA")
head(vote_shp1@data)
unique(vote_shp1@data$variable)

#Voting participation for each election
tm_shape(vote_shp1) +     
  tm_fill('value') +
  tm_borders() +
  tm_facets(by = "variable" ) 

#Evaluating shifts in year to year participation
#Presidential Elections
head(vote_shp@data)
vote_shp@data$Difference2012_2016 <- vote_shp@data$Voter_Participation_2016 - vote_shp@data$Voter_Participation_2012

#Changes in voter participation in Mecklenburg Co in 2012 & 2016 Elections
tm_shape(vote_shp) +
  tm_fill("Difference2012_2016") +
  tm_borders()

#Presidential & Congressional Elections
vote_shp2 <- vote_shp
vote_shp2@data$Difference2014_2016 <- vote_shp2@data$Voter_Participation_2016 - vote_shp2@data$Voter_Participation_2014
vote_shp2@data$Difference2012_2014 <- vote_shp2@data$Voter_Participation_2014 - vote_shp2@data$Voter_Participation_2012
vote_shp2@data$Difference2010_2012 <- vote_shp2@data$Voter_Participation_2012 - vote_shp2@data$Voter_Participation_2010

#Remove columns, then melt data to wrap
vote_shp2@data <- vote_shp2@data[,c(1,8:10)]
head(vote_shp2@data)

vote_shp2@data <- melt(vote_shp2@data, id.vars = "NPA")
head(vote_shp2@data)
unique(vote_shp2@data$variable)

#Changes in participation between Presidential & Congressional Elections
tm_shape(vote_shp2) +     
  tm_fill('value') +
  tm_borders() +
  tm_facets(by = "variable" ) 
