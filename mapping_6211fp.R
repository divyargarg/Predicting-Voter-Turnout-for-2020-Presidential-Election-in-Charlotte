library("rgdal")
library("ggmap")
library("raster")
library("sp")
library("tmap")
library("readxl")
library("dplyr")
library("RColorBrewer")
library('reshape2')

#Shapefile for data; dataset for county
meck_county <- readOGR(dsn = "qol-data", layer = "NPA_2014_meck")
summary(meck_county)
plot(meck_county)

meck_add <- readOGR(dsn = "mat", layer = "MAT")
summary(meck_add)
plot(meck_add)
View(meck_add@data)

coord_data <- data_coord[!is.na(data_coord$longitude),]

#Testing point plotting
x <- as.numeric(unlist(coord_data[,123]))
y <- as.numeric(as.character(unlist(coord_data[,124])))
xy <- data.frame(cbind(y, x))

meck_countyLL <- spTransform(meck_county, "+init=epsg:4326")

plot(meck_countyLL, border = 'grey') 
points(xy, col = 'red', cex = 2)
axis(1)
axis(2)

#Adding additional information
meck_data <- read_excel("qol-data/QOL_Data_October_2018.xls", skip = 1, sheet = 3)
meck_data1 <- read_excel("qol-data/QOL_Data_October_2018.xls", skip = 1, sheet = 4)
meck_data2 <- read_excel("qol-data/QOL_Data_October_2018.xls", skip = 1, sheet = 5)
meck_data3 <- read_excel("qol-data/QOL_Data_October_2018.xls", skip = 1, sheet = 2)
meck_data4 <- read_excel("qol-data/QOL_Data_October_2018.xls", skip = 1, sheet = 7)

#Voter Participation melt
voter_part <- meck_data2[, c(1,10,12,14,16,18)]

shp <- merge(meck_county, meck_data, by = "NPA")
shp1 <- merge(meck_county, meck_data1, by = "NPA")
shp2 <- merge(meck_county, meck_data2, by = "NPA")
shp3 <- merge(meck_county, meck_data3, by = "NPA")
shp4 <- merge(meck_county, meck_data4, by = "NPA")
vote_shp <- merge(meck_county, voter_part, by = "NPA")

#Issues with 122 & 285?
tm_shape(meck_county) +
  tm_text("NPA") +
  tm_borders()

#Unemployment map
tm_shape(shp) +
  tm_fill("Employment_Rate_2016") +
  tm_borders()

#Household income
tm_shape(shp) +
  tm_fill("Household_Income_2016") +
  tm_borders()

#College degrees
tm_shape(shp1) +
  tm_fill("Bachelors_Degree_2016") +
  tm_borders()

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

#School attendance
shp1@data$Neighborhood_School_Attendance_2016
tm_shape(shp1) +
  tm_fill("Neighborhood_School_Attendance_2016") +
  tm_borders()

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
