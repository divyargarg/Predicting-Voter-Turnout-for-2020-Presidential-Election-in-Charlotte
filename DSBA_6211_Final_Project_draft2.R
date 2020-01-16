library(data.table)
library(dplyr)

#Source: http://apps.meckboe.org/pages/Download/VoterDataFile.zip
data <- read.delim("voter_data/voterdatafile.txt", sep = ",", header = TRUE, na.strings = c(" ", "\t", ""))

#Creating sample data for tests
data$address_full <- paste(data$mail_addr1, data$res_city_desc, data$state_cd, data$zip_code)

#Mecklenburg Co Coordinate Date
meck_address <- read.csv("meck_coord.csv")

#Run joins on voter address and coordinates
coord_table <- meck_address[,39:41]
rm(data_coord)
data_coord <- merge(x = data, y = coord_table, by.x = "address_full", by.y = "full_addre", all.x = TRUE)

#Analyzing output
table(!is.na(data_coord$longitude))
length(unique(data_coord[,13]))
length(unique(data_coord[,1]))
