# Author: justmzen
# 
# Exploratory Data Analysis: Course Project 2
#
#

# Libraries loading
library('ggplot2')
library('reshape2')
library('dplyr')

# File downloading
fileUrl <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip'
if(!file.exists('input')) dir.create('input')
download.file(fileUrl, destfile = './input/data.zip')
unzip(zipfile = './input/data.zip', exdir = './input')
rm(fileUrl)

# File reading
NEI <- readRDS("./input/summarySCC_PM25.rds")
SCC <- readRDS("./input/Source_Classification_Code.rds")

## Variable conversion
NEI$fips <- as.factor(NEI$fips)
NEI$SCC <- as.factor(NEI$SCC)
NEI$Pollutant <- as.factor(NEI$Pollutant)
NEI$type <- as.factor(NEI$type)
NEI$year <- as.factor(NEI$year)

# Question 5: How have emissions from motor vehicle sources changed from
#  1999–2008 in Baltimore City?

# Filtering SCC codes for 'vehicle' sector
index <- grep(pattern = '[Vv]ehicle', x = SCC$EI.Sector, value = FALSE)
codes <- SCC$SCC[index]

# Filtering NEI data set for 'vehicle' sector
NEIBaltimoreVehicles <- 
        NEIBaltimore[NEIBaltimore$SCC %in% codes, c('Emissions', 'year')]

# Summarising per year
emissionsBaltimoreVehicles <- tapply(NEIBaltimoreVehicles$Emissions, 
                                     NEIBaltimoreVehicles$year, 
                                     sum)

# Reshaping data set
emissionsBaltimoreVehicles <- data.frame(levels(NEI$year), 
                                         emissionsBaltimoreVehicles)
names(emissionsBaltimoreVehicles) <- c('year', 'emissions')

# Plot
g <- ggplot(data = emissionsBaltimoreVehicles, aes(year, emissions))
g + geom_point() + labs(
        title = 'Emissions from motor vehicle sources in Baltimore City')

if(!file.exists('output')) dir.create('output')

dev.copy(png, './output/plot5.png')
dev.off()
