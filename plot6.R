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

# Question 6: Compare emissions from motor vehicle sources in Baltimore City
# with emissions from motor vehicle sources in Los Angeles County, California
# (fips == "06037"). Which city has seen greater changes over time in motor
# vehicle emissions?

# NEI Los Angeles County data set by subsetting NEI data set
NEILA <- NEI[NEI$fips == '06037', ]

# Filtering NEILA data set for 'vehicle' sector
NEILAVehicles <- NEILA[NEILA$SCC %in% codes, c('Emissions', 'year')]

# Summarising per year on LA
emissionsLAVehicles <- tapply(NEILAVehicles$Emissions, 
                              NEILAVehicles$year, 
                              sum)

# Reshaping data set
emissionsLAVehicles <- data.frame(levels(NEI$year), 
                                  emissionsLAVehicles)
names(emissionsLAVehicles) <- c('year', 'emissions')

# Relative changes for both cities
emissionsBaltimoreVehicles <- 
        dplyr::mutate(emissionsBaltimoreVehicles,
                      relativeChangeBaltimore = (emissions/emissions[1]) -1)

emissionsLAVehicles <- 
        dplyr::mutate(emissionsLAVehicles,
                      relativeChangeLA = (emissions/emissions[1]) -1)

# Data sets join
comp <- merge(emissionsBaltimoreVehicles, emissionsLAVehicles, by = 'year')
names(comp) <- c('year', 'emissionsBaltimore', 'relativeChangeBaltimore',
                 'emissionsLA', 'relativeChangeLA')

# Plot
g <- ggplot(data = comp, aes(year))
g + geom_point(aes(y = relativeChangeBaltimore, 
                   color = 'relativeChangeBaltimore'))+
        geom_point(aes(y = relativeChangeLA, color = 'relativeChangeLA'))+
        labs(title = 'Relative changes from motor vehicle sources for Baltimore City and LA over time')+
        xlab('Year')+ylab('Relative variation')

if(!file.exists('output')) dir.create('output')

dev.copy(png, './output/plot6.png')
dev.off()
