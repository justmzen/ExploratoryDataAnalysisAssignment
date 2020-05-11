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

# Question 1: Have total emissions from PM2.5 decreased in the United States 
# from 1999 to 2008? Using the base plotting system, make a plot showing the
# total PM2.5 emission from all sources for each of the years 1999, 2002, 2005,
# and 2008.

# Calculations
totalEmissions <- tapply(NEI$Emissions, NEI$year, FUN = sum)

# Plot
plot(x = levels(NEI$year), y = totalEmissions, xlab = 'Year',
     ylab = 'Total Emissions [tons]')
title(main = 'Total emissions per year')

if(!file.exists('output')) dir.create('output')

dev.copy(png, './output/plot1.png')
dev.off()

# Question 2: Have total emissions from PM2.5 decreased in the Baltimore 
# City, Maryland (fips=="24510") from 1999 to 2008? Use the base plotting 
# system to make a plot answering this question.

# Subsetting NEI for the Baltimore City
NEIBaltimore <- NEI[NEI$fips == '24510', ]

# Emissions calculation in Baltimore
totalEmissionsBaltimore <- tapply(NEIBaltimore$Emissions, 
                                  NEIBaltimore$year, FUN = sum)

# Reshaping of totalEmissionsBaltimore
totalEmissionsBaltimore <- data.frame(levels(NEIBaltimore$year),
                                      totalEmissionsBaltimore)
names(totalEmissionsBaltimore) <- c('year', 'emissions')

# Plot
plot(x = levels(totalEmissionsBaltimore$year), 
     y = totalEmissionsBaltimore$emissions,
     xlab = 'Year', ylab = 'Total Emissions [tons]')
title(main = 'Total emissions in Baltimore City per year')

if(!file.exists('output')) dir.create('output')

dev.copy(png, './output/plot2.png')
dev.off()

# Question 3: Of the four types of sources indicated by the type
# (point, nonpoint, onroad, nonroad) variable, which of these four sources 
# have seen decreases in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? Use the ggplot2 
# plotting system to make a plot answer this question.

# Summarizing data by year and type
emissionsBaltimore <- tapply(NEIBaltimore$Emissions,
                             list(NEIBaltimore$year, NEIBaltimore$type),
                             sum)

# Reshaping of the obtained data
emissionsBaltimore <- data.frame(levels(NEIBaltimore$year),
                                 emissionsBaltimore)
names(emissionsBaltimore)[1] <- 'year'

emissionsBaltimore <- melt(emissionsBaltimore, id = 'year',
                           value.name = 'emissions',
                           variable.name = 'type')

# Plot
g <- ggplot(data = emissionsBaltimore, aes(year, emissions))
g + geom_point() + facet_grid(. ~ type) + 
        labs(title = 'Emissions in Baltimore City per year and type')

if(!file.exists('output')) dir.create('output')

dev.copy(png, './output/plot3.png')
dev.off()

# Question 4: Across the United States, how have emissions from coal 
# combustion-related sources changed from 1999–2008?

# Filtering SCC codes for 'coal' sources
index <- grep(pattern = '[Cc]oal', x = SCC$EI.Sector, value = FALSE)
codes <- SCC$SCC[index]

# Filtering NEI data set for 'coal' sources
NEICoal <- NEI[NEI$SCC %in% codes, c('Emissions', 'year')]

# Summarising per year
emissionsCoal <- tapply(NEICoal$Emissions, NEICoal$year, sum)

# Reshaping data set
emissionsCoal <- data.frame(levels(NEI$year), emissionsCoal)
names(emissionsCoal) <- c('year', 'emissions')

# Plot
g <- ggplot(data = emissionsCoal, aes(year, emissions))
g + geom_point() + labs(
        title = 'Emissions from coal combustion-related sources in the USA')

if(!file.exists('output')) dir.create('output')

dev.copy(png, './output/plot4.png')
dev.off()

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
