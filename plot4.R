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
