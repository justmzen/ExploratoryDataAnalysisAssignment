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
