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
