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
