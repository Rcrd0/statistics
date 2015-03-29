# Exploratory Data Analysis Course
# Project 2
# Plot 1
# Question: Have total emissions from PM2.5 
#           decreased in the United States 
#           from 1999 to 2008? 
# Plot: Using the base plotting system, 
#           make a plot showing the total PM2.5 emission 
#           from all sources 
#           for each of the years 1999, 2002, 2005, and 2008.
#
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
year = unique(NEI$year)
emission = sapply(split(NEI$Emissions, NEI$year), sum)
emission = emission / 1000000 # units in million

png("plot1.png")
plot(year, emission, main = "Emissions along time", ylab = "Emission MM tons", xlab = "Year", ylim = c(0, 10))
lines(year, emission, col = "red", lwd = 2)
dev.off()
