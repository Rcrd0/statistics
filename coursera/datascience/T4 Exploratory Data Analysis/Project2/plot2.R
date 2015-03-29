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
Baltimore = subset(NEI, fips == "24510")
emission = sapply(split(Baltimore$Emissions, Baltimore$year), sum)
emission = emission / 1000 # units in K tons
df = data.frame(year = year, Emissions = emission)
png("plot2.png")
with(df, plot(year, Emissions, main = "Baltimore Emissions along time", ylab = expression(PM[2.5] * " KTm"), xlab = "Year"))
with(df, lines(year, Emissions, col = "blue", lwd = 1))
abline(lm(Emissions ~ year, df), col = "red", lwd = 2)
dev.off()

