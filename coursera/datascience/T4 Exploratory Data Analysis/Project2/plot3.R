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

# construir un Data Frame de emisiones de baltimore
# Columnas: año, point, nonpoint, onroad, nonroad
BaltimoreEmissions = data.frame(year = unique(Baltimore$year))
ss1 = subset(Baltimore, type == "POINT")
ss2 = subset(Baltimore, type == "NONPOINT")
ss3 = subset(Baltimore, type == "ON-ROAD")
ss4 = subset(Baltimore, type == "NON-ROAD")

BaltimoreEmissions$Point <- sapply(split(ss1$Emissions, ss1$year), sum)
BaltimoreEmissions$Non.Point <- sapply(split(ss2$Emissions, ss2$year), sum)
BaltimoreEmissions$On.Road <- sapply(split(ss3$Emissions, ss3$year), sum)
BaltimoreEmissions$Non.Road <- sapply(split(ss4$Emissions, ss4$year), sum)

# Base system
par(mfrow = c(1, 4))
plot(BaltimoreEmissions$year, BaltimoreEmissions$Point, ylim = c(0, 3000))
abline(lm(Point ~ year, BaltimoreEmissions), col = "red", lwd = 2)

plot(BaltimoreEmissions$year, BaltimoreEmissions$Non.Point, ylim = c(0, 3000))
abline(lm(Non.Point ~ year, BaltimoreEmissions), col = "red", lwd = 2)

plot(BaltimoreEmissions$year, BaltimoreEmissions$On.Road, ylim = c(0, 3000))
abline(lm(On.Road ~ year, BaltimoreEmissions), col = "red", lwd = 2)
# fit <- lm(On.Road ~ year, BaltimoreEmissions)
# abline(fit)
# abline(h = BaltimoreEmissions$Road)
plot(BaltimoreEmissions$year, BaltimoreEmissions$Non.Road, ylim = c(0, 3000))
abline(lm(Non.Road ~ year, BaltimoreEmissions), col = "red", lwd = 2)


#---------------------- Lattice
library(lattice)

Baltimore$year.type = paste(Baltimore$year, Baltimore$type, sep = ".")

# Dataframe columns
# year, type, total.emissions
df = data.frame(year = rep(c(1999, 2002, 2005, 2008), each = 4), 
                type = rep(c("POINT", "NONPOINT", "ON-ROAD", "NON-ROAD"), 4))
df$year.type = paste(df$year, df$type, sep =".")

                total = sum(Baltimore$Emissions[Baltimore$year == year & Baltimore&type == type]))
# Columnas: año, point, nonpoint, onroad, nonroad
BaltimoreEmissions = data.frame(year = unique(Baltimore$year))
ss1 = subset(Baltimore, type == "POINT")
ss2 = subset(Baltimore, type == "NONPOINT")
ss3 = subset(Baltimore, type == "ON-ROAD")
ss4 = subset(Baltimore, type == "NON-ROAD")

with(Baltimore, xyplot(Baltimore)

png("plot2.png")
with(df, plot(year, Emissions, main = "Baltimore Emissions along time", ylab = expression(PM[2.5] * " KTm"), xlab = "Year"))
with(df, lines(year, Emissions, col = "blue", lwd = 1))
abline(lm(Emissions ~ year, df), col = "red", lwd = 2)
dev.off()

