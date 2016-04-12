# Exploratory Data Analysis Course Project 2

# Download and unzip files in the current working directory if they do not already exist

if (!file.exists("summarySCC_PM25.rds")) {
     download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", destfile="NEI_data.zip") 
     unzip("NEI_data.zip") } # Unzip files so that they can be loaded in current working directory 

if (!file.exists("Source_Classification_Code.rds")) {
     download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", destfile="NEI_data.zip") 
     unzip("NEI_data.zip") } # Unzip files so that they can be loaded in current working directory 

#Read in the pollution data and classification codes
if (!exists("NEI")) {
NEI <- readRDS("summarySCC_PM25.rds") }
if (!exists("SCC")) {
SCC <- readRDS("Source_Classification_Code.rds") }

# QUESTION 2:
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (𝚏𝚒𝚙𝚜 == "𝟸𝟺𝟻𝟷𝟶") from 1999
# to 2008? Use the base plotting system to make a plot answering this question.

# Subset data for only Baltimore City, Maryland (fips == "24510")

baltSub <- NEI[NEI$fips == "24510", ]

# Sum Emissions by year for Baltimore City Maryland only

pm25Year <- aggregate(Emissions ~ year, baltSub, sum)

# Verify plot is correct
with(pm25Year, plot(year, Emissions, pch = 20, type = "b", col = "black", xlab = "Year",
                    ylab = "Emissions (tons)", main = "Total PM2.5 Emissions From Baltimore City, Maryland"))

model <- lm(Emissions ~ year, pm25Year)
abline(model, lwd = 2, col = "blue")

# plot line graph to "plot1.png" with pixel (default) dimensions 480 X 480
png("plot2.png", width=480, height=480)
with(pm25Year, plot(year, Emissions, pch = 20, type = "b", col = "black", xlab = "Year",
                    ylab = "Emissions (tons)", main = "Total PM2.5 Emissions From Baltimore City, Maryland"))
model <- lm(Emissions ~ year, pm25Year)
abline(model, lwd = 2, col = "blue")
dev.off()