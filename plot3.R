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

# QUESTION 3:
# Of the four types of sources indicated by the 𝚝𝚢𝚙𝚎 (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make 
# a plot answer this question.

# Load ggplot2

library(ggplot2)

# Subset data for only Baltimore City, Maryland (fips == "24510")

baltSub <- NEI[NEI$fips == "24510", ]

# Sum Emissions by year for Baltimore City Maryland by year and type

pm25Yeartype <- aggregate(Emissions ~ year + type, baltSub, sum)

# Verify plot is correct

g <- ggplot(pm25Yeartype, aes(year, Emissions))
g + geom_point() + geom_line(aes(color = type)) +
     facet_grid(. ~ type) + geom_smooth(method = "lm", se=FALSE) +
     labs(title = "Total PM2.5 Emissions By Type From Baltimore City, Maryland") +
     labs(x = "Year") +
     labs(y = "Emissions (tons)")

# plot shows that Emission type = Point is the only one that increased from 1999 to 2008

# plot line graph to "plot1.png" with pixel (default) dimensions 480 X 480
png("plot3.png", width=880, height=680)
g <- ggplot(pm25Yeartype, aes(year, Emissions))
g + geom_point() + geom_line(aes(color = type)) +
     facet_grid(. ~ type) + geom_smooth(method = "lm", se=FALSE) +
     labs(title = "Total PM2.5 Emissions By Type From Baltimore City, Maryland") +
     labs(x = "Year") +
     labs(y = "Emissions (tons)")
dev.off()