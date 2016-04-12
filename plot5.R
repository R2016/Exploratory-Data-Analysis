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

# QUESTION 5:
# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

# Check for number of coal observations in SCC
vsub <- grepl("Vehicles", SCC$SCC.Level.Two, ignore.case = TRUE)
vTrue <- table(vsub) # 1137 entries match "Vehicles"

# Subset SCC for only observations of "Vehicles"

sccVsub <- SCC[vsub, ]

# Use SCC subset to subset NEI data for vehicle SCC codes

neiVsub <- NEI[NEI$SCC %in% sccVsub$SCC, ]

# Further subset data for only Baltimore City, Maryland (fips == "24510")

baltVsub <- neiVsub[neiVsub$fips == "24510", ]

# Sum Emissions by year for Baltimore City Maryland vehicles only

baltVpm25 <- aggregate(Emissions ~ year, baltVsub, sum)

# Verify plot is correct
with(baltVpm25, plot(year, Emissions, pch = 20, type = "b", col = "black", xlab = "Year",
                    ylab = "Emissions (tons)", main = "Annual PM2.5 Emissions From Motor Vehicles in Baltimore City"))

model <- lm(Emissions ~ year, baltVpm25)
abline(model, lwd = 2, col = "blue")

# plot line graph to "plot5.png" with pixel (default) dimensions 480 X 480
png("plot5.png", width=480, height=480)
with(baltVpm25, plot(year, Emissions, pch = 20, type = "b", col = "black", xlab = "Year",
                     ylab = "Emissions (tons)", main = "Annual PM2.5 Emissions From Motor Vehicles in Baltimore City"))

model <- lm(Emissions ~ year, baltVpm25)
abline(model, lwd = 2, col = "blue")
dev.off()