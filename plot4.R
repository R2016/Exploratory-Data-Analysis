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

# QUESTION 4:
# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

# Check for number of coal observations in SCC
combsub <- grepl("Comb", SCC$EI.Sector, ignore.case = TRUE)
csTrue <- table(combsub) # 530 entries match "Comb"

coalsub <- grepl("Coal", SCC$SCC.Level.Three, ignore.case = TRUE)
table(coalsub) # 181 match coal in SCC.Level.Three

subVect <- combsub & coalsub

# Subset SCC for only observations of "Comb" and "Coal"

sccSub <- SCC[subVect, ]

# Use SCC subset to subset NEI data for SCC codes satisfying both "Comb" and "Coal"

neiSub <- NEI[NEI$SCC %in% sccSub$SCC, ]

# Sum Emissions by year
combCoalpm25 <- aggregate(Emissions ~ year, neiSub, sum)

# Verify plot is correct
with(combCoalpm25, plot(year, Emissions, pch = 20, type = "b", col = "black", xlab = "Year",
                    ylab = "Emissions (tons)", main = "Annual PM2.5 Emissions From Coal Combustion-Related Sources"))

model <- lm(Emissions ~ year, combCoalpm25)
abline(model, lwd = 2, col = "blue")

# plot line graph to "plot4.png" with pixel (default) dimensions 480 X 480
png("plot4.png", width=480, height=480)
with(combCoalpm25, plot(year, Emissions, pch = 20, type = "b", col = "black", xlab = "Year",
                        ylab = "Emissions (tons)", main = "Annual PM2.5 Emissions From Coal Combustion-Related Sources"))

model <- lm(Emissions ~ year, combCoalpm25)
abline(model, lwd = 2, col = "blue")
dev.off()