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
# Compare emissions from motor vehicle sources in Baltimore City with emissions from 
# motor vehicle sources in Los Angeles County, California (𝚏𝚒𝚙𝚜 == "𝟶𝟼𝟶𝟹𝟽"). 
# Which  city has seen greater changes over time in motor vehicle emissions?

# Check for number of coal observations in SCC
vsub <- grepl("Vehicles", SCC$SCC.Level.Two, ignore.case = TRUE)
vTrue <- table(vsub) # 1137 entries match "Vehicles"

# Subset SCC for only observations of "Vehicles" sources 

sccVsub <- SCC[vsub, ]

# Use SCC subset to subset NEI data for vehicle SCC codes

neiVsub <- NEI[NEI$SCC %in% sccVsub$SCC, ]

# Further subset data for only Baltimore City, Maryland (fips == "24510")

baltVsub <- neiVsub[neiVsub$fips == "24510", ]

# Further subset data for only Los Angeles County (fips == "06037")

losVsub <- neiVsub[neiVsub$fips == "06037", ]

# Sum Emissions by year for Baltimore City and Los Angeles vehicles only

baltVpm25 <- aggregate(Emissions ~ year, baltVsub, sum)
losVpm25 <- aggregate(Emissions ~ year, losVsub, sum)

# Add new variable (column) to identify County

baltVpm25$County <- "Baltimore City"
losVpm25$County <- "Los Angeles County"

# Combine Baltimore and LA

baltlosVpm25 <- rbind(baltVpm25, losVpm25)

# Round to whole numbers so I can plot point values
baltlosVpm25$Emissions <- round(baltlosVpm25$Emissions)

# Verify plot is correct

g <- ggplot(baltlosVpm25, aes(year, Emissions))
g + geom_point() + geom_line(aes(color = County)) +
     facet_grid(. ~ County) + geom_smooth(method = "lm", se=FALSE) +
     labs(title = "Annual PM2.5 Emissions From Motor Vehicles") +
     labs(x = "Year") +
     labs(y = "Emissions (tons)") +
     geom_text(aes(label = Emissions), size = 4, nudge_y = 300)

# plot line graph to "plot6.png" with pixel (default) dimensions 680 X 480
png("plot6.png", width=680, height=480)
g <- ggplot(baltlosVpm25, aes(year, Emissions))
g + geom_point() + geom_line(aes(color = County)) +
     facet_grid(. ~ County) + geom_smooth(method = "lm", se=FALSE) +
     labs(title = "Annual PM2.5 Emissions From Motor Vehicles") +
     labs(x = "Year") +
     labs(y = "Emissions (tons)") +
     geom_text(aes(label = Emissions), size = 4, nudge_y = 300)
dev.off()