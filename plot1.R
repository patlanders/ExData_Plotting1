library(lubridate)
library(dplyr)


dat <- read.table("data/household_power_consumption.txt", sep = ";", header = TRUE, as.is = TRUE)

ddate <- dmy(dat$Date)

exint <- ymd("2007-02-01") %--% ymd("2007-02-02")

goodCols <- ddate %within% exint

smallDat <- dat[goodCols == TRUE, ]

smallDat$Global_active_power <- as.numeric(smallDat$Global_active_power)
smallDat$Sub_metering_1 <- as.numeric(smallDat$Sub_metering_1)
smallDat$Sub_metering_2 <- as.numeric(smallDat$Sub_metering_2)
smallDat$Sub_metering_3 <- as.numeric(smallDat$Sub_metering_3)

cleanTime <- paste(smallDat$Date, smallDat$Time)
cleanTime <- dmy_hms(cleanTime)
smallDat <- mutate(smallDat, DateTime = cleanTime)

makePlot1 <- function() {

    hist(smallDat$Global_active_power, col = "red", xlab = "Global Active Power (kilowatts)", main = "Global Active Power")

}


# output plot 1

png(file = "plot1.png", width = 480, height = 480)
makePlot1()
dev.off()
