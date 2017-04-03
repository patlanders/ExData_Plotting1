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
smallDat$Sub_metering_3 <- as.numeric(smallDat$Sub_metering_3=)

cleanTime <- paste(smallDat$Date, smallDat$Time)
cleanTime <- dmy_hms(cleanTime)
smallDat <- mutate(smallDat, DateTime = cleanTime)

makePlot3 <- function() {

    with(smallDat, plot(DateTime, Sub_metering_1, type = "n", xlab = "", ylab = "Energy sub metering"))
    with(smallDat, points(DateTime, Sub_metering_1, type = "l", col = "black"))
    with(smallDat, points(DateTime, Sub_metering_2, type = "l", col = "red"))
    with(smallDat, points(DateTime, Sub_metering_3, type = "l", col = "blue"))
    legend("topright", lwd = 1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

}

                                        # output plot 3

png(file = "plot3.png", width = 480, height = 480)
makePlot3()
dev.off()
