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


# plot 1

makePlot1 <- function() {

    hist(smallDat$Global_active_power, col = "red", xlab = "Global Active Power (kilowatts)", main = "Global Active Power")

}


makePlot2 <- function(style = "long") {

    if (style == "short") {
        y_label <- "Global Active Power"
    }
    else {
        y_label <- "Global Active Power (kilowatts)"
        }



    with(smallDat, plot(DateTime, Global_active_power, type = "l", xlab = "", ylab = y_label))

}



                                        # plot 3


makePlot3 <- function() {

    with(smallDat, plot(DateTime, Sub_metering_1, type = "n", xlab = "", ylab = "Energy sub metering"))
    with(smallDat, points(DateTime, Sub_metering_1, type = "l", col = "black"))
    with(smallDat, points(DateTime, Sub_metering_2, type = "l", col = "red"))
    with(smallDat, points(DateTime, Sub_metering_3, type = "l", col = "blue"))
    legend("topright", lwd = 1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

}



makePlot4_1 <- function() {
    with(smallDat, plot(DateTime, Voltage, type = "l", xlab = "datetime", main = ""))
}

makePlot4_2 <- function() {
    with(smallDat, plot(DateTime, Global_reactive_power, type = "l", xlab = "datetime", main = ""))
    
    }

makePlot4 <- function() {
    par(mfcol = c(2,2), mar = c(4, 4, 2, 1), oma = c(0, 0, 0, 0))
    makePlot2("short")
    makePlot3()
    makePlot4_1()
    makePlot4_2()
    
}


## output plots

# output plot 1

png(file = "plot1.png", width = 480, height = 480)
makePlot1()
dev.off()

# output plot 2
png(file = "plot2.png", width = 480, height = 480)
makePlot2()
dev.off()

                                        # output plot 3

png(file = "plot3.png", width = 480, height = 480)
makePlot3()
dev.off()

                                        # output plot 4
png(file = "plot4.png", width = 480, height = 480)
makePlot4()
dev.off()
