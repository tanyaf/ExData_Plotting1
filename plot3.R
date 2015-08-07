#Dataset: Electric power consumption
#Description: Measurements of electric power consumption in one household with a one-minute sampling rate
#over a period of almost 4 years. Different electrical quantities and some sub-metering values are available
#Date: Date in format dd/mm/yyyy
#Time: time in format hh:mm:ss
#Global_active_power: household global minute-averaged active power (in kilowatt)
#Global_reactive_power: household global minute-averaged reactive power (in kilowatt)
#Voltage: minute-averaged voltage (in volt)
#Global_intensity: household global minute-averaged current intensity (in ampere)
#Sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). It corresponds to the kitchen, containing mainly a dishwasher, an oven and a microwave (hot plates are not electric but gas powered).
#Sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). It corresponds to the laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light.
#Sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). It corresponds to an electric water-heater and an air-conditioner.

getElData <- function() 
{
    con  <- file("household_power_consumption.txt", open = "r")
    
    line <- readLines(con, n = 1, warn = FALSE)
    #headVector <- strsplit(line, ";")
    #df <-read.csv(text=line, sep=';')
    df <-data.frame(Date=character(0),
                    Global_active_power=numeric(0),
                    Global_reactive_power=numeric(0),
                    Voltage=numeric(0),
                    Global_intensity=numeric(0),
                    Sub_metering_1=numeric(0),
                    Sub_metering_2=numeric(0),
                    Sub_metering_3=numeric(0)
    )
    
    repeat
    {
        line <- readLines(con, n = 1, warn = FALSE)
        if (length(line)>0)
        {
            myVector <- strsplit(line, ";")
            if(myVector[[1]][1]=="1/2/2007")
            { 
                break
            }
        }
        else
        {
            myVector <- NULL
            break
        }
    }
    
    if (!is.null(myVector))
    {
        while (myVector[[1]][1]=="1/2/2007" || myVector[[1]][1]=="2/2/2007")
        {
            df <- rbind(df, data.frame(Date=strptime(paste(myVector[[1]][1],myVector[[1]][2],sep=" "), "%d/%m/%Y %H:%M:%S"),
                                       Global_active_power=as.numeric(myVector[[1]][3]),
                                       Global_reactive_power=as.numeric(myVector[[1]][4]),
                                       Voltage=as.numeric(myVector[[1]][5]),
                                       Global_intensity=as.numeric(myVector[[1]][6]),
                                       Sub_metering_1=as.numeric(myVector[[1]][7]),
                                       Sub_metering_2=as.numeric(myVector[[1]][8]),
                                       Sub_metering_3=as.numeric(myVector[[1]][9])
            ))
            #dataList <- c(dataList,line)
            
            line <- readLines(con, n = 1, warn = FALSE)
            myVector <- strsplit(line, ";")
            
        }
    }
    
    close(con)
    #df - data fraim is ready
    df
}

plot1 <- function()
{
    hist(df$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
}

plot2 <- function()
{
    with(df,plot(Date,Global_active_power, type='l', ylab = "Global Active Power (kilowatts)", xlab=''))
}

plot3 <- function()
{
    with(df,plot(Date,Sub_metering_1, type='l', ylab = "Energy sub metering", xlab=''))
    with(df,points(Date,Sub_metering_2, type='l',col='red'))
    with(df,points(Date,Sub_metering_3, type='l',col='blue'))
    legend("topright", col = c('black',"blue", "red"), legend = c("Sub_metering_1", "Sub_metering_2",'Sub_metering_3'), lty=1)
}

plot41 <- function()
{
    with(df,plot(Date,Global_active_power, type='l', ylab = "Global Active Power", xlab=''))
}

plot42 <- function()
{
    with(df,plot(Date,Voltage, type='l', ylab = "Voltage", xlab='datetime'))
}

plot43 <- function()
{
    with(df,plot(Date,Sub_metering_1, type='l', ylab = "Energy sub metering", xlab=''))
    with(df,points(Date,Sub_metering_2, type='l',col='red'))
    with(df,points(Date,Sub_metering_3, type='l',col='blue'))
    legend("topright", col = c('black',"blue", "red"), legend = c("Sub_metering_1", "Sub_metering_2",'Sub_metering_3'), lty=1, bty='n', cex=0.65)
}

plot44 <- function()
{
    with(df,plot(Date,Global_reactive_power, type='l', xlab='datetime'))
}

plot4 <- function()
{
    oldparams <- par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
    plot41()
    plot42()
    plot43()
    plot44()
    par(oldparams)
}

#Sys.setlocale("LC_ALL", "English")
df<-getElData()
plot3()
dev.copy(png, file = "plot3.png")
dev.off()