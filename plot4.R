path <- "D:/coursera/Exploratory Data Analysis/week 1/exdata-data-household_power_consumption"
setwd(path)

datafile <- "household_power_consumption.txt"

con  <- file(datafile, open = "r")

# read header
oneLine <- readLines(con, n = 1, warn = FALSE)
head <- strsplit(oneLine, ";")

# read datafile line by line, only the specified dates
dataList <- list()

while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
        myVector <- strsplit(oneLine, ";")
        date <- as.Date(myVector[[1]][1], format="%d/%m/%Y")
        if( date<"2007-02-01") next 
        if( date>"2007-02-02") break
        dataList <- c(dataList,myVector)
        
} 

close(con)

# convert to dataframe
df <- data.frame(matrix(unlist(dataList), nrow = length(dataList), byrow=T),stringsAsFactors = T)
names(df) <- head[[1]]
cols = 3:9    
df[,cols] = apply(df[,cols], 2, function(x) as.numeric(as.character(x)))

df[,2] <-  interaction(df[,2],df[,1])


# Plot 4 ###########################################################
png(file = "plot4.png", width=480, height=480)
plot.new()
par(mfrow = c(2, 2))
par(bg = "transparent")


# 1,1

with(df, plot(Time, Global_active_power, xaxt="n" , cex.lab=.8,
              ylab = "Global Active Power"))

with(df, lines(Time, Global_active_power ) )
axis(1, at= c(1, nrow(df)/2+1,nrow(df)), labels= c("Thu","Fri","Sat") )

# 1,2

with(df, plot(Time, Voltage, xaxt="n" , cex.lab=1,
              ylab = "Voltage",xlab = "datetime" ))

with(df, lines(Time, Voltage ) )
axis(1, at= c(1, nrow(df)/2+1,nrow(df)), labels= c("Thu","Fri","Sat") )

# 2,1

col <- c("black","red","blue")

with(df, plot(Time, Sub_metering_1, xaxt="n" , cex.lab=1,
              ylab = "Energy sub metering", col = col[1] ,
              ylim=range(c(Sub_metering_1,Sub_metering_2,Sub_metering_3))))

with(df, lines(Time, Sub_metering_1 , col = col[1] ) )

par(new = TRUE)
with(df, plot(Time, Sub_metering_2, xaxt="n" , yaxt="n" ,axes=FALSE, col = col[2]
              ,ylim=range(c(Sub_metering_1,Sub_metering_2,Sub_metering_3))))
with(df, lines(Time, Sub_metering_2 , col = col[2] ) )
par(new = TRUE)
with(df, plot(Time, Sub_metering_3, xaxt="n" , yaxt="n" ,axes=FALSE, col = col[3] 
              , ylim=range(c(Sub_metering_1,Sub_metering_2,Sub_metering_3))))
with(df, lines(Time, Sub_metering_3 , col = col[3] ) )

legend("topright", lty = c(1, 1,1), col = col, 
       legend = c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"),  
       cex = .8,lwd=2)
axis(1, at= c(1, nrow(df)/2+1,nrow(df)), labels= c("Thu","Fri","Sat") )

# 2,2

with(df, plot(Time, Global_reactive_power, xaxt="n" , cex.lab=1,
              ylab = "Global_reactive_power",xlab = "datetime" ))

with(df, lines(Time, Global_reactive_power ) )
axis(1, at= c(1, nrow(df)/2+1,nrow(df)), labels= c("Thu","Fri","Sat") )

dev.off()