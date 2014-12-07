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

# making plots

# Plot 1 ###########################################################

par(bg = "transparent")
hist(df$Global_active_power,xlab="Global Active Power (killowatts)", 
     main= "Global Active Power",col = "red")

dev.copy(png, file = "plot1.png", width=480, height=480) ## Copy my plot to a PNG file
dev.off() ## Don't forget to close the PNG device!