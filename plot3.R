#reading the file, defining that NA = ?, classes of each column
tabela <- read.table("./Exploratory_Data_Analysis/household_power_consumption.txt",
                     header = TRUE, sep = ";", na.strings = "?", colClasses = 
                       c("character","character", "numeric","numeric","numeric",
                         "numeric","numeric","numeric","numeric"))

#changing date to Date Type:
tabela$Date <- as.Date(tabela$Date, "%d/%m/%Y")

#selecting datas from February 1st to February 2nd, 2007
tabela_select <- subset(tabela, Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))

#excluding NA
tabela_select <- tabela_select[complete.cases(tabela_select),]

#combining Date and Time in one column datetime and excluding them from the data
datetime <- paste(tabela_select$Date,tabela_select$Time)
datetime <- setNames(datetime, "DateTime")
tabela_select <- tabela_select[,!(names(tabela_select) %in% c("Date", "Time"))]

#adding datetime column to data
tabela_select <- cbind(datetime,tabela_select)

#formating datetime using calendat time (ct) of POSIX package
tabela_select$datetime <- as.POSIXct(datetime)

#creating Plot 3:
with(tabela_select, {
  plot(Sub_metering_1~datetime, type = "l", ylab = "Global Active Power (kW)",
       xlab = "", col= "black")
  lines(Sub_metering_2~datetime,col = "red")
  lines(Sub_metering_3~datetime,col = "blue")
})
legend("topright", legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
       col = c("black","red","blue"), lty = 'solid', lwd = 2, cex = 0.5)


#saving file plot3.png
#dev.copy(png, "./Exploratory_Data_Analysis/plot3.png", width = 480, height = 480)
#dev.off()
