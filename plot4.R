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

#creating Plot 4:
par(mfrow = c(2,2), mar = c(4,4,2,1), oma = c(0,0,2,0))
with(tabela_select, {
  
  #plot top left
  plot(tabela_select$Global_active_power~tabela_select$datetime, type = "l",
       ylab = "Global Active Power (kW)", xlab = "", col = "blue")
  
  #plot top right
  plot(Voltage~datetime, type="l", ylab = "Voltage (V)", xlab = "")
  
  #plot bottom left
  plot(Sub_metering_1~datetime, type = "l", ylab = "Energy Sub Metering (kW)",
       xlab = "", col= "black")
  lines(Sub_metering_2~datetime,col = "red")
  lines(Sub_metering_3~datetime,col = "blue")
  legend("topright", col=c("black","red","blue"), lty = 1, lwd = 2, bty = "n", 
         legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), cex = 0.5)
  
  #plot bottom right
  plot(Global_reactive_power~datetime, type = "l", 
       ylab = "Global Reactive  Power (kW)", xlab = "")
})



#saving file plot4.png
#dev.copy(png, "./Exploratory_Data_Analysis/plot4.png", width = 480, height = 480)
#dev.off()
