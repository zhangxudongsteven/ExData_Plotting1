preprocess <- function(df_path = "exploratory_proj_1/household_power_consumption.txt") {
  library(dplyr)
  library(tidyr)
  library(lubridate)
  df <- read.csv(df_path, sep = ";", na.strings = c("?", ""))
  df <- df %>% 
    mutate(DateTime=paste(Date, Time)) %>% 
    select(-c(Date:Time)) %>% 
    mutate(DateTime = dmy_hms(DateTime)) %>%
    filter(year(DateTime) == 2007 & month(DateTime) == 2 & day(DateTime) < 3)
  df
}

plot3_func <- function(df) {
  plot(df$Sub_metering_1, type="n", ylab = "Global Active Power (kilowatts)", xlab = "", xaxt = "n")
  axis(1, at = c(0,1450,2900), labels = c("Thu", "Fri", "Sat"))
  lines(df$Sub_metering_1, col = "black")
  lines(df$Sub_metering_2, col = "red")
  lines(df$Sub_metering_3, col = "blue")
  legend("topright", col = c("black", "red", "blue"), lty=c(1,1), 
         legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
}

plot3_png <- function(df = preprocess()) {
  png(filename = "plot3.png", width = 480, height = 480)
  plot3_func(df)
  dev.off()
}