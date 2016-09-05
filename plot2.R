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

plot2_func <- function(df) {
  plot(df$Global_active_power, type="n", ylab = "Global Active Power (kilowatts)", xlab = "", xaxt = "n")
  axis(1, at = c(0,1450,2900), labels = c("Thu", "Fri", "Sat"))
  lines(df$Global_active_power)
}

plot2_png <- function(df = preprocess()) {
  png(filename = "plot2.png", width = 480, height = 480)
  plot2_func(df)
  dev.off()
}