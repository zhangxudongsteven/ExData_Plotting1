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

plot1_func <- function(df) {
  hist(df$Global_active_power, col = "red", xlab = "Global Active Power (kilowatts)",
       main = "Global Active Power", xlim = c(0,6), ylim = c(0,1200), axes = FALSE)
  axis(1, at = seq(0,6,2))
  axis(2, at = seq(0,1200,200))
  axis(2, at = 1000)
}

plot1_png <- function(df = preprocess()) {
  png(filename = "plot1.png", width = 480, height = 480)
  plot1_func(df)
  dev.off()
}