Heart_rate <- read.csv("oura_heart-rate_2024-12-17T01-13-44.csv")
View(Heart_rate)

library(tidyverse)
library(ggplot2)

# Separate date and time into two different columns
Heart_rate_separated <- Heart_rate %>%
  separate(timestamp, into = c("date", "time"), sep = "T") %>%
  mutate(
    time = str_remove(time, "Z$")  # Remove the "Z" at the end of the time string
  )


Heart_rate_separated <- Heart_rate_separated %>%
  mutate(time = hms::as_hms(time))

View(Heart_rate_separated)
class(Heart_rate_separated$time)

#Morning heart rate

start_time <- hms::as_hms("10:00:00")
end_time <- hms::as_hms("11:00:00")


morning_heart_rate <- Heart_rate_separated %>%
  filter(time >= start_time & time <= end_time)

View(morning_heart_rate)

p1 <- ggplot(morning_heart_rate, aes(y = bpm, x = time, color = date)) + 
  geom_line(aes(group = 1)) +
  labs(title = "Morning Heart Rate", x = "Time", y = "BPM")
p1

#Evening heart rate

start_time <- hms::as_hms("15:00:00")
end_time <- hms::as_hms("16:00:00")


evening_heart_rate <- Heart_rate_separated %>%
  filter(time >= start_time & time <= end_time)

View(evening_heart_rate)

p2 <- ggplot(evening_heart_rate, aes(y = bpm, x = time, color = date)) + 
  geom_line(aes(group = 1)) +
  labs(title = "Evening Heart Rate", x = "Time", y = "BPM")
p2