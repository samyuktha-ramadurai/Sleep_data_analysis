#Reading sleep dataset

sleep_dataset <- read.csv("oura_sleep.csv")
View(sleep_dataset)

library(tidyverse)
library(lubridate) #Used for working with dates and times

#The sleep dataset contains the bedtime_start and bedtime_end columns in the format "YYYY-MM-DDTHH:MM:SS-07:00".
#We need to remove the 
# Data wrangling

sleep <- sleep_dataset %>%
  mutate(
    bedtime_start = str_extract(bedtime_start, "(?<=T).+"),  # Extract time after "T"
    bedtime_start = str_remove(bedtime_start, "-07:00$"),  # Remove the "-07:00" at the end of the time string
    bedtime_start = hms::as_hms(bedtime_start),
    bedtime_end = str_extract(bedtime_end, "(?<=T).+"),  # Extract time after "T"
    bedtime_end = str_remove(bedtime_end, "-07:00$"),  # Remove the "-07:00" at the end of the time string
    bedtime_end = hms::as_hms(bedtime_end),
    day_of_week = as.Date(day, format = "%m/%d/%Y") %>% wday(label = TRUE)
  ) 

View(sleep)

sleep_long <- sleep %>%
  filter(total_sleep_duration > 7200)  # Filter out sleep durations less than 2 hours

View(sleep_long)

mean_efficiency <- sleep_long |> 
  group_by(day_of_week) |> 
  summarise(mean_efficiency = mean(efficiency))

mean_efficiency

mean_duration  <- sleep_long |> 
  group_by(day_of_week) |> 
  summarise(mean_duration = mean(total_sleep_duration))
            
#Some plots
p1 <- ggplot(sleep_long, aes(x = bedtime_start, y = efficiency)) + geom_point() + theme_minimal()    
p1
p2 <- ggplot(sleep_long, aes(x = bedtime_start, y = total_sleep_duration)) + geom_point() + theme_minimal()    
p2
p3 <- ggplot(sleep_long, aes(x = bedtime_end, y = total_sleep_duration)) + geom_point() + theme_minimal()    
p3
p4 <- ggplot(sleep_long, aes(x = total_sleep_duration, y = efficiency)) + geom_point() + theme_minimal()
p4
#Plot mean efficiency and day of the week
p4 <- ggplot(mean_efficiency, aes(x = day_of_week, y = mean_efficiency)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Sleep Efficiency by Day of the Week",
       x = "Day of the Week",
       y = "Mean Sleep Efficiency") +
  theme_minimal()

p4

#Plot for sleep duration by day of the week

p5 <- ggplot(mean_duration, aes(x = day_of_week, y = mean_duration)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Sleep Duration by Day of the Week",
       x = "Day of the Week",
       y = "Mean Sleep Duration") +
  theme_minimal()

p5

#Correlation between sleep duration and efficiency

lm(sleep_long$efficiency ~ sleep_long$total_sleep_duration)
summary(lm(sleep_long$efficiency ~ sleep_long$total_sleep_duration))


# Sleep dataset with lower sleep durations (less than 2 hours) included

sleep_short <- sleep %>%
  filter(total_sleep_duration <= 7200) %>%  # Filter out sleep durations less than 2 hours
  group_by(day) %>% #Some days have multiple entries
  summarise(mean_sleep_duration = mean(total_sleep_duration), mean_efficiency = mean(efficiency)) %>%
  mutate(day_of_week = as.Date(day, format = "%m/%d/%Y") %>% wday(label = TRUE)
  )
  
View(sleep_short)

frequency_day_of_week  <- sleep_short |> 
  group_by(day_of_week) |> 
  summarise(frequency = n())
frequency_day_of_week 

#Plot for sleep efficiency vs sleep duration
ggplot(sleep_short, aes(x = mean_sleep_duration, y = mean_efficiency)) + 
  geom_point() + 
  labs(title = "Sleep Efficiency vs sleep duration under 2 hours",
       x = "Mean sleep duration",
       y = "Mean Sleep efficiency") +
  theme_minimal()

ggplot(sleep_short, aes(x = day_of_week, y = mean_sleep_duration)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Mean Sleep Duration by Day of the Week",
       x = "Day of the Week",
       y = "Mean Sleep Duration") +
  theme_minimal()

ggplot(frequency_day_of_week, aes(x = day_of_week, y = frequency)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Frequency of occurence of Sleep Duration under 2 hours in days of the Week",
       x = "Day of the Week",
       y = "Frequency") +
  theme_minimal()

summary(lm(sleep_short$mean_efficiency ~ sleep_short$mean_sleep_duration))