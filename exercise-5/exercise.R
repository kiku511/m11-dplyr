# Exercise 5: DPLYR Grouped Operations

# Install the nycflights13 package and read it in. Require the dplyr package.
# install.packages("nycflights13")
library(nycflights13)
library(dplyr)

# In which month was the average departure delay the greatest?
# Hint: you'll have to perform a grouping operation before summarizing your data
View(flights)
month.max.avg.delay <- flights %>% group_by(month) %>% summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE))  %>% arrange(-avg_dep_delay) %>% .[[1,1]]

# If you create a data.frame with the columns "month", and "delay" above, you should be able to create 
# a scatterplot by passing it to the 'plot' function
plot(month.max.avg.delay)

# In which airport were the average arrival delays the highest?
# Hint: you'll have to perform a grouping operation before summarizing your data
worst.airport <- flights %>% group_by(dest) %>% summarize(avg_arr_delay = mean(dep_delay, na.rm = TRUE)) %>% arrange(-avg_arr_delay) %>% .[[1,1]]

### Bonus ###
# Which city was flown to with the highest average speed?
fastest_arrival_city <- flights %>% mutate(speed = distance/air_time * 60) %>% group_by(dest) %>% summarise(avg_speed = mean(speed, na.rm = TRUE)) %>% 
  arrange(-avg_speed) %>% .[[1,1]]
