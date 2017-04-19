# Exercise 6: DPLYR join introduction

# Install the nycflights13 package and read it in. Require the dplyr package.
# install.packages("nycflights13")
library(nycflights13)
library(dplyr)
View(flights)

# Create a dataframe of the average arrival delay for each destination, then use left_join
# to join on the "airports" dataframe, which has the airport info
avg.arrival.delay <- flights %>% 
  group_by(dest) %>% 
  summarize(avg.delay = mean(arr_delay, na.rm = TRUE)) %>% 
  mutate(cities = dest)
  left_join(airports, by = 'cities')

# Create a dataframe of the average arrival delay for each airline, then use left_join
# to join on the "airlines" dataframe, which has the airline info
avg.flight.delay <- flights %>% 
  group_by(carrier) %>% 
  summarize(delay = mean(arr_delay, na.rm = TRUE)) %>% 
  left_join(airlines, by = 'carrier') %>% 
  arrange(desc(delay))
  
### Bonus ###
# Calculate the average delay by city AND airline, then merge on the city and airline information
avg.city.carrier <- avg.flight.delay %>% left_join(avg.arrival.delay, by )
avg_city_airline <- flights %>% 
  group_by(dest, carrier) %>% 
  summarise(avg.delay = mean(arr_delay, na.rm=TRUE)) %>% 
  left_join(airlines, by='carrier') %>% 
  mutate(faa = dest) %>% 
  left_join(airports, by = 'faa') %>% 
  left_join(airlines, by='carrier') %>% 
  ungroup() %>% 
  arrange(-avg.delay)

# If you're running into sorting issues:
# http://stackoverflow.com/questions/26555297/dplyr-arrange-a-grouped-df-by-group-variable-not-working