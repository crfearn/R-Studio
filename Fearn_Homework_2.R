# Charles Fearn
# CSCI 444
# Homework 2
# Washington DC Bike Sharing Rentals
install.packages("tidyverse")
library(tidyverse)
library(lubridate)

# read data from bike_data and write it to data folder
bikes <- dir(path = "bike_data", pattern = "*.csv") %>%
  map_dfr(function(x) read_csv(file.path("bike_data",x), col_names = TRUE))

write_csv(bikes, file = "data/bikes.csv", append = TRUE)


########### Number 1 ##########
#a
new_bikes <- bikes
str(new_bikes)
new_bikes <- bikes %>%
    mutate(Season = case_when(
    Season == 1 ~ "Winter",
    Season == 2 ~ "Spring",
    Season == 3 ~ "Summer",
    Season == 4 ~ "Fall"))
# with over 3 million observations, I wanted to make sure I didn't mess up and changed all of them incorrectly,
# so I grouped and counted to make sure I had all 4 seasons.
new_bikes %>% 
  group_by(Season) %>% 
  count()

#b and c
new_bikes <- new_bikes %>% 
  mutate(`Member type`= case_when(
    `Member type`== "Casual"~ "Non-Member",
    `Member type` == "Member"~"Member")) %>% 
  rename(Member = `Member type`) 
 #d
new_bikes <- new_bikes %>%
  mutate(Date = as_date(gsub(" .*$","",.$`Start date`),"%m/%d/%Y",tz=NULL))

glimpse(new_bikes)




# What are the new data types of these variables? In a table list the variable name followed by its
# data type. How many observations in bikes? 
# Answer:
# > bike_table <- tibble(bike_col,bike_row) %>% 
#   +   print()
# # A tibble: 11 × 2
# bike_col             bike_row 
# <chr>                <chr>    
# 1 Duration             numeric  
# 2 Start date           character
# 3 End date             character
# 4 Season               character
# 5 Start station number numeric  
# 6 Start station        character
# 7 End station number   numeric  
# 8 End station          character
# 9 Bike number          character
# 10 Member               character
# 11 Date                 Date 

# there are 3398387 observations in bikes

bike_col <- new_bikes %>% 
  colnames()
bike_row <- new_bikes %>% 
  sapply(class)
bike_table <- tibble(bike_col,bike_row) %>% 
  print()

########## Number 2 ##########

# Read in weather-2019 data
weather <- read_csv(file.path("data/weather-2019.csv"), col_names = TRUE)
# Combind Month, Day, Year
new_weather <- weather %>% 
  mutate(Date = as_date(paste(Year, Month, Day, sep = "-"), format = "%Y-%m-%d")) 
#Rename Daily maximum temperature and convert to fahrenheit.
new_weather <- new_weather %>% 
  rename("Temperature" =`Daily maximum temperature`) %>% 
  mutate(Temperature = (Temperature * 9/5) + 32) %>% 
  select(-Month,-Year,-Day)

#Question: How many observations in weather?
# Answer: There are 365 observations in weather

######### Number 3 ###########

bike_count <- new_bikes %>% 
  group_by(Date) %>% 
  count(Date)
  
bike_count <- bike_count %>% 
  rename(`Bike Rentals`= n)

#merge bike_count and weather
bike_count_merge <- merge(bike_count,new_weather)


#Scatter plot for bike rentals(y) to date(x). color for Temperature

bike_count_merge %>% 
  ggplot(aes(x = Date, y = `Bike Rentals`, color = Temperature))+
  geom_point()
# Question: What trends do you see with bike rentals relative to temperature? 
# Do you see any outliers in the data points (state what months, approximately – don’t
# need to list exact dates)? Are these the dates you would expect, why or why not?


######## Number 4 ##########
# What comparisons do you see between bike rentals from members versus non-members?
# Again, do you see any outliers (state what months, approximately – don’t need 
# to list exact dates)? Are these the dates you would expect, why or why not?

bike_membership <- new_bikes %>% 
  group_by(Date,Member) %>% 
  count()

bike_membership <- bike_membership %>% 
  rename(`Bike Rentals` = n)

bike_membership_merge <- merge(bike_membership,new_weather)

bike_membership_merge %>% 
  ggplot(aes(x = Date, y = `Bike Rentals`, color = Temperature))+
  geom_point()+
  facet_wrap(~Member)


########### Number 5 ################

# Which season had the most rentals from members? Which season had the most rentals from
# non-members? Does either answer surprise you (it did me)

##first column chart
bike_member_counts <- new_bikes %>% 
  group_by(Member) %>% 
  count()

bike_member_counts <- bike_member_counts %>% 
  rename(`Bike Rentals`= n)

bike_member_counts %>% 
  ggplot(aes(x=Member, y = `Bike Rentals`, fill = Member))+
  geom_col()

## Second Column chart

bike_member_counts2 <-  new_bikes %>% 
  group_by(Season,Member) %>% 
  count()

bike_member_counts2 <- bike_member_counts2 %>% 
  rename(`Bike Rentals` = n)

bike_member_counts2 %>% 
  ggplot(aes(x= Season, y = `Bike Rentals`, fill = Season))+
  geom_col()+
  facet_wrap(~Member)


############### Number 6 ###############


bike_month <- new_bikes %>% 
  group_by(Member, Date) %>% 
  count() %>% 
  rename(`Bike Rentals` = n)

bike_month %>% 
  ggplot(aes(x = Date, y = `Bike Rentals`, fill = Member))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~month(Date), scale = "free")+ 
  labs(x= element_blank())


bike_rental_max_mem <- new_bikes %>% 
  filter(Member == "Member") %>% 
  group_by(Date) %>% 
  count() %>% 
  rename(`Bike Rentals` = n) %>% 
  arrange(desc(`Bike Rentals`)) %>% 
  head(5) %>% 
  print()

bike_rental_max_non_Mem <- new_bikes %>% 
  filter(Member == "Non-Member") %>% 
  group_by(Date) %>% 
  count() %>% 
  rename(`Bike Rentals` = n) %>% 
  arrange(desc(`Bike Rentals`)) %>% 
  head(5) %>% 
  print()

