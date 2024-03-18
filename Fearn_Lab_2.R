#Charles Fearn
#CSCI 444
#Lab2

#load packages
install.packages('pacman')
pacman::p_load('tidyverse', 'fivethirtyeight','ggplot2')

library(tidyverse)
library(fivethirtyeight)
library(ggplot2)
data(college_recent_grads)
# view the DF 
grads<- college_recent_grads
view(grads)
glimpse(grads)

# Question: Which major has the lowest unemployment rate? Which has the highest?
grads <- grads %>%
  arrange(unemployment_rate)%>%
  select(rank, major, unemployment_rate)
view(grads)

grads_v1 <- grads %>%
  arrange(unemployment_rate)%>%
  select(rank, major, unemployment_rate)%>%
  mutate(unemployment_rate = round(unemployment_rate, digits = 3))

grads_v2 <- grads %>%
  arrange(unemployment_rate) %>%
  select(rank, major, unemployment_rate) %>%
  mutate(unemployment_rate = sprintf("%0.3f%%", unemployment_rate * 100))

grads_v3 <- grads %>%
  arrange(unemployment_rate) %>%
  select(rank, major, unemployment_rate)

options(digits = 3)
#get top 10
grads %>%
  head(10) 
#get bottom 10
grads%>%
  tail(10)
# List ONLY the names of the top 10 majors with the lowest unemployment rate as 
# well as only the names of the bottom 10(do not include rank or rate). Do any 
# of the top 10 or bottom 10majors surprise you?

#Results for the 10 majors with the lowest unemployment rate.
# Mathematics And Computer Science                          
# Military Technologies                                     
# Botany                                                    
# Soil Science                                              
# Educational Administration And Supervision                
# Engineering Mechanics Physics And Science            
# Court Reporting                                       
# Mathematics Teacher Education                         
# Petroleum Engineering                                
# General Agriculture 

#Results for the 10 majors with the highest unemployment rate
# Architecture                                           
# Geography                                              
# Computer Programming And Data Processing               
# Mining And Mineral Engineering                         
# Communication Technologies                             
# Public Policy                                          
# Clinical Psychology                                    
# Computer Networking And Telecommunications             
# Public Administration                                  
# Nuclear Engineering 

# Question: Do any of the top or bottom majors surprise you? 
# Answer: I'm not really surprised by any of these. I can see why some, like
# nuclear engineering, would have a high unemployment rate. I don't know for
# a fact, but there's probably not too many nuclear engineering jobs in general.
# So the unemployment rate might be high.


# Question: Which major has the highest percentage of women? Which has the lowest?
# And Why do you thin this is?
# Answer: The data only tells me so much, therefore, I don't have a good answer for 
# this question. Maybe the majors that have fewer women were historically made up 
# of mostly men, and women are just now breaking down that barrier

# High
high_women_grads <- college_recent_grads %>%
  select(major, women, total) %>%
  mutate(major, women, total, prop_women = women/total ) %>%
  arrange(desc(prop_women)) 
  
view(high_women_grads)

high_women_grads %>% 
  head(10)

#low
low_women_grads <- college_recent_grads %>%
  select(major, women, total) %>%
  mutate(major, women, total, prop_women = women/total) %>%
  arrange(prop_women) 
  
low_women_grads %>% 
  head(10)

# Answer: The 10 majors with the highest percentage of women:   
# Early Childhood Education
# Communication Disorders Sciences And Services
# Medical Assisting Services
# Elementary Education
# Family And Consumer Sciences
# Special Needs Education
# Human Services And Community Organization
# Social Work
# Nursing
# Miscellaneous Health Medical Professions

# Answer: The 10 majors with the lowest percentage of women:
# Military Technologies
# Mechanical Engineering Related Technologies
# Construction Services
# Mining And Mineral Engineering
# Naval Architecture And Marine Engineering
# Mechanical Engineering
# Petroleum Engineering
# Transportation Sciences And Technologies
# Forestry
# Aerospace Engineering 


# Question: Why is median typically used over mean when describing the 
# typical income of a group of people?

# Answer: The mean is the average of all the incomes where as the median is the income 
# that is in the middle of the data.  It gives a better representation of the distribution
# in the data.

# graph data

# Try binwidths of $1000 and $5000 and choose one. To specify a binwidth of $1000, for
# example, you would use geom_histogram(binwidth = 1000). Which did you choose?
# Answer: I went with 5000 binwidth. To me, it seems better visually.

college_recent_grads %>%
  ggplot(mapping = aes(x = median, fill = major_category)) +
  geom_histogram(binwidth = 5000)

college_recent_grads %>%
  summarize(min = min(median), max = max(median),
            mean = mean(median), med = median(median),
            sd = sd(median),
            q1 = quantile(median, probs = 0.25),
            q3 = quantile(median, probs = 0.75))

# Question: Based on your histogram distribution, which statistic is most helpful, mean or median
# (which is closer to the center of your data based on quartile 1 and quartile 3)
# Answer: The Median seems to be closer to the center so I would say it would be more helpful.
# min    max   mean   med     sd    q1    q3
# <dbl>  <dbl>  <dbl> <dbl>  <dbl> <dbl> <dbl>
# 22000 110000 40151. 36000 11470. 33000 45000

#plot by major_category
college_recent_grads %>%
  ggplot(mapping = aes(x = median, fill = major_category)) +
  geom_histogram(binwidth = 5000) +
  facet_wrap( ~ major_category, ncol = 4)
 
# Looking at your visualization, which major category do you think has the highest median
# income? Which has the lowest median income? Enter a comment in your code with the answer.
# 
# Answer: It looks like Engineering has the highest median income. 

# Verify your conclusions by summarizing the results for
# the highest and again for the lowest
# Answer: The Highest median income is Engineering and the lowest median income 
# is Psychology & Social Work
# Verify conclusions
college_recent_grads %>%
  group_by(major_category) %>%
  summarise(high_median = median(median)) %>%
  arrange(desc(high_median)) %>% 
  head(5)
# This is the 5 highest median incomes
# A tibble: 5 × 2
# major_category          high_median
# <chr>                         <dbl>
# Engineering                   57000
# Computers & Mathematics       45000
# Business                      40000
# Physical Sciences             39500
# Social Science                38000

college_recent_grads %>%
  group_by(major_category) %>%
  summarise(low_median = median(median)) %>%
  arrange((low_median)) %>% 
  head(5)
# This is the 5 lowest median incomes
# A tibble: 5 × 2
# major_category                  low_median
# <chr>                                <dbl>
# Psychology & Social Work             30000
# Arts                                 30750
# Humanities & Liberal Arts            32000
# Education                            32750
# Agriculture & Natural Resources      35000

# How do the distributions of median income compare across major categories?
# na.omit() to trash the observations with NA values

# NA_Values <- women_grads_median %>%
#   filter(is.na(prop_women))

women_grads_median <- college_recent_grads %>% 
  na.omit() %>% 
  select(major_category,women, total, median) %>% 
  mutate(major_category, median, prop_women = women/total) %>% 
  arrange(desc(prop_women))
# Question: Whatis the linear relationship (correlation) between the proportion of women (x) 
# and the median income (y)?
# Answer: The correlation between proportion of women and median income is -0.619.
women_grads_median%>%
  summarize(r = cor(women_grads_median$prop_women, women_grads_median$median))
# # A tibble: 1 × 1
# r
# <dbl>
#   1 -0.619

# Question: Scan through your data frame and as a comment in
# your code list the major that has an NA value associated with this.
# Answer: Agriculture & Natural Resources is the major with the NA value.

# Question: Should your aesthetics be defined in ggplot or geom_point?
# Answer: In ggplot(), aesthetics like mappings and visuals,e.i. color, are defined.
count <- women_grads_median %>%
  group_by(major_category) %>%
  summarise(num_categories = n_distinct(major_category))

color_names <- c("navyblue","mediumpurple4",
                 "orange1", "yellowgreen","tomato2" ,
                 "tan","hotpink3" ,"green2","deeppink3",
                 "darkorchid1","dodgerblue1","turquoise1",
                 "thistle" , "steelblue" ,"darkcyan","blue"  )

women_grads_median %>% 
  ggplot(women_grads_median, mapping = aes(x = prop_women, y = median, color = major_category)) +
  geom_point()+
  scale_color_manual(values = color_names)

# What trend do you see between median income and the proportion of women
# by major category? Does this surprise you?  
# Answer: It appears that, according to my scatter plot, women in the 
# major_categories with a lower proportion of women have a higher median incomes.
# I.e: engineering,has fewer women but has a higher
# median income. you can almost draw a downward slopping line to show this trend.
# I cannot say this surprises me. Statistically, women tend to work in certain 
# career fields and this lab's data shows that.
