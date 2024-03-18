# Charles Fearn
# Fall 2023 CSCI 444
# Homework 1
# Enneagram Personality Test
install.packages("tibble")
install.packages("purrr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")
library(tibble)
library(purrr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(dplyr)
library(readr)
library(stringr)
names <- c("Name", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28", "Q29", "Q30", "Q31", "Q32", "Q33", "Q34", "Q35", "Q36", "A", "B", "C", "D", "E", "F", "G", "H", "I", "Personality_Type")

qnames <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28", "Q29", "Q30", "Q31", "Q32", "Q33", "Q34", "Q35", "Q36")

valid <- list(c("B", "E"),
              c("A", "G"),
              c("C", "D"),
              c("H", "I"),
              c("E", "F"),
              c("A", "B"),
              c("D", "G"),
              c("F", "H"),
              c("C", "I"),
              c("A", "E"),
              c("G", "H"),
              c("B", "D"),
              c("C", "F"),
              c("E", "I"),
              c("D", "H"),
              c("B", "G"),
              c("A", "F"),
              c("C", "E"),
              c("B", "I"),
              c("D", "F"),
              c("C", "G"),
              c("A", "H"),
              c("B", "F"),
              c("G", "I"),
              c("D", "E"),
              c("A", "I"),
              c("B", "C"),
              c("E", "H"),
              c("F", "G"),
              c("D", "I"),
              c("A", "C"),
              c("B", "H"),
              c("E", "G"),
              c("A", "D"),
              c("F", "I"),
              c("C", "H"))

#my data
fearn_data <- dir(path = "hwk_data", pattern = "Fearn.csv") %>%
  map_dfr(function(x) read_csv(file.path("hwk_data",x), col_names = FALSE))

# Now, assign the one variable/column in your data frame to be Answer. Then, add 
# a variable/column named Question, assigning it the values from names (one of the
# vectors I’ve given to you in the names.txt file, which is your name, the 36 
# questions, the sum for A-I answers, and the personality type).

fearn_data <- fearn_data %>% 
  rename(Answer = 'X1' ) %>% 
  mutate( Questions = names)
#Use the answers,[2:37], to create a graph using ggplot/geom_bar
fearn_data %>% 
  select(Answer) %>% 
  slice(2:37) %>% 
  group_by(Answer) %>%
  summarise(Count = n()) %>%
  ggplot(mapping = aes(x = Answer, y = Count, fill = Answer)) +
  geom_bar(stat = "identity")+
  ggtitle('Enneagram By Question', subtitle = 'Charles')

# For geom_bar, what is the default for the stat argument?
# Count is the default for the stat argument.

# What should stat be if you want the heights of the bars 
# to represent values in the data? Set the statattribute 
# in geom_bar to this value.
#To make to heights of the bars represent values in the data,
#we can use stat = 'identity'.

# What is your personality type?
# What are characteristics of this personality type 
# (be sure to reference the site you use and verify your
#   answer is in your own words)?
# Do you agree with this description?
#
# Answer:
#
#

## Class Data

Class_data <- dir(path = "hwk_data", pattern = "*.csv") %>%
  map_dfc(function(x) read_csv(file.path("hwk_data",x), col_names = FALSE))

cl_data <- Class_data %>% 
  mutate( Question = names)

  
class_transposed <- cl_data %>%
  pivot_longer(cols = -Question, names_to = "Students", values_to ="Answers") %>%
  pivot_wider(names_from = Question, values_from = Answers) %>% 
  select(-Students)


# Visualize the data using a bar chart.


class_transposed %>% 
  group_by(Personality_Type) %>% 
  count(Personality_Type) %>% 
  ggplot(mapping = aes(x = Personality_Type,y=n, fill = Personality_Type)) +
  geom_bar(stat = 'identity') 



# # Let’s look at each student’s response individually. Below I have given code
# that will take our messy data and make it tidy. Copy and paste this into 
# your script file:
class_transposed_v2 <- class_transposed %>%
  pivot_longer(cols = Q1:Q36, names_to="Question", values_to="Answers")


class_counts <- class_transposed_v2 %>%
  group_by(Name,Answers) %>%
  summarise(count = n())
    
class_counts %>% 
ggplot(mapping = aes(x = Answers, y = count, fill = Answers)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Name, scales = "free") 
  

# looking at the class total for possible answers
results <- cl_data[2:37,] 
  


class_answers <- results %>%
  pivot_longer(cols = -"Question", names_to = "Students", values_to = "Answers") %>% 
  select(-Students) %>% 
  group_by(Answers) %>% 
  count()

class_answers %>% 
  ggplot(mapping = aes(x = Answers, y = n, fill = Answers)) +
  geom_bar(stat = "identity") +
  ggtitle('Enneagram Results', subtitle = 'Charles Fearn')

class_answers_by_question <- results %>%
  pivot_longer(cols = -Question, names_to = "Students", values_to ="Answers") %>% 
  select(-Students) %>% 
  group_by(Question,Answers) %>% 
  count()

class_answers_by_question %>% 
  group_by(Question) %>% 
  ggplot(mapping = aes(x = Answers, y = n, fill = Answers), scale) +
  geom_bar(stat = "identity") +
  ggtitle('Enneagram By Question', subtitle = 'Charles Fearn') +
  facet_wrap(~class_answers_by_question$Question, scales = "free")

# Dirty data

bad_data <- tibble(Name=character(), Question=character(),
                   Answer=character())
for(student in 1:34){
  print(class_transposed[student,1])
  for (number in 1:36) {
    assign(qnames[number], class_transposed[student ,(number+1)])
    temp <- class_transposed %>%
      filter(get(qnames[number]) != valid[[number]][1] & get(qnames[number])
             != valid[[number]][2]) %>%
      mutate(Answer = get(qnames[number]),
             Question = qnames[number]) %>%
      select(Name, Question, Answer)
    bad_data <- bad_data %>%
      add_row(Name=temp$Name, Question = temp$Question, Answer =
                temp$Answer)
  }
}
bad_data <- bad_data %>%
  unique() %>%
  arrange(Name, Question)

class_answers_by_question_alt <- class_answers_by_question %>% 
  mutate(rank = as.numeric(str_sub(Question,2))) %>% 
  arrange(rank, Answers)


  class_answers_by_question_alt %>% 
    ggplot(mapping = aes(x = Answers, y = n, fill = Answers), scale) +
    geom_bar(stat = "identity") +
    ggtitle('Enneagram By Question Alt', subtitle = 'Charles Fearn') +
    facet_wrap(~class_answers_by_question_alt$rank, scales = "free")
  