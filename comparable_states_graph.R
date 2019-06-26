library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
library(ggplot2)


#sets WD to git repo folder, change to run on different computer
setwd("C:/Users/dark_/Documents/NDRN/CAP_Appropriations")
source('Clean_join_data.r')

#plots against dollars recieved per population
ggplot(data = bucketed_data #%>% filter(dollar_2018 < 141000) %>% filter(State = )
       ) +
  aes(x = reorder(State, -dol_per_pop_18)  , 
      y = dol_per_pop_18 ,  
      fill =  funding_cat_18
  ) +
  geom_col() + 
  coord_flip() + 
  labs(x = "States", 
       y = "Dollars of Funding Received per Population in 2018**",
       fill = '*Approx Funding Received in 2018',
       title = 'Visual Exploration for 2018 Comparable States(CAP)',
       subtitle = 'The States that are similar in terms of bar length(calculated by number of dollars received per person)
        may be comparable to each other, despite the differences in funding they recieve(shown via color) ',
       caption = '*Funding amounts drawn from RSA CAP appropriations
       **Estimated Population drawn from Census Bureau'
  ) +
  scale_fill_hue(h = c(0, 270)) 




#Plots showing visualized grouping
ggplot(data = bucketed_data ) +
  aes(x = reorder(State, -dol_per_pop_18)  , 
      y = dol_per_pop_18 ,  
      fill =  compare_cat_18
  ) +
  geom_col() + 
  coord_flip() + 
  labs(x = "States", 
       y = "Dollars of Funding Received per Population in 2018*",
       fill = 'Comparable Buckets',
       title = 'Visual Statement for 2018 Comparable States(CAP)',
       subtitle = 'The States that are similar in terms of bar length(calculated by number of dollars received per person)
       may be comparable to each other, and are grouped together by half a standard deviation(shown with color) ',
       caption = '*Funding amounts drawn from RSA CAP appropriations
       and estimated Population drawn from Census Bureau'
  ) +
  scale_fill_hue(h = c(270,0)) 


