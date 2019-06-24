library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
library(ggplot2)

setwd("C:/Users/dark_/Documents/NDRN/CAP_Appropriations")

source('PPR_cleaning.R')
source('Clean_join_data.r')

indiv_closed_stdzed <-
  indiv_cases_stdzed %>% 
  #selects the columns related to how cases were closed
  select(Year, State, agency_name, workload, d_cases_closed, d_1_per_closed, d_2_per_closed, d_3_per_closed, d_7_per_closed, d_10_per_closed, d_11_per_closed, ID)


#State graphical analysis
ggplot(data = indiv_closed_stdzed %>% 
                  filter(Year == 2017) %>% 
                  mutate(favorable_close = d_1_per_closed + d_2_per_closed)) +
  aes(x = reorder(State, -favorable_close ),
      y = favorable_close) +
  geom_col() +
  coord_flip()


ggplot(data = indiv_closed_stdzed %>% 
         filter(State == 'South Carolina') %>% 
         mutate(favorable_close = d_1_per_closed + d_2_per_closed)) +
  aes(x = Year,
      y = d_1_per_closed,
      fill = agency_name
  )+
  geom_col(position = 'dodge') 
