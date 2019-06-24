library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
library(ggplot2)

setwd("C:/Users/dark_/Documents/NDRN/CAP_Appropriations")

source('PPR_cleaning.R')
source('Clean_join_data.r')

workload_w_state <-
  indiv_cases_stdzed %>% 
  select(Year, State, agency_name, workload, ID)

workload_w_state <- inner_join(bucketed_data, workload_w_state, by='State')

#preps the data for graphing  
workload_w_state_18 <- 
    workload_w_state %>% 
    filter(Year == 2018) %>% 
    mutate(work_relative_resources = workload * dol_per_pop_18) %>% 
    arrange(compare_cat_18,work_relative_resources) %>% 
    mutate(.r = row_number())


ggplot(data = workload_w_state_18) +
  aes(x = reorder(State, -.r),
      y = work_relative_resources,
      fill = funding_cat_18) +
  coord_flip() +
  geom_col() +
  facet_grid(.~compare_cat_18) +
  scale_fill_hue(h = c(0,270)) 

