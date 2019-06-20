library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)

#sets WD to git repo folder, change to run on different computer
setwd("C:/Users/dark_/Documents/NDRN/CAP_Appropriations")

input_data <- 
  read.csv('cleaned_data.csv') %>% 
  mutate(dol_per_pop_18 = round(dollar_2018 / pop_18, digits = 5)) %>% 
  mutate(pop_per_dol_18 = round(pop_18 / dollar_2018, digits = 5)) %>% 
  mutate(normalized_dol = round(dollar_2018 / 131917, digits = 2))

input_data %>% select(State, dollar_2018,normalized_dol, dol_per_pop_18, pop_per_dol_18) %>% View()
