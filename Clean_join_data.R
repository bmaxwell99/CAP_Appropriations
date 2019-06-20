library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)

#sets WD to git repo folder, change to run on different computer
setwd("C:/Users/dark_/Documents/NDRN/CAP_Appropriations")

#inputs the cap appropriations table
dollars_state <-
  read.csv("CAP_3_years.csv", stringsAsFactors = FALSE) %>% 
  #removes whitespace, non digits and converts to numeric
  mutate(dollar_2018 = as.numeric(str_remove_all(trimws(Current.Awards.FY.2018), "\\D"))) %>% 
  mutate(dollar_2017 = as.numeric(str_remove_all(trimws(Total.Awarded.FY.2017), "\\D"))) %>% 
  mutate(dollar_2016 = as.numeric(str_remove_all(trimws(Total.Awarded.FY.2016), "\\D"))) %>%
  #replaces the 'empty' values with 0
  mutate(dollar_2018 = replace_na(dollar_2018, 0), dollar_2017 = replace_na(dollar_2017, 0), dollar_2016 = replace_na(dollar_2016, 0)) %>% 
  select(State, dollar_2018, dollar_2017, dollar_2016) %>% 
  #combines the conneticut and south carolina data into one line, then removes the duplicates
  group_by(State) %>% 
  mutate(dollar_2018 = sum(dollar_2018), dollar_2017 = sum(dollar_2017), dollar_2016 = sum(dollar_2016)) %>% 
  distinct() %>% 
  #at the time of the source data's construction, DC had only recieved a portion of their funding. The following replaces that
  #portion with the full amount they eventually recieved
  mutate(dollar_2018 = replace(dollar_2018, State == "District of Columbia", 131917))


#inputs the census estimations for population by state, pulled directly from the census website
pop_state <-
  read.csv("census_est_pop_2010_2018.csv", stringsAsFactors = FALSE) %>%
  mutate(State = as.character(trimws(GEO.display.label))) %>% 
  select(State, respop72018, respop72017, respop72016) %>% 
  rename(pop_18 = respop72018, pop_17 = respop72017, pop_16 = respop72016)

pop_dollar_data <-
  inner_join(dollars_state, pop_state, by = 'State')


#function to reduce repeat code
normalize_disab_data <- function(input_data){
    input_data <-
      input_data %>% 
      filter(GEO.display.label != 'Geography') %>% 
      #normalizes Geography to be character and removes whitespaces
      mutate(State = as.character(trimws(GEO.display.label))) %>% 
      #normalizes input to be numeric 
      mutate(HC02_EST_VC01 = as.numeric(as.character(trimws(HC02_EST_VC01)))) %>% 
      select(State, HC02_EST_VC01)
  return(input_data)
}

#inputs the 2017 estimated population with disabilities, gathered by the American Community Survey
disab_2017_state <- 
  read.csv("ACS_2017_DISAB_EST.csv") %>% 
  normalize_disab_data %>% 
  rename(disab_17 = HC02_EST_VC01)

#inputs the 2016 estimated population with disabilities, gathered by the American Community Survey
disab_2016_state<-
  read.csv('ACS_2016_DISAB_EST.csv') %>% 
  normalize_disab_data %>% 
  rename(disab_16 = HC02_EST_VC01 )

#join the disab data
disab_16_17<-
  inner_join(disab_2016_state, disab_2017_state, by = "State")

#join the final data
j_data <-
  inner_join(disab_16_17, pop_dollar_data, by = "State")


write.csv(j_data, 'cleaned_data.csv')
