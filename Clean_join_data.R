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
  select(State, respop72018, respop72017, respop72016, respop72015, respop72014, respop72013, respop72012, respop72011) %>% 
  rename(pop_18 = respop72018, pop_17 = respop72017, pop_16 = respop72016, pop_15 = respop72015, pop_14 = respop72014, 
         pop_13 = respop72013, pop_12 = respop72012, pop_11 = respop72011)

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

#adds dollar per pop calculation 
j_data <- 
  j_data %>% 
  mutate(dol_per_pop_18 = round(dollar_2018 / pop_18, digits = 5)) %>% 
  mutate(dol_per_pop_17 = round(dollar_2017 / pop_17, digits = 5)) %>% 
  mutate(dol_per_pop_16 = round(dollar_2016 / pop_16, digits = 5))

bucket_size <- 10000

#creates a sequence of the given data, only created for code concision
sequence <- function(df) {
  r <- seq(min(df$dollar_2018), max(df$dollar_2018), bucket_size )
  return(r)
}

format_money <- function(x){
  r <- paste0("$", formatC(as.numeric(x), format="f", digits=0, big.mark=","))
  return(r)
}

bucketed_data <-
  j_data %>% 
  #attempt to bucket funding data
  mutate(funding_cat_18 = cut_interval(dollar_2018, 
                                    length = bucket_size,
                                    labels = j_data %>% 
                                      sequence() %>% 
                                      format_money()
  )) %>% 
  mutate(funding_cat_17 = cut_interval(dollar_2017, 
                                       length = bucket_size,
                                       labels = j_data %>% 
                                         sequence() %>% 
                                         format_money()
  )) %>% 
  mutate(funding_cat_16 = cut_interval(dollar_2016, 
                                       length = bucket_size,
                                       labels = j_data %>% 
                                         sequence() %>% 
                                         format_money()
  )) %>% 
  #provides a boolean column to determine if the state recieved the min
  mutate(min_funding_18 = if_else(dollar_2018 <= 141917, T, F)) %>% 
  #provides a bucket to compare like states with 
  mutate(compare_cat_18 = cut_interval(dol_per_pop_18, 
                                    #interval for comparison by half the standard deviation of the data
                                    length = sd(j_data$dol_per_pop_18) / 2,
                                    labels = factor(c('<.05', '<.08', '<.10', '<.13', '<.16', '<.19', '<.21', '<.24'))
  ))
#removes original data sets from working memory
remove(j_data, dollars_state, pop_state, disab_2017_state, disab_2016_state, disab_16_17, pop_dollar_data, bucket_size)


