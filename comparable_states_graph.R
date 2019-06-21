library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
library(ggplot2)


#sets WD to git repo folder, change to run on different computer
setwd("C:/Users/dark_/Documents/NDRN/CAP_Appropriations")
source('Clean_join_data.r')

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


j_data <- 
  j_data %>% 
  mutate(dol_per_pop_18 = round(dollar_2018 / pop_18, digits = 5)) %>% 
  mutate(pop_per_dol_18 = round(pop_18 / dollar_2018, digits = 5))

data_2018<-
  j_data %>% 
  select(State, dollar_2018, pop_18, dol_per_pop_18) %>% 
  #attempt to bucket data
  mutate(funding_cat = cut_interval(dollar_2018, 
                                    length = bucket_size,
                                    labels = j_data %>% 
                                      sequence() %>% 
                                      format_money()
  ))

#plots against dollars recieved per population
ggplot(data = data_2018 ) +
  aes(x = reorder(State, -dol_per_pop_18)  , 
      y = dol_per_pop_18 ,  
      fill =  funding_cat
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

