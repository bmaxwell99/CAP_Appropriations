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
  mutate(pop_per_dol_18 = round(pop_18 / dollar_2018, digits = 5)) %>% 
  mutate(normalized_dol = round(dollar_2018 / 131917, digits = 2)) 

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
       y = "Dollars of Funding Received per Population",
       fill = 'Funding Category'
  ) +
  scale_fill_hue(h = c(0, 270)) #+
  theme(legend.title.element_text('Dollars of Funding'))
  

#plots the dollars recieved by state  
ggplot(data = data_2018 ) +
  aes(x = reorder(State, -dollar_2018)  , y = dollar_2018 , show.legend = TRUE) +
  geom_point() + 
  coord_flip() +
  guides(fill=FALSE) + 
  xlab("States") +
  ylab("Dollars of Funding")+
  labs(color = 'Dollars of Funding')

#plots the stnd deviations from the mean by dollars recieved by state  
ggplot(data = data_2018 %>% 
  mutate(stndevs = (abs(dollar_2018 - mean(data_2018$dollar_2018))) / sd(data_2018$dollar_2018)) ) +
  aes(x = reorder(State, -dollar_2018)  , y = stndevs , show.legend = TRUE) +
  geom_point() + 
  coord_flip() +
  guides(fill=FALSE) + 
  xlab("States") +
  ylab("Dollars of Funding") 
  
?guides
?labs
?theme
#plots the dollars recieevd as a histogram
ggplot(data = data_2018) +
  aes(x = dollar_2018) +
  geom_histogram(binwidth = 25000) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_x_discrete(name = 'dollars', labels = as.character(seq(min(data_2018$dollar_2018), max(data_2018$dollar_2018), 25000) )
 )
  
  theme(axis.text.x = element_text(seq(min(data_2018$dollar_2018), max(data_2018$dollar_2018), 25000 )))
  
factor(seq(min(data_2018$dollar_2018), max(data_2018$dollar_2018), 25000 ))

ggplot(data = data_2018) +
  aes(x = data_2018$dollar_2018) +
  stat_function(fun = rnorm, n = n_distinct(data_2018$dollar_2018), args = list(mean = mean(data_2018$dollar_2018), sd = sd(data_2018$dollar_2018)))
  
data_2018 %>% 
  mutate(stndevs = (abs(dollar_2018 - mean(data_2018$dollar_2018))) / sd(data_2018$dollar_2018)) %>% 
  View()
