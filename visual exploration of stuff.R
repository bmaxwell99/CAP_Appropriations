setwd("C:/Users/dark_/Documents/NDRN/CAP_Appropriations")

source('workload_calc.R')

#visual exploration of non normalized performance metrics, grouped by population in the minimums


working_data <- 
  combined_data %>% 
  filter(pop_cat <0) %>% 
  select(Year, State, pop, pop_cat, is_PA, workload, f_close, d_7_not_favor, c_6_formal_review, 
         d_10_lack_resources, c_7_legal, c_5_informal_review, work_per_pop, f_close_per_pop, f_close_per_work, d_7_per_pop, d_7_per_work, d_10_per_pop, 
         d_10_per_work, c_5_per_pop, c_5_per_work, c_6_per_pop, c_6_per_work, c_7_per_pop, c_7_per_work
         )

#Workload by year for the minimums, seperated by pop category and colored by PA status
ggplot(data = working_data)+
  aes(x = Year,
      y = work_per_pop,
      label = State,
      color = is_PA)+
  geom_text() +
  facet_wrap(.~pop_cat)

#f_close by year for the minimums, seperated by pop category and colored by PA status
ggplot(data = working_data)+
  aes(x = Year,
      y = f_close_per_work,
      label = State,
      color = is_PA)+
  geom_text() +
  facet_wrap(.~pop_cat)

#Workload by year for the minimums, seperated by pop category and colored by PA status
ggplot(data = working_data)+
  aes(x = Year,
      y = d_7_not_favor,
      label = State,
      color = is_PA)+
  geom_text() +
  facet_wrap(.~pop_cat)


ggplot(data = working_data)+
  aes(x = Year,
      y = c_6_p,
      label = State,
      color = is_PA)+
  geom_text() +
  facet_wrap(.~pop_cat)

ggplot(data = working_data)+
  aes(x = Year,
      y = d_10_per_pop,
      label = State,
      color = is_PA)+
  geom_text() +
  facet_wrap(.~pop_cat)



#instead of debugging the function just replace this
x <- 
  working_data %>% 
  select(work_per_pop) %>%  
  gather()

outlier <- (summary(x$value)[["3rd Qu."]] - summary(x$value)[["1st Qu."]] ) * 1.5 + summary(x$value)[["3rd Qu."]]


y <- 
  working_data %>% 
  filter(work_per_pop > outlier) %>% 
  group_by(State) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

y <- y %>% select(Year, State, is_PA, pop_cat,work_per_pop, workload ,pop) %>% arrange(is_PA)
