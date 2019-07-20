setwd("C:/Users/dark_/Documents/NDRN/CAP_Appropriations")
source('workload_calc.R')


df_regress <- combined_data


##SECTION RELATED TO WORKLOAD REGRESSION
#adds regression variables to the data
workload_regression <-
  df_regress %>% 
  group_by(State) %>% 
  summarise(p_value_work_per_pop = regression_calc(Year, work_per_pop, 8),
            lm_work_per_pop = regression_calc(Year,work_per_pop,  2),
            p_value_pop = regression_calc(Year, pop, 8),
            lm_pop = regression_calc(Year, pop, 2),
            p_value_work = regression_calc(Year, workload, 8),
            lm_work = regression_calc(Year,workload,  2)
  )  


##implies a downward trend
ggplot(data = df_regress)+
  aes(x = factor(Year),
      y = work_per_pop
  ) +
  geom_boxplot() 

#shows a definite downward trend
ggplot(data = df_regress)+
  aes(x = Year,
      y = work_per_pop
  ) +
  geom_point()+
  geom_smooth(method = lm)


workload_regression %>% filter(p_value_work_per_pop < .05 ) %>% View()
#national trend
summary(lm(df_regress$work_per_pop ~ df_regress$Year))

#graphs just the individual states regression, first filters by signifcant trends and joins with the original data 
test <- inner_join(df_regress, workload_regression %>% filter(p_value_work_per_pop < .05 ), by = 'State')

ggplot() +
  aes(x = Year,
      y = work_per_pop)+ 
  geom_smooth(data = test, aes(group = State), method = lm, se = FALSE) + aes(color = factor(pop_cat)) + 
  scale_color_hue(h = c(0,180))

#TODO make an rmarkdown presentation showing states that are low

combined_data %>% filter(State == 'Utah') %>% View()

##SECTION RELATED TO Closes REGRESSION
#adds regression variables to the data
close_regression <-
  combined_data %>% 
  group_by(State) %>% 
  summarise(p_value_f_close_per_work = regression_calc(Year, f_close_per_work, 8),
            lm_f_close_per_work = regression_calc(Year,f_close_per_work,  2),
            p_value_f_close_per_pop = regression_calc(Year, f_close_per_pop, 8),
            lm_f_close_per_pop = regression_calc(Year, f_close_per_pop, 2),
            p_value_d_7_per_pop = regression_calc(Year, d_7_per_pop, 8),
            lm_d_7_per_pop = regression_calc(Year, d_7_per_pop, 2)
            ) 

regression_stats <- inner_join(close_regression, workload_regression, by = 'State')

regression_stats %>% 
  filter(p_value_f_close_per_pop <= .05 & p_value_work_per_pop ) %>% 
  select(-p_value_f_close_per_work, -lm_f_close_per_work) %>% 
  
  View()

write.csv(regression_stats, 'initial regression analysis.csv')

nan_states <-
  regression_stats %>% 
  filter(is.nan(p_value_d_7_per_pop) ) %>% 
  select(State)

combined_data %>% 
  select(State, Year, d_7_not_favor, d_7_per_pop) %>% 
  filter(is.element(State, c(unlist(nan_states)))) %>% View()
