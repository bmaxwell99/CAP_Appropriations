setwd("C:/Users/dark_/Documents/NDRN/CAP_Appropriations")

source('workload_calc.R')

d_7_stats <- combined_data %>%  
  group_by(State, pop_cat) %>% 
  #adds summary statistics based on d_7
  summarise(mean_pop = round(mean(d_7_per_pop), 2),
            sd = round(sd(d_7_per_pop), 2),
            median = round(median(d_7_per_pop), 2),
            range = round(max(d_7_per_pop), 2)
  ) %>% 
  ungroup() %>% 
  mutate(mean_rank = cut_interval(mean, 
                                  n = 10,
                                  label = as.numeric(c(1:10))),
         sd_rank = cut_interval(sd, 
                                n = 10,
                                label = as.numeric(c(1:10))),
         median_rank = cut_interval(mean, 
                                    n = 10,
                                    label = as.numeric(c(1:10))),
         range_rank = cut_interval(mean, 
                                   n = 10,
                                   label = as.numeric(c(1:10)))) %>% 
  select(State, pop_cat, mean, mean_rank, sd, sd_rank, median, median_rank, range, range_rank)

workload_regression <-
  combined_data %>% 
  group_by(State) %>% 
  summarise(lm_work_per_pop = if_else(regression_calc(Year, work_per_pop, 8) <= .05,
                                      regression_calc(Year,work_per_pop,  2),
                                      NaN),
            lm_pop = if_else(regression_calc(Year, pop, 8) <= .05,
                             regression_calc(Year, pop, 2),
                             NaN),
            lm_work = if_else(regression_calc(Year, workload, 8) <= .05,
                              regression_calc(Year,workload,  2),
                              NaN)
  )  


close_regression <-
  combined_data %>% 
  group_by(State) %>% 
  summarise(lm_f_close_per_work = if_else(regression_calc(Year, f_close_per_work, 8) <= .05,
                                      regression_calc(Year,f_close_per_work,  2),
                                      NaN),
            lm_f_close_per_pop = if_else(regression_calc(Year, f_close_per_pop, 8) <= .05,
                             regression_calc(Year, f_close_per_pop, 2),
                             NaN),
            lm_work = if_else(regression_calc(Year, workload, 8) <= .05,
                              regression_calc(Year,workload,  2),
                              NaN),


            p_value_d_7 = regression_calc(Year, d_7_not_favor, 8),
            lm_d_7 = regression_calc(Year, d_7_not_favor, 2),
            p_value_d_7_per_pop = regression_calc(Year, d_7_per_pop, 8),
            lm_d_7_per_pop = regression_calc(Year, d_7_per_pop, 2)
  ) 

regression_stats <- inner_join(close_regression, workload_regression, by = 'State')

intervention_regression <-
  combined_data %>% 
  group_by(State) %>% 
  summarise(lm_c_5 = if_else(regression_calc(Year, c_5_informal_review, 8) <= .5,
                             regression_calc(Year,c_5_informal_review,  2),
                             NaN))
