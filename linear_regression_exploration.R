setwd("C:/Users/dark_/Documents/NDRN/CAP_Appropriations")
source('workload_calc.R')


p_calc <- function(x, y, value){
  lm_regress <- summary(lm(y ~ x))
  r <- round(lm_regress[['coefficients']][value], 2)
  return(r)
}


workload_regression <-
  workload_w_state %>% 
    group_by(State) %>% 
    summarise(p_value = p_calc(Year, work_per_pop, 8),
              'p<.10' = (p_calc(Year, work_per_pop, 8) < .1),
              'p<.05' = (p_calc(Year, work_per_pop, 8)< .05 ),
              'p<.01' = (p_calc(Year, work_per_pop, 8)< .01 ),
              work_lm = p_calc(Year, work_per_pop, 2)
              )

test <- inner_join(workload_w_state, workload_regression, by = 'State')

df <- test %>% filter(p_value < .10 & work_lm < -.9  )

test %>% filter(p_value < .10 & work_lm < -.9) %>% arrange(State, Year) %>% View()

work

ggplot(data = df)+
  aes(x = Year,
      y = work_per_pop
  ) +
  geom_point()+
  geom_smooth(method = lm) +
  facet_grid(.~State)

#10% less cases