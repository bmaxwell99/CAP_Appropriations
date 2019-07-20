setwd("C:/Users/dark_/Documents/NDRN/CAP_Appropriations")
source('workload_calc.R')

plot(density(combined_data$c_5_per_pop))
hist(combined_data$c_7_per_pop)

#national decrease in c_5, -0.016152
summary(lm(combined_data$c_5_per_pop ~ combined_data$Year))

#P value is .09 for c_6, hard to say with confidence
summary(lm(combined_data$c_6_per_pop ~ combined_data$Year))

#P value is .5 for c_7, disregard entirely
summary(lm(combined_data$c_7_per_pop ~ combined_data$Year))

df_PA <-
  combined_data %>% 
  filter(is_PA)

df_not_PA <- 
  combined_data %>% 
  filter(!is_PA)

#agregate decrease in c_5 for PA, -0.014636 less than the national
summary(lm(df_PA$c_5_per_pop ~ df_PA$Year))

#agregate decrease in c_5 for non PA, -0.019186 more than the national
summary(lm(df_not_PA$c_5_per_pop ~ df_not_PA$Year))

#aggregate decrease in c_6 for PA, barely passes muster, but -0.005961
summary(lm(df_PA$c_6_per_pop ~ df_PA$Year))

#disregard entirely 
summary(lm(df_not_PA$c_6_per_pop ~ df_not_PA$Year))

summary(lm(df_PA$c_7_per_pop ~ df_PA$Year))

summary(lm(df_PA$c_7_per_pop ~ df_PA$Year))

summary(lm(df_not_PA$c_7_per_pop ~ df_not_PA$Year))


