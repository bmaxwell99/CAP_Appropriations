setwd("C:/Users/dark_/Documents/NDRN/CAP_Appropriations")

source('workload_calc.R')

numeric_data <- 
  combined_data %>% 
  select(is_PA, work_per_pop, f_close_per_pop, f_close_per_work, d_7_per_pop, d_7_per_work, d_10_per_pop, 
         d_10_per_work, c_5_per_pop, c_5_per_work, c_6_per_pop, c_6_per_work, c_7_per_pop, c_7_per_work
         )

national_mean <- 
  numeric_data %>% 
  select(-is_PA)%>% 
  summarise_all(mean) %>% 
  t() %>% 
  data.frame() %>% 
  rename(mean_nat = '.')

national_sd <- 
  numeric_data %>% 
  select(-is_PA)%>% 
  summarise_all(sd) %>% 
  t() %>% 
  data.frame() %>% 
  rename(sd_nat = '.')

national_median <- 
  numeric_data %>% 
  select(-is_PA)%>% 
  summarise_all(median) %>% 
  t() %>% 
  data.frame() %>% 
  rename(median_nat = '.')

mean_df <-
  numeric_data %>% 
  group_by(is_PA) %>% 
  summarise_all(mean) 

rownames(mean_df) <- c('mean_not_PA', 'mean_PA')

mean_df<- mean_df %>% select(-is_PA) %>% t() 

sd_df <-
  numeric_data %>% 
  group_by(is_PA) %>% 
  summarise_all(sd) 

rownames(sd_df) <- c('sd_not_PA', 'sd_PA')

sd_df<- sd_df %>% select(-is_PA) %>% t() 

median_df <-
  numeric_data %>% 
  group_by(is_PA) %>% 
  summarise_all(median) 

rownames(median_df) <- c('median_not_PA', 'median_PA')

median_df<- median_df %>% select(-is_PA) %>% t() 

rname <- rownames(median_df)

PA_stats_df<- cbind(mean_df, sd_df, median_df) %>% data.frame(row.names = rname)

PA_stats_df<- 
  cbind(mean_df, sd_df, median_df) %>% 
  data.frame() %>% 
  cbind(national_mean, national_sd, national_median) %>% 
  mutate_all(funs(round(., 5)))

rownames(PA_stats_df) <- rname

#write.csv(PA_stats_df, 'comparitive PA stats.csv')





test <- data.frame(not_PA = filter(numeric_data, !is_PA)$f_close_per_pop)
test2 <- data.frame(PA = filter(numeric_data, is_PA)$f_close_per_pop)
test3 <- data.frame(Nat = numeric_data$f_close_per_pop)
df <- rbind(gather(test), gather(test2), gather(test3))

ggplot(df,aes(x=value, fill=key)) + geom_density(alpha=0.25)
ggplot(df,aes(x=key, y=value, fill=key)) + geom_boxplot()

