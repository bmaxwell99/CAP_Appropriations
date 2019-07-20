setwd("C:/Users/dark_/Documents/NDRN/CAP_Appropriations")

source('workload_calc.R')

#exploration of analysis for d_1, d_2, d_7

#Very initial look at favorable closes per year. Appears to be some outliers.
#2016 has a pretty big spread but appears to not matter
ggplot(data = combined_data)+
  aes(x = factor(Year),
      y = f_close_per_work
  ) +
  geom_boxplot() 

#national trend is trending downwards, but at a rate that seems insignificant
summary(lm(d_7_per_work ~ Year, combined_data))

plot_favorable_time <- function(df){
plot <- ggplot(data = df)+
  aes(x = factor(Year),
      y = f_close_per_work,
      label = State
  ) +
  geom_text()

return(plot)
}


plot_vs <- function(df){

plot <- ggplot(data = df) +
  aes(x = f_close_per_work,
      y = d_7_per_work,
      label = State,
      color = factor(Year)) +
  geom_text() +
  scale_color_brewer(palette = 'Dark2')
return(plot)
}

combined_data %>% filter(pop_cat == 7) %>% plot_vs()
combined_data %>%  plot_favorable_time()

#the data is disproportionaly skewed towards having 0 unfavorable closes
ggplot(data = combined_data #%>% filter(d_7_per_work != 0)
       )+
  aes(x = d_7_per_work,
      fill = factor(pop_cat)) +
  geom_histogram(position = 'stack', binwidth = .01) +
  scale_fill_hue(h = c(0,270))

#filtering out zero ratios
ggplot(data = combined_data %>% filter(d_7_per_work != 0)
        )+
  aes(x = d_7_per_work,
      fill = factor(fund_cat)) +
  geom_histogram(position = 'stack', binwidth = .01) +
  scale_fill_hue(h = c(0,270))


ggplot(data = combined_data %>% filter(d_7_per_work != 0)
       , 
       aes(x = d_7_per_work)) +
  geom_dotplot(dotsize = .4, 
               binwidth = max(combined_data$d_7_per_work)/30 
               ) 


combined_data %>% filter(d_7_per_work != 0)

plot2 <- function(df){
plot <- ggplot(data = df) +
  aes(x = Year,
      y = d_7_per_pop,
      label = State) +
  geom_text()

return(plot)
}  

plot2(combined_data %>% filter(pop_cat == 7))

range(combined_data$d_7_per_pop)
hist(combined_data$d_7_per_pop)
density(combined_data$d_7_per_pop)
ggplot(data = combined_data %>% filter(pop_cat == 7)) +
  aes(x = d_7_per_pop) +
  geom_density()

#shows that the d_7 outliers appear to correlate strongly by population category
ggplot(data = combined_data ) +
  aes(x = factor(pop_cat),
      y = d_7_per_pop) +
  geom_boxplot()

median(combined_data$d_7_per_pop)
median()



combined_data %>% filter(State == 'District of Columbia') %>% View()

sd()
test <-
  combined_data %>% 
  group_by(State) %>% 
  summarise(p_value_d_7 = regression_calc(Year, d_7_not_favor, 8),
            trend = regression_calc(Year, d_7_not_favor, 2),
            p_value_d_7_per_pop = regression_calc(Year, d_7_per_pop, 8),
            trend_inc_pop = regression_calc(Year, d_7_per_pop, 2)
  ) %>% filter(p_value_d_7 <= .05) %>% 
  select(State, trend, trend_inc_pop)

d_7_stats %>% arrange(mean_rank, desc(pop_cat)) %>% View()

summary(lm(combined_data$d_7_not_favor ~ combined_data$pop))
regression_calc(combined_data$pop, combined_data$d_7_not_favor, 2)
        
ggplot(data = combined_data %>%  filter(pop_cat < 4) ) +
  aes(x = pop,
      y = d_7_not_favor,
      label = State) +
  geom_text() +
  geom_smooth()

regression_calc(as.numeric(d_7_stats$pop_cat), d_7_stats$mean, 2)
summary(lm(d_7_stats$mean ~ as.numeric(d_7_stats$pop_cat)))
cor(as.numeric(d_7_stats$pop_cat), d_7_stats$mean)

d <- density(combined_data$d_7_per_pop)
library(MESS)
auc(d[["x"]][1:23], d[["y"]][1:23], type = 'spline')


combined_data %>% 
  filter(State == 'Montana') %>% 
  select(-disab_16, -disab_17, -network_status, -agency_name, -compare_cat_18,
         -c_5_informal_review, -c_6_formal_review, -c_7_legal, c_5_per_work,
         c_6_per_work, c_7_per_work) %>% 
  select(State, Year, pop_cat, d_7_per_pop) %>% 
         View()

P <- .10

helper_regression <- function(x, y, value, p){
  #passing in a value of 8 will return the P value if the result is less than p
  #passing in a value of 2 will return the slope if the p value is less than p
  r <- if_else(regression_calc(x, y, 8) < p,
          regression_calc(x, y, value),
          NaN)
  return(r)
}

regress_df <- function(df){
  df <- df %>% 
    summarise(work_pop_p = helper_regression(Year, work_per_pop, 8, P),
              work_pop_lm = helper_regression(Year, work_per_pop, 2, P),
              f_pop_p = helper_regression(Year, f_close_per_pop, 8, P),
              f_pop_lm = helper_regression(Year, f_close_per_pop, 2, P),
              f_work_p = helper_regression(Year, f_close_per_work, 8, P),
              f_work_lm = helper_regression(Year, f_close_per_work, 2, P),
              d_7_pop_p = helper_regression(Year, d_7_per_pop, 8, P),
              d_7_pop_lm = helper_regression(Year, d_7_per_pop, 2, P),
              d_7_work_p = helper_regression(Year, d_7_per_work, 8, P),
              d_7_work_lm = helper_regression(Year, d_7_per_work, 2, P),
              c_5_pop_p = helper_regression(Year, c_5_per_pop, 8, P),
              c_5_pop_lm = helper_regression(Year, c_5_per_pop, 2, P),
              c_5_work_p = helper_regression(Year, c_5_per_work, 8, P),
              c_5_work_lm = helper_regression(Year, c_5_per_work, 2, P),
              c_6_pop_p = helper_regression(Year, c_6_per_pop, 8, P),
              c_6_pop_lm = helper_regression(Year, c_6_per_pop, 2, P),
              c_6_work_p = helper_regression(Year, c_6_per_work, 8, P),
              c_6_work_lm = helper_regression(Year, c_6_per_work, 2, P),
              c_7_pop_p = helper_regression(Year, c_7_per_pop, 8, P),
              c_7_pop_lm = helper_regression(Year, c_7_per_pop, 2, P),
              c_7_work_p = helper_regression(Year, c_7_per_work, 8, P),
              c_7_work_lm = helper_regression(Year, c_7_per_work, 2, P)
                  )
  return(df)
  
}

group_PA <-
  combined_data %>% 
  group_by(is_PA) %>% 
  regress_df() %>% 
  mutate(agg_grp = 2)
  
national <- 
  combined_data %>% 
  regress_df() %>% 
  mutate(agg_grp = 1)

group_pop_cat <-
  combined_data %>% 
  group_by(pop_cat) %>% 
  regress_df() %>% 
  mutate(agg_grp = 3)

group_PA_pop_cat <-
  combined_data %>% 
  group_by(pop_cat, is_PA) %>% 
  regress_df() %>% 
  mutate(agg_grp = 4)

group_state <-
  combined_data %>% 
  group_by(State, is_PA) %>% 
  regress_df() %>% 
  mutate(agg_grp = 5)

test <- full_join(group_PA, national)
test <- full_join(group_pop_cat, test)
test <- full_join(group_PA_pop_cat, test)
test <- full_join(group_state, test)
write.csv(test, 'comparative linear regression10.csv')

?cor
cor(combined_data$pop, combined_data$d_7_not_favor)
lm(combined_data$d_7_not_favor ~ combined_data$pop, method = 'pearson')
?lm
summary(combined_data)
dc <- combined_data$
ggcorr(combined_data)
