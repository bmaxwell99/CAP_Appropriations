setwd("C:/Users/dark_/Documents/NDRN/CAP_Appropriations")

source('workload_calc.R')

grouping_data <- 
  combined_data %>% 
  select(State, Year, pop_cat,is_PA, work_per_pop, workload, pop, f_close_per_pop, f_close_per_work, d_7_per_pop, d_7_per_work, d_10_per_pop, 
         d_10_per_work, c_5_per_pop, c_5_per_work, c_6_per_pop, c_6_per_work, c_7_per_pop, c_7_per_work
  )

min_df <-
  grouping_data %>% 
  filter(pop_cat < 0)

min_agg <- PA_agg_stats(min_df)
grouping_data %>% filter(pop_cat < 0)



test <- PA_agg_stats(min_df %>% filter(State == 'Texas'))

agg_stats_df <- function(df){
  
df <- 
  df %>% 
  select(-pop_cat, -is_PA, -State) %>% 
  summarise_all(funs(mean, median, sd)) %>% 
  t() %>% 
  data.frame()

metric_df <- data_frame(metric = c("work_per_pop", "f_close_per_pop", "f_close_per_work", "d_7_per_pop", "d_7_per_work",
                                   "d_10_per_pop", "d_10_per_work", "c_5_per_pop", "c_5_per_work", "c_6_per_pop", "c_6_per_work",
                                   "c_7_per_pop", "c_7_per_work")  )
df <- cbind(metric_df,
               df %>% 
                 slice(1:13) %>% 
                 rename(mean = '.'),
               df %>% 
                 slice(14:26) %>% 
                 rename(median = '.'),
               df %>% 
                 slice(27:39) %>% 
                 rename(sd = '.')
                )

return(df)
}
min_PA_stats <-
  agg_stats_df(min_df %>% filter(is_PA)) %>% 
  rename(mean_PA = mean,
         median_PA = median,
         sd_PA = sd)

full_join(min_PA_stats, delaware_stats)
not_PA_state_names <- 
  combined_data %>%
  filter(!is_PA & pop_cat < 0) %>% 
  distinct(State)

delaware_stats <-
  agg_stats_df(grouping_data %>% filter(State == 'Delaware')) %>% 
  rename(mean_del = mean,
         median_del = median,
         sd_del = sd)


test <- data.frame(PA = filter(min_df, is_PA & pop_cat <0)$work_per_pop)
test2 <- data.frame(One_State = filter(min_df, State == 'Iowa')$work_per_pop)
test3 <- data.frame(Nat = grouping_data$work_per_pop)
df <- rbind(gather(test), gather(test2), gather(test3))


ggplot(gather(test3),aes(x=value, fill=key)) + geom_density(alpha=0.25)
ggplot(gather(test3),aes(x=key, y=value, fill=key)) + geom_boxplot()

y <- summary(x$value)
x <- gather(test3)
outliers = boxplot(x$value, plot = FALSE)$out
?boxplot
which(x$value %in% outliers)

outlier <- (summary(x$value)[["3rd Qu."]]-summary(x$value)[["1st Qu."]] ) * 1.5 + summary(x$value)[["3rd Qu."]]
grouping_data %>% 
  filter(work_per_pop > outlier) %>% View()
  group_by(State) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

min_df <- 
  grouping_data %>% 
  filter(pop_cat < 0 & is_PA)  %>% 
  group_by(Year) %>% 
  summarise_at(5:17, mean)
               
del_df <-
  grouping_data %>% 
  filter(State == 'Delaware') %>% 
  group_by(Year) %>% 
  summarise_at(5:17, mean)

iowa_df <-
  grouping_data %>% 
  filter(State == 'Iowa') %>% 
  group_by(Year) %>% 
  summarise_at(5:17, mean)

maine_df <-
  grouping_data %>% 
  filter(State == 'Maine') 

missi_df <-
  grouping_data %>% 
  filter(State == 'Mississippi') 

#shoves work per pop data into the form ggplot would like it in 
test <- 
  rbind(
    working_data %>% 
      filter(pop_cat < 0 & is_PA) %>% 
      group_by(Year) %>% 
      summarise(work_per_pop = mean(work_per_pop)) %>% 
      mutate(State = 'mean PA') %>% 
      select(State, Year, work_per_pop),
    working_data %>% 
      filter(State == 'Delaware') %>% 
      select(State, Year, work_per_pop),
    working_data %>% 
      filter(State == 'Iowa') %>% 
      select(State, Year, work_per_pop),
    working_data %>% 
      filter(State == 'Maine') %>% 
      select(State, Year, work_per_pop),
    working_data %>% 
      filter(State == 'Mississippi') %>% 
      select(State, Year, work_per_pop),
    working_data %>% 
      filter(State == 'Nebraska') %>% 
      select(State, Year, work_per_pop),
    working_data %>% 
      filter(State == 'New Hampshire') %>% 
      select(State, Year, work_per_pop),
    working_data %>% 
      filter(State == 'Oklahoma') %>% 
      select(State, Year, work_per_pop),
    working_data %>% 
      filter(State == 'Vermont') %>% 
      select(State, Year, work_per_pop)
  )
  
  
ggplot(data = test) +
  aes(x = Year) +
  geom_point(aes(y = work_per_pop, shape = factor(State)))
  
  
test <- combined_data %>% filter(pop_cat < 0)

cor(test$workload, test$pop)  

ggplot(data = grouping_data %>% 
  filter(pop_cat < 0 & !is_PA))+
  aes(x = Year,
      y = work_per_pop,
      label = State,
      color = pop_cat)+
  geom_text()

cor(min_df$pop_cat, min_df$work_per_pop)


test3 <- data.frame(Nat = grouping_data$work_per_pop)
x <- gather(test3)

outliers <- function(df, col){
  col <- enquos(col)
  #gurantees a df and gathers the data
  x <- 
    df %>% 
    select(!!col) %>%  
    gather()
    
    
  df <-
    df %>% 
    filter(col > outlier)
    
  return(df)
}
outliers(grouping_data, work_per_pop)
outliers = boxplot(x$value, plot = FALSE)$out
?boxplot
which(x$value %in% outliers)

#instead of debugging the function just replace this
x <- 
  grouping_data %>% 
  select(!!col) %>%  
  gather()

outlier <- (summary(x$value)[["3rd Qu."]] - summary(x$value)[["1st Qu."]] ) * 1.5 + summary(x$value)[["3rd Qu."]]


y <- grouping_data %>% 
  filter(workload > outlier) %>% 
  group_by(State) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

y <- y %>% select(Year, State, is_PA, pop_cat,work_per_pop, workload ,pop)

y %>% filter(Year >= 2015) %>% View()

hist(filter(combined_data, c_6_formal_review > 0)$c_6_formal_review)

formal_data<- 
  combined_data %>% 
  mutate(formal_res = c_6_formal_review + c_7_legal) %>% 
  select(Year, State, pop, pop_cat, is_PA, workload, work_per_pop, formal_res, c_6_formal_review, c_7_legal) %>% 
  arrange(desc(formal_res))

summary(c_6_data$c_6_formal_review)
working_data %>% 
  filter(!is_PA) %>% 
  summarise(mean(c_6_formal_review))

z <- indiv_cases_stdzed %>% select(State, Year, Formal.appeal.fair.hearing) %>% arrange(desc(Formal.appeal.fair.hearing))
indiv_cases_stdzed$Formal.appeal.fair.hearing



cor.test(c_6_data$pop, c_6_data$c_6_formal_review, method = 'kendal')

ggplot(data = formal_data %>% filter(!is_PA))+
  aes(x = Year,
      y = formal_res)+
  geom_point() +
  geom_smooth()

df <- formal_data %>% filter(!is_PA)


 PA <- as.numeric(factor(filter(df, formal_res < 100)$is_PA, 
                                  levels=c("TRUE","FALSE"))) 
 formal_conc <- as.numeric(filter(df, formal_res < 100)$formal_res)

m <- cbind(PA, formal_conc) 
cor.test(PA, formal_conc, method="kendall")


mean(df$formal_res)
median(df$formal_res)
summary(df$formal_res)
sd(df$formal_res)

df %>% filter(formal_res > 1) %>% nrow()

df <- formal_data
df_x <- df %>% filter(is_PA)
df_y <- df %>% filter(!is_PA)
#PA
sum(df_x$formal_res)/sum(df_x$workload)
#NOT PA
sum(df_y$formal_res)/sum(df_y$workload)

gg
x <- 
  formal_data %>% 
  mutate(formal_res_per_work = round(formal_res / workload, 5) ) %>% 
  filter(formal_res < 6)

summary(lm(x$formal_res ~ x$Year))


