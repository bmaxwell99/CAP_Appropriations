setwd("C:/Users/dark_/Documents/NDRN/CAP_Appropriations")
source('workload_calc.R')


p_calc <- function(x, y, value){
  lm_regress <- summary(lm(y ~ x))
  r <- round(lm_regress[['coefficients']][value], 2)
  return(r)
}

df_regress <- 
  workload_w_state %>% 
  #combines the two connecticut agencies in 2017 into just one
  filter(ID != '2017, Connecticut, Disability Rights Connecticut') %>% 
  mutate(workload = replace(workload, 
                            State == 'Connecticut'& Year == 2017, 
                            53)) %>% 
  mutate(d_1_favorable = replace(d_1_favorable, 
                            State == 'Connecticut'& Year == 2017, 
                            25)) %>% 
  #combines the two DC line items in 2011 into just one
  filter(workload != 65 | ID != '2011, District of Columbia, University Legal Services, Inc.') %>% 
  mutate(workload = replace(workload, 
                            State == 'District of Columbia'& Year == 2011, 
                            102)) %>% 
  mutate(c_6_formal_review = replace(c_6_formal_review, 
                                 State == 'District of Columbia'& Year == 2011, 
                                 15)) %>%
  mutate(d_1_favorable = replace(d_1_favorable, 
                                     State == 'District of Columbia'& Year == 2011, 
                                     25)) %>%
  mutate(d_2_some_favor = replace(d_2_some_favor, 
                                 State == 'District of Columbia'& Year == 2011, 
                                 19)) %>%
  mutate(d_7_not_favor = replace(d_7_not_favor, 
                                 State == 'District of Columbia'& Year == 2011, 
                                 2)) %>%
  mutate(d_10_lack_resources = replace(d_10_lack_resources, 
                                 State == 'District of Columbia'& Year == 2011, 
                                 1)) %>% 
  #combines the two Nevada agencies into one line 
  filter(ID != '2013, Nevada, State Of Nevada Rehabilitation Division') %>% 
  mutate(workload = replace(workload, 
                            State == 'Nevada'& Year == 2013, 
                            40)) %>% 
  mutate(c_5_informal_review = replace(c_5_informal_review, 
                            State == 'Nevada'& Year == 2013, 
                            5)) %>% 
  mutate(c_6_formal_review = replace(c_6_formal_review, 
                                     State == 'Nevada'& Year == 2013, 
                                     3)) %>%
  mutate(d_1_favorable = replace(d_1_favorable, 
                                 State == 'Nevada'& Year == 2013,
                                 13)) %>%
  mutate(d_2_some_favor = replace(d_2_some_favor, 
                                  State == 'Nevada'& Year == 2013,
                                  2)) %>%
  mutate(d_7_not_favor = replace(d_7_not_favor, 
                                 State == 'Nevada'& Year == 2013, 
                                 1)) %>%
  mutate(d_10_lack_resources = replace(d_10_lack_resources, 
                                       State == 'Nevada'& Year == 2013, 
                                       1)) %>% 
  #combines the two New York agencies into one line 
  filter(ID != '2013, New York, Disability Advocates, Inc. dba DRNY') %>% 
  mutate(workload = replace(workload, 
                            State == 'New York'& Year == 2013, 
                            826)) %>% 
  mutate(c_5_informal_review = replace(c_5_informal_review, 
                                       State == 'New York'& Year == 2013, 
                                       13)) %>%
  mutate(d_1_favorable = replace(d_1_favorable, 
                                 State == 'New York' & Year == 2013,
                                 126)) %>%
  mutate(d_2_some_favor = replace(d_2_some_favor, 
                                  State == 'New York'& Year == 2013,
                                  62)) %>%
  #combines the South Carolina agencies into one line
  filter(ID != '2015, South Carolina, Department of Admin. Client Assistance Program') %>% 
  mutate(workload = replace(workload, 
                            State == 'South Carolina'& Year == 2015, 
                            151)) %>% 
  mutate(c_5_informal_review = replace(c_5_informal_review, 
                                       State == 'South Carolina'& Year == 2015, 
                                       39)) %>%
  mutate(d_1_favorable = replace(d_1_favorable, 
                                 State == 'South Carolina'& Year == 2015, 
                                 61)) %>%
  mutate(d_2_some_favor = replace(d_2_some_favor, 
                                  State == 'South Carolina'& Year == 2015, 
                                  48)) %>%
  mutate(d_7_not_favor = replace(d_7_not_favor, 
                                 State == 'South Carolina'& Year == 2015, 
                                 4)) %>% 
  #recomputes per ratios to reflect the above changes
  mutate( work_per_pop = round(workload / pop * 100000, 8), #cases per 100,000 people
          work_per_dol = round(workload / funding * 100000, 5), #cases per 100,000 dollars in funding
          c_5_per_work = round(c_5_informal_review / workload, 5), 
          c_6_per_work = round(c_6_formal_review / workload, 5), 
          c_7_per_work = round(c_7_legal / workload, 5), 
          d_1_per_work = round(d_1_favorable / workload, 5),
          d_2_per_work = round(d_2_some_favor / workload, 5), 
          d_7_per_work = round(d_7_not_favor / workload, 5),
          d_10_per_work = round(d_10_lack_resources / workload, 5),
          d_11_per_work = round(d_11_conflict / workload, 5))

  




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

#adds regression variables to the data
workload_regression <-
  df_regress %>% 
  group_by(State) %>% 
  summarise(p_value_work_per_pop = p_calc(Year, work_per_pop, 8),
            lm_work_per_pop = p_calc(Year,work_per_pop,  2),
            p_value_pop = p_calc(Year, pop, 8),
            lm_pop = p_calc(Year, pop, 2),
            p_value_work = p_calc(Year, workload, 8),
            lm_work = p_calc(Year,workload,  2)
  )


df <- workload_regression %>% filter(p_value_work_per_pop < .05 )

test <- inner_join(df_regress, df, by = 'State')

#national trend
summary(lm(df_regress$work_per_pop ~ df_regress$Year))

test %>% filter(Year ==2018) %>% select(State, pop_cat, lm_work_per_pop) %>% View()

#graphs just the individual states regression
ggplot() +
  aes(x = Year,
      y = work_per_pop)+ 
  geom_smooth(data = test, aes(group = State), method = lm, se = FALSE) + aes(color = factor(pop_cat)) + 
  scale_color_hue(h = c(0,180))

bar_data <- test %>% group_by(pop_cat) %>% summarise(avg_lm_pop_cat = mean(lm_work))

ggplot() +
  aes(x = reorder(State, work_lm),
      y = work_lm,
      fill = factor(pop_cat)) +
  geom_col(data = test)+
  coord_flip() +


df1 <- df_regress %>%  group_by(State) %>% distinct(agency_name, Year) 
df2 <-df_regress %>%  group_by(State) %>% summarise(n = n_distinct(agency_name)) %>%  filter(n > 1) %>% select(State)

df3 <- right_join(df1, df2, by= "State")

ggplot(data = df3) +
  aes(x = factor(Year),
      y = work_per_pop,
      color = agency_name) +
  geom_point()
