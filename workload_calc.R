source('PPR_cleaning.R')
source('Clean_join_data.r')

#estimates funding for 2011-2015 using the ratio of total funding recieved in 2016
bucketed_data <-
  bucketed_data %>% 
  mutate(dol_est_2015 = dollar_2016,
         dol_est_2014 = dollar_2016,
         dol_est_2013 = dollar_2016,
         dol_est_2012 = dollar_2016,
         dol_est_2011 = dollar_2016)


#combines workload
workload_w_state <-
  indiv_cases_stdzed %>% 
  select(Year, State, agency_name, workload, ID)

workload_w_state <- inner_join(bucketed_data, workload_w_state, by='State')



bucket_size <- 10000

workload_calc_18 <-
  workload_w_state %>%
  filter(Year == 2018) %>% 
  mutate(pop = pop_18) %>% 
  mutate(funding = dollar_2018) %>% 
  mutate(fund_cat = cut_interval(funding, 
                                 length = bucket_size,
                                 labels = seq(min(funding), max(funding), bucket_size) %>% 
                                   format_money()
  )
  )

workload_calc_17 <-
  workload_w_state %>%
  filter(Year == 2017) %>% 
  mutate(pop = pop_17) %>% 
  mutate(funding = dollar_2017) %>% 
  mutate(fund_cat = cut_interval(funding, 
                                 length = bucket_size,
                                 labels = seq(min(funding), max(funding), bucket_size) %>% 
                                   format_money()
  )
  )

workload_calc_16 <-
  workload_w_state %>% 
  filter(Year == 2016) %>% 
  mutate(pop = pop_16) %>% 
  mutate(funding = dollar_2016) %>% 
  mutate(fund_cat = cut_interval(funding, 
                                 length = bucket_size,
                                 labels = seq(min(funding), max(funding), bucket_size) %>% 
                                   format_money()
  )
  )

workload_calc_15 <-
  workload_w_state %>% 
  filter(Year == 2015) %>% 
  mutate(pop = pop_15) %>% 
  mutate(funding = dol_est_2015) %>% 
  mutate(fund_cat = cut_interval(funding, 
                                 length = bucket_size,
                                 labels = seq(min(funding), max(funding), bucket_size) %>% 
                                   format_money()
  )
  )

workload_calc_14 <-
  workload_w_state %>% 
  filter(Year == 2014) %>% 
  mutate(pop = pop_14)  %>% 
  mutate(funding = dol_est_2014) %>% 
  mutate(fund_cat = cut_interval(funding, 
                                 length = bucket_size,
                                 labels = seq(min(funding), max(funding), bucket_size) %>% 
                                   format_money()
  )
  )


workload_calc_13 <-
  workload_w_state %>% 
  filter(Year == 2013) %>% 
  mutate(pop = pop_13) %>% 
  mutate(funding = dol_est_2013) %>% 
  mutate(fund_cat = cut_interval(funding, 
                                 length = bucket_size,
                                 labels = seq(min(funding), max(funding), bucket_size) %>% 
                                   format_money()
  )
  )


workload_calc_12 <-
  workload_w_state %>% 
  filter(Year == 2012) %>% 
  mutate(pop = pop_12) %>% 
  mutate(funding = dol_est_2012) %>% 
  mutate(fund_cat = cut_interval(funding, 
                                 length = bucket_size,
                                 labels = seq(min(funding), max(funding), bucket_size) %>% 
                                   format_money()
  )
  )


workload_calc_11 <-
  workload_w_state %>% 
  filter(Year == 2011) %>% 
  mutate(pop = pop_11) %>% 
  mutate(funding = dol_est_2011) %>% 
  mutate(fund_cat = cut_interval(funding, 
                                 length = bucket_size,
                                 labels = seq(min(funding), max(funding), bucket_size) %>% 
                                   format_money()
  )
  )


workload_w_state <- 
  #stitches the different years back together
  rbind(workload_calc_11, workload_calc_12, workload_calc_13, workload_calc_14, 
                       workload_calc_15, workload_calc_16, workload_calc_17, workload_calc_18) %>% 
  #adds formula columns
  mutate(work_per_pop = round(workload / pop, 8),
         fund_per_pop = round(funding / pop, 5),
         work_per_dol = round(workload / funding, 5),
         dol_per_work = round(funding / workload, 5),
         work_relative_resources = work_per_pop * fund_per_pop,
         #adds boolean to show whether recieved min funding
         min_funding = if_else(funding <= 141917, T, F)) %>% 
  #removes extra columns
  select(-pop_18, -pop_17, -pop_16, -pop_15, -pop_14, -pop_13, -pop_12, -pop_11,
         -dollar_2018, -dollar_2017, -dollar_2016, -dol_est_2015, -dol_est_2014, -dol_est_2013, -dol_est_2012,
         -dol_est_2011, -funding_cat_18, -funding_cat_17, -funding_cat_16, -dol_per_pop_18, -dol_per_pop_17, -dol_per_pop_16
  )

#row.names(regress_fund_data) <- c(unlist(regress_fund_data[1])) 



remove(workload_calc_11, workload_calc_12, workload_calc_13, workload_calc_14, 
       workload_calc_15, workload_calc_16, workload_calc_17, workload_calc_18)
