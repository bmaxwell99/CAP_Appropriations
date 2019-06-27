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
  select(Year, State, agency_name, workload, ID )

workload_w_state <- inner_join(bucketed_data, workload_w_state, by='State')



bucket_size <- 10000
#TODO this shit is ridiculout figure out a different way to do this
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
  mutate(work_per_pop = round(workload / pop * 100000, 8), #cases per 100,000 people
         fund_per_pop = round(funding / pop * 100000, 5), #dollars of funding per 100,000 people
         work_per_dol = round(workload / funding * 100000, 5), #cases per 100,000 dollars in funding
         fund_per_work = round(funding / workload, 5), #dollars of funding recieved for each case worked
         #adds boolean to show whether recieved min funding
         #TODO Fully write out the decision to include Orgeon and OKlahoma in the mins, but not Kentucky
         min_funding = if_else(funding <= 140000, T, F)
         ) %>% 
  #buckets the states based off population, as presented in the visual argument section of comparable graphs
  mutate(pop_cat = if_else(is.element(State, c('South Dakota', 'Delaware', 'Montana', 'Rhode Island')),
                          -5, 
                          if_else(is.element(State, c('Maine', 'New Hampshire', 'Hawaii')),
                                  -4,
                                  if_else(is.element(State, c('Idaho', 'West Virginia', 'Nebraska', 'New Mexico')),
                                          -3,
                                          if_else(is.element(State, c('Nevada', 'Kansas', 'Utah','Arkansas','Mississippi', 'Iowa')),
                                                  -2,
                                                  if_else(is.element(State, c('Puerto Rico', 'Connecticut', 'Oklahoma', 'Oregon')),
                                                          -1,
                                                          if_else(!min_funding & pop < 5100000, 
                                                                  1, 
                                                                  if_else(!min_funding & pop < 6150000, 
                                                                          2,
                                                                          if_else(!min_funding & pop<7600000,
                                                                                  3,
                                                                                  if_else(!min_funding & pop<9000000,
                                                                                          4,
                                                                                          if_else(!min_funding & pop<11100000,
                                                                                                  5,
                                                                                                  if_else(!min_funding & pop<13000000,
                                                                                                          6,
                                                                                                          if_else(pop>13000000,
                                                                                                                  7,
                                                                                                                  -6))))))))))))
         )%>% 
  #removes extra columns
  select(-pop_18, -pop_17, -pop_16, -pop_15, -pop_14, -pop_13, -pop_12, -pop_11,
         -dollar_2018, -dollar_2017, -dollar_2016, -dol_est_2015, -dol_est_2014, -dol_est_2013, -dol_est_2012,
         -dol_est_2011, -funding_cat_18, -funding_cat_17, -funding_cat_16, -dol_per_pop_18, -dol_per_pop_17, -dol_per_pop_16
  )

#adds the area of each state
state_area <- 
  as.data.frame(state.x77) %>% 
  select(Area) %>% 
  mutate(State = row.names(state.x77)) %>% 
  rbind(data_frame(Area = c(3515, 68) , State = c('Puerto Rico', 'District of Columbia')))

workload_w_state <-
  inner_join(workload_w_state, state_area, by = 'State')


#creates a df where showing the relationship between 
df_network <-
  data_frame(State = unlist(workload_w_state %>% distinct(State)), network_status = if_else(is.element(State, c( 'Delaware' ,'Georgia',	'Guam',	'Mississippi' ,	'Pennsylvania',	'Vermont',	'Washington')),
                                                                                                       'Non-State Agency',
                                                                                                       if_else(is.element(State, c('Alabama',	'Illinois' ,	'Maine' ,	'Maryland' ,	'Nebraska' ,	'North Carolina')),
                                                                                                                          'VR Agency',
                                                                                                                          if_else(is.element(State, c('Iowa' ,	'Kentucky',	'Massachusetts',	'New Hampshire',	'Oklahoma',	'Wisconsin')),
                                                                                                                                  'Other State Agency',
                                                                                                                                  'P&A Agency'))))
                                                                                                                          
workload_w_state <- right_join(df_network, workload_w_state, by = 'State')

remove(workload_calc_11, workload_calc_12, workload_calc_13, workload_calc_14, 
       workload_calc_15, workload_calc_16, workload_calc_17, workload_calc_18, 
       state_area,df_network)


sequence_sd <- function(df, col) {
  col <- enquo(col)
  r <- seq(min(unlist(df %>% select(!!col))), max(unlist(df %>% select(!!col))), sd(unlist(df %>% select(!!col)))/2 )
  return(r)
}

format_money2 <- function(x){
  r <- paste0("$", formatC(as.numeric(x), format="f", digits=0, big.mark=","))
  return(r)
}
