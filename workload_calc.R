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
  select(Year, State, agency_name, workload, a_total_indiv ,c_5_informal_review, c_6_formal_review, c_7_legal, d_1_favorable,
         d_2_some_favor, d_7_not_favor,d_10_lack_resources , d_11_conflict, ID )

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
  mutate( c_5_per_work = round(c_5_informal_review / workload, 5), 
          c_6_per_work = round(c_6_formal_review / workload, 5), 
          c_7_per_work = round(c_7_legal / workload, 5), 
          d_1_per_work = round(d_1_favorable / workload, 5),
          d_2_per_work = round(d_2_some_favor / workload, 5), 
          d_7_per_work = round(d_7_not_favor / workload, 5),
          d_10_per_work = round(d_10_lack_resources / workload, 5),
          d_11_per_work = round(d_11_conflict / workload, 5)) %>% 
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

combined_data <- 
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
          d_11_per_work = round(d_11_conflict / workload, 5),
          f_close = d_1_favorable + d_2_some_favor,
          f_close_per_work = round(f_close / workload, 5)) %>%
  #allows joining with ethnicity data
  mutate(ID = as.character(paste(State, Year, sep = ', ')))



#integrates the ethnicity data from the PPR
combined_data <-
  inner_join(combined_data, ethnicity_data, by = 'ID')

remove(bucketed_data, ethnicity_data, indiv_cases_stdzed)


sequence_sd <- function(df, col) {
  col <- enquo(col)
  r <- seq(min(unlist(df %>% select(!!col))), max(unlist(df %>% select(!!col))), sd(unlist(df %>% select(!!col)))/2 )
  return(r)
}

format_money2 <- function(x){
  r <- paste0("$", formatC(as.numeric(x), format="f", digits=0, big.mark=","))
  return(r)
}

regression_calc <- function(x, y, value){
  lm_regress <- summary(lm(y ~ x))
  r <- round(lm_regress[['coefficients']][value], 2)
  return(r)
}


