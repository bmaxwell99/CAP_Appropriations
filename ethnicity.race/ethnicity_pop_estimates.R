

setwd("C:/Users/dark_/Documents/NDRN/CAP_Appropriations")
source('workload_calc.R')
setwd("C:/Users/dark_/Documents/NDRN/CAP_Appropriations/ethnicity.race")

census_est <- 
  read.csv('sc-est2018-alldata6.csv') %>% 
  filter(SEX == 0, ORIGIN == 1)


sum_by_demo <- function(col, year){
  #returns 
  col <- enquo(col)
  year <- enquo(year)
  df <- 
    census_est %>% 
    group_by(NAME, RACE) %>% 
    summarise(x = sum(!!col)) %>% 
    mutate(ID = paste(NAME, RACE, sep = ', ')) %>% 
    ungroup() %>% 
    select(-NAME, -RACE)
  
  return(df)
  
}

#nested join group in combo with the above function 
joined_pop_est <- inner_join(sum_by_demo(POPESTIMATE2011, 2011), 
                  inner_join(sum_by_demo(POPESTIMATE2012, 2012),
                             inner_join(sum_by_demo(POPESTIMATE2013, 2013),
                                        inner_join(sum_by_demo(POPESTIMATE2014, 2014),
                                                   inner_join(sum_by_demo(POPESTIMATE2015, 2015),
                                                              inner_join(sum_by_demo(POPESTIMATE2016, 2016),
                                                                         inner_join(sum_by_demo(POPESTIMATE2017, 2017),
                                                                                    sum_by_demo(POPESTIMATE2018, 2018),
                                                                                    by = 'ID'),
                                                                         by = 'ID'),
                                                              by = 'ID'),
                                                   by = 'ID'),
                                        by = 'ID'),
                             by = 'ID'),
                  by = 'ID') %>% 
          separate(ID, c('State', 'Race'),sep = ", ") %>% 
  rename('2011' = x.x.x.x.x,
         '2012' = x.y,
         '2013' = x.x.x.x,
         '2014' = x.y.y,
         '2015' = x.x.x,
         '2016' = x.y.y.y,
         '2017' = x.x,
         '2018' = x.y.y.y.y
         )


filter_demo_pop<- function(df, race, year){
  year <- enquo(year)
  df <- 
    df %>% 
    filter(Race == race) %>% 
    select(State, !!year) %>% 
    mutate(Year = as.numeric(!!year)) %>% 
    rename(pop_est = !!year)
    
  return(df)
    
  
}

black_pop_est <- rbind(filter_demo_pop(joined_pop_est, 2, '2011' ), 
                       filter_demo_pop(joined_pop_est, 2, '2012' ), 
                       filter_demo_pop(joined_pop_est, 2, '2013' ), 
                       filter_demo_pop(joined_pop_est, 2, '2014' ), 
                       filter_demo_pop(joined_pop_est, 2, '2015' ), 
                       filter_demo_pop(joined_pop_est, 2, '2016' ), 
                       filter_demo_pop(joined_pop_est, 2, '2017' ), 
                       filter_demo_pop(joined_pop_est, 2, '2018' )) %>% 
  rename(black_pop_est = pop_est) %>% 
  mutate(ID = paste(State, Year, sep = ', ')) %>% 
  select(black_pop_est, ID)


ethnicity_data <-
  inner_join(combined_data, black_pop_est, by = 'ID') %>% 
  mutate(black_per_pop = round(black_pop_est / pop, 4)) %>% 
  mutate(black_clients = round(n_black / a_total_indiv, 4))

remove(census_est, combined_data, joined_pop_est, workload_w_state, black_pop_est)

#analysis
black_client_exp <-
  ethnicity_data %>% 
  select(State, Year, black_per_pop, black_clients) %>% 
  mutate(underserved = black_per_pop > black_clients) %>% 
  filter(underserved)

black_client_exp %>% group_by(State) %>% summarise(n = n()) %>% arrange(desc(n))


