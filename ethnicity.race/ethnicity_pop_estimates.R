

setwd("C:/Users/dark_/Documents/NDRN/CAP_Appropriations")
source('workload_calc.R')
setwd("C:/Users/dark_/Documents/NDRN/CAP_Appropriations/ethnicity.race")

census_est <- 
  read.csv('sc-est2018-alldata6.csv') %>% 
  #where sex = 0 is both, and origin = 1 is non-hispanic and 2 is hispanic
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

american_native_pop_est <- rbind(filter_demo_pop(joined_pop_est, 3, '2011' ), 
                       filter_demo_pop(joined_pop_est, 3, '2012' ), 
                       filter_demo_pop(joined_pop_est, 3, '2013' ), 
                       filter_demo_pop(joined_pop_est, 3, '2014' ), 
                       filter_demo_pop(joined_pop_est, 3, '2015' ), 
                       filter_demo_pop(joined_pop_est, 3, '2016' ), 
                       filter_demo_pop(joined_pop_est, 3, '2017' ), 
                       filter_demo_pop(joined_pop_est, 3, '2018' )) %>% 
  rename(american_native_pop_est = pop_est) %>% 
  mutate(ID = paste(State, Year, sep = ', ')) %>% 
  select(american_native_pop_est, ID)


asian_pop_est <- rbind(filter_demo_pop(joined_pop_est, 4, '2011' ), 
                       filter_demo_pop(joined_pop_est, 4, '2012' ), 
                       filter_demo_pop(joined_pop_est, 4, '2013' ), 
                       filter_demo_pop(joined_pop_est, 4, '2014' ), 
                       filter_demo_pop(joined_pop_est, 4, '2015' ), 
                       filter_demo_pop(joined_pop_est, 4, '2016' ), 
                       filter_demo_pop(joined_pop_est, 4, '2017' ), 
                       filter_demo_pop(joined_pop_est, 4, '2018' )) %>% 
  rename(asian_pop_est = pop_est) %>% 
  mutate(ID = paste(State, Year, sep = ', ')) %>% 
  select(asian_pop_est, ID)

pacific_islander_pop_est <- rbind(filter_demo_pop(joined_pop_est, 5, '2011' ), 
                                 filter_demo_pop(joined_pop_est, 5, '2012' ), 
                                 filter_demo_pop(joined_pop_est, 5, '2013' ), 
                                 filter_demo_pop(joined_pop_est, 5, '2014' ), 
                                 filter_demo_pop(joined_pop_est, 5, '2015' ), 
                                 filter_demo_pop(joined_pop_est, 5, '2016' ), 
                                 filter_demo_pop(joined_pop_est, 5, '2017' ), 
                                 filter_demo_pop(joined_pop_est, 5, '2018' )) %>% 
  rename(pacific_islander_pop_est = pop_est) %>% 
  mutate(ID = paste(State, Year, sep = ', ')) %>% 
  select(pacific_islander_pop_est, ID)

combined_data <-
  combined_data %>% 
  mutate(ID = paste(State, Year, sep = ', '))

ethnicity_data <-
  inner_join(combined_data, black_pop_est, by = 'ID') %>% 
  mutate(black_per_pop = round(black_pop_est / pop, 4)) %>% 
  mutate(black_clients = round(n_black / a_total_indiv, 4))

ethnicity_data <-
  inner_join(ethnicity_data, american_native_pop_est, by = 'ID') %>% 
  mutate(american_native_per_pop = round(american_native_pop_est / pop, 4)) %>% 
  mutate(american_native_clients = round(n_american_native / a_total_indiv, 5))

ethnicity_data <-
  inner_join(ethnicity_data, asian_pop_est, by = 'ID') %>% 
  mutate(asian_per_pop = round(asian_pop_est / pop, 4)) %>% 
  mutate(asian_clients = round(n_asian / a_total_indiv, 4))

ethnicity_data <-
  inner_join(ethnicity_data, pacific_islander_pop_est, by = 'ID') %>% 
  mutate(pacific_islander_per_pop = round(pacific_islander_pop_est / pop, 4)) %>% 
  mutate(pacific_islander_clients = round(n_pacific_islander / a_total_indiv, 4))
  

remove(census_est, joined_pop_est, black_pop_est)

average_ethnicity <- function(df){ 
  df <-
    df %>%
    group_by(State) %>% 
    summarise(#average of clients demo
      avg_black_client = sum(black_clients) / n(),
      avg_native_american_client = sum(american_native_clients) / n(),
      avg_asian_client = sum(asian_clients) / n(),
      avg_pacific_islander_clients = sum(pacific_islander_clients) / n(),
      
      #average of state population
      avg_black_pop = sum(black_per_pop) / n(),
      avg_native_american_pop = sum(american_native_per_pop) / n(),
      avg_asian_pop = sum(asian_per_pop) / n(),
      avg_pacific_islander_pop = sum(pacific_islander_per_pop) / n(),
      
      #shows the difference
      dif_black = (sum(black_clients) - sum(black_per_pop))/n(),
      dif_american_native = (sum(american_native_clients) - sum(american_native_per_pop))/n(),
      dif_asian = (sum(asian_clients) - sum(asian_per_pop))/n(),
      dif_pacific_islander = (sum(pacific_islander_clients) - sum(pacific_islander_per_pop))/n())
  
  return(df)
}




ethnic_part1 <-
  average_ethnicity(ethnicity_data %>% 
                      filter(Year < 2015)) %>% 
  select(State, 
         avg_black_client, avg_black_pop, dif_black,
         avg_native_american_client, avg_native_american_pop, dif_american_native,
         avg_asian_client, avg_asian_pop, dif_asian,
         avg_pacific_islander_clients, avg_pacific_islander_pop, dif_pacific_islander)
ethnic_part2 <-
  average_ethnicity(ethnicity_data %>%  
                      filter(Year > 2014)) %>% 
  select(State, 
         avg_black_client, avg_black_pop, dif_black,
         avg_native_american_client, avg_native_american_pop, dif_american_native,
         avg_asian_client, avg_asian_pop, dif_asian,
         avg_pacific_islander_clients, avg_pacific_islander_pop, dif_pacific_islander)

#write.csv(ethnic_part1, '1st 4 Year Window, Populations Served by CAP.csv')
#write.csv(ethnic_part2, '2nd 4 Year Window, Populations Served by CAP.csv')
#write.csv(ethnicity_summary, 'Ethnic Populations Served by CAP.csv')

