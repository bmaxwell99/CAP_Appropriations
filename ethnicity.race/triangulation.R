
setwd("C:/Users/dark_/Documents/NDRN/CAP_Appropriations")
source('workload_calc.R')
setwd("C:/Users/dark_/Documents/NDRN/CAP_Appropriations/ethnicity.race")
source('ethnicity_pop_estimates.R')

padd_ethnic <- 
  read.csv('ethnicity data served by PADD.csv', stringsAsFactors = FALSE) %>% 
  mutate(Year = clean(ï..), 
         state_abr = X, 
         n_black = clean(Black.African.American), 
         n_asian = clean(Asian),
         n_pacific_islander = clean(Native.Hawaiian.Pacific..Islander), 
         n_american_native = clean(American.Indian.Alaskan.Native),
         a_total_indiv = clean(Total..Clients) ) %>% 
  mutate(black_PAAD = round(n_black / a_total_indiv, 5),
        native_american_PAAD = round(n_american_native / a_total_indiv, 5),
        asian_PAAD = round(n_asian / a_total_indiv, 5),
        pacific_islander_PAAD = round(n_pacific_islander / a_total_indiv, 5)) %>% 
  select(Year, state_abr, black_PAAD, native_american_PAAD, asian_PAAD, pacific_islander_PAAD)

abrev <- 
  cbind(state_abr = as.character(state.abb), State = as.character(state.name)) %>% 
  data.frame()

df <-
  data_frame(state_abr = as.character(c('PR', 'DC' )) , State = as.character(c('Puerto Rico', 'District of Columbia')))

abrev <-
  rbind(df, abrev) 

padd_ethnic <- 
  inner_join(padd_ethnic, abrev, by = 'state_abr') %>% 
  mutate(ID = paste(State, Year, sep = ', '))

  
#imports paimi data
paimi_ethnic <- 
  read.csv('ethnicity data served by PAIMI.csv', stringsAsFactors = FALSE) %>% 
  mutate(Year = clean(ï..Year), 
         state_abr = States, 
         n_black = clean(Black..African.American), 
         n_asian = clean(Asian),
         n_pacific_islander = clean(Native.Hawaiian...Other.Pacific.Islander), 
         n_american_native = clean(Native.American..Alaska.Native),
         a_total_indiv = clean(Clients.Served)) %>% 
  mutate(black_PAIMI = round(n_black / a_total_indiv, 5),
         native_american_PAIMI = round(n_american_native / a_total_indiv, 5),
         asian_PAIMI = round(n_asian / a_total_indiv, 5),
         pacific_islander_PAIMI = round(n_pacific_islander / a_total_indiv, 5)) %>% 
  select(Year, state_abr, black_PAIMI, native_american_PAIMI, asian_PAIMI, pacific_islander_PAIMI)


paimi_ethnic <- 
  inner_join(paimi_ethnic, abrev, by = 'state_abr') %>% 
  mutate(ID = paste(State, Year, sep = ', '))

#reshapes cap data
cap_ethnic <-
  ethnicity_data %>% 
  filter(Year == 2015 | Year == 2016 | Year == 2017) %>% 
  select(Year, State, black_CAP = black_clients, asian_CAP = asian_clients, 
         pacific_islander_CAP = pacific_islander_clients, native_american_CAP = american_native_clients,
         black_per_pop, asian_per_pop, pacific_islander_per_pop, native_american = american_native_per_pop) %>% 
  mutate(ID = paste(State, Year, sep = ', '))


#imports pair data
pair_ethnic <- 
  read.csv('ethnicity data served by PAIR.csv', stringsAsFactors = FALSE) %>% 
  mutate(Year = clean(ï..Year), 
         State = as.character(trimws(State)),
         n_american_native = clean(American.Indian.Alaska),
         n_black = clean(Black), 
         n_asian = clean(Asian),
         n_pacific_islander = clean(Hawaiian.Pacific), 
         a_total_indiv = clean(Total)) %>% 
  mutate(black_PAIR = round(n_black / a_total_indiv, 5),
         native_american_PAIR = round(n_american_native / a_total_indiv, 5),
         asian_PAIR = round(n_asian / a_total_indiv, 5),
         pacific_islander_PAIR = round(n_pacific_islander / a_total_indiv, 5),
         ID = paste(State, Year, sep = ', ')) %>% 
  select(black_PAIR, native_american_PAIR, asian_PAIR, pacific_islander_PAIR, ID) 
         
test <- inner_join(cap_ethnic, paimi_ethnic %>% select(-Year, -State, -state_abr), by = 'ID')

tes1 <- full_join(padd_ethnic %>% 
                    filter(State != 'Puerto Rico')%>% 
                    select(-Year, -State, -state_abr), test, by = 'ID')

tes2 <- inner_join(pair_ethnic,
                   tes1,
                   by = 'ID')

write.csv(tes2, 'triangulated_ethnicity_data.csv')

tes3 <-
  tes2 %>% 
  group_by(State) %>% 
  summarise(
    #average of three years of black ethnicity
    black_PAAD = sum(black_PAAD, na.rm = TRUE) / n(),
    black_PAIR = sum(black_PAIR, na.rm = TRUE) / n(),
    black_PAIMI = sum(black_PAIMI, na.rm = TRUE) / n(),
    black_CAP = sum(black_CAP, na.rm = TRUE) / n(),
    black_pop = sum(black_per_pop, na.rm = TRUE) / n(),
    
    #average of three years of native american ethnicity
    native_american_PAAD = sum(native_american_PAAD, na.rm = TRUE) / n(),
    native_american_PAIR = sum(native_american_PAIR, na.rm = TRUE) / n(),
    native_american_PAIMI = sum(native_american_PAIMI, na.rm = TRUE) / n(),
    native_american_CAP = sum(native_american_CAP, na.rm = TRUE) / n(),
    native_american_pop = sum(native_american, na.rm = TRUE) / n(),
    
    #average of three years of asian american ethnicity
    asian_PAAD = sum(asian_PAAD, na.rm = TRUE) / n(),
    asian_PAIR = sum(asian_PAIR, na.rm = TRUE) / n(),
    asian_PAIMI = sum(asian_PAIMI, na.rm = TRUE) / n(),
    asian_CAP = sum(asian_CAP, na.rm = TRUE) / n(),
    asian_pop = sum(asian_per_pop, na.rm = TRUE) / n(),
    
    #average of three years of pacific islander ethnicity
    pacific_islander_PAAD = sum(pacific_islander_PAAD, na.rm = TRUE) / n(),
    pacific_islander_PAIR = sum(pacific_islander_PAIR, na.rm = TRUE) / n(),
    pacific_islander_PAIMI = sum(pacific_islander_PAIMI, na.rm = TRUE) / n(),
    pacific_islander_CAP = sum(pacific_islander_CAP, na.rm = TRUE) / n(),
    pacific_islander_pop = sum(pacific_islander_per_pop, na.rm = TRUE) / n()
    
  )

write.csv(tes3, 'triangulated_ethnicity_data.csv')

