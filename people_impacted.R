setwd("C:/Users/dark_/Documents/NDRN/CAP_Appropriations")

source('workload_calc.R')


territory_work <- 
  indiv_cases_stdzed %>% 
  select(Year, State, agency_name, workload) %>% 
  filter(is.element(State, c('Guam', 'American Samoa', 'Virgin Islands', 'Northern Marianas'))) %>% 
  mutate(ID = paste(trimws(State), trimws(Year), trimws(agency_name), sep = ', '))

territory_ir <-
  I_R_data %>% 
  filter(is.element(State, c('Guam', 'American Samoa', 'Virgin Islands', 'Northern Marianas'))) %>% 
  select(-State)

territory_data <-
  inner_join(territory_work, territory_ir, by = 'ID') %>% 
  mutate(funding = 59477,
         indiv_helped = workload + total_i_r + indiv_trained) %>%
  select(Year, State, funding, workload, total_i_r, indiv_trained, indiv_helped)

people_data <- 
  combined_data %>% 
  select(Year, State, funding, workload, total_i_r, indiv_trained, indiv_helped) 


people_data <- 
  rbind(people_data, territory_data)

people_sum <-
  people_data %>% 
  group_by(Year) %>% 
  summarise(funding = sum(funding),
            workload = sum(workload),
            total_i_r = sum(total_i_r),
            indiv_trained = sum(indiv_trained),
            indiv_helped = sum(indiv_helped)
            )

people_data<-
  people_data%>%
  slice(-9)

#write.csv(people_data, 'impact per hundred dollar.csv')
#write.csv(people_sum, 'people impacted nationally.csv')

post_2014 <-
  people_data %>% 
  filter(Year > 2014) %>% 
  filter(State != 'California')

cor.test( lapply(log(people_data$total_i_r)), post_2014$workload )

#someone is very different from other states
ggplot(post_2014) +
  aes(x = log(total_i_r),
      y = log(workload)) +
  geom_point()

#no surprises its california
ggplot(post_2014) +
  aes(x = total_i_r,
      y = workload,
      label = State) +
  geom_text()

#let's try a log scale for both axis
ggplot(post_2014) +
  aes(x = log(total_i_r),
      y = log(workload),
      label = State) +
  geom_text()

