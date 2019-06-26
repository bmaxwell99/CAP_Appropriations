

setwd("C:/Users/dark_/Documents/NDRN/CAP_Appropriations")
source('workload_calc.R')

df <-
  workload_w_state %>% 
  filter(pop_cat == 7)

ggplot(data = df) +
  aes(x = Year,
      y = work_per_pop,
      color = factor(fund_per_pop),
      label = State
      )+
  geom_text() +
  scale_color_hue(h = c(0,270))


#TODO Redo the whole rest of this page


#preps the data for graphing  
workload_w_state_18 <- 
    workload_w_state %>% 
    filter(Year == 2018) %>% 
    arrange(compare_cat_18,work_relative_resources) %>% 
    mutate(.r = row_number())



#groups by compare bucket and seperates into facets
ggplot(data = workload_w_state_18) +
  aes(x = reorder(State, -.r),
      y = work_relative_resources,
      fill = fund_cat) +
  coord_flip() +
  geom_col() +
  facet_grid(.~compare_cat_18) +
  scale_fill_hue(h = c(0,270)) 

#Plots a single category
work_rel_resource_cat <- function(cat){
  ggplot(data = workload_w_state_18 %>% 
                    filter(pop_cat == cat)) +
    aes(x = reorder(State, -.r),
        y = work_relative_resources,
        fill = fund_cat) +
    coord_flip() +
    geom_col() +
    scale_fill_hue(h = c(0,270)) 
  
  
}

work_rel_resource_cat('<.05')  

#Uses the name of the states in a text plot to show that 

workload_relative_funding_graph <-function(df){
  ggplot(data= df)+
    aes(x=Year, 
        y= work_per_dol, 
        label = State, 
        color = fund_per_pop)+
    geom_text() +
    labs(x = "Year", 
                  y = "*Workload Relative Resources",
                  color = '**Ratio of Funding per Capita',
                  title = 'Visual Exploration for State Performance(Workload) Relative to Comparable States',
                  subtitle = 'The State names that are vertically high, worked more cases for every dollar of funding they recieved. 
                      State names that are light in color recieved many dollars for every person in the state. ',
                  caption = '*Workload equal to number of cases worked divided by funding 
                  **Dollars of funding recieved per capita'
        ) + 
    scale_color_gradient()

}

df <- workload_w_state %>% filter(pop_cat == 1)
df <- workload_w_state %>% filter(State == 'Wyoming' | State == 'Vermont')


workload_relative_funding_graph(df)


#text graphs work per dollar with a log scale of population
ggplot(data = workload_w_state %>% filter(Year == 2018, funding < 140000)
       
)+
  aes(x = log(pop),
      y = work_per_dol,
      label = State)+
  geom_text() 

cor(workload_w_state$work_per_dol[1], workload_w_state$pop[1] ) 
lm(workload_w_state$work_per_dol ~ workload_w_state$pop)
summ