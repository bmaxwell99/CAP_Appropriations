

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


#TODO elaborate on the linear exploration between workload and pop
df <- workload_w_state
#at .84 there is a strong argument that population is a predictor workload. This is an assumption we have made so far
cor(df$pop, df$workload)

#ajusted R squared is .70(pop explains 70% of workload) and p value is well within statistically significant range
summary(lm(df$pop~ df$workload , data = df)
)

#visual exploration

lm_pop_workload_explore <- function(data){
plot <- ggplot(data = df) +
  aes(x = pop,
      y = workload) +
  geom_point()
return(plot)
}

#visually verifies that there appears to be a correlation
lm_pop_workload_explore(df)

#visually displays the lm model 
lm_pop_workload_explore(df) + geom_smooth(method = lm) 
#the variation increases dramatically at later stages

#Brief look at whoe the strong outliers are
lm_pop_workload_explore(df) + geom_smooth(method = lm) + aes(label = State) + geom_text()
#Might imply that New York, Florida, Cali and Texas should be extruded from the exploration by sheer weight of outlier

#what happens if I add color based on pop category, it should line up into approximate vertical buckets
lm_pop_workload_explore(df) + aes(color = pop_cat)


#refine to remove New York, Florida, Cali and Texas and rerun the graph
df <- workload_w_state %>% filter(pop_cat != 7)
lm_pop_workload_explore(df)

#how does that affect regression
lm_pop_workload_explore(df) + geom_smooth()

#decreased the correlation a fair bit
cor(df$pop, df$workload)

#the adjusted r squared is down to .29, which is a crazy difference. Although the p value is still very low 
#this still might imply that population no longer becomes a strong predictor of workload in less populace states
summary(lm(df$pop~ df$workload , data = df))

#taking a step back lets look at changes in workload over time
df <- workload_w_state 
lm_pop_workload_explore(df) + aes(color = Year)
#there appears to be a trend toward decreasing workload over time

#what does it look like without those big states
df <- workload_w_state %>%  filter(pop_cat != 7)
lm_pop_workload_explore(df) + aes(color = Year)
#maybe there is a decreasing trend of workload over time

#TODO restructure this box plot properly
#further visual argument that the orkload is decreasing over the years
df <- workload_w_state
ggplot(data = df) +
  aes(
      y = workload)+
  geom_boxplot() +
  facet_grid(.~Year)


#Correlation, at -.012, is appropriately negative, but close to zero
cor(df$Year, df$workload)

#Satistically significant argument that Year accounts for very little, at %1, of the variation in workload
summary(lm(df$Year~ df$workload , data = df))

#disregard massive outliers to determine if they might be skewing the result
df <- workload_w_state %>% filter(pop_cat != 7)

#Correlation, at almost -.2, is much larger, but still very small
cor(df$Year, df$workload)

#Even without the more skewing data points, Year only accounts for %3 of the variation in workload
summary(lm(df$Year~ df$workload , data = df))

#Conclusion, workload is trending downwards but not strongly.

#lets look at workload(or number of cases) per 100,000 people per state, 
df <- workload_w_state

#rOrders the point graph in terms of population category. huge spread of information but it looks like lower 
#population cateories either have more spread or do more work per thier population 
ggplot(data = df) +
  aes(x = work_per_pop,
      y = reorder(State, pop_cat))+
  geom_point() 

#argues that the variation is indeed more spread in lower population categories, and trends downwards in recent years
ggplot(data = df) +
  aes(x = work_per_pop,
      y = pop_cat)+
  geom_point() +
  aes(color = Year) 






