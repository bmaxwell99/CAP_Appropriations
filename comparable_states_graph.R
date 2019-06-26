#sets WD to git repo folder, change to run on different computer
setwd("C:/Users/dark_/Documents/NDRN/CAP_Appropriations")
source('workload_calc.R')


#plots against dollars recieved per population
Bar.Dol_per_pop.State.Funding_cat.Single_year <- function(df, year){
  
  ggplot(data = df %>% filter(Year == year)) + #filters the data by passed year
    aes(x = reorder(State, -fund_per_pop)  , 
        y = fund_per_pop ,  
        fill =  fund_cat
        ) +
    geom_col() + 
    coord_flip() + 
    labs(x = "States", 
       y = paste0("Dollars of Funding Received per Population in ", year, "**") ,
       fill = paste0("*Approx Funding Received in ", year),
       title = paste0('Visual Exploration for ',year,' Comparable States(CAP)'),
       subtitle = 'The States that are similar in terms of bar length(calculated by number of dollars received per person)
        may be comparable to each other, despite the differences in funding they recieve(shown via color) ',
       caption = '*Funding amounts drawn from RSA CAP appropriations
       **Estimated Population drawn from Census Bureau'
    ) +
    scale_fill_hue(h = c(0, 270)) 
}
Bar.Dol_per_pop.State.Funding_cat.Single_year(workload_w_state, 2018)

#Groups states by color, based on their ratio of funding per population being within a 1/2 stndev of
Bar.Dol_per_pop.State.Bucketed_dol_per_pop.Single_year <- function(df, year){
  ggplot(data = df %>% filter(Year == year)) +
    aes(x = reorder(State, -fund_per_pop)  , 
        y = fund_per_pop , 
        fill =  compare_cat_18
    ) +
    geom_col() + 
    coord_flip() + 
    labs(x = "States", 
         y = paste0("Dollars of Funding Received per Population in ", year, "**") ,
         fill = 'Comparable Buckets',
         title = paste0('Visual Exploration for ',year,' Comparable States(CAP)'),
         subtitle = 'The States that are similar in terms of bar length(calculated by number of dollars received per person)
       may be comparable to each other, and are grouped together by half a standard deviation(shown with color) ',
         caption = '*Funding amounts drawn from RSA CAP appropriations
       and estimated Population drawn from Census Bureau'
    ) +
    scale_fill_brewer(palette = 'Dark2') 
}

Bar.Dol_per_pop.State.Bucketed_dol_per_pop.Single_year(workload_w_state, 2018)
#boss didnt like that. He asked that I do two hierarchys of groups, those that recieved the minimum, and those 
#that recieved more than that. Those that recieved more than that would be subdivided by population into similar
#group sizes of 2-4

#A text graph showing how funding compares to population 
Text.pop.Funding.Grey.dynamic <- function(df){
  plot <-
    ggplot(data = df )+
    aes(x = pop,
        y = funding,
        label = State) +
    geom_text() +
    theme_bw() +
    #TODO format these into rounded intervals that are easier to read but scale with the graph
    scale_x_continuous(breaks = df %>% sequence_sd(pop)) +
    scale_y_continuous(breaks = df %>% sequence_sd(funding) ,
                       labels = df %>% sequence_sd(funding) %>% format_money2())
    return(plot)    
  
}

#TODO review R markdown to tell a story
#filters the data to just the states that recieved more than the minimum in 2018
non_min_states <-
  workload_w_state %>% 
  filter(Year == 2018,
         !min_funding)

#Graph shows that Cali, Texas, Florida and New York are very different from the remaining States
Text.pop.Funding.Grey.dynamic(non_min_states)

#lets get a look at the spread without those
non_min_states_small <- 
  non_min_states %>% 
    filter(State != 'Texas' & State != 'Florida' & State != 'California' & State != 'New York')

#graph shows a much less uneven spread in this smaller category
Text.pop.Funding.Grey.dynamic(non_min_states_small)

add_color_by_cat_pop <- function(plot){
  plot +
    aes(color = factor(pop_cat) )+
    scale_color_brewer(palette = 'Dark2') +
    #TODO change the legend labelling to be more informative(ex 'Group 1, Group 2' etc)
    labs(color = 'Group number 
         by Population')
}

#shows distribution by color bucketing
add_color_by_cat_pop( Text.pop.Funding.Grey.dynamic(non_min_states_small))
#shows distribution by color bucketing
add_color_by_cat_pop( Text.pop.Funding.Grey.dynamic(non_min_states))

#TODO make a visual argument for grouping for the minimums
min_states <-
  workload_w_state %>% 
  filter(Year == 2018,
         min_funding) 

#TODO add better labeling
#TODO break apart into two dif graphs
ggplot(data = min_states)+
  aes(x = pop,
      y = reorder(State, pop)) +
  geom_point() +
  scale_x_continuous(breaks = min_states %>% sequence_sd(pop)) + 
  aes(color = factor(pop_cat)) +
  scale_color_brewer(palette = 'Dark2')

#graphs name of the state against population and area
Text.Pop.Area.Grey.Dynamic <- function(df){
  #TODO add labeling
  plot <- ggplot(data = df)+
            aes(x = pop,
                y = Area,
                label = State)+
            geom_text()
  
  return(plot)
}

#adds a color to the name of the state, by funding category
add_color_by_fund_cat <- function(plot){
  #TODO add labeling
  plot <-
    plot +
    aes(color = fund_cat) +
    scale_color_hue(c(0,270))
  
  return(plot)
}

##reinforces the fact that funding is determined mostly by population
add_color_by_fund_cat(Text.Pop.Area.Grey.Dynamic(workload_w_state %>% 
                             filter(Year ==2018, 
                                    State != 'Alaska' & State !=  'Texas'& State != 'California')) )

                      