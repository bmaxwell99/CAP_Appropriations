setwd("C:/Users/dark_/Documents/NDRN/CAP_Appropriations/ethnicity.race")
source('ethnicity_pop_estimates.R')


#analysis
black_client_exp <-
  ethnicity_data %>% 
  select(State, Year, black_per_pop, black_clients) %>% 
  mutate(underserved = black_per_pop > black_clients) %>% 
  filter(underserved)

#replaces a likely erroneous data point where in 2011 DC served more black clients than they had clients

ehtnicity_summary <- average_ethnicity(ethnicity_data)
#african american client ratio
ggplot(data = ethnicity_summary)+
  aes(x = dif_black)+
  geom_dotplot(binwidth = sd(ethnicity_summary$dif_black)/2, dotsize = .6)+
  labs(x = 'Difference Between Ratio of Client Ethnicity and 
       Ratio of State Ethnicity',
       title = 'How Well Are P&As Serving African American Populations') +
  theme(axis.text.y = element_blank())

#asian client ratio
ggplot(data = ethnicity_summary)+
  aes(x = dif_asian)+
  geom_dotplot(binwidth = sd(ethnicity_summary$dif_asian)/2, dotsize = .6)+
  labs(x = 'Difference Between Ratio of Client Ethnicity and 
       Ratio of State Ethnicity',
       title = 'How Well Are P&As Serving Asian Populations') +
  theme(axis.text.y = element_blank())

#Alaskan Native client ratio
ggplot(data = ethnicity_summary)+
  aes(x = dif_american_native)+
  geom_dotplot(binwidth = sd(ethnicity_summary$dif_american_native)/2, dotsize = .6)+
  labs(x = 'Difference Between Ratio of Client Ethnicity and 
       Ratio of State Ethnicity',
       title = 'How Well Are P&As Serving Native American Populations') +
  theme(axis.text.y = element_blank())

#Pacific Islander client ratio
ggplot(data = ethnicity_summary)+
  aes(x = dif_pacific_islander)+
  geom_dotplot(binwidth = sd(ethnicity_summary$dif_pacific_islander)/2, dotsize = .6)+
  labs(x = 'Difference Between Ratio of Client Ethnicity and 
       Ratio of State Ethnicity',
       title = 'How Well Are P&As Serving Pacific Islander Populations') +
  theme(axis.text.y = element_blank())


#bucketing function
sequence_sd_ethnic <- function(df, col) {
  col <- enquo(col)
  r <- seq(min(unlist(df %>% select(!!col))), max(unlist(df %>% select(!!col))), sd(unlist(df %>% select(!!col)))/2 )
  r <- paste0('< %', round(r*100, 0))
  return(r)
}



#creates the buckets
ethnicity_summary <-
  ethnicity_summary %>% 
  mutate(black_interval = cut_interval(dif_black, 
                                       length = sd(ethnicity_summary$dif_black)/2,
                                       labels = sequence_sd_ethnic(ethnicity_summary, dif_black)
  )) %>% 
  mutate(american_native_interval = cut_interval(dif_american_native, 
                                                 n = 11,
                                                 labels = sequence_sd_ethnic(ethnicity_summary, dif_american_native)
  )) %>% 
  mutate(asian_interval = cut_interval(dif_asian, 
                                       n = 13,
                                       labels = sequence_sd_ethnic(ethnicity_summary, dif_asian)
  )) %>% 
  mutate(pacific_islander_interval = cut_interval(dif_pacific_islander, 
                                                  n = 10,
                                                  labels = sequence_sd_ethnic(ethnicity_summary, dif_pacific_islander)
  )) 



ethnicity_summary <- inner_join(ethnicity_summary, combined_data %>% filter(Year == 2018) %>% select(State, fund_cat), by = 'State')

test <- inner_join(combined_data %>% filter(Year == 2018) %>% select(State, network_status), ethnicity_summary )

bar_plot_ethnicity <- 
  ggplot(data = ethnicity_summary)+
  aes(fill = fund_cat)+
  geom_bar()+
  labs(x = 'Difference Between Ratio of Client Ethnicity and 
       Ratio of State Ethnicity',
       fill = 'Grouped by 
       Funding Category',
       y= 'number of States') +
  theme(axis.text.y = element_blank()) +
  scale_fill_hue(h = c(0, 270)) 

bar_plot_ethnicity + 
  aes(x = black_interval) + 
  labs(title = 'How Well Are P&As Serving African American Populations')

bar_plot_ethnicity +
  aes(x = american_native_interval) + 
  labs(title = 'How Well Are P&As Serving Native American Populations')

bar_plot_ethnicity +
  aes(x = asian_intr)
labs(title = 'How Well Are P&As Serving Asian Populations')

