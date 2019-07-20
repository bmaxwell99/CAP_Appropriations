setwd("C:/Users/dark_/Documents/NDRN/CAP_Appropriations")

source('workload_calc.R')


combined_data %>% colnames()

lda_data <- 
  combined_data %>% 
  select(-State, -network_status, -compare_cat_18, -ID, -fund_cat, -min_funding, -work_per_pop, -fund_per_pop,
         -c_5_per_pop, -c_6_per_pop, -c_7_per_pop, -d_1_per_work, -d_2_per_work, -d_7_per_work, -d_10_per_work,
         -d_11_per_work, -c_5_per_work, -c_6_per_work, -c_7_per_work, -f_close, -f_close_per_work, -f_close_per_pop, 
         -workload)

lda_data <- 
  combined_data %>% 
  select(is_PA, work_per_pop, c_5_per_pop, c_6_per_pop, c_7_per_pop, f_close_per_pop,
          d_7_per_pop, d_10_per_pop, d_11_per_pop)

model <-
  lda(formula = Type ~. , data = lda_data)


## get the x,y coordinates for the LDA plot
data.lda.values <- predict(model)

?predict

## create a dataframe that has all the info we need to draw a graph
plot.data <- data.frame(X=data.lda.values$x[,1], Y=data.lda.values$x[,2], is_PA=my.data$is_PA)

head(plot.data)

## draw a graph using ggplot2
p <- ggplot(data=plot.data, aes(x=X, y=Y)) +
  geom_point(aes(color=Species)) +
  theme_bw()


ldahist(data.lda.values$x[,1] , g = data.lda.values$class)


ggplot()

ggcorr(lda_data %>% filter(is_PA), method = c('pairwise.complete.obs', 'kendall'))
cor(combined_data$pop, combined_data$workload)
cor(combined_data$funding, combined_data$workload)



combined_data %>% group_by(is_PA) %>% summarise(cor(workload, d_7_not_favor, method = 'kendall'))
,
                                                cor(funding, work_per_pop))
combined_data

?ggcorr

ggplot(combined_data %>% filter(!is_PA)) +
  aes(x = Year,
      y = f_close_per_work,
      group = Year)+
  geom_boxplot()

ggplot(combined_data %>%  filter(!is_PA & Year ==2013)) +
  aes(x = State,
      y = f_close_per_work) +
  geom_col()

plot(density(filter(combined_data, !is_PA & Year ==2013)$f_close_per_work) )



cor(combined_data$d_10_per_work, combined_data$d_7_per_work)
