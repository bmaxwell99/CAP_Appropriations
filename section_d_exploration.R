setwd("C:/Users/dark_/Documents/NDRN/CAP_Appropriations")

source('workload_calc.R')

#exploration of analysis for d_1, d_2, d_7


ggplot(data = combined_data)+
  aes(x = factor(Year),
      y = d_1_per_work
  ) +
  geom_boxplot() 

summary(lm(d_7_per_work ~ Year, combined_data))

hist(df_regress$d_2_per_work)

ggplot(data = combined_data)+
  aes(x = factor(Year),
      y = f_close_per_work,
      label = State
  ) +
  geom_text()

ggplot(data = combined_data) +
  aes(x = f_close_per_work,
      y = d_7_per_work,
      label = State,
      color = factor(Year)) +
  geom_text()

hist(df_regress$f_close_per_work)

?cut_interval

ggplot(data = combined_data, aes(x = f_close_per_work,
                                 color = pop_cat)) +
  geom_dotplot(dotsize = .4, binwidth = max(combined_data$f_close_per_work)/30) 


geom_dotplot()
