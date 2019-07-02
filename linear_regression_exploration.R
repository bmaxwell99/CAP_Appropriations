setwd("C:/Users/dark_/Documents/NDRN/CAP_Appropriations")
source('workload_calc.R')


df_regress <- combined_data


##SECTION RELATED TO WORKLOAD REGRESSION
#adds regression variables to the data
workload_regression <-
  df_regress %>% 
  group_by(State) %>% 
  summarise(p_value_work_per_pop = regression_calc(Year, work_per_pop, 8),
            lm_work_per_pop = regression_calc(Year,work_per_pop,  2),
            p_value_pop = regression_calc(Year, pop, 8),
            lm_pop = regression_calc(Year, pop, 2),
            p_value_work = regression_calc(Year, workload, 8),
            lm_work = regression_calc(Year,workload,  2)
  )  


##implies a downward trend
ggplot(data = df_regress)+
  aes(x = factor(Year),
      y = work_per_pop
  ) +
  geom_boxplot() 

#shows a definite downward trend
ggplot(data = df_regress)+
  aes(x = Year,
      y = work_per_pop
  ) +
  geom_point()+
  geom_smooth(method = lm)



#national trend
summary(lm(df_regress$work_per_pop ~ df_regress$Year))

#graphs just the individual states regression, first filters by signifcant trends and joins with the original data 
test <- inner_join(df_regress, workload_regression %>% filter(p_value_work_per_pop < .05 ), by = 'State')

ggplot() +
  aes(x = Year,
      y = work_per_pop)+ 
  geom_smooth(data = test, aes(group = State), method = lm, se = FALSE) + aes(color = factor(pop_cat)) + 
  scale_color_hue(h = c(0,180))

#TODO make an rmarkdown presentation showing states that are low


##SECTION RELATED TO PCA 
#NON OF THIS WORKS
pca_prep <- df_regress %>%  select(State, Year,  work_per_pop, c_5_per_work, c_6_per_work, c_7_per_work, d_1_per_work, 
                       d_2_per_work, d_7_per_work, d_10_per_work, d_11_per_work)

pca <- prcomp(pca_prep %>% select(-Year, -State, -work_per_pop) %>% t(), scale = TRUE)

plot(pca$x[,1], pca$x[,2])

pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)

barplot(pca.var.per, main = 'variation in percent of variation', xlab = 'Principle component', ylab = 'Percent Variation')

pca_data <- 
  data_frame(State = rownames(pca$x),
             X = pca$x[,1],
             Y = pca$x[,2])


ggplot(data=pca_data, aes(x=X, y=Y, label=State)) +
  geom_text() +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("My PCA Graph")



