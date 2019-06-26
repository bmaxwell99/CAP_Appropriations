setwd("C:/Users/dark_/Documents/NDRN/CAP_Appropriations")


source('Clean_join_data.r')
source('comparable_states_graph.r')
row.names(bucketed_data) <- c(unlist(bucketed_data[1])) 

pca <- prcomp(bucketed_data %>% select(pop_18,dollar_2018) %>% filter(dollar_2018 > 140000), scale = TRUE)

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

## get the name of the top 10 measurements (genes) that contribute
## most to pc1.
loading_scores <- pca$rotation[,1]
gene_scores <- abs(loading_scores) ## get the magnitudes
gene_score_ranked <- sort(gene_scores, decreasing=TRUE)
top_10_genes <- names(gene_score_ranked[1:3])
top_10_genes

pca$rotation[top_10_genes,1]

pca_data

sd(pca_data$X)
density(pca_data$X)

mean(pca_data$X)


play_day <- inner_join(pca_data, data_2018)

ggplot(data = play_day ) +
  aes(x = reorder(State, -play_day$X)  , 
      y = play_day$X ,  
      fill =  play_day$funding_cat
  ) +
  geom_col() + 
  coord_flip() + 
  labs(x = "States", 
       y = "PCA 1*",
       fill = '*Approx Funding Received in 2018',
       title = 'Visual Exploration for 2018 Comparable States(CAP)',
       subtitle = 'Principle Component Analysis using funding and population estimates',
       caption = '*PCA 1 Accounted for %98.7 percent of the variation
       **Estimated Population drawn from Census Bureau'
  ) +
  scale_fill_hue(h = c(0, 270)) 

