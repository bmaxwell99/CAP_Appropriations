#Rhode Island Exploration

ggplot(post_2014 %>%  filter(State == 'Rhode Island')) +
  aes(x = total_i_r,
      y = workload,
      label = State,
      color = factor) +
  geom_text()
