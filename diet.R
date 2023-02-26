# Dependencies
library(tidyverse)
library(ggplot2)
library(patchwork)

# Load data
diet <- read.table("resources/diet.txt", header = TRUE)

diet <- diet %>% 
  mutate(weight_loss = preweight - weight6weeks)


box_plot <- diet %>%
  ggplot(aes(x = factor(diet), y = weight_loss)) +
  geom_boxplot() +
  labs(x = "Diet", y = "Weight lost", title = "Weight lost with respect to diet")

median_plot <- diet %>% 
  group_by(diet) %>% 
  summarize(median_weight_loss = median(weight_loss)) %>%
  ggplot(aes(x=factor(diet), y=median_weight_loss)) +
  geom_bar(stat="identity", fill="blue") +
  labs(x="Diet", y ="Median weight loss", title="Median weight loss with respect to diet") 

median_plot + box_plot

fit <- aov(weight_loss ~ diet, data = diet)
summary(fit)

fit <- aov(weight_loss ~ diet * gender, data = diet)
summary(fit)
