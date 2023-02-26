#A)
# Load the MASS package to access the npk dataset
library(MASS)

# Create a vector of plot labels
plot_labels <- rep(1:24, each = 3)

# Randomly permute the plot labels
set.seed(123) # for reproducibility
plot_labels <- sample(plot_labels)

# Assign treatments to each plot based on the randomized plot labels
treatments <- rep(c("N", "P", "K"), length.out = 72)
treatments <- treatments[plot_labels]

# Create a data frame with the randomized plot labels and treatments
npk_rand <- data.frame(plot = plot_labels, treatment = treatments)

# Use reshape() function from the tidyr package to create a wide format
# data frame with one row per plot and columns for each treatment
library(tidyr)
npk_rand_wide <- reshape(npk_rand, direction = "wide", idvar = "plot", timevar = "treatment")

# View the randomized treatment assignments
npk_rand_wide

#B)
# Calculate the average yield per block for each treatment
library(dplyr)
npk_avg <- npk %>% 
  group_by(block, N) %>% 
  summarize(avg_yield = mean(yield))

# Make a plot of the average yield per block for each treatment
library(ggplot2)
ggplot(npk_avg, aes(x = factor(block), y = avg_yield, fill = factor(N))) +
  geom_col(position = "dodge") +
  labs(x = "Block", y = "Average yield (pounds per plot)", fill = "Nitrogen") +
  scale_fill_discrete(name = "Nitrogen\napplied", labels = c("No", "Yes"))

#This code creates a data frame npk_avg that contains the average yield per block for each treatment combination (nitrogen applied or not applied) using the dplyr package. The code then uses ggplot2 to create a bar plot of the average yield per block, with separate bars for the two nitrogen treatments, and with blocks on the x-axis. The position = "dodge" argument in geom_col() ensures that the bars for the two nitrogen treatments are displayed side-by-side for each block.

#By including the factor block in the analysis, we account for any variability in yield that may be due to differences between blocks (e.g., soil type, sun exposure, etc.). This allows us to focus on the effect of nitrogen specifically, rather than confounding it with block effects. Additionally, the randomized treatment assignments ensure that any differences in yield between the two nitrogen treatments are not due to systematic differences in the placement of nitrogen-treated plots within blocks.
