# 4A
```{r}
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
```

# 4B
```{r}
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
```
#This code creates a data frame npk_avg that contains the average yield per block for each treatment combination (nitrogen applied or not applied) using the dplyr package. The code then uses ggplot2 to create a bar plot of the average yield per block, with separate bars for the two nitrogen treatments, and with blocks on the x-axis. The position = "dodge" argument in geom_col() ensures that the bars for the two nitrogen treatments are displayed side-by-side for each block.

#By including the factor block in the analysis, we account for any variability in yield that may be due to differences between blocks (e.g., soil type, sun exposure, etc.). This allows us to focus on the effect of nitrogen specifically, rather than confounding it with block effects. Additionally, the randomized treatment assignments ensure that any differences in yield between the two nitrogen treatments are not due to systematic differences in the placement of nitrogen-treated plots within blocks.

# 4C
```{r}
anova = aov(yield ~ block + N, data = npk)
summary(anova)
```
#           Df Sum Sq Mean Sq F value Pr(>F)   
#block        5  343.3   68.66   3.395 0.0262 * 
#N            1  189.3  189.28   9.360 0.0071 **
#Residuals   17  343.8   20.22                  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## We see significantly low p-values (0.0262, 0.0071), indicating a significant difference between variable yield and the N and block factors
## It wasn't sensible to include the block since there are the same amount of additives in each block which got randomly distributed in the plots
## The Friedman test couldn't be applied here since every treatment must be able to be applied to every block, it was not a complete block design.

# 4D
```{r}
lm = lm(yield ~., data = npk)
step_model <- stepAIC(lm, direction = "both")
```
#Start:  AIC=73.28
#yield ~ block + N + P + K

#        Df Sum of Sq    RSS    AIC
#- P      1      8.40 248.59 72.106
#<none>               240.18 73.281
#- K      1     95.20 335.39 79.294
#- block  5    343.30 583.48 84.583
#- N      1    189.28 429.47 85.228

#Step:  AIC=72.11
#yield ~ block + N + K

#        Df Sum of Sq    RSS    AIC
#<none>               248.59 72.106
#+ P      1      8.40 240.18 73.281
#- K      1     95.20 343.79 77.887
#- block  5    343.30 591.88 82.926
#- N      1    189.28 437.87 83.693

```{r}
lm2 = lm(formula = yield ~ block + N + K + block*N, data = npk)
lm3 = lm(formula = yield ~ block + N + block*K, data = npk)
```
# Chosen best model is: lm(formula = yield ~ block + N + K, data = npk)
