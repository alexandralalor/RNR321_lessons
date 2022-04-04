# -----------------------------------------------------------------------------#
# Module 3, Assignment 2
# Stratified Random Sampling -- Grasshoppers!
#
# Answer the question with comments OR
# Insert the appropriate code below each question
# -----------------------------------------------------------------------------#

# Load the tidyverse #

library(tidyverse)

# Optimal Allocation of Sample Units #-----------------------------------------#

# Let's say we have a population (sampling frame) with 1000 sample units. We can
# sample 100 of those units. There are 3 sampling strata, all with different 
# characteristics. How do we decide how many sampling units to sample from each
# stratum?

# 1. Create an object, N, with the total number of possible sample units.
N <- 1000

# Stratum A has 200 sampling units, Stratum B has 500 sampling units, and 
# Stratum C has 300 sampling units. 

# 2. Add the correct values to the lines of code below and run each line.
N_A <- 200
N_B <- 500
N_C <- 300

# Due to a pilot study done last year, we also know the variation (standard dev.) 
# for each stratum.


sd_A <- 9
sd_B <- 4
sd_C <- 7

# We also have an estimate of the time-cost (hours) it takes to survey each unit.
cost_A <- 1.5
cost_B <- 2
cost_C <- 4

# Use the optimal allocation equation to calculate how many sample units each 
# stratum should have. Let's do this incrementally.

# 3. Start by calculating Ni * Si / square root of Ci for each stratum. 
opt_allo_A <- N_A * sd_A / sqrt(cost_A)
opt_allo_B <- N_B * sd_B / sqrt(cost_B)
opt_allo_C <- N_C * sd_C / sqrt(cost_C)

# 4. Calculate the denominator of the equation by adding all of them together.
opt_allo_total <- opt_allo_A + opt_allo_B + opt_allo_C

# 5. Calculate how many sampling units we should optimally sample in each stratum.
# Name these objects n_A, n_B, and n_C. Use the round() function to round to 
# the nearest whole number.
n <- 100

n_A <- n * (opt_allo_A / opt_allo_total) # 37
n_A <- round(n_A)
# OR #
n_B <- round(n * (opt_allo_B / opt_allo_total)) # 36
n_C <- round(n * (opt_allo_C / opt_allo_total)) # 27

# 6. If we did not have the pilot study telling us about the variation of the 
# populations in each strata, we could have allocated very differently:
# n_A = 24, n_B = 53, and n_C = 23. Why is this?



# Stratified Random Sampling Calculations #------------------------------------#

## Getting the Data ##

# Hooray! We have gone out and sampled the number of grasshoppers per sample unit, 
# allocating our sample units optimally (including variation in population).

# Now we want to calculate our population parameters:
# mean grasshoppers per sampling unit (y bar)
# total grasshoppers in the population (tau hat)

# Download the "grasshoppers.csv" file from D2L.
# Place it in your "data_raw" folder in your Module 3 RProject.
# Read in the grasshoppers CSV file using the read_csv() function, and 
# call it grasshoppers
grasshoppers <- read_csv("module3/data_raw/grasshoppers.csv")

# Explore the dataset.
str(grasshoppers); head(grasshoppers); glimpse(grasshoppers); view(grasshoppers)

# 7. Run the following line of code. 
# (Hint: do these numbers match your values from question X? If not, you 
# probably want to revisit your calculations up in the previous section!)
grasshoppers %>% 
  group_by(stratum) %>% 
  count(stratum)
# What is it telling us? 


# 8. For now, the easiest way to do our calculations will be to have three different
# data frames, one for each stratum. Use the filter function and create the
# following three dataframes:
stratumA <- grasshoppers %>% filter(stratum == "A")
stratumB <- grasshoppers %>% filter(stratum == "B")
stratumC <- grasshoppers %>% filter(stratum == "C")


## Calculations ##

# NOTE: Open up your script for Module 3, Assignment 1--it's about to come in 
# handy! So will the lecture slides from lectures 14 and 15.


## Calculations for the means of each stratum ##

# 9. Use the code from there to calculate the following for EACH stratum
# (these are all for the mean number of grasshoppers per sample unit; 8 points 
# total, 2 each):
# a. sample mean per stratum (y_bar)
mean_A <- mean(stratumA$abund)
mean_B <- mean(stratumB$abund)
mean_C <- mean(stratumC$abund)

# b. sample variance per stratum (var)
var_A <- var(stratumA$abund)
var_B <- var(stratumB$abund)
var_C <- var(stratumC$abund)

# c. variance of the estimate of the mean per stratum (var_y_bar)
pop_correction_A <- (N_A-n_A)/N_A
pop_correction_B <- (N_B-n_B)/N_B
pop_correction_C <- (N_C-n_C)/N_C

var_y_bar_A <- pop_correction_A * (var_A / n_A)
var_y_bar_B <- pop_correction_B * (var_B / n_B)
var_y_bar_C <- pop_correction_C * (var_C / n_C)

# d. standard error per stratum (SE_y_bar) (Remember the finite population correction factor!)
SE_y_bar_A <- sqrt(var_y_bar_A)
SE_y_bar_B <- sqrt(var_y_bar_B)
SE_y_bar_C <- sqrt(var_y_bar_C)

## Calculations for the totals of each stratum ##

# 12. Calculate the following for EACH stratum (these are all related to the 
# total number of grasshoppers in each stratum)(6 points total, 2 each):
# a. total abundance per stratum (tau_hat)
tau_hat_A <- N_A * mean_A
tau_hat_B <- N_B * mean_B
tau_hat_C <- N_C * mean_C

# b. variance of abundance per stratum (var_tau_hat)
var_tau_hat_A <- N_A * (N_A-n_A) * (var_A/n_A)
var_tau_hat_B <- N_B * (N_B-n_B) * (var_B/n_B)
var_tau_hat_C <- N_C * (N_C-n_C) * (var_C/n_C)

# c. standard error of abundance per stratum (SE_tau_hat)
SE_tau_hat_A <- sqrt(var_tau_hat_A)
SE_tau_hat_B <- sqrt(var_tau_hat_B)
SE_tau_hat_C <- sqrt(var_tau_hat_C)

## Calculating the OVERALL population mean ##

# 13. Calculate the non-weighted overall population mean.
pop_mean_overall <- (tau_hat_A + tau_hat_B + tau_hat_C)/N

# 14. Calculate the weighted overall population mean.
pop_mean_weighted <- ((N_A * mean_A) + (N_B * mean_B) + (N_C * mean_C)) / N

# 15. How do the two means differ? Why?

