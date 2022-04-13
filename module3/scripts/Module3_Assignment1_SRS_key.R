# -----------------------------------------------------------------------------#
# Module 3, Assignment 1
# Simple Random Sampling -- Rocks!
#
# Answer the question with comments OR
# Insert the appropriate code below each question
# -----------------------------------------------------------------------------#

# Short Answer (1 pt each) ----------------------------------------------------#

# 1. Define the sample standard deviation (not the equation but what stdev tells us).

#The most commonly used measure of spread in a dataset is the standard deviation

# 2. Define the standard error of the estimate of the mean (again, what is tells us).

#Standard error of an estimate: is the standard deviation of the sampling 
#distribution of an estimate, which quantifies the amount of uncertainty in or 
#precision of that estimate

# 3. How do standard deviation and standard error differ? (If you've explained that
# above, reiterate it here.)

#While the standard deviation provides a measure of the spread of the data from 
#the mean, standard error (SE) represents the uncertainty in the calculation of 
#the mean

# 4. What does a confidence interval tell us?

#Range of values within which the true value of a parameter is likely to exist 
#with a fixed probability

# Calculations (1 pt unless otherwise noted) ----------------------------------#

# Get your (or your group's) simple random sampling rock data.
# Save the Google Sheet/Excel file as a .csv file. Be sure to save it to your
# Module 3 RProject's data_raw folder.

# Load the tidyverse. For now, we will only be using this to read in our CSV 
# file.
library(tidyverse)

# 5. Use the read_csv() function from the tidyverse to read in your rocks file.
# Save this data in an object called "rocks."
rocks <- read_csv("module3/data_raw/Module3_Lab1_SRSampling.csv") %>% drop_na()

# 6. Create the object N, which is the number of units in your sampling frame.
N <- 64

# 7. Create the object n, which is your sample size. To do this, use the nrow()
# function (Hint: if you don't remember how to use the nrows function, run ?nrow)
n <- nrow(rocks)



# ESTIMATING THE MEAN ####

# One way we can calculate values is the call the column that we want and then 
# save that value to an object.

# For example, let's say we wanted to calculate the median of our dark stone
# abundance data. We could run the following link of code:
# rocks_median <- median(rocks$abund_dark_stones)

# 8. Create an object called rocks_mean that contains the population mean (AKA 
# the mean number of dark stones).
rocks_mean <- mean(rocks$abund_dark_stones)

# 9. Create objects called rocks_var and rocks_sd which contain the sample
# variance and sample standard deviation, respectively. (1 pt each)
# Hint: use var() and sd()
rocks_var <- var(rocks$abund_dark_stones)
rocks_sd <- sd(rocks$abund_dark_stones)


## Calculating Standard Error for the Mean ##

# R doesn't have a function for calculating the standard error (SE), so let's 
# calculate it ourselves.

# 10. Remember that population correction factor? We use that to modify SE when 
# we know what N is. In this case, we do, so let's create an object called
# pop_correction using that formula: (N-n)/N
# Hint: use the objects we've already created--don't enter any numbers directly
pop_correction <- (N-n)/N

# 11. Using the objects you've already created and the formula (check the lecture
# slides from this module!), create an object called rocks_var_ybar which contains
# the variance of the estimate of the mean.
# (Hint: remember, this is different from the sample variance that you've 
# already calculated, but that sample variance is included...)
rocks_var_ybar <- pop_correction * (rocks_var/n)

# 12. Calculate the SE of the estimate of the mean by taking the square root
# of the rocks_var_ybar value. Call this object rocks_SE.
rocks_SE <- sqrt(rocks_var_ybar)

## Calculating Confidence Intervals for the Mean ## 

# Hooray, we've now calculated the sample mean, sample variance, sample standard
# deviation, and the standard error (among other things). Let's calculate a 95%
# confidence interval for our sample mean.

# 13. Create an object called alpha to represent our alpha value for a 95% 
# confidence interval.
alpha95 <- 0.05

# 14. Create an object called df for degrees of freedome. Use the object n in
# your calculation.
df <- n - 1

# We are going to use a function called qt() to find the t value that we need.
# Assuming you've created the objects "alpha" and "df" above, running this line
# of code will give us our t-value, or critical value, for 95% CI.
t95 <- qt(1-(alpha95)/2, df)

# 15. Using t95 and the standard error that we calculated earlier, create an
# object called half_width which contains the half-width value for the 95% CI.
half_width <- t95 * rocks_SE

# 16. Create an object called upperCI_mean that contains the upper value of the 
# CI for the estimate of the mean.
upperCI_mean <- rocks_mean + half_width

# 17. Create an object called lowerCI_mean that contains the lower value of the 
# CI for the estimate of the mean.
lowerCI_mean <- rocks_mean - half_width

# 18. Report the 95% CI for the estimate of the mean here. What does this range 
# tell us about our data? 




# ESTIMATING THE POPULATION ####

# We've calculated SE and CI for the estimate of the mean above. Now, let's 
# calculate them for the estimate of the population abundance (tau hat) #

# 19. First, let's calculate the total number of rocks in the population. Call
# this rocks_total.
rocks_total <- N * rocks_mean

# 20. Now calculate the variance of the population total, rocks_var_total.
rocks_var_total <- N * (N-n) * (rocks_var/n)

# 21. Calculate the SE of the estimate of the population total, rocks_SE_total.
rocks_SE_total <- sqrt(rocks_var_total)

# 22. Calculate the half-width value for the population total estimate.
# (Hint: the values alpha95 and t95 don't change)
half_width_total <- t95 * rocks_SE_total

# 23. Calculate and report the upper and lower 95% CI values for the total
# population of rocks (2 points)
upperCI_total <- rocks_total + half_width_total
lowerCI_total <- rocks_total - half_width_total


# ---------------------------------------------------------------------------- #
### Save this script as "LastName_RNR321_Module3_Assignment1.R" in your scripts folder.
### Submit this version of this file via D2L. 
# ---------------------------------------------------------------------------- #
