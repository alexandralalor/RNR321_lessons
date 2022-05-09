#####################################################
#     RNR 321: Ecological Surveys and Sampling      #
#              Module 5, Assignment 2               #
#  Exploring Other Distance-Based Sampling Methods  #
#####################################################

# As usual, having the PowerPoint slides open as you work through this exercise
# will be helpful! As will the example from lab, Dispersion.R

# Load Libraries #

library(tidyverse)

# POINT-CENTERED QUARTER (PCQ) ---------------------------------------------####

# 1. Read in the data. This is data for saguaros > 2m tall.
saguaro <- read_csv("module5/data_raw/PCQ_Data.csv")

# 2. Check out the data frame. How many points (k) were sampled? How many distances
# did we measure at each point (n)? (2 pts)
head(saguaro)
k <- 8
n <- 4

# 3. Take a look at the equation for estimating density using the PCQ method. The 
# numerator of the equation can be calculated in one step! Calculate the numerator.
numerator <- k * n

# The denominator is a bit more complicated, but we can work through it together.
# Hold on with me while we do some data wrangling to make it easier to calculate!

# Right now, the distance measurements are in 4 different columns. Let's first
# use a function from the tidyverse to put all the distance measurements into 
# one column.
saguaro_long <- pivot_longer(data = saguaro,
                             cols = NE:NW,            # which columns to include in our "pivot"
                             names_to = "Quarter",    # what to name the column with the previous column names
                             values_to = "Distance")  # what to name the new column with the values that were in the previous columns
head(saguaro_long)  # all the distance values are now in one column, 
                    # and there is a new column that has the name of the quarter, which were previously column names

# Point Quarter Distance
# <dbl> <chr>      <dbl>
#   1     1 NE           7  
#   2     1 SE           7.5
#   3     1 SW           9.3
#   4     1 NW          13.9
#   5     2 NE           2.4
#   6     2 SE           5.8


# 4. Now, we need to square all of these values. Fill in the correct column name
# below and run the code.
squared_distances <- saguaro_long %>% 
  mutate(Distance_squared = Distance ^ 2) 
head(squared_distances)

# 5. We now sum the squared distances. Fill in the correct column name below and run the code.
sum_sqrd_distances <- squared_distances %>% 
  summarise(Sum_Squared_Distances = sum(Distance_squared))
head(sum_sqrd_distances)

# 6. We can calculate the value of the denominator by multiple the sum of the squared 
# distances (sum_sqrd_distances) by pi (you can type "pi" into the equation)
denominator <- sum_sqrd_distances * pi

# 7. Finally, we can calculate our density estimate (# saguaros per square meter)
density <- numerator / denominator
print(density)

# 8. Convert the density estimate to # saguaros per hectare (ha = 100m x 100m AKA 10,000 m2)
density_ha <- density * 10000

# 9. Based on the target of these surveys (saguaros > 2m) and what you know about
# the assumptions of PCQ methods, do you think this estimate of density is 
# a reliable estimate? Why or why not? (2 pts)



# DISPERSION (SPATIAL PATTERNS) --------------------------------------------####

## Plot-Based Estimates ##

## The script that we used in lab today (Dispersion.R) will be very helpful! ##

# Below are some data from Pacific silver fir (Abies amabilis) seedlings were 
# counted on 104 10 x 10 cm plots.

seedlings <- c(0, 0, 1, 0, 5, 7, 2, 1, 0, 0, 0, 1, 0, 4, 6, 3, 0, 0, 1, 1, 0, 0, 0, 2, 5, 8, 
               0, 2, 4, 6, 5, 2, 1, 0, 0, 1, 2, 4, 7, 3, 0, 0, 1, 0, 1, 0, 3, 6, 5, 2, 0, 0, 
               0, 1, 3, 8, 4, 1, 0, 0, 1, 1, 0, 5, 6, 3, 1, 0, 0, 0, 1, 0, 1, 4, 7, 4, 2, 0, 
               1, 2, 0, 3, 4, 6, 4, 0, 1, 0, 1, 0, 3, 6, 7, 5, 2, 0, 1, 0, 0, 0, 2, 3, 7, 4) 
table(seedlings)

# 10. Create a histogram of the data (because this data is in vector form rather than
# a data frame, we do not need to specify a column like we have in the past).
hist(seedlings)

# 11. From what we've talked about in class, hypothesize which pattern of dispersion
# these data are likely to show: clumped, random, or uniform. Why? (2 pts)
# Hint: think about the mean and the variance.



# 12. Use the mean function to calculate the mean number of seedlings per plots.
mean_seedlings <- mean(seedlings)

# 13. Use var() to calculate the variance around the mean
var_seedlings <- var(seedlings)

# 14. Calculate the Index of Dispersion (ID) to characterize the dispersion pattern.
# Which pattern does the ID value indicate? (2 pts)
ID <- var_seedlings / mean_seedlings
ID

# We aren't done yet, though! We need to test the significance of our findings.

# 15. First, create the chi.sq test statistic.
df <- (length(seedlings) - 1) 
chi.sq <- ID * df
chi.sq

# 16. Based on the fact that our n is > 100, we will want to use the normal distribution.
# That means we first need to adjust our test statistic.
d <- sqrt(2 * chi.sq) - sqrt((2 * df) - 1)

# 17. Run the correct significance test. 
pnorm(d, lower.tail = FALSE)

# 18. Is the pattern significantly different from random? Remember, the way we've 
# coded this means our interpretation differs a bit from the PowerPoint slides.
# If the P-value is smaller than 0.10, then we reject the null hypothesis and
# say that the pattern differs significantly from random.


# 19. Think critically about what the variance of sample data means. In your own
# words, explain why you would expect the variance for a species with a uniform
# distribution to be smaller than the variance for a species with a clumped 
# distribution? (2 pts)


#---------------------------------------------------------------------------####
# Submit this script with your answers to the assignment dropbox on D2L.
#---------------------------------------------------------------------------####