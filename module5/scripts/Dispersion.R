#############################################
# Dispersion Patterns: Plot-based Estimates #
#   Bob Steidl; modified by EKB, May 2022   #
#############################################

### Larvae of carpenter bees counted in 180 inflorescences of a soap-tree yucca

# Counts reported as frequencies (114 zeroes, 25 ones, 15 twos...)

x  <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)     # no. larvae per inflorescence
Fx <- c(114, 25, 15, 10, 6, 5, 2, 1, 1, 0, 1) # no. inflorescences with x larvae

# frequencies are convenient to report but a pain to work with, so convert to 180 observations
larvae <- rep(x, Fx)
larvae

# let's look at the data
table(larvae)

# we cab create a histogram two different ways:
hist(larvae)
barplot(table(larvae), xlab='No. larver per flower (x)', ylab='Frequency (Fx)')

# calculate the Index of Dispersion
ID <- var(larvae)/mean(larvae)
ID    # ID > 1, so tends towards clumped (if < 1, then tends towards uniform)

# Calculate test statistic and df to evaluate whether there is sufficient evidence to reject 
# the null as false and claim the dispersion pattern is clumped rather than random

  df <- (length(larvae) - 1) # length() provides no. objects in a vector, so sample size (n)
  
  chi.sq <- ID * df
  chi.sq

### VERY IMPORTANT ###
# NOTE!! Interpreting the p-values for both the chi-square and normal distributions
# differ from what is on the PowerPoint slides, so be alert! This is mostly 
# because we are indicating whether we want the lower tail or the upper
# tail of the distribution. 
# I don't need you to understand why it is different--
# what you should know is that in EITHER case, if the p value is LESS than 0.10,
# then we can say it is significant and accept the alternative hypothesis (either
# clumped or uniform)

### If n < 100, use chi-square distribution

 # P-value for clumped (when ID > 1 and we want upper tail)
   pchisq(chi.sq, df, lower.tail = FALSE)

 # P-value for uniform (when ID < 1 and we want lower tail)
   pchisq(chi.sq, df, lower.tail = TRUE)

### If n >= 100, compute d and use normal distribution

  # adjust the test statistic 
  d <- sqrt(2 * chi.sq) - sqrt((2 * df) - 1)
   
  # P-value for clumped (when ID > 1 and we want upper tail)
    pnorm(d, lower.tail = FALSE)
    
  # P-value for uniform (when ID < 1 and we want lower tail)
    pnorm(d, lower.tail = TRUE)
