# RNR 321: Ecological Surveys and Sampling #
# Module 4, Assignment 1 #
# Detection and Occupancy #

# PREP ---------------------------------------------------------------------####

# For this assignment, we have 2 different datasets: one for tiger rattlesnakes
# and one for Western diamondback rattlesnakes. 

# Following the structure of the tortoise exercise we did in lab, create models
# for each species of rattlesnake. Our ultimate goal is to compare the estimated
# occupancy and detection probabilities of our two species.


# LIBRARIES #
# load the packages we want to use
library(tidyverse)
# install.packages("unmarked")
library(unmarked) 

# TIGER RATTLESNAKES -------------------------------------------------------####

## DATA ##

# 1. Load in the tiger rattlesnake occupancy data. Save it as the object "tiger"


# Take a look at the dataframe and explore it a bit. 
# 2a. How many sites were surveyed? 
# 2b. How many times was each individual site surveyed?

# 3. Like we did in the tortoise example, create a list of times for this dataset.
# HINT: Be sure to make each one the correct length based on your answer to Q2


# 4. Create an unmarked dataframe 



## OCCUPANCY MODELS ##

# 5. Fit an occupancy model where occupancy and detection probability are 
# constant. 


# 6. Fit an occupancy model where occupancy stays constant but detection prob
# varies with survey.


## MODEL SELECTION ##

# Use AIC to determine which model is the best. 

# 7. Which model should we choose? Why? (2pts)
# We should choose psidot_pdot because it has the lowest AIC score (and delta = 0)

# 8. Report the estimates of occupancy and detection from our chosen model. 
# (Report means type them out)
# Occupancy: 
# Detection: 

# 9. Calculate the 95% confidence intervals for occupancy. Report them here.
# Lower CI: 
# Upper CI: 

# 10. Calculate the 95% confidence intervals for detection probability and report.
# Lower CI: 
# Upper CI: 

# 11. Calculate the "naive" occupancy and report it. Is it higher or lower than 
# the model estimate? In 1-2 sentences, explain why. (2pts total)



# WESTERN DIAMONDBACK RATTLESNAKES -----------------------------------------####

# Do the same as above for the Western diamondback rattlesnakes.

## DATA ##

# 12. Load in the tiger rattlesnake occupancy data. Save it as the object "diamond"


# 13. Create an unmarked dataframe for the Western diamondback rattlesnakes. 
# Remember to include the "time" covariates.


## OCCUPANCY MODELS ##

# 14. Fit an occupancy model where occupancy and detection probability are 
# constant. 


# 15. Fit an occupancy model where occupancy stays constant but detection prob
# varies with survey.


## MODEL SELECTION ##

# Use AIC to determine which model is the best. 

# 16. Which model should we choose? 
# We should choose psidot_pdot

# 17. Report the estimates of occupancy and detection from our chosen model. 
# (Report means type them out)
# Occupancy:
# Detection:

# 18. Calculate the 95% confidence intervals for occupancy. Report them here.
# Lower CI:
# Upper CI:

# 19. Calculate the 95% confidence intervals for detection probability and report.
# Lower CI:
# Upper CI:

# 20. Calculate the "naive" occupancy and report it. Is it higher or lower than 
# the model estimate? 


# COMPARE THE SPECIES ------------------------------------------------------####

# 21. Write a few sentences comparing the tiger rattlesnake and diamondback
# rattlesnake populations that we surveyed. What are the differences in 
# estimated occupancy? What about detection probability? What might that tell us
# about each species (take a guess here!)? (3pts)




#---------------------------------------------------------------------------####
# Submit this script with your answers to the assignment dropbox on D2L.
#---------------------------------------------------------------------------####