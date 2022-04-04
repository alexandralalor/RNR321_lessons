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
tiger <- read_csv("data_raw/tiger_rattlesnakes.csv")

# Take a look at the dataframe and explore it a bit. 
# 2a. How many sites were surveyed? 
# 2b. How many times was each individual site surveyed?

# 3. Like we did in the tortoise example, create a list of times for this dataset.
# HINT: Be sure to make each one the correct length based on your answer to Q2
survey_time <- data.frame(time_1 = rep('1',40),
                          time_2 = rep('2',40),
                          time_3 = rep('3',40),
                          time_4 = rep('4',40),
                          time_5 = rep('5',40))
survey_time <- list(time = survey_time)

# 4. Create an unmarked dataframe 
tigerUMF <- unmarkedFrameOccu(tiger, obsCovs = survey_time)
head(tigerUMF) # check out the data

## OCCUPANCY MODELS ##

# 5. Fit an occupancy model where occupancy and detection probability are 
# constant. 
tiger_psidot_pdot <- occu(~1 ~1, data = tigerUMF)

# 6. Fit an occupancy model where occupancy stays constant but detection prob
# varies with survey.
tiger_psidot_ptime <- occu(~time-1 ~1, data = tigerUMF)

## MODEL SELECTION ##

# Use AIC to determine which model is the best. 
tiger_models <- fitList(tiger_psidot_pdot  = tiger_psidot_pdot, 
                        tiger_psidot_ptime = tiger_psidot_ptime)
modSel(tiger_models)

# 7. Which model should we choose? Why? (2pts)
# We should choose psidot_pdot because it has the lowest AIC score (and delta = 0)

# 8. Report the estimates of occupancy and detection from our chosen model. 
# (Report means type them out)
plogis(coef(tiger_psidot_pdot))  # estimates of psi and p
# Occupancy: 0.42
# Detection: 0.29

# 9. Calculate the 95% confidence intervals for occupancy. Report them here.
plogis(confint(tiger_psidot_pdot, type = "state", level = 0.95))
# Lower CI: 0.25
# Upper CI: 0.62

# 10. Calculate the 95% confidence intervals for detection probability and report.
plogis(confint(tiger_psidot_pdot, type = "det", level = 0.95))
# Lower CI: 0.18
# Upper CI: 0.44

# 11. Calculate the "naive" occupancy and report it. Is it higher or lower than 
# the model estimate? In 1-2 sentences, explain why. (2pts total)
detects <- apply(tiger, 1, sum)     # Calculate the sum for every row in a vector
nSites <- length(detects)           # Total number of sites surveyed
no_detections <- sum(detects == 0)  # Number of sites with no detections
detections <- sum(detects > 0)      # Number of sites with at least one detection

tiger_naive_occ <- detections/nSites
# Naive occupancy: 0.35
# Underestimate (lower) because we are not taking into account individuals which 
# are present but we did not detect.

# WESTERN DIAMONDBACK RATTLESNAKES -----------------------------------------####

# Do the same as above for the Western diamondback rattlesnakes.

## DATA ##

# 12. Load in the tiger rattlesnake occupancy data. Save it as the object "diamond"
diamond <- read_csv("data_raw/diamondback_rattlesnakes.csv")

# 13. Create an unmarked dataframe for the Western diamondback rattlesnakes
diamondUMF <- unmarkedFrameOccu(diamond, obsCovs = survey_time)
head(diamondUMF) # check out the data

## OCCUPANCY MODELS ##

# 14. Fit an occupancy model where occupancy and detection probability are 
# constant. 
diamond_psidot_pdot <- occu(~1 ~1, data = diamondUMF)

# 15. Fit an occupancy model where occupancy stays constant but detection prob
# varies with survey.
diamond_psidot_ptime <- occu(~time-1 ~1, data = diamondUMF)

## MODEL SELECTION ##

# Use AIC to determine which model is the best. 
diamond_models <- fitList(diamond_psidot_pdot  = diamond_psidot_pdot, 
                          diamond_psidot_ptime = diamond_psidot_ptime)
modSel(diamond_models)

# 16. Which model should we choose? 
# We should choose psidot_pdot

# 17. Report the estimates of occupancy and detection from our chosen model. 
# (Report means type them out)
plogis(coef(diamond_psidot_pdot))  # estimates of psi and p
# Occupancy: 0.78
# Detection: 0.28

# 18. Calculate the 95% confidence intervals for occupancy. Report them here.
plogis(confint(diamond_psidot_pdot, type = "state", level = 0.95))
# Lower CI: 0.48
# Upper CI: 0.93

# 19. Calculate the 95% confidence intervals for detection probability and report.
plogis(confint(diamond_psidot_pdot, type = "det", level = 0.95))
# Lower CI: 0.19
# Upper CI: 0.38

# 20. Calculate the "naive" occupancy and report it. Is it higher or lower than 
# the model estimate? 
detects <- apply(diamond, 1, sum)   # Calculate the sum for every row in a vector
nSites <- length(detects)           # Total number of sites surveyed
no_detections <- sum(detects == 0)  # Number of sites with no detections
detections <- sum(detects > 0)      # Number of sites with at least one detection

diamond_naive_occ <- detections/nSites
# Naive occupancy: 0.65

# COMPARE THE SPECIES ------------------------------------------------------####

# 21. Write a few sentences comparing the tiger rattlesnake and diamondback
# rattlesnake populations that we surveyed. What are the differences in 
# estimated occupancy? What about detection probability? What might that tell us
# about each species (take a guess here!)? (3pts)




#---------------------------------------------------------------------------####
# Submit this script with your answers to the assignment dropbox on D2L.
#---------------------------------------------------------------------------####