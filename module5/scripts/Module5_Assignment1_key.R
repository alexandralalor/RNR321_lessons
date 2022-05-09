###########################################
# RNR 321 Ecological Surveys and Sampling #
#         Module 5: Assignment 1          #
#           Distance Sampling             #
###########################################

# RESOURCES #
# In addition to the slides and practice code, the following might be helpful:
# Information on unmarked: http://cran.r-project.org/web/packages/unmarked/index.html
# User's manual: https://cran.r-project.org/web/packages/unmarked/unmarked.pdf
# Using unmarked with distance data: https://cran.r-project.org/web/packages/unmarked/vignettes/distsamp.pdf

# LIBRARIES #

library(tidyverse)
library(unmarked)

# SAGUAROS -----------------------------------------------------------------####

# 1. Read in the saguaro data
saguaro <- read.csv("module5/data_raw/saguaro.csv")

# 2. Make a dataframe with transect length (i.e., survey effort)
transect_length <- saguaro %>% 
  group_by(group) %>% 
  summarise(length = mean(length))

# 3. Plot the distribution of detection distances
hist(saguaro$distance) 

# 4. Set truncation distance to 20m to eliminate extreme observations
trunc <- 20

# 5. Set 'cut points' for distance bins; for saguaros, let's do every 4 meters
distance_bins <- seq(0, trunc, by = 4) 

# 6. Use unmarked function 'formatDistData' to count the number of detections in 
# each distance bin for each transect. Ignore the warning message.
sag_data <- formatDistData(saguaro, 
                           distCol = "distance", 
                           transectNameCol = "group", 
                           dist.breaks = distance_bins)

# 7. Assemble data into the format required by unmarked, called an 'unmarked frame'
UMF <- unmarkedFrameDS(y = as.matrix(sag_data), 
                       survey = "line",
                       tlength = transect_length$length, 
                       dist.breaks = distance_bins,
                       unitsIn = "m")
UMF

# 8. Check the distribution of detection distances to be used for analysis
hist(UMF)

## Fit Models for the Detection Function ##

# 9. Use the distsamp function to create 4 mdoels: one of half-normal (HN), hazard (HR),
# one for uniform (Unif), and one for negative exponential (Exp). (2pts)
HN   <- distsamp(~1 ~1, UMF, keyfun = "halfnorm", output = "density", unitsOut = "ha")
HR   <- distsamp(~1 ~1, UMF, keyfun = "hazard", output = "density", unitsOut = "ha")
Unif <- distsamp(~1 ~1, UMF, keyfun = "uniform", output = "density", unitsOut = "ha")
Exp  <- distsamp(~1 ~1, UMF, keyfun = "exp", output = "density", unitsOut = "ha")
  

## Model Selection based on AIC ##

# 10. Assemble models into a list to compare their AIC values. Identify the top model
# from this list (HN or HR or Unif or Exp), which you'll use below.
models <- fitList('Half Normal' = HN, 
                  'Hazard Rate' = HR,
                  'Uniform'     = Unif,
                  'Exponential' = Exp)
modSel(models)

#             nPars    AIC delta   AICwt cumltvWt
# Half Normal     2 213.10  0.00 5.3e-01     0.53
# Hazard Rate     3 213.37  0.27 4.7e-01     1.00
# Exponential     2 230.34 17.24 9.6e-05     1.00
# Uniform         1 242.62 29.53 2.1e-07     1.00


# 11. Which model should we use? Why?
# Half-normal, lowest AIC value

## Density Estimate and Confidence Intervals ##

# 12. Use the backTransform function to calculate the density estimate.
backTransform(HN, type="state") # Density estimate (no./ha)

# Estimate   SE LinComb (Intercept)
# 49.5     5.66     3.9           1

# 13. Calculate the confidence intervals for the density estimate.
exp(confint(HN, type="state"))  # CI for density

#             0.025    0.975
# lam(Int) 39.57627 61.96885
  
# OCOTILLOS ----------------------------------------------------------------####

# Now, let's do the same thing for ocotillos. 

# 14. Read in the ocotillo data and make a dataframe with the transect lengths.
# Remember to use read.csv this time (instead of read_csv)! (2pts)
ocotillo <- read.csv("module5/data_raw/ocotillo.csv")

transect_length_oco <- ocotillo %>% 
  group_by(group) %>% 
  summarise(length = mean(length))

# 15. Plot the distribution of detection distances
hist(ocotillo$distance) 

# 16. This time around, set the truncation distance to Set truncation distance 
# to 25m and set cut points at every 5 meters.
trunc_oco <- 25
distance_bins_oco <- seq(0, trunc_oco, by = 5) 

# 17. Use unmarked function 'formatDistData' to count the number of detection in 
# each distance bin for each transect. Ignore the warning message. Remember to 
# use the correct data and distance bins.
oco_data <- formatDistData(ocotillo, 
                           distCol = "distance", 
                           transectNameCol = "group", 
                           dist.breaks = distance_bins_oco)

# 18. Assemble data into the format required by unmarked, called an 'unmarked frame.'
# Remember to refer to the correct data frames and values
UMF_oco <- unmarkedFrameDS(y = as.matrix(oco_data), 
                           survey = "line",
                           tlength = transect_length_oco$length, 
                           dist.breaks = distance_bins_oco,
                           unitsIn = "m")
UMF_oco

## Fit Models for the Detection Function ##

# 19. Fit the four models to the ocotillo data. Again, be sure you're using the 
# correct dataframe. (2pts)
HN_oco   <- distsamp(~1 ~1, UMF_oco, keyfun = "halfnorm", output = "density", unitsOut = "ha")
HR_oco <- distsamp(~1 ~1, UMF_oco, keyfun = "hazard", output = "density", unitsOut = "ha")
Unif_oco <- distsamp(~1 ~1, UMF_oco, keyfun = "uniform", output = "density", unitsOut = "ha")
Exp_oco  <- distsamp(~1 ~1, UMF_oco, keyfun = "exp", output = "density", unitsOut = "ha")


## Model Selection based on AIC ##

# 20. Assemble models into a list to compare their AIC values. 
models_oco <- fitList('Half Normal' = HN_oco, 
                      "Hazard Rate" = HR_oco,
                      'Uniform'     = Unif_oco,
                      'Exponential' = Exp_oco)
modSel(models_oco)

#             nPars    AIC  delta   AICwt cumltvWt
# Half Normal     2 284.10   0.00 8.0e-01     0.80
# Hazard Rate     3 286.89   2.79 2.0e-01     1.00
# Exponential     2 431.82 147.72 6.7e-33     1.00
# Uniform         1 431.82 147.72 6.7e-33     1.00

# Let's go ahead and use the Half-Normal model.

# 21. Get the density estimate and confidence intervals for it.
backTransform(HN_oco, type="state") # Density estimate (no./ha) 

# Estimate  SE LinComb (Intercept)
# 118      8.3    4.77           1


exp(confint(HN_oco, type="state"))  # CI for density

#             0.025    0.975
# lam(Int) 103.0058 135.6364

# COMPARE ------------------------------------------------------------------####

# 22. Which has the higher density per hectare? Saguaros or ocotillos? 
#ocotillos

#---------------------------------------------------------------------------####
# Submit this script with your answers to the assignment dropbox on D2L.
#---------------------------------------------------------------------------####