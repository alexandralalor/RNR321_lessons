# Analyze survey data for desert tortoises from Saguaro National Park

library(tidyverse)
library(unmarked) # Load the unmarked package

# If that command throws an error, you likely need to install the "unmarked" package 
#  on the computer you're using with the command below

# install.packages("unmarked")


# Organize Tortoise Data ---------------------------------------------------####

tort_occ <- read_csv("data_raw/tortoise_occupancy.csv")

# Establish the time of each survey occasion for each plot

survey_time <- data.frame(time_1 = rep('1', 20),
                          time_2 = rep('2', 20),
                          time_3 = rep('3', 20),
                          time_4 = rep('4', 20),
                          time_5 = rep('5', 20))
survey_time <- list(time = survey_time)

# Create data frame required for unmarked -- ignore the warning

tortUMF <- unmarkedFrameOccu(tort_occ, obsCovs = survey_time)
head(tortUMF) # check out the data


# Fit Models for Occupancy -------------------------------------------------####

# unmarked function is "occu"
# Format is:  factors that affect detection probability follow the first ~
#             factors that affect occupancy follow the second ~

# Model psi(.)p(.) -- psi constant across sites, det prob constant over time

psidot_pdot  <- occu(~1 ~1, data = tortUMF)

# Model psi(.)p(t) -- psi constant across sites, det prob varies with time (survey occasion)

psidot_ptime <- occu(~time-1 ~1, data = tortUMF)	


# Model Selection ----------------------------------------------------------####

# Assemble models into a list to compare their AIC values

models <- fitList(psidot_pdot  = psidot_pdot, 
                  psidot_ptime = psidot_ptime)
modSel(models)


# Estimates & 95% CIs for Occupancy (psi) and Detection Probability (p) ----####

### For model:  psi(.)p(.)

plogis(coef(psidot_pdot))  # estimates of psi and p

plogis(confint(psidot_pdot, type = "state", level = 0.95)) # 95% CI for psi
plogis(confint(psidot_pdot, type = "det",   level = 0.95)) # 95% CI for p


### For model:  psi(.)p(t)

plogis(coef(psidot.ptime))

plogis(confint(psidot_ptime, type = "state", level = 0.95))
plogis(confint(psidot_ptime, type = "det",   level = 0.95))


### Naive Occupancy
detects <- apply(tort_occ, 1, sum)     # Calculate the sum for every row in a vector
nSites <- length(detects)           # Total number of sites surveyed
no_detections <- sum(detects == 0)  # Number of sites with no detections
detections <- sum(detects > 0)      # Number of sites with at least one detection

(naive_occ <- detections/nSites)

