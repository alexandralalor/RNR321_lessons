#tortoise script
#Alexandra Lalor
#allielalor@email.arizona.edu
#First created: 2022-04-04
#Last updated: 2022-04-04

#practice
library(tidyverse)
library(unmarked)
tort_occ <- read.csv("module4/data_raw/tortoise_occupancy.csv")

#create a dataframe with a list of times for this dataset
#no great explaination of why we need to turn the DF into a list...
survey_time <- data.frame(time_1 = rep("1", 20),
                          time_2 = rep("2", 20),
                          time_3 = rep("3", 20),
                          time_4 = rep("4", 20),
                          time_5 = rep("5", 20))
survey_time <- list(time = survey_time)

#unmarked frame. Basically combining both our DFs
tortUMF <- unmarkedFrameOccu(tort_occ, obsCovs = survey_time)
head(tortUMF)

#fit models for occupancy
#model constant p(.) and psi(.)
#use tilda (~) to say "keep constant, don't do anything"
#first argument in occu() is a formula describing covariates of detection and
#occupancy in that order
#p(.) is detection, psi(.) is occupancy
tort_psidot_pdot <- occu(~1 ~1, data = tortUMF)

#vary detection probability with our survey time (p), keep occupancy constant (psi)
tort_psidot_ptime <- occu(~time-1 ~1, data = tortUMF)

#model seclection
tort_models <- fitList(tort_psidot_pdot  = tort_psidot_pdot, 
                          tort_psidot_ptime = tort_psidot_ptime)
modSel(tort_models)

#choose lowest AIC, with delta of 0
#choose tort_psidot_pdot because of low AIC and delta=0



#estimates and 95% confidence intervals

#occupancy estimate
plogis(coef(tort_psidot_pdot))
#61% occupancy for psi, and 52% detection for p

#occupancy estimate, for out other model, just for fun and to see differences
plogis(coef(tort_psidot_ptime))

#confidence intervals
#which parameter do we want the CIs for? type = "state", meaning occupied or not
#At what level? 0.95
#should get upper and lower value
plogis(confint(tort_psidot_pdot, type = "state", level = 0.95))
#which parameter do we want the CIs for? type = "det", meaning detected or not
#At what level? 0.95
plogis(confint(tort_psidot_pdot, type = "det", level = 0.95))


#naive occupancy i.e. if we didn't use the model
#create vector, go through tort_occ datafram, go by row (1), get sum of row
detect <- apply(tort_occ, 1, sum)
#how many sites? i.e. how many rows, but using detect vector 
nsites <- length(detect)
#count number within detects that are equal to 0
#number of sites with no detections
no_detections <- sum(detect == 0)
#number of sites with at least one detection, greater than 0
detections <- sum(detect > 0)  
#calculate naive occupancy
tiger_naive_occ <- detections/nsites
#we get 60% occupancy, which is remarkably close to what we got with the model
#but this doesn't often happen... usually we under estimate with naive occupancy



