# RNR 321: Ecological Surveys and Sampling #
# Module 4, Assignment 2 #
# Estimating Abundance: Capture-Mark-Recapture and Removal #

# First, some general questions to answer ----------------------------------####

# 1. In your own words, explain the difference between occupancy encounter 
# histories and capture-mark-recapture encounter histories (2 pts)

# Both the occupancy and capture-mark-recapture method are ways to estimate detection
# probability with imperfect surveys. Two main differences between their
# encounter histories are 1) capture-mark-recapture encounter histories only
# contain histories for individuals captured at least once while occupancy
# encounter histories can contain histories with no sightings (all zeros) and
# 2) In capture-mark-recapture encounter histories, rows represent individuals,
# but in occupancy encounter histories, rows represent sites. In both, columns
# represent survey/sampling occasion.


# 2. In your own words, explain the different between constant-effort and catch-
# per-unit-effort. Give an example of each (3 pts)

# The difference between constant-effort and catch-per-unit-effort (CPUE) models 
# is in constant-effort models, the sampling effort for each occasion is the same, 
# while in CPUE models, the sampling effort can vary among occasions. An example of a
# constant-effort situation is electrofishing and an example of a CPUE situation
# is trapping rodents on different occasions with different numbers of traps.



# PREP ---------------------------------------------------------------------####

# LIBRARIES #
# load the packages we want to use
library(tidyverse)

## NOTE: It is a good idea to have the slides from lectures 19 and 20 open! ##

# CAPTURE-MARK-RECAPTURE ---------------------------------------------------####

## DATA ##

# We are running a capture-mark-recapture study on pocket mice. To do this, we 
# have set out a 10 x 10 grid of Sherman live traps. We catch 37 desert pocket 
# mice, Chaetodipus penicillatus, during our first night of trapping. To mark 
# them, we use a green Sharpie to put a dot on their belly (and giggle because 
# it now looks like they have a Leprechaun's belly button). The next night, we 
# set out the same number of traps. In the morning, we find that we have caught 
# 42 desert pocket mice, and 30 of them have green dots on their bellies. 


# 3. Create objects from the story above to represent the following:
#     * n1 (# individuals captured (and marked) in the first sample)
#     * n2 (# individuals captured in the second sample (total))
#     * m2 (# individuals in the second sample which were marked)

n1 <- 37
n2 <- 42
m2 <- 30

## ESTIMATE ABUNDANCE and DETECTION PROBABILITY ##

# 4. Use the Lincoln-Peterson estimator to estimate the population total (N_hat)
N_hat <- n1 * n2 / m2
# 51.8

# 5. Remember, we can consider this equation a version of the "canonical
# abundance estimator," meaning we can calculate an estimate of detection
# probability. What is our detection probability estimate for desert pocket mice?
p_hat <- m2 / n1
# 0.810810

# During this same trapping experiment, we also caught 12 Bailey's pocket mice,
# Chaetodipus baileyi, on the first night and marked them. The second night, we
# caught another 12 Bailey's, 4 of which were previously marked.

# 6. Calculate the abundance of Bailey's pocket mice.
n1_Baileys <- 12
n2_Baileys <- 12
m2_Baileys <- 4

N_hat_Baileys <- (n1_Baileys * n2_Baileys) / m2_Baileys
# 36

# 7. Calculate the estimated detection probability of Bailey's pocket mouse.
p_hat_Baileys <- m2_Baileys / n1_Baileys
# 0.333333

# 8. In your own words, briefly explain what the differences in estimated
# detection probabilities means in the context of this example (2pts). 

# The estimated detection probability for desert pocket mice is much greater
# than that of Bailey's pocket mice. This means that the probability of seeing
# a Bailey's pocket mouse during a survey is lower than the probability of
# seeing a desert pocket mouse during a survey. This could be due to site
# characteristics that make it more difficult to spot Bailey's pocket mice
# compared to desert pocket mice.



## MODIFIED ESTIMATORS ##

# 9. As it turns out, the Lincoln-Peterson estimator can be biased with small
# sample sizes. The modified equation to deal with that potential bias is the
# following: (n1+1)*(n2+1) / (m2+1)-1. Calculate the unbiased estimate of N_hat
# for the desert pocket mice.
# NOTE: Don't worry, I won't be testing you on this modified equation! 
N_hat_adj <- (n1 + 1)*(n2 + 1) / (m2 + 1) - 1
# 51.7

# 10. Now calculate the adjusted N_hat for Bailey's pocket mouse.
N_hat_B_adj <- (n1_Baileys + 1)*(n2_Baileys + 1) / (m2_Baileys + 1) - 1
# 32.8

# Note that while the estimate for the abundance of desert pocket mice didn't
# change very much, the abundance estimate for Bailey's pocket mice actually
# changed by a few individuals. This makes sense, given that the sample size
# for Bailey's pocket mouse was much smaller than the desert pocket mouse.

# REMOVAL METHODS ----------------------------------------------------------####

## CONSTANT EFFORT ##

# As we mentioned in class, there are a number of different ways you can do
# removal methods to estimate abundance of populations. One of those ways is
# to use capture-mark-recapture data, ignoring those individuals who have been
# marked already and only focusing on new captures.

# Let's use our desert pocket mouse (Chaetodipus penicillatus) capture-mark-
# recapture data from above but treat it as a removal study. Because we set the
# same number of traps both nights, it is a constant-effort study.

# 11. Using the numbers from the CMR section above, set the following objects:
#     * n1 (# individuals removed in first sample)
#     * n2 (# individuals removed in second sample)
# HINT: Think carefully about what number should be assigned to n2.
n1_removal <- n1 # 37
n2_removal <- n2 - m2 # 42 - 30 = 12

# 12. Estimate the abundance of the population
N_hat_removal <- (n1_removal^2) / (n1_removal - n2_removal)
# 54.76

# Compare this estimate of abundance with that of the Lincoln-Peterson estimator.
# Similar, but not exactly the same! 

# 13. Let's do the same for the Bailey's pocket mouse.
n1_removalB <- n1_Baileys #12
n2_removalB <- n2_Baileys - m2_Baileys #8

N_hat_removalB <- (n1_Baileys^2) / (n1_removalB - n2_removalB)
# 36

## CATCH PER UNIT EFFORT ##

# 14. Read in the catch per unit effort data (CPUE_data.csv). In this dataset, 
# let catch (ni) be the abundance of crab (x1000) and effort (fi) be trap hauls
# (x1000)
CPUE <- read_csv("module4/data_raw/CPUE_data.csv")
  
# 15. Examine the dataset using head() or glimpse()
head(cpue)
glimpse(cpue)

# 16. Use the mutate() function from tidyverse to create CPUE by filling in
# the correct column names in the blanks below.
cpue <- cpue %>% 
  mutate(CPUE = Catch / Effort)

# 17. Calculate the average CPUE using either base R ($) or tidyverse (summarize)
meanCPUE <- mean(cpue$CPUE)
# 1.105

# 18. Calculate the average cumulative catch
meanCumulativeCatch <- mean(cpue$CumulativeCatch)
# 147.583
  
# 19. In order to calculate the slope, we need to run a regression model. We can
# do this with the function lm()
model <- lm(CPUE ~ CumulativeCatch, data = cpue)

# 20. Run summary(model) to print out a summary of the regression model we just
# ran. The value listed under "estimate" and next to "CumulativeCatch" is the
# slope of the best fit line. If we run model$coefficients[2], it will pull out 
# second of the coefficient values in the object "model." Assign this value to k.
k <- model$coefficients[2]
# -0.00527

# 21. We can now calculate our estimate of N1_hat (our x-intercept).
N1_hat <- meanCumulativeCatch - (meanCPUE / k)
# 357

#---------------------------------------------------------------------------####
# Submit this script with your answers to the assignment dropbox on D2L.
#---------------------------------------------------------------------------####