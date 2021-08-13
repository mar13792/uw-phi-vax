# # Author: Francisco Rios 
# # Purpose: prep missed opportunities analyses using DHS data
# # Date: Last modified August 13, 2021

#####
# to-do
#  i.KID_AGECAT 
#  i.edu literate i.WOM_AGECAT2 i.TOTAL_CHILDREN_BORN_CAT i.marital2 i.wom_occ2 i.exp_quintile assets hhsize urban female_head
# - calcualte age in years
# - exponentiate the logistic regression to get an OR


####################################################
# read in dataset
####################################################
dt <- read_rds(outputFile2d)

####################################################
# 1. calculate new variables
####################################################
dt$has_health_card_bin <- as.character(dt$has_health_card)

dt <- dt %>% mutate(has_health_card_bin=recode(has_health_card_bin,
              `0`=0,                                 
              `1`=1,
              `2`=0,
              `3`=1,
              `4`=0,
              `5`=1,
              `6`=1,
              `7`=1,
              `8`=0))

dt$female <- as.character(dt$sex_of_child)

dt <- dt %>% mutate(female = recode(female,
                                    `2`=TRUE,
                                    `1`=FALSE))

####################################################
# 2: descriptive characteristics of children with and without card
####################################################
table1(~ factor(female) | has_health_card_bin, data=dt)

####################################################
# 3: regression to investigate who has card 
####################################################
mylogit <- glm(has_health_card_bin ~ female, data = dt, family = "binomial")

####################################################
# 4. coverage cascade
####################################################

####################################################
# 5. compare missed opportunities- chisquare
####################################################

####################################################
# 6. Regression to compare missed opportunities
####################################################

####################################################
# 7. hazards
####################################################