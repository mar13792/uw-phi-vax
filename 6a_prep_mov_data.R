# # Author: Francisco Rios 
# # Purpose: prep missed opportunities analyses using DHS data
# # Date: Last modified August 13, 2021

#####
# to-do: 
# recreate each of the analyses for each of the two time points in Liberia

# - exponentiate the logistic regression to get an OR


####################################################
# read in dataset
####################################################
dt <- read_rds(outputFile2d)

####################################################
# 1. calculate new variables
####################################################

# has health card binary
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

dt$has_health_card_bin <- factor(dt$has_health_card_bin,
                                 levels = c(0,1),
                                 labels = c("No", "Yes"))

# female sex
dt$female <- as.character(dt$sex_of_child)
dt <- dt %>% mutate(female = recode(female,
                                    `2`=TRUE,
                                    `1`=FALSE))

#kid age category
dt$kid_agecat <- round(time_length(difftime(dt$intv_date, dt$dob), "years"), digits = 0)
dt$kid_agecat <- as.factor(dt$kid_agecat)

# mother's education
dt$edu <- as.character(dt$v106)
dt <- dt %>% mutate(edu = recode(edu,
                                    `0`=0,
                                    `1`=1,
                                    `2`= 2,
                                    `3`=2))
dt$edu <- factor(dt$edu,
                 levels = c(0,1,2),
                 labels = c("no education", "primary", "secondary or higher"))

# mother's literacy levels
dt$literate <- as.character(dt$v155)
dt <- dt %>% mutate(literate = recode(literate,
                                               `0`=0,
                                               `1`=1,
                                               `2`=1,
                                               `3`=9,
                                               `4`=9))

dt <- dt %>% replace_with_na(replace = list(literate = 9))

dt$literate <- factor(dt$literate,
                      levels = c(0,1),
                      labels = c("iliterate", "literate"))

# mother's age category
dt$wom_agecat <- dt$v012

dt <- dt %>%
  mutate(wom_agecat=case_when(
    wom_agecat %in% 15:19 ~ "1",
    wom_agecat %in% 20:34 ~ "2",
    wom_agecat %in% 35:49 ~ "3"))

dt$wom_agecat <- factor(dt$wom_agecat,
                        levels = c(1,2,3),
                        labels = c("15-19", "20-34", "35-49"))

# parity
dt$total_children_born <- dt$v201 

dt <- dt %>% 
  mutate(total_children_born=case_when(
    total_children_born %in% 1 ~ "1",
    total_children_born %in% 2:3 ~ "2",
    total_children_born %in% 4:5 ~ "3",
    total_children_born %in% 6:20 ~ "4"
  ))

dt$total_children_born <- factor(dt$total_children_born,
                                 levels = c(1,2,3,4),
                                 labels = c("1 child", "2-3 children", "4-5 children", "6+ children"))

# marital status
dt$marital <- as.character(dt$v501)

dt <- dt %>% mutate(marital = recode(marital,
                                      `0`=1,
                                      `1`=2,
                                      `2`=3,
                                      `3`=4,
                                      `4`=4,
                                      `5`=4))

dt$marital<- factor(dt$marital,
                    levels = c(1,2,3,4),
                    labels = c("single", "married", "union", "divorced, seperated, widowed, or other"))

# mother's employment
dt$wom_occ <- dt$v716

dt <- dt %>% 
  mutate(wom_occ=case_when(
    wom_occ %in% 0 ~ "1",
    wom_occ %in% 1:97 ~ "2"
  ))

dt$wom_occ <- factor(dt$wom_occ,
                     levels = c(1,2),
                     labels = c("not employed in last 12 months", "employed"))

# assets
dt$assets <- as.factor(dt$v190a)

# average household size
dt$hhsize <- dt$v136

# urban
dt$urban <- abs(dt$v025-2)
  
dt$urban <-factor(dt$urban,
                      levels = c(0,1),
                      labels = c("rural household", "urban household"))

# sex of head of household
dt$female_head <- dt$v151

dt$female_head <- factor(dt$female_head,
                         levels = c(1,2),
                         labels = c('male', 'female'))

##############################
# Additional calculations: 
#   1. Days at risk per vaccine per child
#   2. Yes/no there was a missed opportunity per vaccine per child 
#   3. Potential days at risk per vaccine per child (based on missed opportunity status) 
# 
# Assumptions (others are tagged with "Assumption:" throughout the code): 
#   1. Children without health cards are NOT immunized. (We are not considering recall information)
# 
# Definitions: 
#   1. Days at risk = number of days between when the child was due for the vaccine and when they actually received it

#####  Assign the MINIMUM and MAXIMUM ages each vaccine should be given 
#  Assumption: Vaccine is due at month X. We will accept vaccines given at month X-0.5 through X+1.5. 
# Example: Measles due at 9 mos is acceptable 8.5-10.5 months of age.
dt$mea1_age_due_min = 9*30.4 - 15.2
dt$mea1_age_due_max = 10*30.4 +  15.2

##### do Rotavirus (3 series) as well as DPT

# calculate the age of child in days
dt$kid_age <- time_length(difftime(dt$intv_date, dt$dob), "days")

# calculate the age at which child was vaccinated with measles-containing vaccine
dt$age_at_mea1 <- time_length(difftime(dt$mea1, dt$dob), "days")

# Add indicator:
# What is the difference in days between appropriate timing and the actual age at vaccination? 
# How to interpret: 
# (1) a value of <0 for mmr_age_minus_min means that they were vaccinated too early -- all of the days lived after the end of the mmr window will be considered days at risk; 
# (2) a value of >0 for mmr_age_minus_min and <0 for mmr_age_minus_max means that they were vaccinated in the proper time window and they will accrue 0 days at risk; 
# (3) a value of >0 for mmr_age_minus_max means that they lived days at risk after appropriate vaccination 

dt$mea1_age_minus_min <- dt$age_at_mea1 - dt$mea1_age_due_min
dt$mea1_age_minus_max <- dt$age_at_mea1 - dt$mea1_age_due_max

# Identify whether children were (1) vaccinated too early,
# (2) vaccinated within the appropriate interval
# (3) vaccinated too late, or
# (4) never vaccinated

dt <- dt %>% mutate(early_mea1 = case_when(mea1_age_minus_min<0 & kid_age>=mea1_age_due_max ~ 1, # vaccinated too early
                                           TRUE ~ 0),
                    mea1_within_interval = case_when(mea1_age_minus_min>=0 & mea1_age_minus_max<=0 & kid_age>=mea1_age_due_max ~ 1, # vaccinated within appropriate time frame
                                                     TRUE ~ 0),
                    mea1_late = case_when(mea1_age_minus_max>0 & !is.na(mea1_age_minus_max) & kid_age>=mea1_age_due_max ~ 1, # vaccinated but too late
                                          TRUE ~ 0),
                    
                    never_got_mea1 = case_when(!is.na(age_at_mea1) & has_health_card_bin=="Yes" ~ 0,
                                               is.na(age_at_mea1) & has_health_card_bin=="Yes" ~ 1), #  never received vaccine (1 is never received, 0 is did receive)
                                               
                    # variable that transposes the age at which the vaccination was given (for those vaccinated early, late, or within the correct time frame)
                    mea1_age_at_counted_vac = case_when(mea1_age_minus_min<0 & kid_age>=mea1_age_due_max ~ age_at_mea1,
                                                        mea1_age_minus_min>=0 & mea1_age_minus_max<=0 & kid_age>=mea1_age_due_max ~ age_at_mea1,
                                                        mea1_age_minus_max>0 & !is.na(mea1_age_minus_max) & kid_age>=mea1_age_due_max ~ age_at_mea1),
                    
                    # variable that indicates how much time each child was at risk (for those that were vaccinated early or late)
                    mea1_days_at_risk = case_when(mea1_age_minus_min<0 & kid_age>=mea1_age_due_max ~ kid_age - mea1_age_due_max,
                                                  mea1_age_minus_min>=0 & mea1_age_minus_max<=0 & kid_age>=mea1_age_due_max ~ 0,
                                                  mea1_age_minus_max>0 & !is.na(mea1_age_minus_max) & kid_age>=mea1_age_due_max ~ mea1_age_minus_max,
                                                  never_got_mea1==1 & kid_age>=mea1_age_due_max ~ kid_age - mea1_age_due_max),
                    
                    # variable indicating when the oldest vaccination date took place
                    oldest_visit = pmax(bcg, dpt1, pol1, dpt2, pol2, dpt3, pent1, pent2, pent3, pneu1, pneu2, pneu3, rota1, rota2, rota3, poln, hepb1, hepb2, hepb3, hib1, hib2, hib3,
                                       na.rm=TRUE),
                    # calculate the age at the oldest visit
                    age_at_oldest_visit = time_length(difftime(dt$oldest_visit, dt$dob), "days"),
                    
                    # variable that indicates missed opportunities
                    mea1_missed_opportunity = case_when(never_got_mea1==1 & age_at_oldest_visit>mea1_age_due_min & kid_age>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_oldest_visit) ~ 1,
                                                        TRUE ~ 0),
                    
                    # We also want to know how old the child was at the earliest eligible visit 
                    
                    # The child got measles late, but there was a visit for another vaccine in between the MMR due age and actual age at MMR 
                    
                    # Now compute a compliance with MO
                    mea1_compliant_card_mop = case_when(mea1_date_recorded%in%c(0) ~0,
                                                        mea1_date_recorded%in%c(1,3) ~1,
                                                        mea1_missed_opportunity==1 ~1),
                    
                    # compute days of risk if opportunity was not missed
                    mea1_days_at_risk_mop = case_when(mea1_missed_opportunity==0 ~ mea1_days_at_risk,
                                                      )
                    )
                    

# // 7a. MMR
# {
#   gen mmr_days_at_risk_mop = .
#   // If the kid didn't have a missed opportunity, then the days at risk stays the same
# 	replace mmr_days_at_risk_mop = mmr_days_at_risk if mmr_missed_opportunity==0
# 	
# // But, if they did have MO, then we need to calculate days at risk with our new number
# 	gen mmr_mop_age_minus_max = earliest_visit_btwn_mmr - `mmr_age_due_max' if earliest_visit_btwn_mmr!=. & mmr_missed_opportunity==1
#   replace mmr_days_at_risk_mop = mmr_mop_age_minus_max if mmr_mop_age_minus_max>0 & age_in_days>=`mmr_age_due_max' & age_in_days!=. & mmr_missed_opportunity==1
# 	
# 	hist mmr_days_at_risk_mop 
# 	* graph box mmr_days_at_risk_mop if mmr_days_at_risk_mop>=0, over(AGE_MO)

##### calculate the missed opportunities and what potential coverage could be

# Case 1: The child never got vaccinated for Measles, but had other vaccination visits during or after the measles due age
# dt <- dt %>% mutate(mea1_missed_oppourtinity)
# replace mmr_missed_opportunity = 1 if never_got_mmr==1 & oldest_visit>`mmr_age_due_min' & age_in_days>=`mmr_age_due_max' & age_in_days!=. & mmr_days_at_risk!=. & oldest_visit!=.

# save output
saveRDS(dt, file=outputFile6a)

# print final statement
print("Step 6a: Prepping DHS data now complete.")

