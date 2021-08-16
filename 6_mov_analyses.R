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

dt <- dt %>% mutate(wom_agecat = recode(wom_agecat,
                                        ))

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

##### calculate the acceptable ages at which bcg vaccine can be given 
dt$bcg_age_due_min = 0

##### BCG vaccine is due at birth

# calculate the age of child in days
dt$kid_age <- time_length(difftime(dt$intv_date, dt$dob), "days")

# calculate the age at which child was vaccinated with BCG
dt$age_at_bcg <- time_length(difftime(dt$bcg, dt$dob), "days")

# Add indicator:
# What is the difference in days between appropriate timing and the actual age at vaccination? 
# How to interpret: 
# (1) a value of <0 for mmr_age_minus_min means that they were vaccinated too early -- all of the days lived after the end of the mmr window will be considered days at risk; 
# (2) a value of >0 for mmr_age_minus_min and <0 for mmr_age_minus_max means that they were vaccinated in the proper time window and they will accrue 0 days at risk; 
# (3) a value of >0 for mmr_age_minus_max means that they lived days at risk after appropriate vaccination 



##### Measles is due at 9 months
####################################################
# 2: descriptive characteristics of children with and without card
####################################################
table1(~  female + kid_agecat + edu + literate + wom_agecat + total_children_born + marital + wom_occ + assets + hhsize + urban + female_head | has_health_card_bin, data=dt)

# export table 1 as an image

####################################################
# 3: regression to investigate who has card 
####################################################

# fit a regression to see which variables are associated with having vaccine coverage card
mylogit <- glm(has_health_card_bin ~ female + kid_agecat + edu + literate + wom_agecat + total_children_born + marital + wom_occ + assets + hhsize + urban + female_head, data = dt, family = "binomial")


exp(cbind(OR = coef(mylogit), confint(mylogit)))

# export table 2 as an image

####################################################
# 4. coverage cascade
####################################################
# save data as datatable
dt <- as.data.table(dt)

# calculate the number of children in sample
dt[, .(.N), by = .(v000)]

# calculate how many children has a vaccination card
dt[has_health_card_bin == "Yes",.(total_with_card= .N), by = v000]

# calculate how many children were covered by each vaccine according to recall and card
dt[bcg_date_recorded%in%c(1,2,3),.(received_bcg= .N), by = v000]

# calculate how many children received bcg according to health card only
dt[has_health_card_bin == "Yes" & bcg_date_recorded%in%c(1,3),.(received_bcg= .N), by = v000]

# calculate the 

####################################################
# 5. compare missed opportunities- chisquare
####################################################

####################################################
# 6. Regression to compare missed opportunities
####################################################

####################################################
# 7. hazards
####################################################