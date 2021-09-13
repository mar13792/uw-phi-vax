####################################################
# Part I
# Load prepped dataset for analyses
####################################################
data <- readRDS(outputFile6a)

# subset data to only Nigeria
data <- data %>% filter(v000 %in% c("NG6", "NG7"))

###### prep additional variables for tables/graphs #####

###################
# mother's education
###################

data$edu <- as.character(data$v106)
data <- data %>% mutate(edu = recode(edu,
                                 `0`=0,
                                 `1`=1,
                                 `2`= 2,
                                 `3`=2))
data$edu <- factor(data$edu,
                 levels = c(0,1,2),
                 labels = c("No education", "Primary", "Secondary or higher"))

###################
# mother's literacy levels
###################
data$literate <- as.character(data$v155)

data <- data %>% replace_with_na(replace = list(literate = 9))

data <- data %>% mutate(literate = recode(literate,
                                      `0`=0,
                                      `1`=1,
                                      `2`=1,
                                      `3`=9,
                                      `4`=9))

data$literate <- factor(data$literate,
                      levels = c(0,1),
                      labels = c("Iliterate", "Literate"))

###################
# mother's age category
###################

data$wom_agecat <- data$v012

data <- data %>%
  mutate(wom_agecat=case_when(
    wom_agecat %in% 15:19 ~ "1",
    wom_agecat %in% 20:34 ~ "2",
    wom_agecat %in% 35:49 ~ "3"))

data$wom_agecat <- factor(data$wom_agecat,
                        levels = c(1,2,3),
                        labels = c("15-19", "20-34", "35-49"))

###################
# parity
###################
data$total_children_born <- data$v201 

data <- data %>% 
  mutate(total_children_born=case_when(
    total_children_born %in% 1 ~ "1",
    total_children_born %in% 2:3 ~ "2",
    total_children_born %in% 4:5 ~ "3",
    total_children_born %in% 6:20 ~ "4"
  ))

data$total_children_born <- factor(data$total_children_born,
                                 levels = c(1,2,3,4),
                                 labels = c("1 child", "2-3 children", "4-5 children", "6+ children"))

###################
# marital status
###################
data$marital <- as.character(data$v501)

data <- data %>% mutate(marital = recode(marital,
                                     `0`=1,
                                     `1`=2,
                                     `2`=3,
                                     `3`=4,
                                     `4`=4,
                                     `5`=4))

data$marital<- factor(data$marital,
                    levels = c(1,2,3,4),
                    labels = c("Single", "Married", "Union", "Divorced, seperated, widowed, or other"))

###################
# mother's employment
###################
data$wom_occ <- data$v717

data <- data %>% replace_with_na(replace = list(wom_occ = 99))

data <- data %>% 
  mutate(wom_occ=case_when(
    wom_occ %in% 0 ~ 1,
    wom_occ %in% 1:97 ~ 2
  ))

data$wom_occ <- factor(data$wom_occ,
                     levels = c(1,2),
                     labels = c("Not employed", "Employed"))

###################
# assets
###################

# each survey should have either v190a or v190 for the household assets
data$assets <- ifelse(!is.na(data$v190a), data$v190a, data$v190)

data$assets <- factor(data$assets,
                      levels = c(1,2,3,4,5),
                      labels = c("Quintile 1", "Quintile 2", "Quintile 3","Quintile 4", "Quintile 5"))

###################
# average household size
###################
data$hhsize <- data$v136

###################
# urban
###################
data$urban <- abs(data$v025-2)

data$urban <-factor(data$urban,
                  levels = c(0,1),
                  labels = c("Rural household", "Urban household"))

###################
# sex of head of household
###################
data$female_head <- data$v151

data$female_head <- factor(data$female_head,
                         levels = c(1,2),
                         labels = c('Male', 'Female'))

###################
# asign labels to variable names
###################
data$sex_of_child <- factor(data$sex_of_child,
                          levels=c(1,2),
                          labels=c("Male", "Female"))

###################
#kid age category
###################

data$kid_agecat <- round(time_length(difftime(data$intv_date, data$dob), "years"), digits = 0)
data$kid_agecat <- factor(data$kid_agecat, 
                          levels = c(0,1,2,3),
                          labels = c("0 years", "1 year", "2 years", "3 years"))

# format data structures
data$mea1_missed_opportunity <-factor(data$mea1_missed_opportunity,
                                      levels=c(0,1),
                                      labels=c("No", "Yes"))

###################
# Year of data
###################
data$strata <- data$v000

# format the data structure
data$strata <- factor(data$strata, 
                      levels = c("NG7", "NG6"),
                      labels=c("2018", "2013"))

###################
# DPT missed opportunity
###################
data$dpt_missed_opportunity <-factor(data$dpt_missed_opportunity,
                                      levels=c(0,1),
                                      labels=c("No", "Yes"))

##################
# variable labels
##################

label(data$sex_of_child) <- "Child's sex"
label(data$kid_agecat) <- "Child's age (in years)"
label(data$edu) <- "Mother's education"
label(data$literate) <- "Literacy"
label(data$wom_agecat) <- "Mother's age (in years)"
label(data$total_children_born) <- "Parity"
label(data$marital) <- "Marital status"
label(data$wom_occ) <- "Mother's occupation"
label(data$hhsize) <- "Household size"
label(data$female_head) <-"Sex of head of household"
label(data$urban) <-"Urbanicity"
label(data$mea1_missed_opportunity) <-"Missed measles opportunity"
label(data$assets) <-"Household assets"
label(data$strata) <- "DHS version"

####################################################
# Part Ib. Coverage cascade
####################################################
# calculate the number of children in sample
dt <- as.data.table(data)
dt[, .(.N), by = .(v000)]
# 
# # calculate how many children has a vaccination card
dt[has_health_card_bin == "Yes",.(total_with_card= .N), by = strata]
# 
# # calculate how many children were covered by each vaccine according to recall and card
# data[mea1_date_recorded%in%c(1,2,3),.(received_mea1= .N), by = v000]
# 
# # calculate how many children received mea1 according to health card only
dt[has_health_card_bin == "Yes" & !is.na(age_at_mea1),.(received_mea= .N), by = strata]
dt[has_health_card_bin == "Yes" & !is.na(age_at_dpt1),.(received_dpt= .N), by = strata]

# calculate the totals for DPT vaccines

####################################################
# Part II
# Explore what variables are associated with a missed opportunity
####################################################

# this function will automatically add pvalue from chisquare test or t-test to a table
pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

# make two datasets one for each year in Nigeria
data1 <- data %>% filter(v000=="NG7") # 2018
data2 <- data %>% filter(v000=="NG6") # 2013

table1(~  sex_of_child + kid_agecat + edu + literate + wom_agecat + total_children_born + marital + assets + hhsize + urban + female_head | mea1_missed_opportunity, data=data1, overall=F, extra.col=list(`P-value`=pvalue), topclass="Rtable1-zebra")
table1(~  sex_of_child + kid_agecat + edu + literate + wom_agecat + total_children_born + marital + assets + hhsize + urban + female_head | mea1_missed_opportunity, data=data2, overall=F, extra.col=list(`P-value`=pvalue), topclass="Rtable1-zebra")

# calculate the chi-square for child's age seperately since children that are too young cannot have a missed opportunity yet
tes1 <- chisq.test(table(data1$kid_agecat, data1$mea1_missed_opportunity, exclude = "0 years"))$p.value
format.pval(tes1, digits=3, eps=0.001)

tes2 <- chisq.test(table(data2$kid_agecat, data2$mea1_missed_opportunity, exclude = c("0 years")))$p.value
format.pval(tes2, digits=3, eps=0.001)

####################################################
# repeat descriptive analysis with DPT vaccine
###################################################

table1( ~ sex_of_child + kid_agecat + edu + literate + wom_agecat + total_children_born + marital + assets + hhsize + urban + female_head | dpt_missed_opportunity, data=data1, overall=F, extra.col=list(`P-value`=pvalue), topclass="Rtable1-zebra")
table1(~  sex_of_child + kid_agecat + edu + literate + wom_agecat + total_children_born + marital + assets + hhsize + urban + female_head | dpt_missed_opportunity, data=data2, overall=F, extra.col=list(`P-value`=pvalue), topclass="Rtable1-zebra")

####################################################
# Part III.
# Compare days to vaccination (observed and potential coverage) 
# Comparing DHS data from 2013 and 2018
####################################################

####################################################
##### 1. Measles
####################################################

####################################################
#### Observed
####################################################
# keep kids that are older than the max age of measles and with vaccination card
obsmea1dat <- data %>% filter(age_in_days>=mea1_age_due_max,
                            has_health_card_bin=="Yes")

# calculate hazard days
obsmea1dat <- obsmea1dat %>% mutate(hazard_days_mea1 = case_when(
  # For the early group, this is just the total number of days since the start of the MMR interval that the child lived
  early_mea1==1 ~ mea1_days_at_risk + (mea1_age_due_max - mea1_age_due_min),

  # For the late group - this is the number of days lived from start of the interval to the date of vaccination. Use the already-calculated days at risk to help.
  mea1_late==1 ~ mea1_days_at_risk + (mea1_age_due_max - mea1_age_due_min),

  # For those who got vaccinated on time, the number of days to vaccination is just the age at vaccination minus the age at start of the interval
  mea1_within_interval==1 ~ mea1_age_at_counted_vac - mea1_age_due_min,

  # For those who never got a vaccine it is their age minus minimum interval bound,
  never_got_mea1==1 ~ age_in_days - mea1_age_due_min
))

# Now we need a failure indicator -- consider failure to be getting a vaccine.
# Kids who NEVER got measles or kids who got Measles early (and not again in the interval or late) will be censored at our observation days.
# Kids who got the vaccine in the interal or late will have known observation time.
obsmea1dat <- obsmea1dat %>% mutate(gotit = case_when(

  # censored
  never_got_mea1==1 | early_mea1==1 ~ 0,

  # known observation time
  mea1_late==1 | mea1_within_interval ~ 1,
))

####################################################
# Survival curve

# create survival object using observed data
observed.mea1 <- Surv(time=obsmea1dat$hazard_days_mea1, event=obsmea1dat$gotit)

# use survfit function to create survival curves based on a formula
f0 <- survfit(observed.mea1 ~ strata, data = obsmea1dat)

# Plot Survival Curves
ggsurvplot(
  fit=f0,
  data=obsmea1dat,
  xlab="Days of observation",
  ylab="",
  title="Observed number of days to measles vaccination",
  fun="event",
  conf.int = FALSE,
  censor=FALSE,
  legend = "bottom", 
  legend.title = "Year of Nigeria DHS",
  ylim = c(0,1),
  ggtheme=theme_linedraw()
  )

# find the median survival time
f0

# statistical test to see if there was a difference in survival time according to year
sd <- survdiff(observed.mea1 ~ strata, data = obsmea1dat)
sd

####################################################
#### Potential coverage
####################################################

# keep kids older that the max age of measles1 and keep if kids have a vaccine card
potmea1dat <- data %>% filter(age_in_days>=mea1_age_due_max,
                        has_health_card_bin=="Yes")

# Calculate the days at risk for the hazard analysis  -- the first part is the same as non_MOP analysis
potmea1dat <- potmea1dat %>% mutate(hazard_days_mea1 = case_when(
  
  # Replace time at risk for MOP
  mea1_missed_opportunity=="Yes" & !is.na(mea1_age_at_mop_vac) ~ mea1_age_at_mop_vac - mea1_age_due_min,
  
  # For the early group, this is just the total number of days since the start of the MMR interval that the child lived
  early_mea1==1 ~ mea1_days_at_risk + (mea1_age_due_max-mea1_age_due_min),
  
  # For the late group - this is the number of days lived from start of the interval to the date of vaccination. Use the already-calculated days at risk to help.
  mea1_late==1 ~ mea1_days_at_risk + (mea1_age_due_max - mea1_age_due_min),
  
  # For those who got vaccinated on time, the number of days to vaccination is just the age at vaccination minus the age at start of the interval
  mea1_within_interval==1 ~ mea1_age_at_counted_vac - mea1_age_due_min,
  
  # For those who never got a vaccine it is their age minus minimum interval bound,
  never_got_mea1==1 ~ age_in_days - mea1_age_due_min
))

# # Now we need a failure indicator -- consider failure to be getting a vaccine. 
# # Kids who NEVER got measles or kids who got Measles early (and not again in the interval or late) will be censored at our observation days. 
# # Kids who got the vaccine in the interal or late will have known observation time.

potmea1dat <- potmea1dat %>% mutate(gotit = case_when(
  # known observation time
  mea1_late==1 | mea1_within_interval==1 | mea1_missed_opportunity=="Yes" ~ 1,
  
  # censored
  never_got_mea1==1 | early_mea1==1 ~ 0))

# use survfit function to create survival curves based on a formula
f1 <- survfit(Surv(time = hazard_days_mea1, event=gotit) ~ strata, data = potmea1dat)

ggsurvplot(
  fit=f1,
  data=potmea1dat,
  xlab="Days of observation",
  ylab="",
  title="Potential number of days to measles vaccination",
  fun="event",
  conf.int = FALSE,
  censor=FALSE,
  legend = "bottom", 
  legend.title = "Year of Nigeria DHS",
  ylim = c(0,1),
  ggtheme=theme_linedraw()
  )

####################################################
##### 2. DPT
####################################################

####################################################
#### Observed
####################################################

obsdptdat <- data %>% filter(age_in_days>=dpt3_age_due_max, 
                       has_health_card_bin=="Yes")

# Calculate the days at risk for the hazard analysis
obsdptdat <- obsdptdat %>% mutate(hazard_days_dpt = case_when(
  # For the group with insufficient # of dpt, this is just the total number of days since the start of the interval
  too_few_elig_dpt==1 ~ dpt_days_at_risk + (dpt3_age_due_max - dpt3_age_due_min),
  
  # For the late group - this is the number of days lived from start of the interval to the date of vaccination. Use the already-calculated days at risk to help.
  dpt_late==1 ~ dpt_days_at_risk + (dpt3_age_due_max - dpt3_age_due_min),
  
  # For those who got vaccinated on time, the number of days to vaccination is just the age at vaccination minus the age at start of the interval
  dpt_within_interval==1 ~ dpt_age_at_counted_vac - dpt3_age_due_min,
  
  # For those who never got a vaccine it is their age minus minimum interval bound,
  never_got_dpt==1 ~ age_in_days - dpt3_age_due_min
))

# create a failure indicator
obsdptdat <- obsdptdat %>% mutate(gotit = case_when(
  # for those that never got dpt or kids who got dpt early and not again in the interval or late
  never_got_dpt==1 | too_few_elig_dpt==1 ~ 0,
  dpt_late==1 | dpt_within_interval==1 ~ 1
))

####################################################
# Survival curve

# create survival object using observed data
observed.dpt <- Surv(time=obsdptdat$hazard_days_dpt, event=obsdptdat$gotit)

# use survfit function to create survival curves based on a formula
f2 <- survfit(observed.dpt ~ strata, data = obsdptdat)

# Plot Survival Curves
ggsurvplot(
  fit=f2,
  data = obsdptdat,
  xlab="Days of observation",
  ylab="",
  title="Observed number of days to DPT vaccination",
  fun="event",
  conf.int = FALSE,
  censor=FALSE,
  legend = "bottom", 
  legend.title = "Year of Nigeria DHS",
  ylim = c(0,1),
  ggtheme=theme_linedraw()
)

# find the median survival time
f2

# statistical test to see if there was a difference in survival time according to year
sd2 <- survdiff(observed.dpt ~ strata, data = obsdptdat)
sd2

####################################################
#### Potential coverage
####################################################

# keep kids older that the max age of dpt3 and keep if kids have a vaccine card
potdptdat <- data %>% filter(age_in_days>=dpt3_age_due_max,
                        has_health_card_bin=="Yes")

# Calculate the days at risk for the hazard analysis  -- the first part is the same as non_MOP analysis
potdptdat <- potdptdat %>% mutate(hazard_days_dpt = case_when(
  # for those with a missed opportunity, replace days at risk
  dpt_missed_opportunity=="Yes" & !is.na(dpt_age_at_mop_vac) ~ dpt_age_at_mop_vac - dpt3_age_due_min,
  
  # For the group with insufficient # of dpt, this is just the total number of days since the start of the interval
  too_few_elig_dpt==1 ~ dpt_days_at_risk + (dpt3_age_due_max - dpt3_age_due_min),
  
  # For the late group - this is the number of days lived from start of the interval to the date of vaccination. Use the already-calculated days at risk to help.
  dpt_late==1 ~ dpt_days_at_risk + (dpt3_age_due_max - dpt3_age_due_min),
  
  # For those who got vaccinated on time, the number of days to vaccination is just the age at vaccination minus the age at start of the interval
  dpt_within_interval==1 ~ dpt_age_at_counted_vac - dpt3_age_due_min,
  
  # For those who never got a vaccine it is their age minus minimum interval bound,
  never_got_dpt==1 ~ age_in_days - dpt3_age_due_min
))

# # Now we need a failure indicator -- consider failure to be getting a vaccine. 
# # Kids who NEVER got measles or kids who got Measles early (and not again in the interval or late) will be censored at our observation days. 
# # Kids who got the vaccine in the interval or late will have known observation time.

potdptdat <- potdptdat %>% mutate(gotit = case_when(
  # known observation time
  dpt_late==1 | dpt_within_interval==1 | dpt_missed_opportunity=="Yes" ~ 1,
  
  # censored
  never_got_dpt==1 | too_few_elig_dpt==1 ~ 0))

# create survival object using observed data
potential.dpt <- Surv(time=potdptdat$hazard_days_dpt, event=potdptdat$gotit)

# use survfit function to create survival curves based on a formula
f3 <- survfit(potential.dpt ~ strata, data = potdptdat)

ggsurvplot(
  fit=f3,
  data=potdptdat,
  xlab="Days of observation",
  ylab="",
  title="Potential number of days to dpt vaccination",
  fun="event",
  conf.int = FALSE,
  censor=FALSE,
  legend = "bottom", 
  legend.title = "Year of Nigeria DHS",
  ylim = c(0,1),
  ggtheme=theme_linedraw()
)

# find the median survival time
f3

# statistical test to see if there was a difference in survival time according to year
sd3 <- survdiff(potential.dpt ~ strata, data = potdptdat)
sd3



# create a survival model to see if hazard differs in year and controlling for other variables
# coxph(Surv(hazard_days_mea1, gotit) ~ strata, data=mdt)
# coxph(Surv(hazard_days_mea1, gotit) ~ strata + edu + literate + kid_agecat + total_children_born + assets + urban, data=mdt)

# # plot curves showing difference according to year of data
# # use survfit function to create survival curves based on a formula
# f0b <- survfit(Surv(time = hazard_days_mea1, event=gotit) ~ strata, data = omdt)
# 
