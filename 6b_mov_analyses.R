####################################################
# 1: Load prepped dataset for analyses
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

data$assets <- facotr(data$assets,
                      levels = c(1,2,3,4,5),
                      labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 3", "Quintile 4", "Quintile 5"))

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

####################################################
# 2. Explore what variables are associated with a missed opportunity
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
data1 <- data %>% filter(v000=="NG7")
data2 <- data %>% filter(v000=="NG6")

table1(~  sex_of_child + kid_agecat + edu + literate + wom_agecat + total_children_born + marital + assets + hhsize + urban + female_head | mea1_missed_opportunity, data=data1, overall=F, extra.col=list(`P-value`=pvalue), topclass="Rtable1-zebra")
table1(~  sex_of_child + kid_agecat + edu + literate + wom_agecat + total_children_born + marital + assets + hhsize + urban + female_head | mea1_missed_opportunity, data=data2, overall=F, extra.col=list(`P-value`=pvalue), topclass="Rtable1-zebra")

# calculate the chi-square for child's age seperately since children that are too young cannot have a missed opportunity yet
chidt1 <- data1 %>% filter(kid_agecat!="0 years")
format.pval(tes1, digits=3, eps=0.001)
tes1 <- chisq.test(table(chidt1$kid_agecat, chidt1$mea1_missed_opportunity, exclude = "0 years"))$p.value

chidt2 <- data2 %>% filter(kid_agecat!="0 years")
tes2 <- chisq.test(table(chidt2$kid_agecat, chidt2$mea1_missed_opportunity, exclude = c("0 years", "3 years")))$p.value
format.pval(tes2, digits=3, eps=0.001)


####################################################
# repeat analysis with DPT vaccines
###################################################

####################################################
# 3: Was there an improvement in missed opportunities between the two time points of interest in Nigeria
# create observed and potential coverage for measles
####################################################

# keep kids older that the max age of measles1 and keep if kids have a vaccine card
mdt <- data %>% filter(age_in_days>=mea1_age_due_max,
                           has_health_card_bin=="Yes")

# Calculate the days at risk for the hazard analysis  -- the first part is the same as non_MOP analysis
mdt <- mdt %>% mutate(hazard_days_mea1 = case_when(
  
  # Replace time at risk for MOP
  mea1_missed_opportunity==1 & !is.na(mea1_age_at_mop_vac) ~ mea1_age_at_mop_vac - mea1_age_due_min,
  
  # For the early group, this is just the total number of days since the start of the MMR interval that the child lived
  early_mea1==1 ~ mea1_days_at_risk + (mea1_age_due_max-mea1_age_due_min),

  # For the late group - this is the number of days lived from start of the interval to the date of vaccination. Use the already-calculated days at risk to help.
  mea1_late==1 ~ mea1_days_at_risk + (mea1_age_due_max - mea1_age_due_min),

  # For those who got vaccinated on time, the number of days to vaccination is just the age at vaccination minus the age at start of the interval
  mea1_within_interval==1 ~ mea1_age_at_counted_vac - mea1_age_due_min,

  # For those who never got a vaccine it is their age minus minimum interval bound,
  never_got_mea1==1 ~ age_in_days - mea1_age_due_min
))

# // Look at the results 
hist(mdt$hazard_days_mea1)
mean(mdt$hazard_days_mea1)

# * hist hazard_days_mmr 
# summ hazard_days_mmr, det

# # Now we need a failure indicator -- consider failure to be getting a vaccine. 
# # Kids who NEVER got measles or kids who got Measles early (and not again in the interval or late) will be censored at our observation days. 
# # Kids who got the vaccine in the interal or late will have known observation time.
# 
mdt <- mdt %>% mutate(gotit = case_when(
  # known observation time
  mea1_late==1 | mea1_within_interval==1 | mea1_missed_opportunity==1 ~ 1,

  # censored
  never_got_mea1==1 | early_mea1==1 ~ 0))

# view results
table(mdt$gotit)

# specify the hazard
library(survival)
library(cmprsk)
mov.survival <- Surv(time=mdt$hazard_days_mea1, event=mdt$gotit)

# create plot of cumulative incidence of vaccination
ci_fit <- 
  cuminc(
    ftime = mdt$hazard_days_mea1, 
    fstatus = mdt$gotit,
    group = mdt$v000
  )

plot(ci_fit)

ci_fit[["Tests"]]

my.survfit <- survfit(mov.survival  ~ v000, data = mdt)
plot(my.survfit, xlab="Days of Observation")

# 
# // Specify the hazard 
# stset hazard_days_mmr, failure(gotit)    // 14553 observations 
# // Make graph -- 0 days is 11.5 mos old and 60.8 days is 2 mos (15.2days*4=60.8) later --> 13.5 mos old 
# // 0 days = start, 11.5 mos
# // 15.4 days = 12 mos 
# // 60.8 days = end of interval at which they are elig for on-time MMR 
# // 12.3*30.4  = 380  = age 2 years
# // 24.3*30.4  = 744.8  = age 3 years
# // 1109.6 = 4 years 
# // 1474.4 = 5 years 
# sts graph, failure title("Potential number of days to MMR vaccination (N=14553)", size(med)) ci xtitle("Days of potential observation") xlabel(0 60 380 745 1110 1475, labsize(small)) xline(0 60.8) ylabel(,labsize(small) angle(0))
# *graph export "J:\Project\IDB\9. Analysis\Immunization\figures\hz_mmr_mop.png", replace
# *graph export "J:\Project\IDB\9. Analysis\Immunization\figures\hz_mmr_mop.tif", replace
# sts graph, by(iso) failure title("Potential number of days to MMR vaccination (N=14553)", size(med)) ci xtitle("Days of potential observation") xlabel(0 60 380 745 1110 1475, labsize(small)) xline(0 60.8) ylabel(,labsize(small) angle(0))
# *graph export "J:\Project\IDB\9. Analysis\Immunization\figures\hz_mmr_mop_bycountry.png", replace
# *graph export "J:\Project\IDB\9. Analysis\Immunization\figures\hz_mmr_mop_bycountry.tif", replace



####################################################
# 2: descriptive characteristics of children with and without card
####################################################
# table1(~ sex_of_child + kid_agecat + edu + literate + wom_agecat + total_children_born + marital + wom_occ + assets + hhsize + urban + female_head | has_health_card_bin, data=data)

# export table 1 as an image

####################################################
# 3: regression to investigate who has card 
####################################################

# fit a regression to see which variables are associated with having vaccine coverage card
# mylogit <- glm(has_health_card_bin ~ female + kid_agecat + edu + literate + wom_agecat + total_children_born + marital + wom_occ + assets + hhsize + urban + female_head, data = data, family = "binomial")
# 
# 
# exp(cbind(OR = coef(mylogit), confint(mylogit)))

# export table 2 as an image

####################################################
# 4. coverage cascade
####################################################
# # save data as datatable
# data <- as.data.table(data)
# 
# # calculate the number of children in sample
# data[, .(.N), by = .(v000)]
# 
# # calculate how many children has a vaccination card
# data[has_health_card_bin == "Yes",.(total_with_card= .N), by = v000]
# 
# # calculate how many children were covered by each vaccine according to recall and card
# data[mea1_date_recorded%in%c(1,2,3),.(received_mea1= .N), by = v000]
# 
# # calculate how many children received mea1 according to health card only
# data[has_health_card_bin == "Yes" & mea1_date_recorded%in%c(1,3),.(received_mea= .N), by = v000]

# calculate the totals for DPT vaccines

####################################################
# 5. compare missed opportunities- chisquare
####################################################




# create a series of tables   
####################################################
# 6. Regression to compare missed opportunities
####################################################


####################################################
# 7. hazards

## Was there a difference in addressing missed opportunities for vaccination between the 2 most recent time points?

####################################################


# copy data for Measles analysis
# mdat <- data

# # keep kids that are older than the max age of measles and with vaccination card 
# mdat <- mdat %>% filter(age_in_days>=mea1_age_due_max,
#                             has_health_card_bin=="Yes")
# 
# # // Since we are looking at the number of days lived at risk, we use the information about age at vaccination -- these will vary by the interval in which the child was vaccinated ( early, on time, late, etc)  vs never vaccinated 
# # summ mmr_age_at_counted_vac, det
# # summ mmr_age_at_counted_vac if mmr_within_interval==1
# # summ mmr_age_at_counted_vac if mmr_late==1
# # summ mmr_age_at_counted_vac if early_mmr==1
# 
# # calculate
# mdat <- mdat %>% mutate(hazard_days_mea1 = case_when(
#   # For the early group, this is just the total number of days since the start of the MMR interval that the child lived
#   early_mea1==1 ~ mea1_days_at_risk + (mea1_age_due_max-mea1_age_due_min),
#   
#   # For the late group - this is the number of days lived from start of the interval to the date of vaccination. Use the already-calculated days at risk to help. 
#   mea1_late==1 ~ mea1_days_at_risk + (mea1_age_due_max - mea1_age_due_min),
#   
#   # For those who got vaccinated on time, the number of days to vaccination is just the age at vaccination minus the age at start of the interval 
#   mea1_within_interval==1 ~ mea1_age_at_counted_vac - mea1_age_due_min,
#   
#   # For those who never got a vaccine it is their age minus minimum interval bound,
#   never_got_mea1==1 ~ age_in_days - mea1_age_due_min
# ))
# 
# # // Look at the results 
# hist(mdat$hazard_days_mea1) 
# # summ hazard_days_mmr, det
# 
# # Now we need a failure indicator -- consider failure to be getting a vaccine. 
# # Kids who NEVER got measles or kids who got Measles early (and not again in the interval or late) will be censored at our observation days. 
# # Kids who got the vaccine in the interal or late will have known observation time.
# 
# mdat <- mdat %>% mutate(gotit = case_when(
#   
#   # censored
#   never_got_mea1==1 | early_mea1==1 ~ 0,
#   
#   # known observation time
#   mea1_late==1 | mea1_within_interval ~ 1,
# ))
# 
# 
