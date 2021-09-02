####################################################
# 1: Load prepped dataset for analyses
####################################################
data <- readRDS(paste0(g_drive, "Data/prepped_data/2d_prepped_dhs_data_for_analysis.RDS"))

# subset data to only Nigeria
data <- data %>% filter(v000 %in% c("NG6", "NG7"))

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

###################
# mother's education
###################

dt$edu <- as.character(dt$v106)
dt <- dt %>% mutate(edu = recode(edu,
                                 `0`=0,
                                 `1`=1,
                                 `2`= 2,
                                 `3`=2))
dt$edu <- factor(dt$edu,
                 levels = c(0,1,2),
                 labels = c("no education", "primary", "secondary or higher"))

###################
# mother's literacy levels
###################
dt$literate <- as.character(dt$v155)

dt <- dt %>% replace_with_na(replace = list(literate = 9))

dt <- dt %>% mutate(literate = recode(literate,
                                      `0`=0,
                                      `1`=1,
                                      `2`=1,
                                      `3`=9,
                                      `4`=9))

dt$literate <- factor(dt$literate,
                      levels = c(0,1),
                      labels = c("iliterate", "literate"))

###################
# mother's age category
###################

dt$wom_agecat <- dt$v012

dt <- dt %>%
  mutate(wom_agecat=case_when(
    wom_agecat %in% 15:19 ~ "1",
    wom_agecat %in% 20:34 ~ "2",
    wom_agecat %in% 35:49 ~ "3"))

dt$wom_agecat <- factor(dt$wom_agecat,
                        levels = c(1,2,3),
                        labels = c("15-19", "20-34", "35-49"))

###################
# parity
###################
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

###################
# marital status
###################
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

###################
# mother's employment
###################
dt$wom_occ <- dt$v717

dt <- dt %>% replace_with_na(replace = list(wom_occ = 99))

dt <- dt %>% 
  mutate(wom_occ=case_when(
    wom_occ %in% 0 ~ "1",
    wom_occ %in% 1:97 ~ "2"
  ))

dt$wom_occ <- factor(dt$wom_occ,
                     levels = c(1,2),
                     labels = c("not employed in last 12 months", "employed"))

###################
# assets
###################

# each survey should have either v190a or v190 for the household assets
dt$assets <- ifelse(!is.na(dt$v190a), dt$v190a, dt$v190)

dt$assets <- as.factor(dt$assets)

###################
# average household size
###################
dt$hhsize <- dt$v136

###################
# urban
###################
dt$urban <- abs(dt$v025-2)

dt$urban <-factor(dt$urban,
                  levels = c(0,1),
                  labels = c("rural household", "urban household"))

###################
# sex of head of household
###################
dt$female_head <- dt$v151

dt$female_head <- factor(dt$female_head,
                         levels = c(1,2),
                         labels = c('male', 'female'))

###################
# asign labels to variable names
###################
dt$sex_of_child <- factor(dt$sex_of_child,
                          levels=c(1,2),
                          labels=c("male", "female"))

###################
#kid age category
###################

dt$kid_agecat <- round(time_length(difftime(dt$intv_date, dt$dob), "years"), digits = 0)
dt$kid_agecat <- as.factor(dt$kid_agecat)

# format data structures
data$mea1_missed_opportunity <-factor(data$mea1_missed_opportunity,
                                       levels=c(0,1),
                                       labels=c("No", "Yes"))
label(data$sex_of_child) <- "Child's sex"
label(data$edu) <- "Mother's education"
label(data$wom_occ) <- "Mother's occupation"
label(data$hhsize) <- "Household size"
label(data$female_head) <-"Sex of head of household"
label(data$urban) <-"Household location"
label(data$mea1_missed_opportunity) <-"Missed measles opportunity"

data1 <- data %>% filter(v000=="NG7")
table1(~  sex_of_child + edu + wom_occ + hhsize + urban + female_head| mea1_missed_opportunity, data=data1, overall=F, extra.col=list(`P-value`=pvalue))

table1(~  sex_of_child + edu + wom_agecat +  + wom_occ + assets + hhsize + urban + female_head | mea1_missed_opportunity, data=data, overall=F, extra.col=list(`P-value`=pvalue))


# create a series of tables   
####################################################
# 6. Regression to compare missed opportunities
####################################################


####################################################
# 7. hazards

## Was there a difference in addressing missed opportunities for vaccination between the 2 most recent time points?

####################################################


# copy data for Measles analysis
mdat <- data

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
