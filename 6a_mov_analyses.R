####################################################
# 1: Load prepped dataset for analyses
####################################################
data <- readRDS(paste0(g_drive, "Data/prepped_data/2d_prepped_dhs_data_for_analysis.RDS"))

# subset data to only Nigeria
data <- data %>% filter(v000 %in% c("NG6", "NG7"))

####################################################
# 2: descriptive characteristics of children with and without card
####################################################
table1(~ sex_of_child + kid_agecat + edu + literate + wom_agecat + total_children_born + marital + wom_occ + assets + hhsize + urban + female_head | has_health_card_bin, data=data)

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
# save data as datatable
data <- as.data.table(data)

# calculate the number of children in sample
data[, .(.N), by = .(v000)]

# calculate how many children has a vaccination card
data[has_health_card_bin == "Yes",.(total_with_card= .N), by = v000]

# calculate how many children were covered by each vaccine according to recall and card
data[mea1_date_recorded%in%c(1,2,3),.(received_mea1= .N), by = v000]

# calculate how many children received mea1 according to health card only
data[has_health_card_bin == "Yes" & mea1_date_recorded%in%c(1,3),.(received_mea= .N), by = v000]

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

data$mea1_missed_opportunity <-factor(data$mea1_missed_opportunity, 
                                       levels=c(0,1),
                                       labels=c("No", "Yes"))
label(data$female) <- "Child is Female"
label(data$edu) <- "Mother's education"
label(data$wom_occ) <- "Mother's occupation"
label(data$hhsize) <- "Household size"
label(data$female_head) <-"Sex of head of household"
label(data$urban) <-"Household location"
label(data$mea1_missed_opportunity) <-"Missed measles opportunity"

table1(~  female + edu + wom_occ + hhsize + urban + female_head| mea1_missed_opportunity, data=data, overall=F, extra.col=list(`P-value`=pvalue))

table1(~  female + edu + wom_agecat +  + wom_occ + assets + hhsize + urban + female_head | mea1_missed_opportunity, data=data, overall=F, extra.col=list(`P-value`=pvalue))


# create a series of tables   
####################################################
# 6. Regression to compare missed opportunities
####################################################


####################################################
# 7. hazards
####################################################

# Measles


m.data <- data

# keep kids that are older than the max age of measles and with vaccination card 
m.data <- m.data %>% filter(age_in_days>=mea1_age_due_max,
                            has_health_card_bin=="Yes")

# // Since we are looking at the number of days lived at risk, we use the information about age at vaccination -- these will vary by the interval in which the child was vaccinated ( early, on time, late, etc)  vs never vaccinated 
# summ mmr_age_at_counted_vac, det
# summ mmr_age_at_counted_vac if mmr_within_interval==1
# summ mmr_age_at_counted_vac if mmr_late==1
# summ mmr_age_at_counted_vac if early_mmr==1

m.data %>% mutate(hazard_days_mea1 = case_when(
  early_mea1==1 ~ mea1_days_at_risk + (mea1_age_due_max-mea1_age_due_min),
  
))


# // Calculate the days at risk for the hazard analysis 
# gen hazard_days_mmr = . 
# // For the early group, this is just the total number of days since the start of the MMR interval that the child lived. 
# replace hazard_days_mmr = mmr_days_at_risk + (`mmr_age_due_max' - `mmr_age_due_min') 	if early_mmr==1 
# 	// For the late group - this is the number of days lived from start of the interval to the date of vaccination. Use the already-calculated days at risk to help. 
# 	replace hazard_days_mmr = mmr_days_at_risk + (`mmr_age_due_max' - `mmr_age_due_min') 	if mmr_late==1 
# 	// For those who got vaccinated on time, the number of days to vaccination is just the age at vaccination minus the age at start of the interval 
# 	replace hazard_days_mmr = mmr_age_at_counted_vac - `mmr_age_due_min' 					if mmr_within_interval==1 
#                                               // For those who never got a vaccine it is their age minus minimum interval bound 
#                                               replace hazard_days_mmr = age_in_days - `mmr_age_due_min' if never_got_mmr==1 

# // Look at the results 
# * hist hazard_days_mmr 
# summ hazard_days_mmr, det

# // Now we need a failure indicator -- consider failure to be getting a vaccine. Kids who NEVER got MMR or kids who got MMR early (and not again in the interval or late) will be censored at our observation days. Kids who got the vaccine in the interal or late will have known observation time. 
# gen gotit = 0 if never_got_mmr==1 | early_mmr==1
# replace gotit = 1 if mmr_late==1 | mmr_within_interval==1
# // Check out the results 
# tab gotit, m    


