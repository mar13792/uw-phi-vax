####################################################
# Part I
# Load prepped dataset for analyses
####################################################
data <- readRDS(outputFile6a)

# subset data to only Nigeria
data <- data %>% filter(v000 %in% c("NG6", "NG7"))

############## Part II: Explore what variables are associated with a missed opportunity ######

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

##### repeat descriptive analysis with DPT vaccine #####

test <- table1( ~ sex_of_child + kid_agecat + edu + literate + wom_agecat + total_children_born + marital + assets + hhsize + urban + female_head | dpt_missed_opportunity, data=data1, overall=F, extra.col=list(`P-value`=pvalue), topclass="Rtable1-zebra")

table1(~  sex_of_child + kid_agecat + edu + literate + wom_agecat + total_children_born + marital + assets + hhsize + urban + female_head | dpt_missed_opportunity, data=data2, overall=F, extra.col=list(`P-value`=pvalue), topclass="Rtable1-zebra")

# png(filename = paste0(visDir, "aim_1/missed_opportunities/nigeria_2018_dpt_mov_table1.png") )
# test
# dev.off()
# write.table(test, file = paste0(visDir, "aim_1/missed_opportunities/nigeria_2018_dpt_mov_table1.txt"), sep = ",", quote = FALSE, row.names = F)
###### Part III: Compare days to vaccination (observed and potential coverage) #####
# Comparing DHS data from 2013 and 2018

##### 1. Measles #####

######### Observed ########

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

######### Survival curve #########

# create survival object using observed data
observed.mea1 <- Surv(time=obsmea1dat$hazard_days_mea1, event=obsmea1dat$gotit)

# use survfit function to create survival curves based on a formula
f0 <- survfit(observed.mea1 ~ strata, data = obsmea1dat)

# Plot Survival Curves
plot1 <- ggsurvplot(
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
  ggtheme=theme_linedraw(), 
  risk.table = FALSE)

# save plot
png(filename = paste0(visDir, "aim_1/missed_opportunities/measles_observed_curve.png"),
    height = 8, width = 10, units = "in", res = 300)
plot1
dev.off()

# find the median survival time
f0

# statistical test to see if there was a difference in survival time according to year
sd <- survdiff(observed.mea1 ~ strata, data = obsmea1dat)
sd

##### Potential coverage #####

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

plot2 <- ggsurvplot(
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
  ggtheme=theme_linedraw())

# save plot
png(filename = paste0(visDir, "aim_1/missed_opportunities/measles_potential_curve.png"),
    height = 8, width = 10, units = "in", res = 300)
plot2
dev.off()

########## 2. DPT ########## 

######### Observed ##########

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

# test <- obsdptdat %>% filter(is.na(gotit))

####################################################
# Survival curve

# create survival object using observed data
observed.dpt <- Surv(time=obsdptdat$hazard_days_dpt, event=obsdptdat$gotit)

# use survfit function to create survival curves based on a formula
f2 <- survfit(observed.dpt ~ strata, data = obsdptdat)

# Plot Survival Curves
plot3 <- ggsurvplot(
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

# save plot
png(filename = paste0(visDir, "aim_1/missed_opportunities/dpt_observed_curve.png"),
    height = 8, width = 10, units = "in", res = 300)
plot3
dev.off()

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

plot4 <- ggsurvplot(
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

# save plots
png(filename = paste0(visDir, "aim_1/missed_opportunities/dpt_potential_curve.png"),
    height = 8, width = 10, units = "in", res = 300)
plot4
dev.off()

# find the median survival time
f3

# statistical test to see if there was a difference in survival time according to year
sd3 <- survdiff(potential.dpt ~ strata, data = potdptdat)
sd3

###### Create results table
dt <- as.data.table(data)
dt[, .(.N), by = .(v000)]

####################################################
# Part IV. Coverage cascade
####################################################

# subset only to those with a vaccination card
# dtnew <- dt[has_health_card_bin=="Yes" & age_in_days>=mea1_age_due_max]
dtnew <- as.data.table(obsmea1dat)
################
# Measles
################
dt1 <- dtnew[,.(total_with_card= .N), by = strata]

# # calculate how many children received mea1 according to health card only
dt2 <- dtnew[mea1_within_interval==1 | mea1_late==1,.(received_vaccine= .N), by = strata]

# calculate how many children did not receive the measles vaccine
dt3 <- dtnew[never_got_mea1==1 | early_mea1==1, .(no_vaccine=.N), by=strata]

# calculate how many children that were not vaccinated had a missed opportunity
dt4 <- dtnew[(never_got_mea1==1 | early_mea1==1) & mea1_missed_opportunity=="Yes", .(mop=.N), by=strata]

# calculate how many children has a vaccination card
# dt1 <- dtnew[,.(total_with_card= .N), by = strata]
# 
# # # calculate how many children received mea1 according to health card only
# dt2 <- dtnew[mea1_within_interval==1 & mea1_late==1,.(received_vaccine= .N), by = strata]
# 
# # calculate how many children did not receive the measles vaccine
# dt3 <- dtnew[never_got_mea1==1 & mea1_late==1, .(no_vaccine=.N), by=strata]
# 
# # calculate how many children that were not vaccinated had a missed opportunity
# dt4 <- dtnew[never_got_mea1==1 & mea1_late==1 & mea1_missed_opportunity=="Yes", .(mop=.N), by=strata]

# # calculate how many children has a vaccination card
# dt1 <- dtnew[,.(total_with_card= .N), by = strata]
# 
# # # calculate how many children received mea1 according to health card only
# dt2 <- dtnew[age_at_mea1>=mea1_age_due_min,.(received_vaccine= .N), by = strata]
# 
# # calculate how many children did not receive the measles vaccine
# dt3 <- dtnew[is.na(age_at_mea1) | age_at_mea1<mea1_age_due_min, .(no_vaccine=.N), by=strata]
# 
# # calculate how many children that were not vaccinated had a missed opportunity
# dt4 <- dtnew[is.na(age_at_mea1) | age_at_mea1<mea1_age_due_min & mea1_missed_opportunity=="Yes", .(mop=.N), by=strata]

# # calculate how many children has a vaccination card
# dt1 <- dtnew[,.(total_with_card= .N), by = strata]
# 
# # # calculate how many children received mea1 according to health card only
# dt2 <- dtnew[!is.na(age_at_mea1),.(received_vaccine= .N), by = strata]
# 
# # calculate how many children did not receive the measles vaccine
# dt3 <- dtnew[is.na(age_at_mea1), .(no_vaccine=.N), by=strata]
# 
# # calculate how many children that were not vaccinated had a missed opportunity
# dt4 <- dtnew[is.na(age_at_mea1) & mea1_missed_opportunity=="Yes", .(mop=.N), by=strata]

# merge datatables together
mea1_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
mea1_dt[,vaccine:="mea1"]

###################
# DPT All
###################
# dtnew2 <- dt[has_health_card_bin=="Yes" & age_in_days>=dpt3_age_due_max]
dtnew2 <- as.data.table(obsdptdat)

# calculate how many children has a vaccination card
dt1 <- dtnew2[,.(total_with_card= .N), by = strata]

# calculate how many children received all dpt vaccines
dt2 <- dtnew2[gotit==1,. (received_vaccine=.N), by=strata]

# calculate how many children did not receive all dpt doses
dt3 <- dtnew2[gotit==0,. (no_vaccine=.N), by=strata]

# calculate how many of the children that did not receive all dpt doses have a dpt missed opportunity
dt4 <- dtnew2[gotit==0 & dpt_missed_opportunity=="Yes",. (mop=.N), by=strata]

# # calculate how many children has a vaccination card
# dt1 <- dtnew2[,.(total_with_card= .N), by = strata]
# 
# # calculate how many children received all dpt vaccines
# dt2 <- dtnew2[tot_num_dpt==3,. (received_vaccine=.N), by=strata]
# 
# # calculate how many children did not receive all dpt doses
# dt3 <- dtnew2[tot_num_dpt<3,. (no_vaccine=.N), by=strata]
# 
# # calculate how many of the children that did not receive all dpt doses have a dpt missed opportunity
# dt4 <- dtnew2[tot_num_dpt<3 & dpt_missed_opportunity=="Yes",. (mop=.N), by=strata]

# merge dataset
dpt_all_dt <-Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt_all_dt[,vaccine:="dpt_all"]

###################
# DPT 1
###################

dtnew3 <- dt[has_health_card_bin=="Yes" & age_in_days>=dpt1_age_due_max]

# calculate how many children have a vaccination card
dt1 <- dtnew3[,.(total_with_card= .N), by = strata]

# calculate how many children received the dpt1 dose
dt2 <- dtnew3[!is.na(age_at_dpt1),. (received_vaccine=.N), by=strata]

# calculate how many children did not receive dpt1 dose
dt3 <- dtnew3[is.na(age_at_dpt1),. (no_vaccine=.N), by=strata]

# calculate how many of the children that did not receive dpt1 dose have a dpt1 missed opportunity
dt4 <- dtnew3[is.na(age_at_dpt1) & dpt1_missed_opportunity==1,. (mop=.N), by=strata]

# merge data sets together
dpt1_dt <-Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt1_dt[,vaccine:="dpt1"]

###################
# DPT 2
###################

dtnew4 <- dt[has_health_card_bin=="Yes" & age_in_days>=dpt2_age_due_max]

# # calculate how many children have a vaccination card
dt1 <- dtnew4[,.(total_with_card= .N), by = strata]

# # calculate how many children received dpt2
dt2 <- dtnew4[!is.na(age_at_dpt2),. (received_vaccine=.N), by=strata]

# calculate how many children did not receive dpt2 dose
dt3 <- dtnew4[is.na(age_at_dpt2),. (no_vaccine=.N), by=strata]

# calculate how many of the children that did not receive dpt2 dose have a dpt2 missed opportunity
dt4 <- dtnew4[is.na(age_at_dpt2) & dpt2_missed_opportunity==1,. (mop=.N), by=strata]

# merge dataset
dpt2_dt <-Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt2_dt[,vaccine:="dpt2"]

###################
# DPT 3
###################

dtnew5 <- dt[has_health_card_bin=="Yes" & age_in_days>=dpt3_age_due_max]

# # calculate how many children have a vaccination card
dt1 <- dtnew5[,.(total_with_card= .N), by = strata]

# # calculate how many children received dpt3
dt2 <- dtnew5[!is.na(age_at_dpt3),. (received_vaccine=.N), by=strata]

# calculate how many children did not receive dpt3 dose
dt3 <- dtnew5[is.na(age_at_dpt3),. (no_vaccine=.N), by=strata]

# calculate how many of the children that did not receive dpt3 dose have a dpt3 missed opportunity
dt4 <- dtnew5[is.na(age_at_dpt3) & dpt3_missed_opportunity==1,. (mop=.N), by=strata]

# merge dataset
dpt3_dt <-Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt3_dt[,vaccine:="dpt3"]

# merge all vaccine data together
all_vax_data <- rbind(mea1_dt, dpt_all_dt, dpt1_dt, dpt2_dt, dpt3_dt)

# calculate vaccination coverage
all_vax_data[,vac_coverage:=round((received_vaccine/total_with_card)*100, 1)]

# calculate percent of children with a missed opportunity
all_vax_data[,percent_with_mop:=round((mop/no_vaccine)*100, 1)]

# # calculate coverage if no missed opportunity
all_vax_data[,potential_coverage_with_no_mop:=round((mop+received_vaccine)/total_with_card*100, 1)]

# view results
View(all_vax_data)

setcolorder(all_vax_data, 
            c("strata", "total_with_card", "vaccine", "received_vaccine", "no_vaccine", 
              "vac_coverage", "mop", "percent_with_mop", "potential_coverage_with_no_mop"))

write.csv(all_vax_data, file=paste0(resDir, "aim_1/missed_opportunities/mop_vaccine_table.csv"))
