# ----------------------------------------------
# AUTHOR: Francisco Rios
# PURPOSE: Prep of DHS data for MOV analyses
# DATE: Last updated September 2 2021
# 2d_prep_dhs_data_for_analysis

dt <- readRDS(outputFile2d)

####################################################
# 1. calculate new variables necessary for risk analysis
####################################################

# has health card binary
dt$has_health_card_bin <- as.character(dt$has_health_card)

dt <- dt%>%
  mutate(has_health_card_bin = na_if(has_health_card_bin, 9)) %>%
  mutate(has_health_card_bin=recode(has_health_card_bin,
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

# Assign the MINIMUM and MAXIMUM age at which each vaccine should be given 
#  Assumption: Vaccine is due at month X. We will accept vaccines given at month X-0.5 through X+1.5. 

# Example: Measles due at 9 mos is acceptable 8.5-10.5 months of age.
dt$mea1_age_due_min <- 9*30.4 - 15.2 # 258 days
dt$mea1_age_due_max <- 10*30.4 +  15.2 # 319.2 days

# DPT (or pentavalent) (3 series) (due due at 6, 10, 14 weeks)
# timeliness considered appropriate if between 6th and end of the 7th week of life (with a range of 4 days before and 7 days after)
dt$dpt1_age_due_min <- 6*7 - 4  # 38 days
dt$dpt1_age_due_max <- 7*7 + 7  # 56 days

dt$dpt2_age_due_min <- 10*7 - 4 # 66 days
dt$dpt2_age_due_max <- 11*7 + 7 # 84 days

dt$dpt3_age_due_min <- 14*7 - 4 # 94 days
dt$dpt3_age_due_max <- 15*7 + 7 # 112 days

# # calculate the age of child in days
# dt$age_in_days <- time_length(difftime(dt$intv_date, dt$dob), "days")

# calculate the age at which child was vaccinated with measles-containing vaccine 
dt$age_at_mea1 <- time_length(difftime(dt$mea1, dt$dob), "days")
dt$age_at_mea2 <- time_length(difftime(dt$mea2, dt$dob), "days")

# calculate the age at which child was vaccinated with DPT vaccines
dt$age_at_dpt1 <- time_length(difftime(dt$dpt1, dt$dob), "days")
dt$age_at_dpt2 <- time_length(difftime(dt$dpt2, dt$dob), "days")
dt$age_at_dpt3 <- time_length(difftime(dt$dpt3, dt$dob), "days")

# variable indicating when the oldest vaccination date took place
dt <- dt %>% mutate(oldest_visit = pmax(bcg, dpt1, pol1, dpt2, pol2, dpt3, pent1, pent2, pent3, pneu1, pneu2, pneu3, rota1, rota2, rota3, poln, hepb1, hepb2, hepb3, hib1, hib2, hib3, yelfev,
                                        na.rm=TRUE))

# calculate the age at the oldest visit
dt$age_at_oldest_visit = time_length(difftime(dt$oldest_visit, dt$dob), "days")

# Add indicator for the difference in days between appropriate timing and the actual age at vaccination
# How to interpret: 
# (1) a value of <0 for mmr_age_minus_min means that they were vaccinated too early -- all of the days lived after the end of the mmr window will be considered days at risk; 
# (2) a value of >0 for mmr_age_minus_min and <0 for mmr_age_minus_max means that they were vaccinated in the proper time window and they will accrue 0 days at risk; 
# (3) a value of >0 for mmr_age_minus_max means that they lived days at risk after appropriate vaccination 

# measles
dt$mea1_age_minus_min <- dt$age_at_mea1 - dt$mea1_age_due_min
dt$mea1_age_minus_max <- dt$age_at_mea1 - dt$mea1_age_due_max

# dpt
dt$dpt1_age_minus_min <- dt$age_at_dpt1 - dt$dpt1_age_due_min
dt$dpt1_age_minus_max <- dt$age_at_dpt1 - dt$dpt1_age_due_max

dt$dpt2_age_minus_min <- dt$age_at_dpt2 - dt$dpt2_age_due_min
dt$dpt2_age_minus_max <- dt$age_at_dpt2 - dt$dpt2_age_due_max

dt$dpt3_age_minus_min <- dt$age_at_dpt3 - dt$dpt3_age_due_min
dt$dpt3_age_minus_max <- dt$age_at_dpt3 - dt$dpt3_age_due_max

# identify variables related to timing of the measles-containing vaccine
dt <- dt %>% 
  mutate(
    # vaccinated too early
    early_mea1 = case_when(mea1_age_minus_min<0 ~ 1,
                           mea1_age_minus_min>0 ~ 0),
    
    # vaccinated within appropriate time frame
    mea1_within_interval = case_when(mea1_age_minus_min>=0 & mea1_age_minus_max<=0 ~ 1,
                                     mea1_age_minus_min<0 | mea1_age_minus_max>0 ~ 0),
    
    # vaccinated but too late
    mea1_late = case_when(mea1_age_minus_max>0 ~ 1,
                          mea1_age_minus_max<0 ~ 0),
    
    #  never received vaccine (1 is never received, 0 is did receive))                
    never_got_mea1 = case_when(!is.na(age_at_mea1) & has_health_card_bin=="Yes" ~ 0,
                               is.na(age_at_mea1) & has_health_card_bin=="Yes" ~ 1), 
    
    # transpose the age at which children that did receive measles were counted
    mea1_age_at_counted_vac = case_when(mea1_age_minus_min<0 ~ age_at_mea1,
                                        mea1_age_minus_min>=0 & mea1_age_minus_max<=0 ~ age_at_mea1,
                                        mea1_age_minus_max>0 ~ age_at_mea1),
    
    # variable that indicates how much time each child was at risk (for those that were vaccinated early or late)
    mea1_days_at_risk = case_when(never_got_mea1==1 & age_in_days>=mea1_age_due_max  ~ age_in_days - mea1_age_due_max,
                                  mea1_age_minus_min<0 & age_in_days>=mea1_age_due_max ~ age_in_days - mea1_age_due_max,
                                  mea1_age_minus_min>=0 & mea1_age_minus_max<=0 ~ 0,
                                  mea1_age_minus_max>0  ~ mea1_age_minus_max))

# calculate additional measles coverage variables that require re-shaping of the data

# calculate how many doses of DPT were received
for (i in 1:nrow(dt)){
  dt$tot_num_dpt[i] <- sum(!is.na(dt$dpt1[i]), !is.na(dt$dpt2[i]), !is.na(dt$dpt3[i]))
}

## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! 
## ! REVIEW ! chunk is missing from here to account for DPT dates that are too close to each other!
##
## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! 

# calculate variables related to timing among the DPT-containing vaccine (pentavalent)
dt <- dt %>% 
  mutate(
    # calculate variable indicating if never received dpt vaccine
    never_got_dpt = case_when(is.na(age_at_dpt1) & is.na(age_at_dpt2) & is.na(age_at_dpt3) & has_health_card_bin=="Yes" ~ 1,
                              !is.na(age_at_dpt1) | !is.na(age_at_dpt2) | !is.na(age_at_dpt3) & has_health_card_bin=="Yes" ~ 0),
    
    never_got_dpt1 = case_when(is.na(age_at_dpt1) & has_health_card_bin=="Yes" ~ 1,
                               !is.na(age_at_dpt1) & has_health_card_bin=="Yes" ~ 0),
    
    never_got_dpt2 = case_when(is.na(age_at_dpt2) & has_health_card_bin=="Yes" ~ 1,
                               !is.na(age_at_dpt2) & has_health_card_bin=="Yes" ~ 0),
    
    never_got_dpt3 = case_when(is.na(age_at_dpt3) & has_health_card_bin=="Yes" ~ 1,
                               !is.na(age_at_dpt3) & has_health_card_bin=="Yes" ~0),
    
    # calculate whether they got the first dose in sixth week
    dpt_dose_6wks = case_when(age_at_dpt1>=dpt1_age_due_min & age_at_dpt1<=dpt1_age_due_max & age_in_days>= dpt3_age_due_max ~ 1,
                              never_got_dpt1==1 ~ 0),
    
    # calculate age when first dose was received
    dpt_dose_6wks_when = case_when(age_at_dpt1>=dpt1_age_due_min & age_at_dpt1<=dpt1_age_due_max & age_in_days>= dpt3_age_due_max ~ age_at_dpt1),
    
    # calculate first age eligible after first dose
    first_elig_dpt_after_6wks = case_when(age_at_dpt1>=dpt1_age_due_min & age_in_days>= dpt3_age_due_max ~ age_at_dpt1,
                                          age_at_dpt2>=dpt1_age_due_min & age_in_days>= dpt3_age_due_max ~ age_at_dpt2,
                                          age_at_dpt3>=dpt1_age_due_min & age_in_days>= dpt3_age_due_max ~ age_at_dpt3),
    
    # calculate whether they got the second dose in the tenth week
    dpt_dose_10wks = case_when(age_at_dpt2>=dpt2_age_due_min & age_at_dpt2<=dpt2_age_due_max & age_in_days>= dpt3_age_due_max ~ 1,
                               never_got_dpt2==1 ~ 0),
    
    # calculate age when second dose was received
    dpt_dose_10wks_when = case_when(age_at_dpt2>=dpt2_age_due_min & age_at_dpt2<=dpt2_age_due_max & age_in_days>= dpt3_age_due_max ~ age_at_dpt2),
    
    # calculate first age eligible after second dose
    first_elig_dpt_after_10wks = case_when(age_at_dpt1>=dpt2_age_due_min & age_in_days>= dpt3_age_due_max ~ age_at_dpt1,
                                           age_at_dpt2>=dpt2_age_due_min & age_in_days>= dpt3_age_due_max ~ age_at_dpt2,
                                           age_at_dpt3>=dpt2_age_due_min & age_in_days>= dpt3_age_due_max ~ age_at_dpt3),
    
    # calculate whether they got the third dose in the 14th week
    dpt_dose_14wks = case_when(age_at_dpt3>=dpt3_age_due_min & age_at_dpt3<=dpt3_age_due_max & age_in_days>= dpt3_age_due_max ~ 1,
                               never_got_dpt3==1 ~ 0),
    
    # calculate age when third dose was received
    dpt_dose_14wks_when = case_when(age_at_dpt3>=dpt3_age_due_min & age_at_dpt3<=dpt3_age_due_max & age_in_days>= dpt3_age_due_max ~ age_at_dpt3),
    
    # calculate first age eligible after third dose
    first_elig_dpt_after_14wks = case_when(age_at_dpt1>=dpt3_age_due_min & age_in_days>= dpt3_age_due_max ~ age_at_dpt1,
                                           age_at_dpt2>=dpt3_age_due_min & age_in_days>= dpt3_age_due_max ~ age_at_dpt2,
                                           age_at_dpt3>=dpt3_age_due_min & age_in_days>= dpt3_age_due_max ~ age_at_dpt3),
    
    # generate indicator for correct interval
    dpt_within_interval = case_when(
      
      # kids that did not have all visits during the interval, but had 3 doses with the right spacing
      tot_num_dpt==3 &first_elig_dpt_after_14wks>=dpt3_age_due_min & first_elig_dpt_after_14wks <=dpt3_age_due_max & age_in_days>=dpt3_age_due_max ~1,
      
      # kids that have perfect adherence
      dpt_dose_6wks==1 & dpt_dose_10wks==1 & dpt_dose_14wks==1 & age_in_days>=dpt3_age_due_max ~ 1,
      
      # kids without perfect adherence 
      dpt_dose_6wks!=1 & dpt_dose_10wks!=1 & dpt_dose_14wks!=1 ~ 0),
    
    # generate indicator for vaccine 3rd dose too late
    dpt_late = case_when(first_elig_dpt_after_14wks>dpt3_age_due_max & age_in_days>=dpt3_age_due_max ~ 1),
    
    # generate indicator for age at counted vaccine
    dpt_age_at_counted_vac = case_when(
      
      # kids that have perfect adherence
      dpt_within_interval==1 ~ dpt_dose_14wks_when,
      
      # kids that did not have all visits during the interval, but had 3 doses with the right spacing
      dpt_within_interval==1 ~ first_elig_dpt_after_14wks,
      
      # third dose was too late
      dpt_late==1 ~ first_elig_dpt_after_14wks),
    
    # assign indicator for too few dpt vacccines
    too_few_elig_dpt = case_when(is.na(first_elig_dpt_after_14wks) & tot_num_dpt>0 & age_in_days>=dpt3_age_due_max & has_health_card_bin=="Yes" ~ 1),
    
    # assign days at risk
    dpt_days_at_risk = case_when(never_got_dpt==1 & age_in_days>=dpt3_age_due_max ~ age_in_days - dpt3_age_due_max,
                                 dpt_within_interval==1 ~ 0,
                                 dpt_late==1 ~ first_elig_dpt_after_14wks - dpt3_age_due_max,
                                 too_few_elig_dpt==1 ~ age_in_days - dpt3_age_due_max))


# calculate ages at other vaccines if relevant
dt$age_at_bcg <- time_length(difftime(dt$bcg, dt$dob), "days")
dt$age_at_dpt1 <- time_length(difftime(dt$dpt1, dt$dob), "days")
dt$age_at_pol1 <- time_length(difftime(dt$pol1, dt$dob), "days")
dt$age_at_dpt2 <- time_length(difftime(dt$dpt2, dt$dob), "days")
dt$age_at_pol2 <- time_length(difftime(dt$pol2, dt$dob), "days")
dt$age_at_dpt3 <- time_length(difftime(dt$dpt3, dt$dob), "days")
dt$age_at_pol3 <- time_length(difftime(dt$pol3, dt$dob), "days")
dt$age_at_mea2 <- time_length(difftime(dt$mea2, dt$dob), "days")
dt$age_at_pol0 <- time_length(difftime(dt$pol0, dt$dob), "days")
dt$age_at_pent1 <- time_length(difftime(dt$pent1, dt$dob), "days")
dt$age_at_pent2 <- time_length(difftime(dt$pent2, dt$dob), "days")
dt$age_at_pent3 <- time_length(difftime(dt$pent3, dt$dob), "days")
dt$age_at_pneu1 <- time_length(difftime(dt$pneu1, dt$dob), "days")
dt$age_at_pneu2 <- time_length(difftime(dt$pneu2, dt$dob), "days")
dt$age_at_pneu3 <- time_length(difftime(dt$pneu3, dt$dob), "days")
dt$age_at_rota1 <- time_length(difftime(dt$rota1, dt$dob), "days")
dt$age_at_rota2 <- time_length(difftime(dt$rota2, dt$dob), "days")
dt$age_at_rota3 <- time_length(difftime(dt$rota3, dt$dob), "days")
dt$age_at_poln <- time_length(difftime(dt$poln, dt$dob), "days")
dt$age_at_hepb1 <- time_length(difftime(dt$hepb1, dt$dob), "days")
dt$age_at_hepb2 <- time_length(difftime(dt$hepb2, dt$dob), "days")
dt$age_at_hepb3 <- time_length(difftime(dt$hepb3, dt$dob), "days")
dt$age_at_hib1 <- time_length(difftime(dt$hib1, dt$dob), "days")
dt$age_at_hib2 <- time_length(difftime(dt$hib2, dt$dob), "days")
dt$age_at_hib3 <- time_length(difftime(dt$hib3, dt$dob), "days")
dt$age_at_yelfev <- time_length(difftime(dt$yelfev, dt$dob), "days")

# find out how old child was at the earliest possible visit (using all vaccination dates except measles)
dt <- dt %>% mutate(no_mea1_mop_age = case_when(
  never_got_mea1==1 & age_at_dpt1  > mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_dpt1) & age_at_dpt1 < age_at_bcg ~ age_at_dpt1,
  never_got_mea1==1 & age_at_pol1  > mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_pol1) & age_at_pol1 < age_at_dpt1 ~ age_at_pol1,
  never_got_mea1==1 & age_at_dpt2  > mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_dpt2) & age_at_dpt2 < age_at_pol1 ~ age_at_dpt2,
  never_got_mea1==1 & age_at_pol2  > mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_pol2) & age_at_pol2 < age_at_dpt2 ~ age_at_pol2,
  never_got_mea1==1 & age_at_dpt3  > mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_dpt3) & age_at_dpt3 < age_at_pol2 ~ age_at_dpt3,
  never_got_mea1==1 & age_at_pol3  > mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_pol3) & age_at_pol3 < age_at_dpt3 ~ age_at_pol3,
  never_got_mea1==1 & age_at_mea2  > mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_mea2) & age_at_mea2 < age_at_pol3 ~ age_at_mea2,
  never_got_mea1==1 & age_at_pol0  > mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_pol0) & age_at_pol0 < age_at_mea2 ~ age_at_pol0,
  never_got_mea1==1 & age_at_pent1 > mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_pent1) & age_at_pent1 < age_at_pol0 ~ age_at_pent1,
  never_got_mea1==1 & age_at_pent2 > mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_pent2) & age_at_pent2 < age_at_pent1 ~ age_at_pent2,
  never_got_mea1==1 & age_at_pent3 > mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_pent3) & age_at_pent3 < age_at_pent2 ~ age_at_pent3,
  never_got_mea1==1 & age_at_pneu1 > mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_pneu1) & age_at_pneu1 < age_at_pent3 ~ age_at_pneu1,
  never_got_mea1==1 & age_at_pneu2 > mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_pneu2) & age_at_pneu2 < age_at_pneu1 ~ age_at_pneu2,
  never_got_mea1==1 & age_at_pneu3 > mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_pneu3) & age_at_pneu3 < age_at_pneu2 ~ age_at_pneu3,
  never_got_mea1==1 & age_at_rota1 > mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_rota1) & age_at_rota1 < age_at_pneu3 ~ age_at_rota1,
  never_got_mea1==1 & age_at_rota2 > mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_rota2) & age_at_rota2 < age_at_rota1 ~ age_at_rota2,
  never_got_mea1==1 & age_at_rota3 > mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_rota3) & age_at_rota3 < age_at_rota2 ~ age_at_rota3,
  never_got_mea1==1 & age_at_poln  > mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_poln) & age_at_poln < age_at_rota3 ~ age_at_poln,
  never_got_mea1==1 & age_at_hepb1 > mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_hepb1) & age_at_hepb1 < age_at_poln ~ age_at_hepb1,
  never_got_mea1==1 & age_at_hepb1 > mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_hepb1) & age_at_hepb1 < age_at_poln ~ age_at_hepb1,
  never_got_mea1==1 & age_at_hepb2 > mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_hepb2) & age_at_hepb2 < age_at_hepb1 ~ age_at_hepb2,
  never_got_mea1==1 & age_at_hepb3 > mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_hepb3) & age_at_hepb3 < age_at_hepb2 ~ age_at_hepb3,
  never_got_mea1==1 & age_at_hib1  > mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_hib1) & age_at_hib1 < age_at_hepb3 ~ age_at_hib1,
  never_got_mea1==1 & age_at_hib2  > mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_hib2) & age_at_hib2 < age_at_hib1 ~ age_at_hib2,
  never_got_mea1==1 & age_at_hib3  > mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_hib3) & age_at_hib3 < age_at_hib2 ~ age_at_hib3,
  never_got_mea1==1 & age_at_yelfev  > mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_yelfev) & age_at_yelfev < age_at_yelfev ~ age_at_yelfev,
  never_got_mea1==1 & age_at_bcg   > mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_bcg) ~ age_at_bcg))

# Case 2: The Child got Mea1 late, but there was another visit for another vaccine in between the mea1 due age and actual age at Mea1
dt <- dt %>% mutate(earliest_visit_btwn_mea1 = case_when(
  
  # Need to calculate the age at earliest visit between measles1 due age and first measles1 vaccination
  mea1_late==1 & age_at_dpt1 > mea1_age_due_min & age_at_dpt1 < mea1_age_at_counted_vac & !is.na(mea1_age_at_counted_vac) & age_at_dpt1 < age_at_bcg & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_dpt1) & age_at_dpt1<age_at_bcg ~ age_at_dpt1,
  mea1_late==1 & age_at_pol1 > mea1_age_due_min & age_at_pol1 < mea1_age_at_counted_vac & !is.na(mea1_age_at_counted_vac) & age_at_pol1 < age_at_dpt1 & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_pol1) & age_at_pol1<age_at_dpt1 ~ age_at_pol1,
  mea1_late==1 & age_at_dpt2 > mea1_age_due_min & age_at_dpt2 < mea1_age_at_counted_vac & !is.na(mea1_age_at_counted_vac) & age_at_dpt2 < age_at_pol1 & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_dpt2) & age_at_dpt2<age_at_pol1 ~ age_at_dpt2,
  mea1_late==1 & age_at_pol2 > mea1_age_due_min & age_at_pol2 < mea1_age_at_counted_vac & !is.na(mea1_age_at_counted_vac) & age_at_pol2 < age_at_dpt2 & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_pol2) & age_at_pol2<age_at_dpt2 ~ age_at_pol2,
  mea1_late==1 & age_at_dpt3 > mea1_age_due_min & age_at_dpt3 < mea1_age_at_counted_vac & !is.na(mea1_age_at_counted_vac) & age_at_dpt3 < age_at_pol2 & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_dpt3) & age_at_dpt3<age_at_pol2 ~ age_at_dpt3,
  mea1_late==1 & age_at_pol3 > mea1_age_due_min & age_at_pol3 < mea1_age_at_counted_vac & !is.na(mea1_age_at_counted_vac) & age_at_pol3 < age_at_dpt3 & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_pol3) & age_at_pol3<age_at_dpt3 ~ age_at_pol3,
  mea1_late==1 & age_at_mea2 > mea1_age_due_min & age_at_mea2 < mea1_age_at_counted_vac & !is.na(mea1_age_at_counted_vac) & age_at_mea2 < age_at_pol3 & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_mea2) & age_at_mea2<age_at_pol3 ~ age_at_mea2,
  mea1_late==1 & age_at_pol0 > mea1_age_due_min & age_at_pol0 < mea1_age_at_counted_vac & !is.na(mea1_age_at_counted_vac) & age_at_pol0 < age_at_mea2 & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_pol0) & age_at_pol0<age_at_mea2 ~ age_at_pol0,
  mea1_late==1 & age_at_pent1 > mea1_age_due_min & age_at_pent1 < mea1_age_at_counted_vac & !is.na(mea1_age_at_counted_vac) & age_at_pent1 < age_at_pol0 & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_pent1) & age_at_pent1<age_at_pol0 ~ age_at_pent1,
  mea1_late==1 & age_at_pent2 > mea1_age_due_min & age_at_pent2 < mea1_age_at_counted_vac & !is.na(mea1_age_at_counted_vac) & age_at_pent2 < age_at_pent1 & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_pent2) & age_at_pent2<age_at_pent1 ~ age_at_pent2,
  mea1_late==1 & age_at_pent3 > mea1_age_due_min & age_at_pent3 < mea1_age_at_counted_vac & !is.na(mea1_age_at_counted_vac) & age_at_pent3 < age_at_pent2 & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_pent3) & age_at_pent3<age_at_pent2 ~ age_at_pent3,
  mea1_late==1 & age_at_pneu1 > mea1_age_due_min & age_at_pneu1 < mea1_age_at_counted_vac & !is.na(mea1_age_at_counted_vac) & age_at_pneu1 < age_at_pent3 & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_pneu1) & age_at_pneu1<age_at_pent3 ~ age_at_pneu1,
  mea1_late==1 & age_at_pneu2 > mea1_age_due_min & age_at_pneu2 < mea1_age_at_counted_vac & !is.na(mea1_age_at_counted_vac) & age_at_pneu2 < age_at_pneu1 & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_pneu2) & age_at_pneu2<age_at_pneu1 ~ age_at_pneu2,
  mea1_late==1 & age_at_pneu3 > mea1_age_due_min & age_at_pneu3 < mea1_age_at_counted_vac & !is.na(mea1_age_at_counted_vac) & age_at_pneu3 < age_at_pneu2 & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_pneu3) & age_at_pneu3<age_at_pneu2 ~ age_at_pneu3,
  mea1_late==1 & age_at_rota1 > mea1_age_due_min & age_at_rota1 < mea1_age_at_counted_vac & !is.na(mea1_age_at_counted_vac) & age_at_rota1 < age_at_pneu3 & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_rota1) & age_at_rota1<age_at_pneu3 ~ age_at_rota1,
  mea1_late==1 & age_at_rota2 > mea1_age_due_min & age_at_rota2 < mea1_age_at_counted_vac & !is.na(mea1_age_at_counted_vac) & age_at_rota2 < age_at_rota1 & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_rota2) & age_at_rota2<age_at_rota1 ~ age_at_rota2,
  mea1_late==1 & age_at_rota3 > mea1_age_due_min & age_at_rota3 < mea1_age_at_counted_vac & !is.na(mea1_age_at_counted_vac) & age_at_rota3 < age_at_rota2 & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_rota3) & age_at_rota3<age_at_rota2 ~ age_at_rota3,
  mea1_late==1 & age_at_poln > mea1_age_due_min & age_at_poln < mea1_age_at_counted_vac & !is.na(mea1_age_at_counted_vac) & age_at_poln < age_at_rota3 & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_poln) & age_at_poln<age_at_rota3 ~ age_at_poln,
  mea1_late==1 & age_at_hepb1 > mea1_age_due_min & age_at_hepb1 < mea1_age_at_counted_vac & !is.na(mea1_age_at_counted_vac) & age_at_hepb1 < age_at_poln & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_hepb1) & age_at_hepb1<age_at_poln ~ age_at_hepb1,
  mea1_late==1 & age_at_hepb2 > mea1_age_due_min & age_at_hepb2 < mea1_age_at_counted_vac & !is.na(mea1_age_at_counted_vac) & age_at_hepb2 < age_at_hepb1 & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_hepb2) & age_at_hepb2<age_at_hepb1 ~ age_at_hepb2,
  mea1_late==1 & age_at_hepb3 > mea1_age_due_min & age_at_hepb3 < mea1_age_at_counted_vac & !is.na(mea1_age_at_counted_vac) & age_at_hepb3 < age_at_hepb2 & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_hepb3) & age_at_hepb3<age_at_hepb2 ~ age_at_hepb3,
  mea1_late==1 & age_at_hib1 > mea1_age_due_min & age_at_hib1 < mea1_age_at_counted_vac & !is.na(mea1_age_at_counted_vac) & age_at_hib1 < age_at_hepb3 & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_hib1) & age_at_hib1<age_at_hepb3 ~ age_at_hib1,
  mea1_late==1 & age_at_hib2 > mea1_age_due_min & age_at_hib2 < mea1_age_at_counted_vac & !is.na(mea1_age_at_counted_vac) & age_at_hib2 < age_at_hib1 & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_hib2) & age_at_hib2<age_at_hib1 ~ age_at_hib2,
  mea1_late==1 & age_at_hib3 > mea1_age_due_min & age_at_hib3 < mea1_age_at_counted_vac & !is.na(mea1_age_at_counted_vac) & age_at_hib3 < age_at_hib2 & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_hib3) & age_at_hib3<age_at_hib2 ~ age_at_hib3,
  mea1_late==1 & age_at_yelfev > mea1_age_due_min & age_at_yelfev < mea1_age_at_counted_vac & !is.na(mea1_age_at_counted_vac) & age_at_yelfev < age_at_hib2 & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_yelfev) & age_at_hib3<age_at_hib3 ~ age_at_yelfev,
  mea1_late==1 & age_at_bcg > mea1_age_due_min & age_at_bcg < mea1_age_at_counted_vac & !is.na(mea1_age_at_counted_vac)  & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_bcg) ~ age_at_bcg))


# calculate WHO HAS A MISSED OPPORTUNITY & WHAT IS POTENTIAL COVERAGE?

dt <- dt %>% mutate(
  
  # variable that indicates missed opportunities
  mea1_missed_opportunity = case_when(
    
    # CASE 1: the child never got measles vaccination but had another vaccination visit during or after measles vaccine was due
    never_got_mea1==1 & age_at_oldest_visit>mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_oldest_visit) ~ 1,
    
    # replace measles missed opportunity with 1 if they had an earlier visit that they could have attended
    !is.na(earliest_visit_btwn_mea1) & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) ~ 1,
    
    # default value for missed opportunities
    age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) ~ 0
    
  ))


dt <- dt %>% mutate(
  # we want to know how old the child was when they could have gotten Mea1
  mea1_age_at_mop_vac = case_when(
    !is.na(earliest_visit_btwn_mea1) ~ earliest_visit_btwn_mea1,
    is.na(earliest_visit_btwn_mea1) ~ no_mea1_mop_age))

# calculate when the child's first vaccination after each dose age was -- the way we do this will account for both DPT and non-DPT vaccinations
dt <- dt %>% mutate(potential_dpt_6wks = case_when(
  age_at_pol1 >= dpt1_age_due_min & age_at_pol1<age_at_bcg & age_in_days>= dpt3_age_due_max & !is.na(age_at_pol1) ~ age_at_pol1,
  age_at_pol2 >= dpt1_age_due_min & age_at_pol2<age_at_pol1 & age_in_days>= dpt3_age_due_max & !is.na(age_at_pol2) ~ age_at_pol2,
  age_at_pol3 >= dpt1_age_due_min & age_at_pol3<age_at_pol2 & age_in_days>= dpt3_age_due_max & !is.na(age_at_pol3) ~ age_at_pol3,
  age_at_mea1 >= dpt1_age_due_min & age_at_mea1<age_at_pol3 & age_in_days>= dpt3_age_due_max & !is.na(age_at_mea1) ~ age_at_mea1,
  age_at_pol0 >= dpt1_age_due_min & age_at_pol0<age_at_mea1 & age_in_days>= dpt3_age_due_max & !is.na(age_at_pol0) ~ age_at_pol0,
  age_at_pneu1 >= dpt1_age_due_min & age_at_pneu1<age_at_pol0 & age_in_days>= dpt3_age_due_max & !is.na(age_at_pneu1) ~ age_at_pneu1,
  age_at_pneu2 >= dpt1_age_due_min & age_at_pneu2<age_at_pneu1 & age_in_days>= dpt3_age_due_max & !is.na(age_at_pneu2) ~ age_at_pneu2,
  age_at_mea2 >= dpt1_age_due_min & age_at_mea2<age_at_pneu2 & age_in_days>= dpt3_age_due_max & !is.na(age_at_mea2) ~ age_at_mea2,
  age_at_pneu3 >= dpt1_age_due_min & age_at_pneu3<age_at_mea2 & age_in_days>= dpt3_age_due_max & !is.na(age_at_pneu3) ~ age_at_pneu3,
  age_at_rota1 >= dpt1_age_due_min & age_at_rota1<age_at_pneu3 & age_in_days>= dpt3_age_due_max & !is.na(age_at_rota1) ~ age_at_rota1,
  age_at_rota2 >= dpt1_age_due_min & age_at_rota2<age_at_rota1 & age_in_days>= dpt3_age_due_max & !is.na(age_at_rota2) ~ age_at_rota2,
  age_at_rota3 >= dpt1_age_due_min & age_at_rota3<age_at_rota2 & age_in_days>= dpt3_age_due_max & !is.na(age_at_rota3) ~ age_at_rota3,
  age_at_poln >= dpt1_age_due_min & age_at_poln<age_at_rota3 & age_in_days>= dpt3_age_due_max & !is.na(age_at_poln) ~ age_at_poln,
  age_at_hepb1 >= dpt1_age_due_min & age_at_hepb1<age_at_poln & age_in_days>= dpt3_age_due_max & !is.na(age_at_hepb1) ~ age_at_hepb1,
  age_at_hepb2 >= dpt1_age_due_min & age_at_hepb2<age_at_hepb1 & age_in_days>= dpt3_age_due_max & !is.na(age_at_hepb2) ~ age_at_hepb2,
  age_at_hepb3 >= dpt1_age_due_min & age_at_hepb3<age_at_hepb2 & age_in_days>= dpt3_age_due_max & !is.na(age_at_hepb3) ~ age_at_hepb3,
  age_at_hib1 >= dpt1_age_due_min & age_at_hib1<age_at_hepb3 & age_in_days>= dpt3_age_due_max & !is.na(age_at_hib1) ~ age_at_hib1,
  age_at_hib2 >= dpt1_age_due_min & age_at_hib2<age_at_hib1 & age_in_days>= dpt3_age_due_max & !is.na(age_at_hib2) ~ age_at_hib2,
  age_at_hib3 >= dpt1_age_due_min & age_at_hib3<age_at_hib2 & age_in_days>= dpt3_age_due_max & !is.na(age_at_hib3) ~ age_at_hib3,
  age_at_yelfev >= dpt1_age_due_min & age_at_yelfev<age_at_hib3 & age_in_days>= dpt3_age_due_max & !is.na(age_at_yelfev) ~ age_at_yelfev,
  age_at_bcg >= dpt1_age_due_min & age_in_days>= dpt3_age_due_max & !is.na(age_at_bcg) ~ age_at_bcg))

dt <- dt %>% mutate(potential_dpt_10wks = case_when(
  
  age_at_pol1 >= dpt2_age_due_min & age_at_pol1<age_at_bcg & age_in_days>= dpt3_age_due_max & !is.na(age_at_pol1) & (age_at_pol1!=potential_dpt_6wks|is.na(potential_dpt_6wks)) ~ age_at_pol1,
  age_at_pol2 >= dpt2_age_due_min & age_at_pol2<age_at_pol1 & age_in_days>= dpt3_age_due_max & !is.na(age_at_pol2) & (age_at_pol2!=potential_dpt_6wks|is.na(potential_dpt_6wks))~ age_at_pol2,
  age_at_pol3 >= dpt2_age_due_min & age_at_pol3<age_at_pol2 & age_in_days>= dpt3_age_due_max & !is.na(age_at_pol3) & (age_at_pol3!=potential_dpt_6wks|is.na(potential_dpt_6wks)) ~ age_at_pol3,
  age_at_mea1 >= dpt2_age_due_min & age_at_mea1<age_at_pol3 & age_in_days>= dpt3_age_due_max & !is.na(age_at_mea1) & (age_at_mea1!=potential_dpt_6wks|is.na(potential_dpt_6wks)) ~ age_at_mea1,
  age_at_pol0 >= dpt2_age_due_min & age_at_pol0<age_at_mea1 & age_in_days>= dpt3_age_due_max & !is.na(age_at_pol0) & (age_at_pol0!=potential_dpt_6wks|is.na(potential_dpt_6wks)) ~ age_at_pol0,
  age_at_pneu1 >= dpt2_age_due_min & age_at_pneu1<age_at_pol0 & age_in_days>= dpt3_age_due_max & !is.na(age_at_pneu1) & (age_at_pneu1!=potential_dpt_6wks|is.na(potential_dpt_6wks)) ~ age_at_pneu1,
  age_at_pneu2 >= dpt2_age_due_min & age_at_pneu2<age_at_pneu1 & age_in_days>= dpt3_age_due_max & !is.na(age_at_pneu2) & (age_at_pneu2!=potential_dpt_6wks|is.na(potential_dpt_6wks)) ~ age_at_pneu2,
  age_at_mea2 >= dpt2_age_due_min & age_at_mea2<age_at_pneu2 & age_in_days>= dpt3_age_due_max & !is.na(age_at_mea2) & (age_at_mea2!=potential_dpt_6wks|is.na(potential_dpt_6wks)) ~ age_at_mea2,
  age_at_pneu3 >= dpt2_age_due_min & age_at_pneu3<age_at_mea2 & age_in_days>= dpt3_age_due_max & !is.na(age_at_pneu3) & (age_at_pneu3!=potential_dpt_6wks|is.na(potential_dpt_6wks)) ~ age_at_pneu3,
  age_at_rota1 >= dpt2_age_due_min & age_at_rota1<age_at_pneu3 & age_in_days>= dpt3_age_due_max & !is.na(age_at_rota1) & (age_at_rota1!=potential_dpt_6wks|is.na(potential_dpt_6wks)) ~ age_at_rota1,
  age_at_rota2 >= dpt2_age_due_min & age_at_rota2<age_at_rota1 & age_in_days>= dpt3_age_due_max & !is.na(age_at_rota2) & (age_at_rota2!=potential_dpt_6wks|is.na(potential_dpt_6wks)) ~ age_at_rota2,
  age_at_rota3 >= dpt2_age_due_min & age_at_rota3<age_at_rota2 & age_in_days>= dpt3_age_due_max & !is.na(age_at_rota3) & (age_at_rota3!=potential_dpt_6wks|is.na(potential_dpt_6wks)) ~ age_at_rota3,
  age_at_poln >= dpt2_age_due_min & age_at_poln<age_at_rota3 & age_in_days>= dpt3_age_due_max & !is.na(age_at_poln) & (age_at_poln!=potential_dpt_6wks|is.na(potential_dpt_6wks)) ~ age_at_poln,
  age_at_hepb1 >= dpt2_age_due_min & age_at_hepb1<age_at_poln & age_in_days>= dpt3_age_due_max & !is.na(age_at_hepb1) & (age_at_hepb1!=potential_dpt_6wks|is.na(potential_dpt_6wks)) ~ age_at_hepb1,
  age_at_hepb2 >= dpt2_age_due_min & age_at_hepb2<age_at_hepb1 & age_in_days>= dpt3_age_due_max & !is.na(age_at_hepb2) & (age_at_hepb2!=potential_dpt_6wks|is.na(potential_dpt_6wks)) ~ age_at_hepb2,
  age_at_hepb3 >= dpt2_age_due_min & age_at_hepb3<age_at_hepb2 & age_in_days>= dpt3_age_due_max & !is.na(age_at_hepb3) & (age_at_hepb3!=potential_dpt_6wks|is.na(potential_dpt_6wks)) ~ age_at_hepb3,
  age_at_hib1 >= dpt2_age_due_min & age_at_hib1<age_at_hepb3 & age_in_days>= dpt3_age_due_max & !is.na(age_at_hib1) & (age_at_hib1!=potential_dpt_6wks|is.na(potential_dpt_6wks)) ~ age_at_hib1,
  age_at_hib2 >= dpt2_age_due_min & age_at_hib2<age_at_hib1 & age_in_days>= dpt3_age_due_max & !is.na(age_at_hib2) & (age_at_hib2!=potential_dpt_6wks|is.na(potential_dpt_6wks)) ~ age_at_hib2,
  age_at_hib3 >= dpt2_age_due_min & age_at_hib3<age_at_hib2 & age_in_days>= dpt3_age_due_max & !is.na(age_at_hib3) & (age_at_hib3!=potential_dpt_6wks|is.na(potential_dpt_6wks)) ~ age_at_hib3,
  age_at_yelfev >= dpt2_age_due_min & age_at_yelfev<age_at_hib3 & age_in_days>= dpt3_age_due_max & !is.na(age_at_yelfev) & (age_at_yelfev!=potential_dpt_6wks|is.na(potential_dpt_6wks)) ~ age_at_yelfev,
  age_at_bcg >= dpt2_age_due_min & age_in_days>= dpt3_age_due_max & !is.na(age_at_bcg) & (age_at_bcg!=potential_dpt_6wks|is.na(potential_dpt_6wks)) ~ age_at_bcg))

dt <- dt %>% mutate(potential_dpt_14wks = case_when(
  age_at_pol1 >= dpt3_age_due_min & age_at_pol1<age_at_bcg & age_in_days>= dpt3_age_due_max & !is.na(age_at_pol1) & (age_at_pol1!=potential_dpt_6wks|is.na(potential_dpt_6wks)) & (age_at_pol1!=potential_dpt_10wks|is.na(potential_dpt_10wks)) ~ age_at_pol1,
  age_at_pol2 >= dpt3_age_due_min & age_at_pol2<age_at_pol1 & age_in_days>= dpt3_age_due_max & !is.na(age_at_pol2) & (age_at_pol2!=potential_dpt_6wks|is.na(potential_dpt_6wks)) & (age_at_pol2!=potential_dpt_10wks|is.na(potential_dpt_10wks)) ~ age_at_pol2,
  age_at_pol3 >= dpt3_age_due_min & age_at_pol3<age_at_pol2 & age_in_days>= dpt3_age_due_max & !is.na(age_at_pol3) & (age_at_pol3!=potential_dpt_6wks|is.na(potential_dpt_6wks)) & (age_at_pol3!=potential_dpt_10wks|is.na(potential_dpt_10wks)) ~ age_at_pol3,
  age_at_mea1 >= dpt3_age_due_min & age_at_mea1<age_at_pol3 & age_in_days>= dpt3_age_due_max & !is.na(age_at_mea1) & (age_at_mea1!=potential_dpt_6wks|is.na(potential_dpt_6wks)) & (age_at_mea1!=potential_dpt_10wks|is.na(potential_dpt_10wks)) ~ age_at_mea1,
  age_at_pol0 >= dpt3_age_due_min & age_at_pol0<age_at_mea1 & age_in_days>= dpt3_age_due_max & !is.na(age_at_pol0) & (age_at_pol0!=potential_dpt_6wks|is.na(potential_dpt_6wks)) & (age_at_pol0!=potential_dpt_10wks|is.na(potential_dpt_10wks)) ~ age_at_pol0,
  age_at_pneu1 >= dpt3_age_due_min & age_at_pneu1<age_at_pol0 & age_in_days>= dpt3_age_due_max & !is.na(age_at_pneu1) & (age_at_pneu1!=potential_dpt_6wks|is.na(potential_dpt_6wks)) & (age_at_pneu1!=potential_dpt_10wks|is.na(potential_dpt_10wks)) ~ age_at_pneu1,
  age_at_pneu2 >= dpt3_age_due_min & age_at_pneu2<age_at_pneu1 & age_in_days>= dpt3_age_due_max & !is.na(age_at_pneu2) & (age_at_pneu2!=potential_dpt_6wks|is.na(potential_dpt_6wks)) & (age_at_pneu2!=potential_dpt_10wks|is.na(potential_dpt_10wks)) ~ age_at_pneu2,
  age_at_mea2 >= dpt3_age_due_min & age_at_mea2<age_at_pneu2 & age_in_days>= dpt3_age_due_max & !is.na(age_at_mea2) & (age_at_mea2!=potential_dpt_6wks|is.na(potential_dpt_6wks)) & (age_at_mea2!=potential_dpt_10wks|is.na(potential_dpt_10wks)) ~ age_at_mea2,
  age_at_pneu3 >= dpt3_age_due_min & age_at_pneu3<age_at_mea2 & age_in_days>= dpt3_age_due_max & !is.na(age_at_pneu3) & (age_at_pneu3!=potential_dpt_6wks|is.na(potential_dpt_6wks)) & (age_at_pneu3!=potential_dpt_10wks|is.na(potential_dpt_10wks)) ~ age_at_pneu3,
  age_at_rota1 >= dpt3_age_due_min & age_at_rota1<age_at_pneu3 & age_in_days>= dpt3_age_due_max & !is.na(age_at_rota1) & (age_at_rota1!=potential_dpt_6wks|is.na(potential_dpt_6wks)) & (age_at_rota1!=potential_dpt_10wks|is.na(potential_dpt_10wks)) ~ age_at_rota1,
  age_at_rota2 >= dpt3_age_due_min & age_at_rota2<age_at_rota1 & age_in_days>= dpt3_age_due_max & !is.na(age_at_rota2) & (age_at_rota2!=potential_dpt_6wks|is.na(potential_dpt_6wks)) & (age_at_rota2!=potential_dpt_10wks|is.na(potential_dpt_10wks)) ~ age_at_rota2,
  age_at_rota3 >= dpt3_age_due_min & age_at_rota3<age_at_rota2 & age_in_days>= dpt3_age_due_max & !is.na(age_at_rota3) & (age_at_rota3!=potential_dpt_6wks|is.na(potential_dpt_6wks)) & (age_at_rota3!=potential_dpt_10wks|is.na(potential_dpt_10wks)) ~ age_at_rota3,
  age_at_poln >= dpt3_age_due_min & age_at_poln<age_at_rota3 & age_in_days>= dpt3_age_due_max & !is.na(age_at_poln) & (age_at_poln!=potential_dpt_6wks|is.na(potential_dpt_6wks)) & (age_at_poln!=potential_dpt_10wks|is.na(potential_dpt_10wks)) ~ age_at_poln,
  age_at_hepb1 >= dpt3_age_due_min & age_at_hepb1<age_at_poln & age_in_days>= dpt3_age_due_max & !is.na(age_at_hepb1) & (age_at_hepb1!=potential_dpt_6wks|is.na(potential_dpt_6wks)) & (age_at_hepb1!=potential_dpt_10wks|is.na(potential_dpt_10wks)) ~ age_at_hepb1,
  age_at_hepb2 >= dpt3_age_due_min & age_at_hepb2<age_at_hepb1 & age_in_days>= dpt3_age_due_max & !is.na(age_at_hepb2) & (age_at_hepb2!=potential_dpt_6wks|is.na(potential_dpt_6wks)) & (age_at_hepb2!=potential_dpt_10wks|is.na(potential_dpt_10wks)) ~ age_at_hepb2,
  age_at_hepb3 >= dpt3_age_due_min & age_at_hepb3<age_at_hepb2 & age_in_days>= dpt3_age_due_max & !is.na(age_at_hepb3) & (age_at_hepb3!=potential_dpt_6wks|is.na(potential_dpt_6wks)) & (age_at_hepb3!=potential_dpt_10wks|is.na(potential_dpt_10wks)) ~ age_at_hepb3,
  age_at_hib1 >= dpt3_age_due_min & age_at_hib1<age_at_hepb3 & age_in_days>= dpt3_age_due_max & !is.na(age_at_hib1) & (age_at_hib1!=potential_dpt_6wks|is.na(potential_dpt_6wks)) & (age_at_hib1!=potential_dpt_10wks|is.na(potential_dpt_10wks)) ~ age_at_hib1,
  age_at_hib2 >= dpt3_age_due_min & age_at_hib2<age_at_hib1 & age_in_days>= dpt3_age_due_max & !is.na(age_at_hib2) & (age_at_hib2!=potential_dpt_6wks|is.na(potential_dpt_6wks)) & (age_at_hib2!=potential_dpt_10wks|is.na(potential_dpt_10wks)) ~ age_at_hib2,
  age_at_hib3 >= dpt3_age_due_min & age_at_hib3<age_at_hib2 & age_in_days>= dpt3_age_due_max & !is.na(age_at_hib3) & (age_at_hib3!=potential_dpt_6wks|is.na(potential_dpt_6wks)) & (age_at_hib3!=potential_dpt_10wks|is.na(potential_dpt_10wks)) ~ age_at_hib3,
  age_at_yelfev >= dpt3_age_due_min & age_at_yelfev<age_at_hib3 & age_in_days>= dpt3_age_due_max & !is.na(age_at_yelfev) & (age_at_yelfev!=potential_dpt_6wks|is.na(potential_dpt_6wks)) & (age_at_yelfev!=potential_dpt_10wks|is.na(potential_dpt_10wks)) ~ age_at_yelfev,
  age_at_bcg >= dpt3_age_due_min & age_in_days>= dpt3_age_due_max & !is.na(age_at_bcg) & (age_at_bcg!=potential_dpt_6wks|is.na(potential_dpt_6wks)) ~ age_at_bcg))

# calculate missed opportunities for DPT vaccines
dt <- dt %>% mutate(
  
  dpt_missed_opportunity=case_when(
    
    # Case 3: The child was vaccinated for DPT late and had an earlier dose (can this be combined with Case 2?)
    dpt_late==1 & potential_dpt_14wks<first_elig_dpt_after_14wks & !is.na(first_elig_dpt_after_14wks) & !is.na(potential_dpt_14wks) & has_health_card_bin=="yes" & age_in_days>=dpt3_age_due_max ~ 1,
    
    # Case 1: The child was never vaccinated for DPT, but had at least 3 visits 4 weeks apart during or after the DPT due ages (by dose)  
    never_got_dpt==1 & !is.na(potential_dpt_14wks) & has_health_card_bin=="Yes" & age_in_days>=dpt3_age_due_max ~ 1,
    
    # Case 2: The child WAS vaccinated for DPT, but did not have 3 doses with correct spacing by age 6 mos 
    too_few_elig_dpt==1 & !is.na(potential_dpt_14wks) & has_health_card_bin=="Yes" & age_in_days>=dpt3_age_due_max ~ 1,
    
    # default value
    age_in_days>=dpt3_age_due_max & !is.na(dpt_days_at_risk) ~ 0),
  
  dpt_age_at_mop_vac = case_when(
    
    # Case 3: The child was vaccinated for DPT late and had an earlier dose (can this be combined with Case 2?)
    dpt_late==1 & potential_dpt_14wks<first_elig_dpt_after_14wks & !is.na(first_elig_dpt_after_14wks) & !is.na(potential_dpt_14wks) & has_health_card_bin=="Yes" & age_in_days>= dpt3_age_due_max ~ potential_dpt_14wks,
    
    # Case 1: The child was never vaccinated for DPT, but had at least 3 visits 4 weeks apart during or after the DPT due ages (by dose)  
    never_got_dpt==1 & !is.na(potential_dpt_14wks) & has_health_card_bin=="Yes" & age_in_days>=dpt3_age_due_max ~ potential_dpt_14wks,
    
    # Case 2: The child WAS vaccinated for DPT, but did not have 3 doses with correct spacing by age 6 mos 
    too_few_elig_dpt==1 & !is.na(potential_dpt_14wks) & has_health_card_bin=="Yes" & age_in_days>=dpt3_age_due_max ~ potential_dpt_14wks))

# COMPUTE DAYS AT RISK IF OPPORTUNITY WAS NOT MISSED 

# measles 
dt <- dt %>% mutate(
  mea1_mop_age_minus_max = case_when(
    # if child did have MO, then we need to calculate days at risk with our new number 
    !is.na(earliest_visit_btwn_mea1) & mea1_missed_opportunity==1 ~ earliest_visit_btwn_mea1 - mea1_age_due_max))

dt <- dt %>% mutate(
  mea1_days_at_risk_mop = case_when(
    # If the kid didn't have a missed opportunity, then the days at risk stays the same
    mea1_missed_opportunity==0 ~ mea1_days_at_risk,
    
    # if they did add the new days of risk
    mea1_mop_age_minus_max>0 & age_in_days>=mea1_age_due_max & mea1_missed_opportunity==1 ~ mea1_mop_age_minus_max))


# DPT3 

# if kid has missed opportunity, calculate a new total days at risk
dt <- dt %>% mutate(
  dpt_mop_age_minus_max = case_when(
    !is.na(dpt_age_at_mop_vac) & dpt_missed_opportunity==1 ~ dpt_age_at_mop_vac - dpt3_age_due_max))

dt <- dt %>% mutate(
  dpt_days_at_risk_mop = case_when(
    # if they did have a  MO, add enw days of risk
    dpt_mop_age_minus_max>0 & age_in_days>=dpt3_age_due_max & dpt_missed_opportunity==1 ~ dpt_mop_age_minus_max,
    
    # if the child didn't have a missed opportunity then the days of risk stays the same
    dpt_missed_opportunity==0 ~ dpt_days_at_risk))

##### data checks to make sure variables are not implausible
# hist(dt$mea1_days_at_risk)
# hist(dt$dpt_days_at_risk)
# table(dt$early_mea1, dt$mea1_within_interval, useNA = 'always')
# table(dt$early_mea1, dt$mea1_late, useNA = 'always')
# table(dt$mea1_within_interval, dt$mea1_late, useNA = 'always')
# hist(dt$mea1_days_at_risk_mop)
# hist(dt$dpt_days_at_risk_mop)

# # // these should be mutually exclusive: dpt_within_interval, too_few_elig_dpt, never_got_dpt, dpt_late
# table(dt$dpt_within_interval, dt$too_few_elig_dpt, useNA = 'always')
# table(dt$dpt_within_interval, dt$dpt_late, useNA = 'always')
# table(dt$dpt_within_interval, dt$never_got_dpt, useNA = 'always')
# table(dt$dpt_late, dt$never_got_dpt, useNA = 'always')
# table(dt$too_few_elig_dpt, dt$never_got_dpt, useNA = 'always')
# table(dt$too_few_elig_dpt, dt$dpt_late, useNA = 'always')

# make visuals to explor implausible values in the dataset
# Make histograms
# organize series and label for graphing
codebookFile <- paste0(codebook_directory, 'dhs_mov_codebook.xlsx')
codebook <- as.data.table(read_xlsx(codebookFile))
dataVariables = unique(codebook[Category=="derived variable" & Class=="numeric" & `Possible Values`=="many possible values"]$Variable)
# labelTable <- unique(codebook[,.(Variable)])


histograms = lapply(dataVariables, function(v){
ggplot(dt, aes_string(x=v)) + 
  geom_histogram() +
    facet_wrap(~v000)
})


# save histograms as a PDF
outputFile6a2 <- paste0(visDir, "aim_1/missed_opportunities/6a_prepped_dhs_data_histograms.pdf") 

print(paste('Saving:', outputFile6a2)) 
pdf(outputFile6a2, height=5.5, width=9)
for(i in seq(length(histograms))) { 
  print(histograms[[i]])
}
dev.off()

# transformed data as seen by the model
# histograms = lapply(modelVars, function(v) {
#   l = nodeTable[variable==v]$label
#   ggplot(data(), aes_string(x=v)) + 
#     geom_histogram() + 
#     facet_wrap(~region, scales='free') + 
#     labs(title=paste('Histograms of', l), y='Frequency', x=l, 
#          subtitle=paste('Random Sample of', n, 'Health Zones'),
#          caption='Variables are post-transformation. Transformations may include: 
# 			cumulative, log, logit and lag.') + 
#     theme_bw()
# })

# untransformed data
# histograms_untr = lapply(modelVars, function(v) {
#   l = nodeTable[variable==v]$label
#   for(ext in c('_cumulative','lag_','lead_')) {
#     if (grepl(ext, v)) v = gsub(ext,'',v) 
#   }
#   if (!v %in% names(sample_untr)) v = paste0('value_',v) 
#   ggplot(sample_untr, aes_string(x=v)) + 
#     geom_histogram() + 
#     facet_wrap(~region, scales='free') + 
#     labs(title=paste('Histograms of', l, '(Without Transformation)'), 
#          y='Frequency', x=l, 
#          subtitle=paste('Random Sample of', n, 'Regions'), 
#          caption='Variables are pre-transformation.') + 
#     theme_bw()
# })
# -------------------------------------------------------------------

# 
# View(dt %>% select(age_in_days, dpt_days_at_risk_mop, dpt1, dpt2, dpt3, dpt_within_interval, dpt_missed_opportunity, age_at_dpt1, age_at_dpt2, age_at_dpt3, dpt1_age_due_min))

# save output
saveRDS(dt, outputFile6a)

# print final statement
print("Step 2d: Prepping of dhs vaccination data completed.")
