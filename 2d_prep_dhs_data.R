# ----------------------------------------------
# AUTHOR: Francisco Rios
# PURPOSE: Extract all necessary DHS data
# DATE: Last updated August 24 2021

# set boolean in case you want to extract dhs data again or simply work on prepping previously extracted data
extract_again = FALSE

# Read in list of files to prep
file_list <- data.table(read_excel(paste0(data_dir, "data_file_list.xlsx")))

# subset files to dhs data
file_list <- file_list[data_source%in%c("dhs6", "dhs7"),.(file_name, data_type, data_source, year, containing_folder, location_name)]

if (extract_again==TRUE){
  # run loop to extract the data from the file list
  
  for(i in 1:nrow(file_list)){
    # Set up file path 
    file_dir = paste0(data_dir, 'raw_data/', file_list$data_type[i], '/dhs/', file_list$containing_folder[i], '/' )
  
    # set up arguments
    args <- list(file_dir, file_list$file_name[i], file_list$containing_folder[i], file_list$data_source[i])
  
    ### RUN THE PREP FUNCTION HERE ###
    tmpData = do.call(extract_dhs_data, args)
  
    #Add indexing data
    append_cols = file_list[i, .(file_name)]
  
    stopifnot(nrow(append_cols)==1)
  
    tmpData = cbind(tmpData, append_cols)
  
    #Bind data together 
    if(i==1){
      extracted_dhs_data = tmpData
    } else {
      extracted_dhs_data = rbind.fill(extracted_dhs_data, tmpData)
    }
    print("Now prepping:")
    print(paste0(i, " ", file_list$location_name[i], " ", file_list$data_source[i], " ", file_list$file_name[i])) ## if the code breaks, you know which file it broke on
  }
  
  # save raw extracted data 
  saveRDS(extracted_dhs_data, outputFile2d)
  
} else if (extract_again==FALSE){
  
  # read in the previously extracted data file 
  dt <- readRDS(outputFile2d)
  
}

####################################################
# 1. recode variables into new formats with labels
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

# assets

# each survey should have either v190a or v190 for the household assets
dt$assets <- ifelse(!is.na(dt$v190a), dt$v190a, dt$v190)

dt$assets <- as.factor(dt$assets)

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

# asign labels to variable names


####################################################
# 2. calculate new variables necessary to calculate risk
####################################################

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

# calculate the age of child in days
dt$age_in_days <- time_length(difftime(dt$intv_date, dt$dob), "days")

# calculate the age at which child was vaccinated with measles-containing vaccine 
dt$age_at_mea1 <- time_length(difftime(dt$mea1, dt$dob), "days")
dt$age_at_mea2 <- time_length(difftime(dt$mea2, dt$dob), "days")

# calculate the age at which child was vaccinated with DPT vaccines
dt$age_at_dpt1 <- time_length(difftime(dt$dpt1, dt$dob), "days")
dt$age_at_dpt2 <- time_length(difftime(dt$dpt2, dt$dob), "days")
dt$age_at_dpt3 <- time_length(difftime(dt$dpt3, dt$dob), "days")

# variable indicating when the oldest vaccination date took place
dt <- dt %>% mutate(oldest_visit = pmax(bcg, dpt1, pol1, dpt2, pol2, dpt3, pent1, pent2, pent3, pneu1, pneu2, pneu3, rota1, rota2, rota3, poln, hepb1, hepb2, hepb3, hib1, hib2, hib3,
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
                           TRUE ~ 0),
    
    # vaccinated within appropriate time frame
    mea1_within_interval = case_when(mea1_age_minus_min>=0 & mea1_age_minus_max<=0 ~ 1, 
                                     TRUE ~ 0),
    
    # vaccinated but too late
    mea1_late = case_when(mea1_age_minus_max>0 ~ 1, 
                          TRUE ~ 0),
    
    #  never received vaccine (1 is never received, 0 is did receive))                
    never_got_mea1 = case_when(!is.na(age_at_mea1) & has_health_card_bin=="Yes" ~ 0,
                               is.na(age_at_mea1) & has_health_card_bin=="Yes" ~ 1), 
    
    # transpose the age at which children that did receive measles were counted
    mea1_age_at_counted_vac = case_when(mea1_age_minus_min<0 ~ age_at_mea1,
                                        mea1_age_minus_min>=0 & mea1_age_minus_max<=0 ~ age_at_mea1,
                                        mea1_age_minus_max>0 ~ age_at_mea1),
    
    # variable that indicates how much time each child was at risk (for those that were vaccinated early or late)
    mea1_days_at_risk = case_when(mea1_age_minus_min<0 & age_in_days>=mea1_age_due_max ~ age_in_days - mea1_age_due_max,
                                  mea1_age_minus_min>=0 & mea1_age_minus_max<=0 ~ 0,
                                  mea1_age_minus_max>0  ~ mea1_age_minus_max,
                                  never_got_mea1==1 & age_in_days>=mea1_age_due_max  ~ age_in_days - mea1_age_due_max),
    
    # variable that indicates missed opportunities
    mea1_missed_opportunity = case_when(never_got_mea1==1 & age_at_oldest_visit>mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_oldest_visit) ~ 1,
                                        age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) ~ 0)
  )

# calculate additional measles coverage variables that require re-shaping of the data

# calculate how many doses of DPT were received
for(i in 1:nrow(dt)){
  dt$tot_num_dpt[i] <- sum(!is.na(dt$dpt1[i]), !is.na(dt$dpt2[i]), !is.na(dt$dpt3[i]))
}

## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! 
## ! REVIEW ! chunk is missing from here to calculate something or other (see stata code)
## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! 

# calculate variables related to timing among the DPT-containing vaccine (pentavalent)
dt <- dt %>% 
  mutate(
    # calculate variable indicating if never received dtap vaccine
    never_got_dpt = case_when(!is.na(age_at_dpt1) & has_health_card_bin=="Yes" ~ 0,
                              is.na(age_at_dpt1) & has_health_card_bin=="Yes" ~ 1),
    
    # calculate whether they got the first dose in sixth week
    dpt_dose_6wks = case_when(age_at_dpt1>=dpt1_age_due_min & age_at_dpt1<=dpt1_age_due_max & age_in_days>= dpt3_age_due_max ~ 1,
                              never_got_dpt==1 ~ 0),
    
    # calculate  age when first dose was received
    dpt_dose_6wks_when = case_when(age_at_dpt1>=dpt1_age_due_min & age_at_dpt1<=dpt1_age_due_max & age_in_days>= dpt3_age_due_max ~ age_at_dpt1),
    
    # calculate first age eligible after first dose
    first_elig_dpt_after_6wks = case_when(age_at_dpt1>=dpt1_age_due_min & age_in_days>= dpt3_age_due_max ~ age_at_dpt1,
                                          age_at_dpt2>=dpt1_age_due_min & age_in_days>= dpt3_age_due_max ~ age_at_dpt2,
                                          age_at_dpt3>=dpt1_age_due_min & age_in_days>= dpt3_age_due_max ~ age_at_dpt3),
    
    # calculate whether they got the second dose in the tenth week
    dpt_dose_10wks = case_when(age_at_dpt2>=dpt2_age_due_min & age_at_dpt2<=dpt2_age_due_max & age_in_days>= dpt3_age_due_max ~ 1,
                               never_got_dpt==1 ~ 0),
    
    # calculate age when second dose was received
    dpt_dose_10wks_when = case_when(age_at_dpt2>=dpt2_age_due_min & age_at_dpt2<=dpt2_age_due_max & age_in_days>= dpt3_age_due_max ~ age_at_dpt2),
    
    # calculate first age eligible after second dose
    first_elig_dpt_after_10wks = case_when(age_at_dpt1>=dpt2_age_due_min & age_in_days>= dpt3_age_due_max ~ age_at_dpt1,
                                          age_at_dpt2>=dpt2_age_due_min & age_in_days>= dpt3_age_due_max ~ age_at_dpt2,
                                          age_at_dpt3>=dpt2_age_due_min & age_in_days>= dpt3_age_due_max ~ age_at_dpt3),
    
    # calculate whether they got the third dose in the 14th week
    dpt_dose_14wks = case_when(age_at_dpt3>=dpt3_age_due_min & age_at_dpt3<=dpt3_age_due_max & age_in_days>= dpt3_age_due_max ~ 1,
                               never_got_dpt==1 ~ 0),
    
    # calculate age when third dose was received
    dpt_dose_14wks_when = case_when(age_at_dpt3>=dpt3_age_due_min & age_at_dpt3<=dpt3_age_due_max & age_in_days>= dpt3_age_due_max ~ age_at_dpt3),
    
    # calculate first age eligible after third dose
    first_elig_dpt_after_14wks = case_when(age_at_dpt1>=dpt3_age_due_min & age_in_days>= dpt3_age_due_max ~ age_at_dpt1,
                                           age_at_dpt2>=dpt3_age_due_min & age_in_days>= dpt3_age_due_max ~ age_at_dpt2,
                                           age_at_dpt3>=dpt3_age_due_min & age_in_days>= dpt3_age_due_max ~ age_at_dpt3),
    
    # generate indicator for correct interval
    dpt_within_interval = case_when(
      
      # kids that have perfect adherence
      dpt_dose_6wks==1 & dpt_dose_10wks==1 & dpt_dose_14wks==1 & age_in_days>=dpt3_age_due_max ~ 1,
      
      # kids that did not have all visits during the interval, but had 3 doses with the right spacing
     first_elig_dpt_after_14wks>=dpt3_age_due_min & first_elig_dpt_after_14wks <=dpt3_age_due_max & age_in_days>=dpt3_age_due_max ~1,
     
     # otherwise 0
     TRUE ~ 0
     ),
    
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
    too_few_elig_dpt = case_when(is.na(first_elig_dpt_after_14wks) & !is.na(tot_num_dpt) & age_in_days>=dpt3_age_due_max & has_health_card_bin==1 ~ 1),

    # assign days at risk
  dpt_days_at_risk = case_when(dpt_within_interval==1 ~ 0,
                               dpt_late==1 ~ first_elig_dpt_after_14wks - dpt3_age_due_max,
                               too_few_elig_dpt==1 ~ age_in_days - dpt3_age_due_max)
  
  )

# // these should be mutually exclusive: dpt_within_interval, too_few_elig_dpt, never_got_dpt, dpt_late
# * tab dpt_within_interval too_few_elig_dpt, m
# * tab dpt_within_interval dpt_late, m
# * tab dpt_within_interval never_got_dpt, m
# * tab dpt_late never_got_dpt, m
# * tab too_few_elig_dpt never_got_dpt, m
# * tab too_few_elig_dpt dpt_late, m

#   gen dpt_dose_2mos = 0 if never_got_dpt==0 
# gen dpt_dose_2mos_when = . 
# gen first_elig_dpt_after_2mos = . 
# forvalues value = 1/10 {
#   replace dpt_dose_2mos = 1 								if age_`value'_dpt>=`dpt1_age_due_min' & age_`value'_dpt<=`dpt1_age_due_max' & age_in_days>= `dpt3_age_due_max'
# 		replace dpt_dose_2mos_when = age_`value'_dpt 			if age_`value'_dpt>=`dpt1_age_due_min' & age_`value'_dpt<=`dpt1_age_due_max' & age_in_days>= `dpt3_age_due_max' & (age_`value'_dpt<dpt_dose_2mos_when | dpt_dose_2mos_when==.)
# replace first_elig_dpt_after_2mos = age_`value'_dpt 	if age_`value'_dpt>=`dpt1_age_due_min' 										 & age_in_days>= `dpt3_age_due_max' & (age_`value'_dpt<first_elig_dpt_after_2mos | first_elig_dpt_after_2mos==.)
# 	}
# dt <- dt %>%
#   mutate(tot_num_dpt=case_when(
#     dpt1_date_recorded==1 & dpt2_date_recorded==1 & dpt3_date_recorded==1 ~ "3",
#     dpt1_date_recorded==1 & dpt2_date_recorded==1 & dpt3_date_recorded!=1 ~ "2",
#     dpt1_date_recorded==1 & dpt2_date_recorded!=1 & dpt3_date_recorded!=1 ~ "1",
#     dpt1_date_recorded!=1 & dpt2_date_recorded==1 & dpt3_date_recorded==1 ~ "2",
#     dpt1_date_recorded!=1 & dpt2_date_recorded!=1 & dpt3_date_recorded==1 ~ "1"
#     dpt1_date_recorded!=1 & dpt2_date_recorded!=1 & dpt3_date_recorded!=1 ~ "0"))



# dt %>%
#   rowwise %>%
#   summarise(NA_per_row = sum(is.na(dpt1)))

# dt <- dt %>% 
#   mutate(
#     # vaccinated too early
#     early_mea1 = case_when(mea1_age_minus_min<0 ~ 1, 
#                            TRUE ~ 0),
#     
#     # vaccinated within appropriate time frame
#     mea1_within_interval = case_when(mea1_age_minus_min>=0 & mea1_age_minus_max<=0 ~ 1, 
#                                      TRUE ~ 0),
#     
#     # vaccinated but too late
#     mea1_late = case_when(mea1_age_minus_max>0 ~ 1, 
#                           TRUE ~ 0),
#     
#     #  never received vaccine (1 is never received, 0 is did receive))                
#     never_got_mea1 = case_when(!is.na(age_at_mea1) & has_health_card_bin=="Yes" ~ 0,
#                                is.na(age_at_mea1) & has_health_card_bin=="Yes" ~ 1), 
#     
#     # transpose the age at which children that did receive measles were counted
#     mea1_age_at_counted_vac = case_when(mea1_age_minus_min<0 ~ age_at_mea1,
#                                         mea1_age_minus_min>=0 & mea1_age_minus_max<=0 ~ age_at_mea1,
#                                         mea1_age_minus_max>0 ~ age_at_mea1),
#     
#     # variable that indicates how much time each child was at risk (for those that were vaccinated early or late)
#     mea1_days_at_risk = case_when(mea1_age_minus_min<0 & age_in_days>=mea1_age_due_max ~ age_in_days - mea1_age_due_max,
#                                   mea1_age_minus_min>=0 & mea1_age_minus_max<=0 ~ 0,
#                                   mea1_age_minus_max>0  ~ mea1_age_minus_max,
#                                   never_got_mea1==1 & age_in_days>=mea1_age_due_max  ~ age_in_days - mea1_age_due_max),
#     
#     # variable that indicates missed opportunities
#     mea1_missed_opportunity = case_when(never_got_mea1==1 & age_at_oldest_visit>mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_oldest_visit) ~ 1,
#                                         age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) ~ 0)
#   )
# calculate age at each vaccine

# make vector of vaccine_dates
# dateVars = names(dt)[grepl('age_at', names(dt))][4:29]


# dt <- dt %>% 
#   mutate_at(
#     vars(one_of(dateVars)),
#     list(case_when(
#       .>=mea1_age_due_min ~ .
#     )))


#   ))
# # We also want to know how old the child was at the earliest eligible visit 
# no_mea1_mop_age = case_when(never_got_mea1==0 ~ NA,
#                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
#                             never_got_mea1==1 & age_at_dpt1>=mea1_age_due_min ~ age_at_dpt1,
#                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
#                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
#                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
#                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
#                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
#                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
#                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
#                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
#                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
#                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
#                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
#                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
#                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
#                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
#                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
#                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
#                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
#                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,)
#                   
#                   # The child got measles late, but there was a visit for another vaccine in between the measles due age and actual age at measles 
#                   
#                   # Now compute a compliance with MO
#                   mea1_compliant_card_mop = case_when(mea1_date_recorded%in%c(0) ~0,
#                                                       mea1_date_recorded%in%c(1,3) ~1,
#                                                       mea1_missed_opportunity==1 ~1),
#                   
#                   # compute days of risk if opportunity was not missed
#                   mea1_days_at_risk_mop = case_when(mea1_missed_opportunity==0 ~ mea1_days_at_risk,
#                                                     )
#                   )


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
saveRDS(dt, file=paste0(g_drive, "Data/prepped_data/2d_prepped_dhs_data_for_analysis.RDS"))

# print final statement
print("Step 2d: Extracting and prepping of dhs vaccination data completed.")
