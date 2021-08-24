# ----------------------------------------------
# AUTHOR: Francisco Rios
# PURPOSE: Extract all necessary DHS data
# DATE: Last updated August 20 2021

# Read in list of files to prep
file_list <- data.table(read_excel(paste0(data_dir, "data_file_list.xlsx")))

# subset files to dhs data
file_list <- file_list[data_source%in%c("dhs6", "dhs7"),.(file_name, data_type, data_source, year, containing_folder, location_name)]

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
# dt$female <- as.character(dt$sex_of_child)
# dt <- dt %>% mutate(female = recode(female,
#                                     `2`=TRUE,
#                                     `1`=FALSE))

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

# 

###############################################################
# maybe create a codebook for dhs derived dataset which could be read in to make sure all names are consistent
#
# subset columsn to only include vaccines of interest
#
# 
###############################################################



# print final statement
print("Step 2a: Reading in vaccination trend data completed.")
