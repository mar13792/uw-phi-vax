# Author: Francisco Rios 
# Purpose: Merge together different variables to create one data set
# Date: Last modified November 15, 2021

# Load all of the prepped data sets

prepped_file_01 <- readRDS(paste0(prepped_data_dir, "aim_2/01_prepped_ihme_dah_data.RDS"))
prepped_file_02 <- readRDS(paste0(prepped_data_dir, "aim_2/02_prepped_ihme_haqi_data.RDS"))

# Making 
merged_data <- prepped_file_01 %>% 
  full_join(prepped_file_02, by=c("location", "year", "gbd_location_id"))

# Save Final Prepped Data
saveRDS(merged_data, file = paste0(prepped_data_dir, "aim_2/14_merged_dataset.RDS"))
