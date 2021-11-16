# Author: Francisco Rios Casas
# Purpose: Load and prep IHME HAQI data files
# Date: November 08, 2021

# Read in list of files to prep
file_list <- read_excel(paste0(g_drive, "data/list_of_data_used.xlsx")) %>% 
  filter(data_type=="index_variables" & data_source=="ihme")

# Set file path that will indicate which file to prep
file_path <- paste0(raw_data_dir, "/", file_list$data_type[2], "/", file_list$data_source[2], "/", file_list$containing_folder[2], "/", file_list$file_name[2])

# Load data file
dt1 <- read_csv(file_path)

# subset rows to only include HAQI values
dt1 <- dt1 %>% filter(indicator_name=="Healthcare Access and Quality")

# keep only certain columns
dt1 <- dt1 %>% select(location_id, location_name, year_id, val)

# rename the "val" variable
dt1 <- rename(dt1, 
       haqi = val,
       gbd_location_id = location_id, 
       location = location_name, 
       year = year_id)

# save the new data set
saveRDS(dt1, file = paste0(prepped_data_dir, "aim_2/02_prepped_ihme_haqi_data.RDS"))
