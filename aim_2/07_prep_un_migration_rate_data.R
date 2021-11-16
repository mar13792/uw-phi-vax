# Author: Francisco Rios Casas
# Purpose: Load and prep UN immigrant rate (net immigrants minus emmigrants)
# Date: November 09, 2021

# Read in list of files to prep
file_list <- read_excel(paste0(g_drive, "data/list_of_data_used.xlsx")) %>% 
  filter(data_type=="index_variables")

# Set file path that will indicate which file to prep
file_path <- paste0(raw_data_dir, file_list$data_type[6], "/", file_list$data_source[6], "/", file_list$file_name[6])

# Read data sheet
dt1 <- read_xlsx(file_path, sheet = 1)

# rename columns of data
name_row <- 12
names <- unlist(dt1[name_row,])
names(dt1) <- names

# Remove unnecessary columns and rows
dt1 <- dt1[-c(1:12),-c(1, 2, 4)]

# Subset rows
# Remove the unnecessary location divisions
dt1 <- dt1 %>% filter(Type=="Country/Area")

# assign correct names to columns
colnames(dt1)[1] <- "location"
colnames(dt1)[2] <- "iso_num_code"
colnames(dt1)[4] <- "parent_iso_num_code"

# Reshape the data
dt2 <- data.table(dt1)
prepped_migr_rate_dataset <- melt(dt2, id.vars=c("location", "iso_num_code", "Type", "parent_iso_num_code"),
                                  value.name = "mig_rate", 
                                  variable.name = "year_range")

# subset columns
prepped_migr_rate_dataset <- prepped_migr_rate_dataset %>% select(location, iso_num_code, year_range, mig_rate)

# Reformat variable
prepped_migr_rate_dataset$mig_rate <- as.numeric(prepped_migr_rate_dataset$mig_rate)
prepped_migr_rate_dataset$iso_num_code <- as.numeric(prepped_migr_rate_dataset$iso_num_code)

# Save file in the prepped data folder
saveRDS(prepped_migr_rate_dataset, file = paste0(prepped_data_dir, "aim_2/06_prepped_un_net_migr_rate_data.RDS"))
