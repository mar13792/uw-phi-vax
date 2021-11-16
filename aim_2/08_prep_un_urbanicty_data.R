# Author: Francisco Rios Casas
# Purpose: Load and prep UN data on proportion of population living in urban areas
# Date: November 09, 2021

# Read in list of files to prep
file_list <- read_excel(paste0(g_drive, "data/list_of_data_used.xlsx")) %>% 
  filter(data_type=="index_variables")

# Set file path that will indicate which file to prep
file_path <- paste0(raw_data_dir, file_list$data_type[7], "/", file_list$data_source[7], "/", file_list$file_name[7])

# Read data sheet
dt1 <- read_xls(file_path, sheet = 1)

# rename columns of data
name_row <- 12
names <- unlist(dt1[name_row,])
names(dt1) <- names

# Remove unnecessary columns and rows
dt1 <- dt1[-c(1:12),-c(1, 3)]

# assign correct names to columns
colnames(dt1)[1] <- "location"
colnames(dt1)[2] <- "iso_num_code"

# change value format for the year 1950 to numeric to match other year values
dt1$`1950` <- as.numeric(dt1$`1950`)

# Reshape the data
dt2 <- data.table(dt1)
prepped_urban_dataset <- melt(dt2, id.vars=c("location", "iso_num_code"),
                                  value.name = "perc_urban", 
                                  variable.name = "year")

# Save file in the prepped data folder
saveRDS(prepped_urban_dataset, file = paste0(prepped_data_dir, "aim_2/07_prepped_un_perc_urban_data.R"))
