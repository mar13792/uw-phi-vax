# Author: Francisco Rios Casas
# Purpose: Load and prep UN skilled attendant data
# Date: November 08, 2021

# Read in list of files to prep
file_list <- read_excel(paste0(g_drive, "data/list_of_data_used.xlsx")) %>% 
  filter(data_type=="index_variables")

# Set file path that will indicate which file to prep
file_path <- paste0(raw_data_dir, file_list$data_type[4], "/", file_list$data_source[4], "/", file_list$file_name[4])

# Load data file
dt1 <- read.csv(file_path)

# subset columns
loc_col <- 2
year_col <- 5
val_col <- 6

# rename
names(dt1)[loc_col] <- "location"
names(dt1)[year_col] <- "year"
names(dt1)[val_col] <- "perc_skil_attend"

# keep only relevant columns
total_subset <- c(loc_col, year_col, val_col)
dt1 <- dt1[, total_subset]

# split variable columns
dt1 <- as_tibble(dt1)
dt2 <- dt1 %>% separate(location, c("iso_code", "location"), sep = ":")
dt2$location <- str_trim(dt2$location, side="left")

# save the file on the prepped data folder
saveRDS(dt2, file = paste0(prepped_data_dir, "aim_2/04_prepped_un_birth_attendant_data.R"))
