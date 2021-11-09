# Author: Francisco Rios Casas
# Purpose: Load and prep IHME DAH data files
# Date: October 29, 2021

# Read in list of files to prep
file_list <- read_excel(paste0(g_drive, "data/list_of_data_used.xlsx")) %>% 
  filter(data_type=="index_variables" & data_source=="ihme")

# Set file path that will indicate which file to prep
file_path <- paste0(raw_data_dir, "/", file_list$data_type[1], "/", file_list$data_source[1], "/", file_list$containing_folder[1], "/", file_list$file_name[1])

# Load data file
dt1 <- read_csv(file_path, show_col_types = FALSE)

# Drop certain rows to avoid double counting
dt1 <- dt1 %>% filter(elim_ch==0)

# Subset columns
dt1 <- dt1 %>% select(year, source, recipient_isocode, recipient_country, gbd_location_id, wb_location_id, nch_cnv_dah_20)

# Transform column from character to numeric
dt1$nch_cnv_dah_20 <- as.numeric(dt1$nch_cnv_dah_20)

# sum donations across all sources
dt1 <- as.data.table(dt1)
dt1 <- dt1[, .(nch_cnv_dah_20=sum(nch_cnv_dah_20, na.rm = T)), by=c('year', 'recipient_isocode', 'recipient_country', 'gbd_location_id', 'wb_location_id')]

# Change the name of the variables to make sure they are standardized
setnames(dt1, 
         old = c("recipient_isocode", "recipient_country"),
         new = c("iso_code", "location"))

# Save data
saveRDS(dt1, file=paste0(prepped_data_dir, "aim_2/01_prepped_ihme_dah_data.RDS"))
