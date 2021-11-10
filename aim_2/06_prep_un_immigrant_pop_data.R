# Author: Francisco Rios Casas
# Purpose: Load and prep UN skilled attendant data
# Date: November 09, 2021

# Read in list of files to prep
file_list <- read_excel(paste0(g_drive, "data/list_of_data_used.xlsx")) %>% 
  filter(data_type=="index_variables")

# Set file path that will indicate which file to prep
file_path <- paste0(raw_data_dir, file_list$data_type[5], "/", file_list$data_source[5], "/", file_list$file_name[5])

# Read data sheet
dt1 <- read_xlsx(file_path, sheet = 4)

# Remove unnecessary columns
dt1 <- dt1[-c(1:8),-c(1, 13:26)]

name_row <- 1
names <- unlist(dt1[name_row,])
names(dt1) <- names

# Remove blank first rows
dt1 <- dt1[-1,]

# Remove unnecessary columns
dt1 <- dt1 %>% select(1, 3, 5:11)

# assign correct names to columns
colnames(dt1)[1] <- "location"
colnames(dt1)[2] <- "iso_num_code"

# Reshape data layout
dt2 <- data.table(dt1)
imm_pop_dataset <- melt(dt2, id.vars=c("location", "iso_num_code"),
                     value.name = "imm_pop_perc",
                     variable.name = "year")

# Reformat numeric columns
imm_pop_dataset$imm_pop_perc <- as.numeric(imm_pop_dataset$imm_pop_perc)

# Save file in the prepped data folder
saveRDS(imm_pop_dataset, file = paste0(prepped_data_dir, "aim_2/05_prepped_un_immigrant_data.R"))
