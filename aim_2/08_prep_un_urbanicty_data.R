# Author: Francisco Rios Casas
# Purpose: Load and prep UN data on proportion of population living in urban areas
# Date: November 09, 2021

# Read in list of files to prep
file_list <- read_excel(paste0(g_drive, "data/list_of_data_used.xlsx")) %>% 
  filter(data_type=="index_variables")

``````````