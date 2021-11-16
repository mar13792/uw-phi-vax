# Author: Francisco Rios 
# Purpose: Create map of country locations to map standardized names onto data
# Date: Last modified November 15, 2021

# -----
# GBD Location Hierarchy
# -----

# read in file
gbd_code <- read_xlsx(paste0(codebook_directory, "IHME_GBD_2019_GBD_LOCATION_HIERARCHY_Y2020M10D15.xlsx"))

# subset to countries only
gbd_code <- gbd_code %>% filter(Level==3)

# Subset columns
gbd_code <- gbd_code[,c(2:3)]

# -----
# Add ISO codes using GBD location IDs
# -----

# Read ISO/GBD codebook from DAH data set
dah_code <- read_delim(paste0(codebook_directory, "IHME_DAH_DATABASE_1990_2020_CODEBOOK_Y2021M09D22.csv"))

# subset gbd codebook to necessary columns
name_row <- 1
names <- unlist(dah_code[name_row,])
names(dah_code) <- names

# subset unnecessary columns and data
dah_code <- dah_code[-c(1,2), c(5:7)]

# change data structure
dah_code$gbd_location_id <- as.numeric(dah_code$gbd_location_id)

# # assign correct names to columns
# colnames(dah_code)[1] <- "iso_code"
# colnames(dah_code)[2] <- "location"

# merge together
location_map <- gbd_code %>% 
  left_join(dah_code, by=c("Location ID"="gbd_location_id"))

# Identify the ones that are missing manually
output <- location_map %>% filter(is.na(recipient_isocode))
write.csv(output, file=paste0(codebook_directory, "iso_locations_corrections.csv"))

# codebook <- read_xlsx(paste0(codebook_directory, "iso_codebook.xlsx"))

# 

# # rename iso codebook
# colnames(codebook)[1] <- "location"
# colnames(codebook)[2] <- "iso_code"
# colnames(codebook)[3] <- "iso_num_code"

