# Author: Francisco Rios 
# Purpose: Prep socio-demographic data for analyses
# Date: Last modified July 13, 2021

# read in SDI data downloaded from GBD
sdi.dat <- as.data.table(read_xlsx(path = paste0(local_data_dir, "/sdi/IHME_GBD_2019_SDI_1990_2019_Y2020M10D15.xlsx")))

# re-name variable names
names_sdi <- as.character(sdi.dat[1])
names(sdi.dat) <- names_sdi
setnames(sdi.dat, old=c("Location"), new=c("location"))

# remove extra name row from the field
sdi.dat <- sdi.dat[-c(1),]

# read in codebook
sdi.codebook <- as.data.table(read_xlsx(path = paste0(codebook_directory, "/IHME_GBD_2019_GBD_LOCATION_HIERARCHY_Y2020M10D15.xlsx")))

# rename location name variable 
setnames(sdi.codebook, old=c("Location Name"), new=c("location"))

# subset codebook to location ID and Name

# bind rows with country names that are not spelled exactly the same in the codebook

# create merge label name in both codebook and data
sdi.dat <- strip_chars(sdi.dat)
sdi.codebook <- strip_chars(sdi.codebook)

# check to make sure that all locations in codebook are in the data
code_check <- paste0(sdi.codebook$location)
data_check <- paste0(sdi.dat$location)

unmapped_codes <- sdi.dat[!data_check%in%code_check]

if(nrow(unmapped_codes)>0){
  print(unique(unmapped_codes[, c("location"), with= FALSE]))
  # print(unique(unmapped_codes$file_name)) #For documentation in the comments above. 
  stop("You have locations in the data that aren't in the codebook!")
}

# what to do with the locations that are not mapped--drop them from SDI?


# merge codebook with the SDI data-sheet using the location ID
sdi.test <- merge(sdi.dat, sdi.codebook, by.x = "Location ID", by.y = "Location ID", all.x = TRUE)

# subset data

# reshape data

# save in prepped data folder

