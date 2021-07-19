# Author: Francisco Rios 
# Purpose: Merges all data to be used in analyses
# Date: Last modified July 13, 2021

# Read in the previously saved files for vaccination trends
vax_dt <- readRDS(outputFile2a)

# read in the previously saved files for sdi
sdi_dt <- readRDS(outputFile2b) # this datatable contains all of the data

# merge two data tables sheets together
dt <- merge(vax_dt, sdi_dt, by=c("location_name", "location_id", "year_id"), all.x = TRUE)

# subset from year 1990 to present (as this is extent of SDI data)
dt <- dt[year_id>=1990]

# run error checks or missing location codes--

# save location data
saveRDS(dt, outputFile3a)
