# Author: Francisco Rios 
# Purpose: Merges all data to be used in analyses
# Date: Last modified July 13, 2021

# Read in the previously saved files for vaccination trends
vax_dt <- readRDS(outputFile2a)

# read in the previously saved files for sdi
sdi_dt <- readRDS(outputFile2b) # this datatable contains all of the data

# make sure that the merge variables align in both data tables
sdi_concat <- paste0(sdi_dt$location_name)
vax_concat <- paste0(vax_dt$location_name)
unmapped_loc <- vax_dt[!vax_concat%in%sdi_concat]

if(nrow(unmapped_loc)>0){
  print(unique(unmapped_loc[, c("location_name"), with= FALSE]))
  print(unique(unmapped_loc$file_name)) #For documentation in the comments above.
  stop("You have location names in the vaccine data that aren't in the SDI data!")
}

# merge two data tables sheets together
dt <- merge(vax_dt, sdi_dt, by=c("location_name", "location_id", "year_id"), all.x = TRUE)

# subset from year 1990 to present (as this is extent of SDI data)
dt <- dt[year_id>=1990]

# run error checks or missing location codes--

# save location data
saveRDS(dt, outputFile3)

# Print final statement
print("Step 3: Merging complete; data set saved in prepped data folder")