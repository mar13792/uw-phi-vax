# Author: Francisco Rios Casas
# Purpose: Load and prep Corruptions Perceptions Index
# Date: November 08, 2021

# Read in list of files to prep
file_list <- read_excel(paste0(g_drive, "data/list_of_data_used.xlsx")) %>% 
  filter(data_type=="index_variables")

# Set file path that will indicate which file to prep
file_path <- paste0(raw_data_dir, file_list$data_type[3], "/", file_list$data_source[3], "/", file_list$containing_folder[3], "/", file_list$file_name[3])

# Load data file
dt1 <- data.table(read_xlsx(file_path, sheet = 2))

# remove first row

# Rename data headers
name_row <- 2
names <- unlist(dt1[name_row,])
names(dt1) <- names

# Identify columns of interest
loc_col <- grepl("Country", names)
iso_col <- grepl("ISO3", names)
cpi_col <- grepl("CPI ", names)

# assign correct names to columns
colnames(dt1)[loc_col] <- "location"
colnames(dt1)[iso_col] <- "iso_code"

# keep columns of interest
loc_dt <- dt1[, loc_col, with=FALSE]
iso_dt <- dt1[, iso_col, with=FALSE]
cpi_dt <- dt1[, cpi_col, with=FALSE]

# merge columns together
dt2 <- cbind(loc_dt, iso_dt, cpi_dt)

# remove blank rows
dt2 <- dt2[!(is.na(location) & is.na(iso_code))]
dt2 <- dt2[-1,]

# melt data long
cpi_dataset = melt(dt2, id.vars = c('location', 'iso_code'), 
                      value.name = "cpi", 
                   variable.name = "year")

# re-code the year variable
levels(cpi_dataset$year)[1] <-"2020"
levels(cpi_dataset$year)[2] <-"2019"
levels(cpi_dataset$year)[3] <-"2018"
levels(cpi_dataset$year)[4] <-"2017"
levels(cpi_dataset$year)[5] <-"2016"
levels(cpi_dataset$year)[6] <-"2015"
levels(cpi_dataset$year)[7] <-"2014"
levels(cpi_dataset$year)[8] <-"2013"
levels(cpi_dataset$year)[9] <-"2012"

# save CPI as numeric
cpi_dataset$cpi <- as.numeric(cpi_dataset$cpi)

# save the file on the prepped data folder
saveRDS(cpi_dataset, file = paste0(prepped_data_dir, "aim_2/03_prepped_cpi_data.R"))
