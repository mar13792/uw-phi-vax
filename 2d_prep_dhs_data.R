# Author: Francisco Rios 
# Purpose: prep dhs data for 'missed opportunities' analyses
# Date: Last modified August 6, 2021

# # initial data exploration
# tags <- dhs_tags()
# 
# # search tags for any relating to vaccinations
# tags[grepl("Immunization", tags$TagName), ]
# 
# ## make a call with no arguments
# sc <- dhs_survey_characteristics()
# sc[grepl("Vaccinations", sc$SurveyCharacteristicName), ]
# 
# # ids
# ids <- dhs_countries(returnFields=c("CountryName", "DHS_CountryCode"))
# str(ids)
# 
# ## what are the countryIds
# ids <- dhs_countries(returnFields=c("CountryName", "DHS_CountryCode"))
# str(ids)

# Liberia ID is 'LB'

# Find all surveys that fit our criteria
survs <- dhs_surveys(countryIds = "LB",
                     surveyYear = c(2013, 2016, 2019))

# survs <- dhs_surveys(surveyCharacteristicIds = 107,
#                      countryIds = "LB",
#                      surveyType = "DHS",
#                      surveyYearStart = 2013)
# 
# survs <- dhs_surveys(surveyCharacteristicIds = 89,
#                      countryIds = c("CD","TZ"),
#                      surveyType = "DHS",
#                      surveyYearStart = 2013)

# find the datasets we want to download
datasets <- dhs_datasets(surveyIds = survs$SurveyId, 
                         fileFormat = "flat", selectSurveys = TRUE)

# change format of datasets objec to make it easier to subset
datasets <- as.data.table(datasets)

# keep only the datasets I know I would like to download
datasets <-datasets[DatasetType=="Survey Datasets" & FileType != "Fieldworker Questionnaire"]

test1 <- datasets[1]

## set up your credentials
set_rdhs_config(email = "frc2@uw.edu",
                project = "Targeting key factors to realizing improved global vaccination coverage",
                config_path = "rdhs.json",
                cache_path = paste0(data_dir,"vaccination_trends/dhs"),
                global = FALSE,
                data_frame = "data.table::as.data.table")

# download datasets
downloads <- get_datasets(test1$FileName)

# read in our dataset
cdpr <- readRDS(downloads$LBBR6AFL)

