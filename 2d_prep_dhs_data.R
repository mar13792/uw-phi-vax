# # Author: Francisco Rios 
# # Purpose: prep dhs data for 'missed opportunities' analyses
# # Date: Last modified August 6, 2021

data <- read_dta(file=paste0(data_dir,"vaccination_trends/dhs/LBIR7ADT/LBIR7AFL.DTA"))

# subset to only relevant variables
idVars = c("caseid", "v000", "v001", "v002", "v003", "v004", "v006", "v007", "v016")
vaxVars = names(data)[grepl('h2_|h2m|h2d|h2y|h3_|h3m|h3d|h3y|h4_|h4m|h4d|h4y|h5_|h5m|h5d|h5y|h6_|h6m|h6d|h6y|h7_|h7m|h7d|h7y|h8_|h8m|h8d|h8y|h9_|h9m|h9d|h9y|h50|h51|h54|h57|h58|h59|h60|h61|h62|h63|h64|h65|h66',names(data))]
demoVars = c("v012", "v106", "v151", "v155", "v201", "v716", "v501", "v136", "v190", "v191a", "v025")

# calculate new variables

# save as an r-object

# new line of test code

