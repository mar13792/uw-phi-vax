# Author: Francisco Rios 
# Purpose: Set up R for prepping UW PHI Vaccination Data
# Date: Last modified July 12, 2021

# Manually set the working directory

###############################
# load required packages
###############################
library(data.table)
library(ggplot2)
library(readxl)

###############################
# important variables
###############################
shared_data_dir <- "G:/.shortcut-targets-by-id/1P7ITMVB9x01fuYfHW8-uWogw4SpbuvwO/Merck Vaccine Improvement Index Project/Data"
local_data_dir <- "G:/My Drive/PHI/local_data"
code_dir <- "./"
codebook_directory <- paste0(local_data_dir,"/codebooks")

###############################
# source shared functions
###############################
source(paste0(code_dir, "functions/", "prep_vax_trend_data.R"))
source(paste0(code_dir, "functions/", "strip_chars.R"), encoding = "UTF-8")

###############################
# set Boolean switches
##############################