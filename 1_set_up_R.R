# Author: Francisco Rios 
# Purpose: Set up R for prepping UW PHI Vaccination Data
# Date: Last modified July 12, 2021

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

###############################
# source shared functions
###############################
source(paste0(code_dir, "functions/", "prep_vax_trend_data.R"))

###############################
# set Boolean switches
##############################