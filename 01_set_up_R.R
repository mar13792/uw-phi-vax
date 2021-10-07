# Author: Francisco Rios 
# Purpose: Set up R for prepping UW PHI Vaccination Data
# Date: Last modified September 20, 2021

# Load required packages -----
library(data.table)
library(ggplot2)
library(readxl)
library(GGally)
library(ggrepel)
library(reactable)
library(rdhs)
library(devtools)
library(rdhs)
library(haven)
library(table1)
library(tidyr)
library(plyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(naniar)
library(survival)
library(cmprsk)
library(survminer)
library(vtable)

# Define important variables -----

# set shared team Google drive dynamically
if (Sys.info()[1]=='Windows'){
  stop("Windows filepaths need to be defined")
    g_drive = "G:/.shortcut-targets-by-id/1P7ITMVB9x01fuYfHW8-uWogw4SpbuvwO/Merck Vaccine Improvement Index Project/"
    code_dir = "tbd"
  } else {
    g_drive = "/Volumes/GoogleDrive/.shortcut-targets-by-id/1P7ITMVB9x01fuYfHW8-uWogw4SpbuvwO/Merck Vaccine Improvement Index Project/"
    code_dir = ("~/Documents/uw-phi-vax/")
    }

setwd(code_dir)
raw_data_dir <- paste0(g_drive,"Data/raw_data/")
prepped_data_dir <- paste0(g_drive,"Data/prepped_data/")
codebook_directory <- paste0(g_drive,"Data/documentation/codebooks/")
resDir <- paste0(g_drive, "Results/")
visDir <- paste0(g_drive,"Visualizations/")

# Define output file names that will be created in future scripts -----

# name of prepped data files stored in shared google drive
outputFile02 = paste0(prepped_data_dir, "02_vaccine_trends.RDS")
outputFile03 = paste0(prepped_data_dir, "03_sdi.RDS")
outputFile04 = paste0(prepped_data_dir, "04_disease_trends.RDS")
outputFile05 = paste0(prepped_data_dir, "05_raw_extracted_dhs.RDS")
outputFile06 = paste0(prepped_data_dir, "06_merged_data_for_visuals.RDS")
outputFile09 = paste0(prepped_data_dir, "09_prepped_dhs_for_mov.RDS")

# name of sample visualizations
# --these need to be completed
# outputFile4 = paste0(visDir, "aim1_sample_visualizations.PDF")
# outputFile4b = paste0(visDir, "aim1_sample_country_trends.PDF")

# names of visualizations to identify high-performing countries
outputFile08a = paste0(visDir, "aim_1/high_performing_locs/1_outliers_low_sdi.PDF")
outputFile08b = paste0(visDir, "aim_1/high_performing_locs/2_barplot_low_sdi.PDF")
outputFile08c = paste0(visDir, "aim_1/high_performing_locs/3_detailed_table_low_sdi.PDF")
outputFile08d = paste0(visDir, "aim_1/high_performing_locs/4_timeseries_plots_improvement_low_sdi.PDF")
outputFile08e = paste0(visDir, "aim_1/high_performing_locs/5_timeseries_plots_missing_data_low_sdi.PDF")

# Source shared functions -----
source(paste0(code_dir, "functions/prep_vax_trend_data.R"))
source(paste0(code_dir, "functions/prep_dx_trend_data.R"))
source(paste0(code_dir, "functions/strip_chars.R"), encoding = "UTF-8")
source(paste0(code_dir, "functions/extract_dhs_data.R"), encoding = "UTF-8")

# Set Boolean switches -----
prep_vax_trends  = FALSE  # this reads in and preps data on vaccination trends
prep_sdi         = FALSE # this reads in and preps SDI location values 
prep_dx_trends   = FALSE # this reads in GBD data on vaccine-preventable diseases
run_extract_dhs  = FALSE # this reads in DHS data and extracts relevant variables

merge_files      = FALSE # merges SDI, vaccine coverage trends and disease trends by location

id_low_sdi       = FALSE # identifies highest performing countries in the low SDI category
id_med_sdi       = FALSE # identifies highest performing countries in the medium SDI category
id_high_sdi      = FALSE # identifies highest performing countries in the high SDI category

prep_dhs_data    = FALSE # reads in extracted DHS data and prepares for further analyses

# Source scripts in prep pipeline -----

# ---------------------------------------
# Prep vaccination trends data 
# ---------------------------------------
if(prep_vax_trends == TRUE){
  source('./02_prep_vaccination_trend_data.R')
}

# ---------------------------------------
# Prep SDI data 
# ---------------------------------------
if(prep_sdi == TRUE){
  source('./03_prep_sdi_data.R')
}

# ---------------------------------------
# Prep disease trends data
# ---------------------------------------
if(prep_dx_trends == TRUE){
  source('./04_prep_disease_data.R')
}

# ---------------------------------------
# Extract DHS data
# ---------------------------------------
if(run_extract_dhs == TRUE){
  source('./05_run_extract_dhs_data.R')
}

# ---------------------------------------
# Merge major data sets together
# --------------------------------------- 
if(merge_files == TRUE){
  source('./06_merge_data.R')
}

# ---------------------------------------
# Exploratory graphs etc 
# ---------------------------------------

# ---------------------------------------
# Identify outlier countries
# ---------------------------------------
if (id_low_sdi == TRUE){
  source('./08_id_countries_low_sdi.R')
}

# if(id_med_sdi == TRUE){
#   source('.archive/08b_id_countries_med_sdi.R')
# }
# 
# if(id_high_sdi == TRUE){
#   source('.archive/08c_id_countries_high_sdi.R')
# }

# ---------------------------------------
# Prep DHS data for analyses
# ---------------------------------------
if (prep_dhs_data == TRUE){
  source('./09_prep_dhs_data.R')
}

