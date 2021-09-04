# Author: Francisco Rios 
# Purpose: Set up R for prepping UW PHI Vaccination Data
# Date: Last modified August 24, 2021

###############################
# load required packages
###############################
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

###############################
# important variables
###############################

# set shared team Google drive dynamically
if (Sys.info()[1]=='Windows'){
    g_drive = "G:/.shortcut-targets-by-id/1P7ITMVB9x01fuYfHW8-uWogw4SpbuvwO/Merck Vaccine Improvement Index Project/"
    code_dir = "tbd"
  } else {
    g_drive = "/Volumes/GoogleDrive/.shortcut-targets-by-id/1P7ITMVB9x01fuYfHW8-uWogw4SpbuvwO/Merck Vaccine Improvement Index Project/"
    code_dir = ("~/Documents/uw-phi-vax/")
    }

setwd(code_dir)
data_dir <- paste0(g_drive,"Data/")
prepped_data_dir <- paste0(data_dir,"prepped_data/")
codebook_directory <- paste0(data_dir,"documentation/codebooks/")
visDir <- paste0(g_drive,"Visualizations/")
aim1_vis <- paste0(visDir, "aim_1/")

##############################################################
# output file names that will be created in future scripts
##############################################################
outputFile2a = paste0(prepped_data_dir, "2a_vaccine_trends.RDS")
outputFile2b = paste0(prepped_data_dir, "2b_sdi.RDS")
outputFile2c = paste0(prepped_data_dir, "2c_disease_trends.RDS")
outputFile2d = paste0(prepped_data_dir, "2d_raw_dhs_vaccination_data_all_locations.RDS")

outputFile3 = paste0(prepped_data_dir, "3a_merged_data.RDS")
outputFile4 = paste0(visDir, "aim1_sample_visualizations.PDF")
outputFile4b = paste0(visDir, "aim1_sample_country_trends.PDF")

# names of visualizations that will be created in future scripts (maybe consider moving to the actual file script since some of these are archived already)
outputFile5a = paste0(aim1_vis, "exemplar_countries/1_outliers_low_sdi.PDF")
outputFile5b = paste0(aim1_vis, "exemplar_countries/2_barplot_low_sdi.PDF")
outputFile5c = paste0(aim1_vis, "exemplar_countries/3_detailed_table_low_sdi.PDF")
outputFile5d = paste0(aim1_vis, "exemplar_countries/4_timeseries_plots_improvement_low_sdi.PDF")
outputFile5e = paste0(aim1_vis, "exemplar_countries/5_timeseries_plots_missing_data_low_sdi.PDF")

outputFile5f = paste0(aim1_vis, "exemplar_countries/6_outliers_med_sdi.PDF")
outputFile5g = paste0(aim1_vis, "exemplar_countries/7_barplot_med_sdi.PDF")
outputFile5h = paste0(aim1_vis, "exemplar_countries/8_detailed_table_med_sdi.PDF")
outputFile5i = paste0(aim1_vis, "exemplar_countries/9_timeseries_plots_improvement_med_sdi.PDF")
outputFile5j = paste0(aim1_vis, "exemplar_countries/10_timeseries_plots_missing_data_med_sdi.PDF")

outputFile5k = paste0(aim1_vis, "exemplar_countries/11_outliers_high_sdi.PDF")
outputFile5l = paste0(aim1_vis, "exemplar_countries/12_barplot_high_sdi.PDF")
outputFile5m = paste0(aim1_vis, "exemplar_countries/13_detailed_table_high_sdi.PDF")
outputFile5n = paste0(aim1_vis, "exemplar_countries/14_timeseries_plots_improvement_high_sdi.PDF")
outputFile5o = paste0(aim1_vis, "exemplar_countries/15_timeseries_plots_missing_data_high_sdi.PDF")

outputFile5p = paste0(aim1_vis, "exemplar_countries/16_outliers_low_sdi_decade.PDF")
outputFile5q = paste0(aim1_vis, "exemplar_countries/17_barplot_low_sdi_decade.PDF")
outputFile5r = paste0(aim1_vis, "exemplar_countries/18_detailed_table_low_sdi_decade.PDF")

outputFile6a = paste0(prepped_data_dir, "6a_prepped_dhs_vaccination_data_for_analysis.RDS")
# outputFile6a = paste0(prepped_data_dir, "6b_dhs_data_for_analyses.RDS")

###############################
# source shared functions
###############################
source(paste0(code_dir, "functions/", "prep_vax_trend_data.R"))
source(paste0(code_dir, "functions/", "prep_dx_trend_data.R"))
source(paste0(code_dir, "functions/", "strip_chars.R"), encoding = "UTF-8")
source(paste0(code_dir, "functions/", "extract_dhs_data.R"), encoding = "UTF-8")


###############################
# set Boolean switches
##############################
prep_vax_trends = FALSE
prep_sdi = FALSE
prep_dx_trends = FALSE

run_extract_dhs_data = FALSE

merge_files = FALSE

id_low_sdi = FALSE
id_med_sdi = FALSE
id_high_sdi = FALSE

prep_dhs_data = FALSE

################################
#
################################

# ---------------------------------------
# Prep vaccination trends data 
# ---------------------------------------
if(prep_vax_trends == TRUE){
  source('./2a_prep_vaccination_trend_data.R')
}

# ---------------------------------------
# Prep SDI data 
# ---------------------------------------
if(prep_sdi == TRUE){
  source('./2b_prep_sdi_data.R')
}

# ---------------------------------------
# Prep disease trends data
# ---------------------------------------
if(prep_dx_trends == TRUE){
  source('./2c_prep_disease_data.R')
}

# ---------------------------------------
# Prep DHS data
# ---------------------------------------
if(run_extract_dhs_data == TRUE){
  source('./2d_run_extract_dhs_data.R')
}

# ---------------------------------------
# Merge major data sets together
# --------------------------------------- 
if(merge_files == TRUE){
  source('./3_merge_data.R')
}

# ---------------------------------------
# Exploratory graphs etc 
# ---------------------------------------

# ---------------------------------------
# Identify outlier countries
# ---------------------------------------
if(id_low_sdi == TRUE){
  source('./5a_id_countries_low_sdi.R')
}

if(id_med_sdi == TRUE){
  source('./5b_id_countries_med_sdi.R')
}

if(id_high_sdi == TRUE){
  source('./5c_id_countries_high_sdi.R')
}


