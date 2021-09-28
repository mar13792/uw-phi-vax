# Author: Francisco Rios 
# Purpose: Prep Vaccination coverage data for analyses
# Date: Last modified July 13, 2021

####### read in vaccine trend data

# Read in list of files to prep
file_list <- data.table(read_excel(paste0(data_dir, "list_of_data_used.xlsx")))

# subset files to latest vaccine trends data
file_list <- file_list[data_type=="vaccination_trends" & disease=="all" & gbd_cycle=="2020"]

for(i in 1:nrow(file_list)){
  # Set up file path 
  file_dir = paste0(data_dir, file_list$data_type[i], '/', file_list$data_source[i], '/' )
  
  # set up arguments
  args <- list(file_dir, file_list$file_name[i], file_list$containing_folder[i], file_list$data_source[i])
  
  ### RUN THE PREP FUNCTION HERE ###
  tmpData = do.call(extract_dhs_data, args)
  
  #Add indexing data
  append_cols = file_list[i, .(file_name, data_source)]
  
  stopifnot(nrow(append_cols)==1)
  
  tmpData = cbind(tmpData, append_cols)
  
  #Bind data together 
  if(i==1){
    prepped_vax_data = tmpData
  } else {
    prepped_vax_data = rbind(prepped_vax_data, tmpData, use.names=TRUE, fill = TRUE)
  }
  
  print(paste0(i, " ", file_list$data_type[i], " ", file_list$disease[i], " ", file_list$file_name[i])) ## if the code breaks, you know which file it broke on
}

# formatting of data
#--if necessary--

# save prepped data 
saveRDS(prepped_vax_data, outputFile2a)

# print final statement
print("Step 2a: Reading in vaccination trend data completed.")
