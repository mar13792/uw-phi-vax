# Author: Francisco Rios 
# Purpose: prep disease trend data
# Date: Last modified July 29, 2021

####### read in vaccine-preventable disease trend data

# Read in list of files to prep
file_list <- data.table(read_excel(paste0(local_data_dir, "data_file_list.xlsx")))

# subset files to latest disease trends data
file_list <- file_list[data_type=="disease_trends" & gbd_cycle=="2019"]

print("Now prepping:")

for(i in 1:nrow(file_list)){
  # Set up file path 
  file_dir = paste0(data_dir, file_list$data_type[i], '/', file_list$data_source[i], '/' )
  
  # set up arguments
  args <- list(file_dir, file_list$file_name[i], file_list$data_type[i])
  
  ### RUN THE PREP FUNCTION HERE ###
  tmpData = do.call(prep_dx_trend_data, args)
  
  #Add indexing data
  append_cols = file_list[i, .(file_name, data_type, data_source)]
  
  stopifnot(nrow(append_cols)==1)
  
  tmpData = cbind(tmpData, append_cols)
  
  #Bind data together 
  if(i==1){
    prepped_dx_data = tmpData
  } else {
    prepped_dx_data = rbind(prepped_dx_data, tmpData, use.names=TRUE, fill = TRUE)
  }

  print(paste0(i, " ", file_list$data_type[i], " ", file_list$disease[i], " ", file_list$file_name[i])) ## if the code breaks, you know which file it broke on
}

# formatting of data
#--if necessary--

# save prepped data 
saveRDS(prepped_dx_data, outputFile2c)

# print final statement
print("Step 2c: Reading in disease trend data completed.")
