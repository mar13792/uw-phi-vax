# # Author: Francisco Rios 
# # Purpose: prep dhs data for 'missed opportunities' analyses
# # Date: Last modified August 12, 2021

# read in raw dataset
dhs_data <- as.data.table(read_dta(file=paste0(data_dir,"vaccination_trends/dhs/LBIR7ADT/LBIR7AFL.DTA")))

# to-do #####
# pivot the vaccine dates out wider because currently not stored as tidy variable

###############################################################
# subset columns into id variables and variables that need tidying
###############################################################
idVars = c("caseid", "v000", "v005", "v007", "v006", "v016") # id variables for merging
demoVars = c("v012", "v106", "v151", "v155", "v201", "v716", "v501", "v136", "v190", "v191a", "v025") # demographic variables

dobyearVars = names(dhs_data)[grepl('b2_', names(dhs_data))] # child's year of birth
dobmonthVars = names(dhs_data)[grepl('b1_', names(dhs_data))] # child's month of birth
dobdayVars = names(dhs_data)[grepl('b17_', names(dhs_data))] # child's day of birth

childsexVars = names(dhs_data)[grepl('b4', names(dhs_data))] # child's sex
childlivingVars = names(dhs_data)[grepl('b5', names(dhs_data))] # child dead or living status

vaxcardVars = names(dhs_data)[grepl('h1a', names(dhs_data))] # indicates whether child had vaccine card
vaxcarddateVars = names(dhs_data)[grepl('h2_|h3_|h4_|h5_|h6_|h7_|h8_|h9_|h9a_|h0_|h50_|h51_|h54_|h55_|h56_|h57_|h58_|h59_|h60_|h61_|h62_|h63_|h64_|h65_|h66_', names(dhs_data))]

vaxyearVars = names(dhs_data)[grepl('h.*y_', names(dhs_data))] # year vaccines given
vaxmonthVars = names(dhs_data)[grepl('h.*m_', names(dhs_data))] # months vaccine given
vaxdayVars = names(dhs_data)[grepl('h.*d_', names(dhs_data))] # day vaccine given

###############################################################
# tidy the data set according to variables
###############################################################

######################
# dob year variable
######################
dobyearData <- dhs_data[, c(idVars, dobyearVars), with=F]

dobyearData <- dobyearData %>%
  pivot_longer(
    !c(caseid, v000, v005, v007, v006, v016),
    names_to = c("variable", "child"),
    names_sep = "_",
    values_to = "birth_year",
    values_drop_na = FALSE
  )

# drop extra variable column
dobyearData <- select(dobyearData, -(variable))

######################
# dob month variable
######################
dobmonthData <- dhs_data[, c(idVars, dobmonthVars), with=F]

dobmonthData <- dobmonthData %>%
  pivot_longer(
    !c(caseid, v000, v005, v007, v006, v016),
    names_to = c("variable", "child"),
    names_sep = "_",
    values_to = "birth_month",
    values_drop_na = FALSE
  )

# drop extra variable column
dobmonthData <- select(dobmonthData, -(variable))

######################
# dob day variable
######################
dobdayData <- dhs_data[, c(idVars, dobdayVars), with=F]

dobdayData <- dobdayData %>%
  pivot_longer(
    !c(caseid, v000, v005, v007, v006, v016),
    names_to = c("variable", "child"),
    names_sep = "_",
    values_to = "birth_day",
    values_drop_na = FALSE
  )

# drop extra variable column
dobdayData <- select(dobdayData, -(variable))

######################
# child sex data
######################
childsexData <- dhs_data[, c(idVars, childsexVars), with=F]

childsexData <- childsexData %>%
  pivot_longer(
    !c(caseid, v000, v005, v007, v006, v016),
    names_to = c("remove", "child"),
    names_sep = "_",
    values_to = "sex_of_child",
    values_drop_na = FALSE
  )

# remove extra column
childsexData <- select(childsexData, -(remove))

######################
# child living status
######################
childlivingData <- dhs_data[, c(idVars, childlivingVars), with=F]

childlivingData <- childlivingData %>%
  pivot_longer(
    !c(caseid, v000, v005, v007, v006, v016),
    names_to = c("remove", "child"),
    names_sep = "_",
    values_to = "is_child_alive",
    values_drop_na = FALSE
  )

# remove extra column
childlivingData <- select(childlivingData, -(remove))

######################
# vaccine card coverage data
######################
vaxcardData <- dhs_data[, c(idVars, vaxcardVars), with=F]

vaxcardData <- vaxcardData %>%
  pivot_longer(
    !c(caseid, v000, v005, v007, v006, v016),
    names_to = c("remove", "child"),
    names_sep = "_",
    values_to = "has_health_card",
    values_drop_na = FALSE
  )

# remove extra column
vaxcardData <- select(vaxcardData, -(remove))

######################
# vaccines card has date recorded (yes or no)
######################
vaxcarddateData <- as_tibble(dhs_data[, c(idVars, vaxcarddateVars), with=F])

vaxcarddateData <- vaxcarddateData %>%
  pivot_longer(
    !c(caseid, v000, v005, v007, v006, v016),
    names_to = c("vaccine", "child"),
    names_sep = "_",
    values_to = "is_date_recorded",
    values_drop_na = FALSE
  )

######################
# vaccine year variable
######################
vaxyearData <- dhs_data[, c(idVars, vaxyearVars), with=F]

vaxyearData <- vaxyearData %>%
  pivot_longer(
    !c(caseid, v000, v005, v007, v006, v016),
    names_to = c("vaccine", "child"),
    names_sep = "_",
    values_to = "year",
    values_drop_na = FALSE
  )

# remove suffix specifying year vaccine given
vaxyearData <- vaxyearData %>% 
  separate(vaccine, c("vaccine", "y"), -1)

# drop extra column
vaxyearData <- select(vaxyearData, -(y))

######################
# vaccine month variable
######################
vaxmonthData <- dhs_data[, c(idVars, vaxmonthVars), with=F]

vaxmonthData <- vaxmonthData %>%
  pivot_longer(
    !c(caseid, v000, v005, v007, v006, v016),
    names_to = c("vaccine", "child"),
    names_sep = "_",
    values_to = "month",
    values_drop_na = FALSE
  )

# remove suffix specifying year vaccine given
vaxmonthData <- vaxmonthData %>% 
  separate(vaccine, c("vaccine", "m"), -1)

# drop extra column
vaxmonthData <- select(vaxmonthData, -(m))

######################
# vaccine day variable
######################
vaxdayData <- dhs_data[, c(idVars, vaxdayVars), with=F]

vaxdayData <- vaxdayData %>%
  pivot_longer(
    !c(caseid, v000, v005, v007, v006, v016),
    names_to = c("vaccine", "child"),
    names_sep = "_",
    values_to = "day",
    values_drop_na = FALSE
  )

# remove suffix specifying year vaccine given
vaxdayData <- vaxdayData %>% 
  separate(vaccine, c("vaccine", "d"), -1)

# drop extra column
vaxdayData <- select(vaxdayData, -(d))

###############################################################
# merge different data tables together
###############################################################

##### create table of demographic variables
demoData <- as_tibble(dhs_data[, c(idVars, demoVars), with=F])

##### merge the datasets that are identified by idVars and child vars
mergeCols <- c(idVars, "child")
dt1 <- demoData %>% full_join(dobyearData, by = idVars) %>% 
  full_join(dobmonthData,  by = mergeCols) %>%
  full_join(dobdayData, by = mergeCols) %>%
  full_join(childsexData, by = mergeCols) %>%
  full_join(childlivingData, by = mergeCols)

##### merge the datasets that are identified by idVars and vaccine variables
vaxmergeCols <- c(idVars, "child", "vaccine")
dt2 <- vaxcardData %>% full_join(vaxcarddateData, by = mergeCols) %>%
  full_join(vaxyearData, by = vaxmergeCols) %>%
  full_join(vaxmonthData, by = vaxmergeCols) %>%
  full_join(vaxdayData, by = vaxmergeCols)

# re-code child variable in second datatable (dt2) in order to facilitate merge
dt2 <- dt2 %>% mutate(child=recode(child, 
                                   `1`="01",
                                   `2`="02",
                                   `3`="03",
                                   `4`="04",
                                   `5`="05",
                                   `6`="06"))

##### merge both datasets together
prepped_dhs_data <- dt1 %>% full_join(dt2, by = mergeCols)

###############################################################
# recode important variables
###############################################################

# bcg, dpt, polio, mea (measles only), hepb, pent, pneu, rota, hepb, hib
prepped_dhs_data <- prepped_dhs_data %>% mutate(vaccine=recode(vaccine, 
                                                               `h2`="bcg",
                                                               `h3`="dpt1",
                                                               `h4`="pol1",
                                                               `h5`="dpt2",
                                                               `h6`="pol2",
                                                               `h7`="dpt3",
                                                               `h8`="pol3",
                                                               `h9`="mea1",
                                                               `h9a`="mea2",
                                                               `h0`="pol0",
                                                               `h50`="hepbbirth",
                                                               `h51`="pent1",
                                                               `h52`="pent2",
                                                               `h53`="pent3",
                                                               `h54`="pneu1",
                                                               `h55`="pneu2",
                                                               `h56`="pneu3",
                                                               `h57`="rota1",
                                                               `h58`="rota2",
                                                               `h59`="rota3",
                                                               `h60`="poln",
                                                               `h61`="hepb1",
                                                               `h62`="hepb2",
                                                               `h63`="hepb3",
                                                               `h64`="hib1",
                                                               `h65`="hib2",
                                                               `h66`="hib3"))

###############################################################
# calculate new variables
###############################################################

# date interview was conducted and date of birth (dob)
prepped_dhs_data <- prepped_dhs_data %>%
  mutate(intv_date := make_date(v007, v006, v016)) %>%
  mutate(dob := make_date(birth_year, birth_month, birth_day))

###############################################################
# subset rows
###############################################################

# drop rows that have na for key columns
prepped_dhs_data <- prepped_dhs_data %>% drop_na(dob, sex_of_child, is_child_alive)

# keep only info for the six most recent births
target <- c("01", "02", "03", "04", "05", "06")
prepped_dhs_data <- filter(prepped_dhs_data, child %in% target)

# remove child health variables that don't relate to vaccines
vaccines <- c("bcg", "dpt1", "pol1", "dpt2", "pol2", "dpt3", "pol3", "mea1", 
              "mea2", "pol0", "hepbbirth", "pent1", "pneu1", "pneu2", "pneu3", 
              "rota1", "rota2", "rota3", "poln", "hepb1", "hepb2", "hepb3", "hepb",
              "hib1", "hib2", "hib3")
prepped_dhs_data <- filter(prepped_dhs_data, vaccine %in% vaccines)

###############################################################
# fix variable labeling system and factor
###############################################################

###############################################################
# read in next year and country file and bind dataset together once confirmed what the final variable names will be--
# maybe create a codebook for dhs derived dataset which could be read in to make sure all names are consistent
###############################################################

###############################################################
# save as an r-object
###############################################################

###############################################################
# create table to compare groups with and without vaccination coverage card --this should go in a separate script
###############################################################
# table1(~ factor(b4_01) | h51_1, data=data2)

# #####
# # bcg (h51) for each child
# dhs_data[, bcg_1 := as.Date(paste(h51y_1, h51m_1, h51d_1, sep="-"), "%Y-%m-%d")]
# dhs_data[, bcg_2 := as.Date(paste(h51y_2, h51m_2, h51d_2, sep="-"), "%Y-%m-%d")]
# dhs_data[, bcg_3 := as.Date(paste(h51y_3, h51m_3, h51d_3, sep="-"), "%Y-%m-%d")]
# dhs_data[, bcg_4 := as.Date(paste(h51y_4, h51m_4, h51d_4, sep="-"), "%Y-%m-%d")]
# dhs_data[, bcg_5 := as.Date(paste(h51y_5, h51m_5, h51d_5, sep="-"), "%Y-%m-%d")]
# dhs_data[, bcg_6 := as.Date(paste(h51y_6, h51m_6, h51d_6, sep="-"), "%Y-%m-%d")]
# 

# # dpt 1 (h3) for each child
# dhs_data[, dpt1_1 := as.Date(paste(h3y_1, h3m_1, h3d_1, sep="-"), "%Y-%m-%d")]
# dhs_data[, dpt1_2 := as.Date(paste(h3y_2, h3m_2, h3d_2, sep="-"), "%Y-%m-%d")]
# dhs_data[, dpt1_3 := as.Date(paste(h3y_3, h3m_3, h3d_3, sep="-"), "%Y-%m-%d")]
# dhs_data[, dpt1_4 := as.Date(paste(h3y_4, h3m_4, h3d_4, sep="-"), "%Y-%m-%d")]
# dhs_data[, dpt1_5 := as.Date(paste(h3y_5, h3m_5, h3d_5, sep="-"), "%Y-%m-%d")]
# dhs_data[, dpt1_6 := as.Date(paste(h3y_6, h3m_6, h3d_6, sep="-"), "%Y-%m-%d")]
# 
# # polio 1 (h4) for each child
# dhs_data[, pol1_1 := as.Date(paste(h4y_1, h4m_1, h4d_1, sep="-"), "%Y-%m-%d")]
# dhs_data[, pol1_2 := as.Date(paste(h4y_2, h4m_2, h4d_2, sep="-"), "%Y-%m-%d")]
# dhs_data[, pol1_3 := as.Date(paste(h4y_3, h4m_3, h4d_3, sep="-"), "%Y-%m-%d")]
# dhs_data[, pol1_4 := as.Date(paste(h4y_4, h4m_4, h4d_4, sep="-"), "%Y-%m-%d")]
# dhs_data[, pol1_5 := as.Date(paste(h4y_5, h4m_5, h4d_5, sep="-"), "%Y-%m-%d")]
# dhs_data[, pol1_6 := as.Date(paste(h4y_6, h4m_6, h4d_6, sep="-"), "%Y-%m-%d")]
# 
# # dpt 2 (h5) for each child
# dhs_data[, dpt2_1 := as.Date(paste(h5y_1, h5m_1, h5d_1, sep="-"), "%Y-%m-%d")]
# dhs_data[, dpt2_2 := as.Date(paste(h5y_2, h5m_2, h5d_2, sep="-"), "%Y-%m-%d")]
# dhs_data[, dpt2_3 := as.Date(paste(h5y_3, h5m_3, h5d_3, sep="-"), "%Y-%m-%d")]
# dhs_data[, dpt2_4 := as.Date(paste(h5y_4, h5m_4, h5d_4, sep="-"), "%Y-%m-%d")]
# dhs_data[, dpt2_5 := as.Date(paste(h5y_5, h5m_5, h5d_5, sep="-"), "%Y-%m-%d")]
# dhs_data[, dpt2_6 := as.Date(paste(h5y_6, h5m_6, h5d_6, sep="-"), "%Y-%m-%d")]
# 
# # polio 2 (h6) for each child
# dhs_data[, pol2_1 := as.Date(paste(h6y_1, h6m_1, h6d_1, sep="-"), "%Y-%m-%d")]
# dhs_data[, pol2_2 := as.Date(paste(h6y_2, h6m_2, h6d_2, sep="-"), "%Y-%m-%d")]
# dhs_data[, pol2_3 := as.Date(paste(h6y_3, h6m_3, h6d_3, sep="-"), "%Y-%m-%d")]
# dhs_data[, pol2_4 := as.Date(paste(h6y_4, h6m_4, h6d_4, sep="-"), "%Y-%m-%d")]
# dhs_data[, pol2_5 := as.Date(paste(h6y_5, h6m_5, h6d_5, sep="-"), "%Y-%m-%d")]
# dhs_data[, pol2_6 := as.Date(paste(h6y_6, h6m_6, h6d_6, sep="-"), "%Y-%m-%d")]
# 
# # measles 1 (h9) for each child
# dhs_data[, mea1_1 := as.Date(paste(h9y_1, h9m_1, h9d_1, sep="-"), "%Y-%m-%d")]
# dhs_data[, mea1_2 := as.Date(paste(h9y_2, h9m_2, h9d_2, sep="-"), "%Y-%m-%d")]
# dhs_data[, mea1_3 := as.Date(paste(h9y_3, h9m_3, h9d_3, sep="-"), "%Y-%m-%d")]
# dhs_data[, mea1_4 := as.Date(paste(h9y_4, h9m_4, h9d_4, sep="-"), "%Y-%m-%d")]
# dhs_data[, mea1_5 := as.Date(paste(h9y_5, h9m_5, h9d_5, sep="-"), "%Y-%m-%d")]
# dhs_data[, mea1_6 := as.Date(paste(h9y_6, h9m_6, h9d_6, sep="-"), "%Y-%m-%d")]
# 
# # measles 2 (h9a) for each child
# dhs_data[, mea2_1 := as.Date(paste(h9ay_1, h9am_1, h9ad_1, sep="-"), "%Y-%m-%d")]
# dhs_data[, mea2_2 := as.Date(paste(h9ay_2, h9am_2, h9ad_2, sep="-"), "%Y-%m-%d")]
# dhs_data[, mea2_3 := as.Date(paste(h9ay_3, h9am_3, h9ad_3, sep="-"), "%Y-%m-%d")]
# dhs_data[, mea2_4 := as.Date(paste(h9ay_4, h9am_4, h9ad_4, sep="-"), "%Y-%m-%d")]
# dhs_data[, mea2_5 := as.Date(paste(h9ay_5, h9am_5, h9ad_5, sep="-"), "%Y-%m-%d")]
# dhs_data[, mea2_6 := as.Date(paste(h9ay_6, h9am_6, h9ad_6, sep="-"), "%Y-%m-%d")]
# 
# # polio 0 (at birth) (h0) for each child
# dhs_data[, pol0_1 := as.Date(paste(h0y_1, h0m_1, h0d_1, sep="-"), "%Y-%m-%d")]
# dhs_data[, pol0_2 := as.Date(paste(h0y_2, h0m_2, h0d_2, sep="-"), "%Y-%m-%d")]
# dhs_data[, pol0_3 := as.Date(paste(h0y_3, h0m_3, h0d_3, sep="-"), "%Y-%m-%d")]
# dhs_data[, pol0_4 := as.Date(paste(h0y_4, h0m_4, h0d_4, sep="-"), "%Y-%m-%d")]
# dhs_data[, pol0_5 := as.Date(paste(h0y_5, h0m_5, h0d_5, sep="-"), "%Y-%m-%d")]
# dhs_data[, pol0_6 := as.Date(paste(h0y_6, h0m_6, h0d_6, sep="-"), "%Y-%m-%d")]
# 
# # hep b at birth (h50) for each child
# dhs_data[, hepb_1 := as.Date(paste(h50y_1, h50m_1, h50d_1, sep="-"), "%Y-%m-%d")]
# dhs_data[, hepb_2 := as.Date(paste(h50y_2, h50m_2, h50d_2, sep="-"), "%Y-%m-%d")]
# dhs_data[, hepb_3 := as.Date(paste(h50y_3, h50m_3, h50d_3, sep="-"), "%Y-%m-%d")]
# dhs_data[, hepb_4 := as.Date(paste(h50y_4, h50m_4, h50d_4, sep="-"), "%Y-%m-%d")]
# dhs_data[, hepb_5 := as.Date(paste(h50y_5, h50m_5, h50d_5, sep="-"), "%Y-%m-%d")]
# dhs_data[, hepb_6 := as.Date(paste(h50y_6, h50m_6, h50d_6, sep="-"), "%Y-%m-%d")]
# 
# # pent1 (h51) for each child
# dhs_data[, pent1_1 := as.Date(paste(h51y_1, h51m_1, h51d_1, sep="-"), "%Y-%m-%d")]
# dhs_data[, pent1_2 := as.Date(paste(h51y_2, h51m_2, h51d_2, sep="-"), "%Y-%m-%d")]
# dhs_data[, pent1_3 := as.Date(paste(h51y_3, h51m_3, h51d_3, sep="-"), "%Y-%m-%d")]
# dhs_data[, pent1_4 := as.Date(paste(h51y_4, h51m_4, h51d_4, sep="-"), "%Y-%m-%d")]
# dhs_data[, pent1_5 := as.Date(paste(h51y_5, h51m_5, h51d_5, sep="-"), "%Y-%m-%d")]
# dhs_data[, pent1_6 := as.Date(paste(h51y_6, h51m_6, h51d_6, sep="-"), "%Y-%m-%d")]
# 
# # pent2 (h52) for each child
# dhs_data[, pent2_1 := as.Date(paste(h52y_1, h52m_1, h52d_1, sep="-"), "%Y-%m-%d")]
# dhs_data[, pent2_2 := as.Date(paste(h52y_2, h52m_2, h52d_2, sep="-"), "%Y-%m-%d")]
# dhs_data[, pent2_3 := as.Date(paste(h52y_3, h52m_3, h52d_3, sep="-"), "%Y-%m-%d")]
# dhs_data[, pent2_4 := as.Date(paste(h52y_4, h52m_4, h52d_4, sep="-"), "%Y-%m-%d")]
# dhs_data[, pent2_5 := as.Date(paste(h52y_5, h52m_5, h52d_5, sep="-"), "%Y-%m-%d")]
# dhs_data[, pent2_6 := as.Date(paste(h52y_6, h52m_6, h52d_6, sep="-"), "%Y-%m-%d")]
# 
# # pent3 (h53) for each child
# dhs_data[, pent3_1 := as.Date(paste(h53y_1, h53m_1, h53d_1, sep="-"), "%Y-%m-%d")]
# dhs_data[, pent3_2 := as.Date(paste(h53y_2, h53m_2, h53d_2, sep="-"), "%Y-%m-%d")]
# dhs_data[, pent3_3 := as.Date(paste(h53y_3, h53m_3, h53d_3, sep="-"), "%Y-%m-%d")]
# dhs_data[, pent3_4 := as.Date(paste(h53y_4, h53m_4, h53d_4, sep="-"), "%Y-%m-%d")]
# dhs_data[, pent3_5 := as.Date(paste(h53y_5, h53m_5, h53d_5, sep="-"), "%Y-%m-%d")]
# dhs_data[, pent3_6 := as.Date(paste(h53y_6, h53m_6, h53d_6, sep="-"), "%Y-%m-%d")]
# 
# # pneu 1 (h54) for each child
# dhs_data[, pneu1_1 := as.Date(paste(h54y_1, h54m_1, h54d_1, sep="-"), "%Y-%m-%d")]
# dhs_data[, pneu1_2 := as.Date(paste(h54y_2, h54m_2, h54d_2, sep="-"), "%Y-%m-%d")]
# dhs_data[, pneu1_3 := as.Date(paste(h54y_3, h54m_3, h54d_3, sep="-"), "%Y-%m-%d")]
# dhs_data[, pneu1_4 := as.Date(paste(h54y_4, h54m_4, h54d_4, sep="-"), "%Y-%m-%d")]
# dhs_data[, pneu1_5 := as.Date(paste(h54y_5, h54m_5, h54d_5, sep="-"), "%Y-%m-%d")]
# dhs_data[, pneu1_6 := as.Date(paste(h54y_6, h54m_6, h54d_6, sep="-"), "%Y-%m-%d")]
# 
# # pneu 2 (h55) for each child
# dhs_data[, pneu2_1 := as.Date(paste(h55y_1, h55m_1, h55d_1, sep="-"), "%Y-%m-%d")]
# dhs_data[, pneu2_2 := as.Date(paste(h55y_2, h55m_2, h55d_2, sep="-"), "%Y-%m-%d")]
# dhs_data[, pneu2_3 := as.Date(paste(h55y_3, h55m_3, h55d_3, sep="-"), "%Y-%m-%d")]
# dhs_data[, pneu2_4 := as.Date(paste(h55y_4, h55m_4, h55d_4, sep="-"), "%Y-%m-%d")]
# dhs_data[, pneu2_5 := as.Date(paste(h55y_5, h55m_5, h55d_5, sep="-"), "%Y-%m-%d")]
# dhs_data[, pneu2_6 := as.Date(paste(h55y_6, h55m_6, h55d_6, sep="-"), "%Y-%m-%d")]
# 
# # pneu 3 (h56) for each child
# dhs_data[, pneu3_1 := as.Date(paste(h56y_1, h56m_1, h56d_1, sep="-"), "%Y-%m-%d")]
# dhs_data[, pneu3_2 := as.Date(paste(h56y_2, h56m_2, h56d_2, sep="-"), "%Y-%m-%d")]
# dhs_data[, pneu3_3 := as.Date(paste(h56y_3, h56m_3, h56d_3, sep="-"), "%Y-%m-%d")]
# dhs_data[, pneu3_4 := as.Date(paste(h56y_4, h56m_4, h56d_4, sep="-"), "%Y-%m-%d")]
# dhs_data[, pneu3_5 := as.Date(paste(h56y_5, h56m_5, h56d_5, sep="-"), "%Y-%m-%d")]
# dhs_data[, pneu3_6 := as.Date(paste(h56y_6, h56m_6, h56d_6, sep="-"), "%Y-%m-%d")]
# 
# # rota 1 (h57) for each child
# dhs_data[, rota1_1 := as.Date(paste(h57y_1, h57m_1, h57d_1, sep="-"), "%Y-%m-%d")]
# dhs_data[, rota1_2 := as.Date(paste(h57y_2, h57m_2, h57d_2, sep="-"), "%Y-%m-%d")]
# dhs_data[, rota1_3 := as.Date(paste(h57y_3, h57m_3, h57d_3, sep="-"), "%Y-%m-%d")]
# dhs_data[, rota1_4 := as.Date(paste(h57y_4, h57m_4, h57d_4, sep="-"), "%Y-%m-%d")]
# dhs_data[, rota1_5 := as.Date(paste(h57y_5, h57m_5, h57d_5, sep="-"), "%Y-%m-%d")]
# dhs_data[, rota1_6 := as.Date(paste(h57y_6, h57m_6, h57d_6, sep="-"), "%Y-%m-%d")]
# 
# # rota 2 (h58) for each child
# dhs_data[, rota2_1 := as.Date(paste(h58y_1, h58m_1, h58d_1, sep="-"), "%Y-%m-%d")]
# dhs_data[, rota2_2 := as.Date(paste(h58y_2, h58m_2, h58d_2, sep="-"), "%Y-%m-%d")]
# dhs_data[, rota2_3 := as.Date(paste(h58y_3, h58m_3, h58d_3, sep="-"), "%Y-%m-%d")]
# dhs_data[, rota2_4 := as.Date(paste(h58y_4, h58m_4, h58d_4, sep="-"), "%Y-%m-%d")]
# dhs_data[, rota2_5 := as.Date(paste(h58y_5, h58m_5, h58d_5, sep="-"), "%Y-%m-%d")]
# dhs_data[, rota2_6 := as.Date(paste(h58y_6, h58m_6, h58d_6, sep="-"), "%Y-%m-%d")]
# 
# # rota 3 (h59) for each child
# dhs_data[, rota3_1 := as.Date(paste(h59y_1, h59m_1, h59d_1, sep="-"), "%Y-%m-%d")]
# dhs_data[, rota3_2 := as.Date(paste(h59y_2, h59m_2, h59d_2, sep="-"), "%Y-%m-%d")]
# dhs_data[, rota3_3 := as.Date(paste(h59y_3, h59m_3, h59d_3, sep="-"), "%Y-%m-%d")]
# dhs_data[, rota3_4 := as.Date(paste(h59y_4, h59m_4, h59d_4, sep="-"), "%Y-%m-%d")]
# dhs_data[, rota3_5 := as.Date(paste(h59y_5, h59m_5, h59d_5, sep="-"), "%Y-%m-%d")]
# dhs_data[, rota3_6 := as.Date(paste(h59y_6, h59m_6, h59d_6, sep="-"), "%Y-%m-%d")]
# 
# # polio inactive (h60) for each child
# dhs_data[, poln_1 := as.Date(paste(h60y_1, h60m_1, h60d_1, sep="-"), "%Y-%m-%d")]
# dhs_data[, poln_2 := as.Date(paste(h60y_2, h60m_2, h60d_2, sep="-"), "%Y-%m-%d")]
# dhs_data[, poln_3 := as.Date(paste(h60y_3, h60m_3, h60d_3, sep="-"), "%Y-%m-%d")]
# dhs_data[, poln_4 := as.Date(paste(h60y_4, h60m_4, h60d_4, sep="-"), "%Y-%m-%d")]
# dhs_data[, poln_5 := as.Date(paste(h60y_5, h60m_5, h60d_5, sep="-"), "%Y-%m-%d")]
# dhs_data[, poln_6 := as.Date(paste(h60y_6, h60m_6, h60d_6, sep="-"), "%Y-%m-%d")]
# 
# # hep b 1 (h61) for each child
# dhs_data[, hepb1_1 := as.Date(paste(h61y_1, h61m_1, h61d_1, sep="-"), "%Y-%m-%d")]
# dhs_data[, hepb1_2 := as.Date(paste(h61y_2, h61m_2, h61d_2, sep="-"), "%Y-%m-%d")]
# dhs_data[, hepb1_3 := as.Date(paste(h61y_3, h61m_3, h61d_3, sep="-"), "%Y-%m-%d")]
# dhs_data[, hepb1_4 := as.Date(paste(h61y_4, h61m_4, h61d_4, sep="-"), "%Y-%m-%d")]
# dhs_data[, hepb1_5 := as.Date(paste(h61y_5, h61m_5, h61d_5, sep="-"), "%Y-%m-%d")]
# dhs_data[, hepb1_6 := as.Date(paste(h61y_6, h61m_6, h61d_6, sep="-"), "%Y-%m-%d")]
# 
# # hep b 2 (h62) for each child
# dhs_data[, hepb2_1 := as.Date(paste(h62y_1, h62m_1, h62d_1, sep="-"), "%Y-%m-%d")]
# dhs_data[, hepb2_2 := as.Date(paste(h62y_2, h62m_2, h62d_2, sep="-"), "%Y-%m-%d")]
# dhs_data[, hepb2_3 := as.Date(paste(h62y_3, h62m_3, h62d_3, sep="-"), "%Y-%m-%d")]
# dhs_data[, hepb2_4 := as.Date(paste(h62y_4, h62m_4, h62d_4, sep="-"), "%Y-%m-%d")]
# dhs_data[, hepb2_5 := as.Date(paste(h62y_5, h62m_5, h62d_5, sep="-"), "%Y-%m-%d")]
# dhs_data[, hepb2_6 := as.Date(paste(h62y_6, h62m_6, h62d_6, sep="-"), "%Y-%m-%d")]
# 
# # hep b 3 (h63) for each child
# dhs_data[, hepb3_1 := as.Date(paste(h63y_1, h63m_1, h63d_1, sep="-"), "%Y-%m-%d")]
# dhs_data[, hepb3_2 := as.Date(paste(h63y_2, h63m_2, h63d_2, sep="-"), "%Y-%m-%d")]
# dhs_data[, hepb3_3 := as.Date(paste(h63y_3, h63m_3, h63d_3, sep="-"), "%Y-%m-%d")]
# dhs_data[, hepb3_4 := as.Date(paste(h63y_4, h63m_4, h63d_4, sep="-"), "%Y-%m-%d")]
# dhs_data[, hepb3_5 := as.Date(paste(h63y_5, h63m_5, h63d_5, sep="-"), "%Y-%m-%d")]
# dhs_data[, hepb3_6 := as.Date(paste(h63y_6, h63m_6, h63d_6, sep="-"), "%Y-%m-%d")]
# 
# # Hib 1 (h64) for each child
# dhs_data[, hib1_1 := as.Date(paste(h64y_1, h64m_1, h64d_1, sep="-"), "%Y-%m-%d")]
# dhs_data[, hib1_2 := as.Date(paste(h64y_2, h64m_2, h64d_2, sep="-"), "%Y-%m-%d")]
# dhs_data[, hib1_3 := as.Date(paste(h64y_3, h64m_3, h64d_3, sep="-"), "%Y-%m-%d")]
# dhs_data[, hib1_4 := as.Date(paste(h64y_4, h64m_4, h64d_4, sep="-"), "%Y-%m-%d")]
# dhs_data[, hib1_5 := as.Date(paste(h64y_5, h64m_5, h64d_5, sep="-"), "%Y-%m-%d")]
# dhs_data[, hib1_6 := as.Date(paste(h64y_6, h64m_6, h64d_6, sep="-"), "%Y-%m-%d")]
# 
# # Hib 2 (h65) for each child
# dhs_data[, hib2_1 := as.Date(paste(h65y_1, h65m_1, h65d_1, sep="-"), "%Y-%m-%d")]
# dhs_data[, hib2_2 := as.Date(paste(h65y_2, h65m_2, h65d_2, sep="-"), "%Y-%m-%d")]
# dhs_data[, hib2_3 := as.Date(paste(h65y_3, h65m_3, h65d_3, sep="-"), "%Y-%m-%d")]
# dhs_data[, hib2_4 := as.Date(paste(h65y_4, h65m_4, h65d_4, sep="-"), "%Y-%m-%d")]
# dhs_data[, hib2_5 := as.Date(paste(h65y_5, h65m_5, h65d_5, sep="-"), "%Y-%m-%d")]
# dhs_data[, hib2_6 := as.Date(paste(h65y_6, h65m_6, h65d_6, sep="-"), "%Y-%m-%d")]
# 
# # Hib 3 (h66) for each child
# dhs_data[, hib3_1 := as.Date(paste(h66y_1, h66m_1, h66d_1, sep="-"), "%Y-%m-%d")]
# dhs_data[, hib3_2 := as.Date(paste(h66y_2, h66m_2, h66d_2, sep="-"), "%Y-%m-%d")]
# dhs_data[, hib3_3 := as.Date(paste(h66y_3, h66m_3, h66d_3, sep="-"), "%Y-%m-%d")]
# dhs_data[, hib3_4 := as.Date(paste(h66y_4, h66m_4, h66d_4, sep="-"), "%Y-%m-%d")]
# dhs_data[, hib3_5 := as.Date(paste(h66y_5, h66m_5, h66d_5, sep="-"), "%Y-%m-%d")]
# dhs_data[, hib3_6 := as.Date(paste(h66y_6, h66m_6, h66d_6, sep="-"), "%Y-%m-%d")]

######################
# child's date of birth
######################
# birthData <- dhs_data[, c(idVars, birthVars), with=F]
# 
# birthData2 <- birthData %>%
#   pivot_longer(
#     !c(caseid, v000, v005, intv_date),
#     names_to = c("remove", "child"),
#     names_sep = 10,
#     values_to = "dob",
#     values_drop_na = TRUE
#   )
# 
# # remove extra column
# birthData2 <- select(birthData2, -(remove))

# subset to only children that are the last six born
#### PRoblem: figure out how to filter out the children beyond the  six most recent 
# childlivingData2 <- filter(childlivingData2, child == "01|02|03|04|05|06")

# subset to only children that are the last six born
# childsexData2 <- filter(childsexData2, child == "01|02|03|04|05|06")

