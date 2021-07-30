# Author: Francisco Rios 
# Purpose: calculate observed percent change for all countries and locations
# Date: Last modified July 13, 2021

# load data
data = readRDS(outputFile3)

# subset data to include only values of Vaccine coverage between 2014-2019
dt2 <- data[year_id %in% c('2014', '2019')]

# subset to only include national-level data
dt2 <- 

# subset to only low-SDI countries
dt2 <- dt2[sdi_group=="low"]

# temporary fix remove the SDI value
dt2 <- dt2[,c("sdi", "sdi_group"):=NULL]

# reshape data from long to wide
dt3 <- dcast(dt2, location_id + location_name + measure_id + measure_name + vaccine_name + sex_id + sex_name + file_name + data_type + data_source + location_set_version_id
      + parent_id + level + sort_order ~ year_id, value.var = c("val", "upper", "lower"))

# calculate percent change for each country
dt3[, percent_change := ((val_2019-val_2014)/val_2014)*100]

# plot two values against each other
plot(dt3$val_2014, dt3$val_2019)
ggplot(dt3, aes(x=val_2014, y=val_2019, color=vaccine_name)) +
  geom_point()+
  facet_wrap(~vaccine_name) +
# label points above a certain point
  geom_text(aes(label=ifelse(val_2019>.75 & val_2014<0.75,as.character(location_name),'')),hjust=0,vjust=0)

# make table among low SDI countries
# ggplot(dt3, aes(location_name, vaccine_name, color = vaccine_name, size= percent_change)) + geom_point() + theme_classic() 

ggplot(dt3, aes(vaccine_name, location_name, fill= percent_change)) + geom_tile() + theme_bw()
