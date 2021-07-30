# Author: Francisco Rios 
# Purpose: calculate observed percent change for all countries and locations
# Date: Last modified July 13, 2021

# load data
data = readRDS(outputFile3)

# subset data to include only values of Vaccine coverage between 2014-2019
dt2 <- data[year_id %in% c('2014', '2019')]

# subset to only low-SDI countries
dt2 <- dt2[sdi_group=="low"]

# # temporary fix remove the unnecessary columns
dt2 <- dt2[,c("location_name", "year_id", "vaccine_name", "val")]

# reshape data from long to wide
dt3 <- dcast(dt2, location_name + vaccine_name ~ year_id, value.var = c("val"))

# calculate percent change for each country
dt3[, percent_change := ((`2019`-`2014`)/`2014`)*100]

# remove locations without a real number
dt3 <- dt3[!is.na(percent_change)]
dt3 <- dt3[!is.infinite(percent_change)]

# create a function to spot outliers in data
is_outlier <- function(x) {
  return(x > quantile(x, 0.75) + 1.5 * IQR(x))
}

vaccines = unique(dt3$vaccine_name)


dt3[,outlier:=ifelse(is_outlier(percent_change), location_name, as.numeric(NA)), by=.(vaccine_name)]

# Save file
print(paste('Saving:', outputFile5a)) 
pdf(outputFile5a, height=5.5, width=9)

for (i in 1:length(vaccines)) {
  g <- ggplot(dt3[vaccine_name==vaccines[i]], aes(x = vaccine_name, y = percent_change)) +
  geom_boxplot() +
  # geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3)+
    theme_minimal()+
    geom_text_repel(aes(label = outlier))
print(g)
}

dev.off()

# dat <- dt3 %>% tibble::rownames_to_column(var="outlier") %>% group_by(vaccine_name) %>% mutate(is_outlier=ifelse(is_outlier(percent_change), percent_change, as.numeric(NA)))
# dat$outlier[which(is.na(dat$is_outlier))] <- as.numeric(NA)
# 
# ggplot(dat, aes(y=percent_change, x=factor(vaccine_name))) + geom_boxplot() + geom_text(aes(label=outlier),na.rm=TRUE,nudge_y=0.05)
# 
# # plot distribution of percent change
# ggplot(dt3, aes(x=percent_change)) + geom_histogram()+
#   facet_wrap(~vaccine_name)

# ggplot(dt3, aes(x=percent_change)) + geom_boxplot()+
#   facet_wrap(~vaccine_name)


# # plot two values against each other
# plot(dt3$val_2014, dt3$val_2019)
# ggplot(dt3, aes(x=val_2014, y=val_2019, color=vaccine_name)) +
#   geom_point()+
#   facet_wrap(~vaccine_name) +
# # label points above a certain point
#   geom_text(aes(label=ifelse(val_2019>.75 & val_2014<0.75,as.character(location_name),'')),hjust=0,vjust=0)
# 
# # make table among low SDI countries
# # ggplot(dt3, aes(location_name, vaccine_name, color = vaccine_name, size= percent_change)) + geom_point() + theme_classic() 

# calculate average change according to vaccine name
dt4 <- dt3

# create a table of the countries that are outliers



ggplot(dt3, aes(vaccine_name, location_name, fill= percent_change)) + geom_tile() + theme_bw()
