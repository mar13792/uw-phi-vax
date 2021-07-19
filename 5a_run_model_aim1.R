# Author: Francisco Rios 
# Purpose: calculate observed vs expected ratios for all countries and locations
# Date: Last modified July 13, 2021

# load data
data = readRDS(outputFile3)

# data transformations

# run model
test.model <- glm(val ~ sdi + year_id + location_id + age_group_id + sex_id , data=data, family = binomial())

summary(test.model)
