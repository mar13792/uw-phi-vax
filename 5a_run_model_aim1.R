# Author: Francisco Rios 
# Purpose: calculate observed vs expected ratios for all countries and locations
# Date: Last modified July 13, 2021

# load data
data = readRDS(outputFile3)

# plot data
ggplot(data = data[location_name=="Zambia" & vaccine_name=="BCG"], aes(x=year_id, y=val))+
  geom_point()

# data transformations



# # run model
# test.model <- glm(val ~ sdi + year_id + location_id, data=data, family = binomial())
# 
# summary(test.model)
# 
# # predict values
# newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")
# newdata1
