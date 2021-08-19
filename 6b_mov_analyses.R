####################################################
# 1: Load prepped dataset for analyses
####################################################


####################################################
# 2: descriptive characteristics of children with and without card
####################################################
table1(~  female + kid_agecat + edu + literate + wom_agecat + total_children_born + marital + wom_occ + assets + hhsize + urban + female_head | has_health_card_bin, data=dt)

# export table 1 as an image

####################################################
# 3: regression to investigate who has card 
####################################################

# fit a regression to see which variables are associated with having vaccine coverage card
mylogit <- glm(has_health_card_bin ~ female + kid_agecat + edu + literate + wom_agecat + total_children_born + marital + wom_occ + assets + hhsize + urban + female_head, data = dt, family = "binomial")


exp(cbind(OR = coef(mylogit), confint(mylogit)))

# export table 2 as an image

####################################################
# 4. coverage cascade
####################################################
# save data as datatable
dt <- as.data.table(dt)

# calculate the number of children in sample
dt[, .(.N), by = .(v000)]

# calculate how many children has a vaccination card
dt[has_health_card_bin == "Yes",.(total_with_card= .N), by = v000]

# calculate how many children were covered by each vaccine according to recall and card
dt[bcg_date_recorded%in%c(1,2,3),.(received_bcg= .N), by = v000]

# calculate how many children received bcg according to health card only
dt[has_health_card_bin == "Yes" & bcg_date_recorded%in%c(1,3),.(received_bcg= .N), by = v000]

# calculate the 

####################################################
# 5. compare missed opportunities- chisquare
####################################################
dt$mea1_missed_opportunity <-factor(dt$mea1_missed_opportunity, 
                                       levels=c(0,1),
                                       labels=c("No", "Yes"))

table1(~  female + kid_agecat + edu + literate + wom_agecat + total_children_born + marital + wom_occ + assets + hhsize + urban + female_head | mea1_missed_opportunity, data=dt, extra.col=list(`P-value`=pvalue))

pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

# create a series of tables   
####################################################
# 6. Regression to compare missed opportunities
####################################################

####################################################
# 7. hazards
####################################################