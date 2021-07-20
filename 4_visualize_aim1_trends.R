# Author: Francisco Rios 
# Purpose: Prep tables and visualizations for aim 1
# Date: Last modified July 20, 2021

dt <- readRDS(outputFile3)

n=6
lctns <- sample(dt[sdi_group=="low"]$location_name, n)
sample = dt[location_name %in% lctns]

# sample plots for vaccine trends

# organize groups of variables
varGroups = unique(dt$covariate_name_short)
labelTable <- unique(dt[,.(covariate_name_short, disease)])

tsPlots = lapply(seq(length(varGroups)), function(g) {
  l = labelTable[covariate_name_short==varGroups[[g]]]$disease
  ggplot(sample[covariate_name_short%in%varGroups[[g]]], aes(y=val, x=year_id, color=covariate_name_short)) + 
    geom_line(size = 1, alpha = .8) + 
    facet_wrap(~location_name) + 
    labs(title=paste('Time series of vaccine coverage for', l), y='Percent', x='Year', 
         subtitle=paste('Random Sample of', n, 'locations'), caption='') + 
    theme_minimal()
}) 

# Save file
print(paste('Saving:', outputFile4)) 
pdf(outputFile4, height=5.5, width=9)
for(i in seq(length(tsPlots))) { 
  print(tsPlots[[i]])
}

dev.off()
