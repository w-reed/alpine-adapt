

veg = read.csv("data/veg01.csv")
veg$year = as.character(veg$year)

veg = veg[veg$year %in% c(2018, 2019, 2020, 2021), ]

veg_imp = veg[, c("species", "abundance", "sensor_node", "year")]
veg_agg = aggregate(veg_imp$abundance, by = list(veg_imp$year, veg_imp$sensor_node, veg_imp$species), FUN = sum)

colnames(veg_agg) = c("year", "node", "species", "abundance")
veg_agg$name = paste(veg_agg$node, veg_agg$year, sep = "-")

#This section excludes the abundance data that lines up with the nodes and years
#that are excluded from the environmental predictors. I also exclude two species that
#only ever occur in node 17 during 2018, which is excluded from the environmental
#predictors in the glm script.
veg_2 <- veg_agg %>%
  filter(!node==1)%>%
  filter(!node==15)%>%
  filter(!node==21)%>%
  filter(!node==18)%>%
  filter(!species=='SALIX')%>%
  filter(!species=='SWEPER')


veg_agg = veg_2[, c(5, 4, 3)]
str(veg_agg)
veg_agg$name = as.factor(veg_agg$name)

library(ncar)
matrix_sep = picante::sample2matrix(veg_agg)

#this Round function from the ncar package rounds anything with a 0.5 up to the nearest integer. During 
#sampling, rare species (i.e., species that were seen but did not have a hit on the abundance survey) were
#coded as 0.5. This doesn't work for a Poisson model as it cannot take non-integers as a response (the model
#can fit but the logLik and AIC are Inf and the variance components are all exactly 1). So here we basically
#recode them as if rare species each had 1 hit - giving them an abundance equivalent to a species that
#was observed once during the survey.

matrix_round <- ncar::Round(matrix_sep)

veg_pa = veg_agg
veg_pa$abundance = 1

veg_pa$name

colnames(matrix2)
matrix2 = picante::sample2matrix(veg_pa)
rm(veg, veg_agg, veg_imp, veg_pa)