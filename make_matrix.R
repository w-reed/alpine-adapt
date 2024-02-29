veg = read.csv("data/veg01.csv")
veg$year = as.character(veg$year)

veg = veg[veg$year %in% c(2018, 2019, 2020, 2021), ]

veg_imp = veg[, c("species", "abundance", "sensor_node", "year")]
veg_agg = aggregate(veg_imp$abundance, by = list(veg_imp$year, veg_imp$sensor_node, veg_imp$species), FUN = sum)

colnames(veg_agg) = c("year", "node", "species", "abundance")
veg_agg$name = paste(veg_agg$node, veg_agg$year, sep = "-")
veg_agg = veg_agg[, c(5, 4, 3)]
str(veg_agg)
veg_agg$name = as.factor(veg_agg$name)


matrix = picante::sample2matrix(veg_agg)

veg_pa = veg_agg
veg_pa$abundance = 1

matrix2 = picante::sample2matrix(veg_pa)
rm(veg, veg_agg, veg_imp, veg_pa)




