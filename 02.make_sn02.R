## __.make_sn02.R ####
## Will Reed
## November 01, 2021
## Aggregate sn.01 by daily values (min, max, and mean)
## This script expands the normal columns of the sensor network data
## to include the daily minimum, daily maximum and daily mean for this plot

sn.01 = read.csv("data/sn01.csv")

sn.01$TIMESTAMP = sn.01$date
sn.01$date = format(as.POSIXct(sn.01$date,format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d')

sn_means = aggregate(sn.01[, 5:13], by = list(sn.01$sensornode, sn.01$date), FUN = mean, na.rm = T)
sn_minimums = aggregate(sn.01[, 5:13], by = list(sn.01$sensornode, sn.01$date), FUN = min)
sn_maximums = aggregate(sn.01[, 5:13], by = list(sn.01$sensornode, sn.01$date), FUN = max)

sn_sds = aggregate(sn.01[, 5:13], by = list(sn.01$sensornode, sn.01$date), FUN = sd, na.rm = T)


min_names = c("node", "date", "airtemp_min", "soiltemp_5cm_min", "soiltemp_30cm_min", 
              "soilmoisture_a_5cm_min", "soilmoisture_a_30cm_min", "soilmoisture_b_5cm_min", 
              "soilmoisture_b_30cm_min", "soilmoisture_c_5cm_min", "soilmoisture_c_30cm_min")

max_names = c("node", "date", "airtemp_max","soiltemp_5cm_max", "soiltemp_30cm_max", 
              "soilmoisture_a_5cm_max", "soilmoisture_a_30cm_max", "soilmoisture_b_5cm_max", 
              "soilmoisture_b_30cm_max", "soilmoisture_c_5cm_max", "soilmoisture_c_30cm_max")

mean_names = c("node", "date","airtemp_avg", "soiltemp_5cm_avg", "soiltemp_30cm_avg", 
               "soilmoisture_a_5cm_avg", "soilmoisture_a_30cm_avg", "soilmoisture_b_5cm_avg", 
               "soilmoisture_b_30cm_avg", "soilmoisture_c_5cm_avg", "soilmoisture_c_30cm_avg")


sd_names = c("node", "date","airtemp_sd", "soiltemp_5cm_sd", "soiltemp_30cm_sd", 
               "soilmoisture_a_5cm_sd", "soilmoisture_a_30cm_sd", "soilmoisture_b_5cm_sd", 
               "soilmoisture_b_30cm_sd", "soilmoisture_c_5cm_sd", "soilmoisture_c_30cm_sd")

colnames(sn_minimums) = min_names
colnames(sn_maximums) = max_names
colnames(sn_means) = mean_names
colnames(sn_sds) = sd_names


for(i in 1:nrow(sn_minimums)){
  sn_minimums$month[i] = as.numeric(unlist(strsplit(sn_minimums$date[i], split = "-"))[2])
  sn_minimums$year[i] = as.numeric(unlist(strsplit(sn_minimums$date[i], split = "-"))[1])
  
  sn_maximums$month[i] = as.numeric(unlist(strsplit(sn_maximums$date[i], split = "-"))[2])
  sn_maximums$year[i] = as.numeric(unlist(strsplit(sn_maximums$date[i], split = "-"))[1])
  
  sn_means$month[i] = as.numeric(unlist(strsplit(sn_means$date[i], split = "-"))[2])
  sn_means$year[i] = as.numeric(unlist(strsplit(sn_means$date[i], split = "-"))[1])
  
  sn_sds$month[i] = as.numeric(unlist(strsplit(sn_sds$date[i], split = "-"))[2])
  sn_sds$year[i] = as.numeric(unlist(strsplit(sn_sds$date[i], split = "-"))[1])
  
}

sn_means$plot.year = paste(sn_means$node, sn_means$year, sep = "-")
sn_minimums$plot.year = paste(sn_minimums$node, sn_minimums$year, sep = "-")
sn_maximums$plot.year = paste(sn_maximums$node, sn_maximums$year, sep = "-")
sn_sds$plot.year = paste(sn_sds$node, sn_sds$year, sep = "-")



sn = merge(sn_means, sn_minimums)
sn = merge(sn, sn_maximums)
sn = merge(sn, sn_sds)

sn = sn[sn$year != 2017, ]
sn = sn[sn$node != 1, ]
sn = sn[sn$node != 21, ]

write.csv(sn, "data/sn02.csv", row.names = F)
rm(list = ls())
