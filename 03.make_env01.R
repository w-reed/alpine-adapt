
library(imputeTS)
sn02 = read.csv("data/sn02.csv")


threshold.crossing = function(vector, theta){
  vector = vector[!is.na(vector)]
  
  vector1 = vector
  vector2 = c(0, vector)
  combined = suppressWarnings( as.data.frame(cbind((vector1 <= theta), (vector2 <= theta))))
  combined$cross = ifelse(combined$V1 != combined$V2, 1, 0) 
  return(sum(combined$cross))
} 

sn02$soilmoisture_a_30cm_dr = sn02$soilmoisture_a_30cm_max - sn02$soilmoisture_a_30cm_min
sn02$soilmoisture_a_5cm_dr = sn02$soilmoisture_a_5cm_max - sn02$soilmoisture_a_5cm_min
sn02$soiltemp_30cm_dr = sn02$soiltemp_30cm_max - sn02$soiltemp_30cm_min
sn02$soiltemp_5cm_dr = sn02$soiltemp_5cm_max - sn02$soiltemp_5cm_min

sn02$soilmoisture_bc_5cm_avg = rowMeans(sn02[, c("soilmoisture_b_5cm_avg", "soilmoisture_c_5cm_avg")], na.rm=T)

sn.mc = sn02[, c("node", "year", "month", "date", "soilmoisture_a_30cm_avg", "soilmoisture_a_5cm_avg", "soilmoisture_bc_5cm_avg")]
sn.tc = sn02[, c("node", "year", "month", "date", "soiltemp_30cm_avg", "soiltemp_5cm_avg")]


sn.mc = sn.mc[sn.mc$month %in% c(5,6,7,8,9,10), ]


years = unique(sn.mc$year)
plots = unique(sn.mc$node)
plots = c(10, 11, 12, 13, 14, 16, 17, 19, 20, 6, 7, 8, 9)

df = as.data.frame(matrix(nrow = 0, ncol = 5))
colnames(df) = c("year", "node", "30cm_smtc", "5cm_smtc", "5cm_bc_smtc")

for(i in years){
  for(j in plots){
    cc = sn.mc[sn.mc$year == i & sn.mc$node == j, ]
    
    if(i != 2021 | j != 12){
      
    
    df[nrow(df) + 1, "year"] = i
    df[nrow(df), "node"] = j
    df[nrow(df), "30cm_smtc"] = threshold.crossing(cc$soilmoisture_a_30cm_avg, theta = 0.13)
    df[nrow(df), "5cm_smtc"] = threshold.crossing(cc$soilmoisture_a_5cm_avg, theta = 0.13)
    df[nrow(df), "5cm_bc_smtc"] = threshold.crossing(cc$soilmoisture_bc_5cm_avg, theta = 0.13)
    }
  }
  
  
}

#dff = merge(df, df3)
#dff$plot.year = paste(dff$node, dff$year, sep  = "-")
#dff = dff[, c("plot.year", "node", "year", "5cm_smtc", "5cm_bc_smtc", "5cm_smcv", "5cm_bc_smcv")]
#dfff = merge(dff, holder5)

#write.csv(dfff, "data/env05.csv", row.names = F)

df2 = as.data.frame(matrix(nrow = 0, ncol = 4))
colnames(df2) = c("year", "node", "30cm_sttc", "5cm_sttc")

for(i in years){
  for(j in plots){
    cc = sn.tc[sn.tc$year == i & sn.tc$node == j, ]
    
    if(i != 2021 | j != 12){
      
      
      df2[nrow(df2) + 1, "year"] = i
      df2[nrow(df2), "node"] = j
      df2[nrow(df2), "30cm_sttc"] = threshold.crossing(cc$soiltemp_30cm_avg, theta = 4.4)
      df2[nrow(df2), "5cm_sttc"] = threshold.crossing(cc$soiltemp_5cm_avg, theta = 4.4)
    }
  }
  
  
}



df3 = as.data.frame(matrix(nrow = 0, ncol = 5))
colnames(df3) = c("year", "node", "30cm_smcv", "5cm_smcv", "5cm_bc_smcv")

for(i in years){
  for(j in plots){
    cc = sn.mc[sn.mc$year == i & sn.mc$node == j, ]
    
    if(i != 2021 | j != 12){
      
      df3[nrow(df3) + 1, "year"] = i
      df3[nrow(df3), "node"] = j
      df3[nrow(df3), "30cm_smcv"] = sd(cc$soilmoisture_a_30cm_avg, na.rm = T)/mean(cc$soilmoisture_a_30cm_avg, na.rm = T)
      df3[nrow(df3), "5cm_smcv"] = sd(cc$soilmoisture_a_5cm_avg, na.rm = T)/mean(cc$soilmoisture_a_5cm_avg, na.rm = T)
      df3[nrow(df3), "5cm_bc_smcv"] = sd(cc$soilmoisture_bc_5cm_avg, na.rm = T)/mean(cc$soilmoisture_bc_5cm_avg, na.rm = T)
    }
  }
  
  
}


df4 = as.data.frame(matrix(nrow = 0, ncol = 4))
colnames(df4) = c("year", "node", "30cm_stcv", "5cm_stcv")

for(i in years){
  for(j in plots){
    cc = sn.tc[sn.tc$year == i & sn.tc$node == j, ]
    
    if(i != 2021 | j != 12){
      
      df4[nrow(df4) + 1, "year"] = i
      df4[nrow(df4), "node"] = j
      df4[nrow(df4), "30cm_stcv"] = sd(cc$soiltemp_30cm_avg, na.rm = T)/mean(cc$soiltemp_30cm_avg, na.rm = T)
      df4[nrow(df4), "5cm_stcv"] = sd(cc$soiltemp_5cm_avg, na.rm = T)/mean(cc$soiltemp_5cm_avg, na.rm = T)
    }
  }
  
  
}



sn03 = sn02[c("node", "year", "plot.year", "airtemp_avg", 
              "soiltemp_5cm_avg", "soiltemp_30cm_avg", "soilmoisture_a_5cm_avg", 
              "soilmoisture_a_30cm_avg", "airtemp_sd", 
              "soiltemp_5cm_sd", "soiltemp_30cm_sd", "soilmoisture_a_5cm_sd", 
              "soilmoisture_a_30cm_sd",  "soilmoisture_a_30cm_dr", 
              "soiltemp_30cm_dr", "soilmoisture_a_5cm_dr", "soiltemp_5cm_dr")]

sn.avgs.year = aggregate(sn03[, c(4:17)], by = list(sn03$node, sn03$year), FUN = mean, na.rm = T)
sn.avgs.all = aggregate(sn03[, c(4:17)], by = list(sn03$node), FUN = mean, na.rm = T)
colnames(sn.avgs.year) = c("plot", "year", "airtemp_avg", "soiltemp_5cm_avg", "soiltemp_30cm_avg", 
                      "soilmoisture_a_5cm_avg", "soilmoisture_a_30cm_avg", "airtemp_sd", 
                      "soiltemp_5cm_sd", "soiltemp_30cm_sd", "soilmoisture_a_5cm_sd", 
                      "soilmoisture_a_30cm_sd", "soilmoisture_a_30cm_dr", "soiltemp_30cm_dr", 
                      "soilmoisture_a_5cm_dr", 'soiltemp_5cm_dr')


plot.years = unique(sn03$plot.year)



holder1 = as.data.frame(matrix(nrow = 0, ncol = 2))
colnames(holder1) = c("plot.year", "st_acf_5cm_14")
for(i in plot.years){
  tmp = sn03[sn03$plot.year %in% i, ]
  acf = acf(na_interpolation(tmp$soiltemp_5cm_avg))[14]
  holder1[nrow(holder1) + 1, "plot.year"] = i
  holder1[nrow(holder1), "st_acf_5cm_14"] = as.numeric(acf[[1]])
  
}


holder2 = as.data.frame(matrix(nrow = 0, ncol = 2))
colnames(holder2) = c("plot.year", "st_acf_30cm_14")
for(i in plot.years){
  if(i != "15-2020"){
    tmp = sn03[sn03$plot.year %in% i, ]
    acf = acf(na_interpolation(tmp$soiltemp_30cm_avg))[14]
    holder2[nrow(holder2) + 1, "plot.year"] = i
    holder2[nrow(holder2), "st_acf_30cm_14"] = as.numeric(acf[[1]])
  }
}

holder3 = as.data.frame(matrix(nrow = 0, ncol = 2))
colnames(holder3) = c("plot.year", "sm_acf_5cm_14")
for(i in plot.years){
  if(i != "15-2020" & i != "15-2021"){
    tmp = sn03[sn03$plot.year %in% i, ]
    acf = acf(na_interpolation(tmp$soilmoisture_a_5cm_avg))[14]
    holder3[nrow(holder3) + 1, "plot.year"] = i
    holder3[nrow(holder3), "sm_acf_5cm_14"] = as.numeric(acf[[1]])
  }
}

holder4 = as.data.frame(matrix(nrow = 0, ncol = 2))
colnames(holder4) = c("plot.year", "sm_acf_30cm_14")
for(i in plot.years){
  if(i != "15-2020" & i != "15-2018"){
    tmp = sn03[sn03$plot.year %in% i, ]
    acf = acf(na_interpolation(tmp$soilmoisture_a_30cm_avg))[14]
    holder4[nrow(holder4) + 1, "plot.year"] = i
    holder4[nrow(holder4), "sm_acf_30cm_14"] = as.numeric(acf[[1]])
   }
}

sn01 = read.csv("data/sn01.csv")
sn01$time <- format(as.POSIXct(sn01$date,format="%Y-%m-%d %H:%M:%S"),"%H:%M:%S")
sn01$year <- format(as.POSIXct(sn01$date,format="%Y-%m-%d %H:%M:%S"),"%Y")
sn01$hour <- format(as.POSIXct(sn01$date,format="%Y-%m-%d %H:%M:%S"),"%H")
sn01$day <- format(as.POSIXct(sn01$date,format="%Y-%m-%d %H:%M:%S"),"%m-%d")
sn01$plot.year = paste(sn01$sensornode, sn01$year, sep = "-")
sn01.1 = aggregate(sn01[5:13], by = list(sn01$plot.year, sn01$day, sn01$hour), FUN = mean)

colnames(sn01.1) = c("plot.year", "day", "hour", "airtemp_avg", "soiltemp_5cm_avg", 
                     "soiltemp_30cm_avg", "soilmoisture_a_5cm_avg", "soilmoisture_a_30cm_avg", 
                     "soilmoisture_b_5cm_avg", "soilmoisture_b_30cm_avg", "soilmoisture_c_5cm_avg", 
                     "soilmoisture_c_30cm_avg")

sn01.1$soilmoisture_bc_5cm_avg = rowMeans(sn01.1[, c("soilmoisture_b_5cm_avg", "soilmoisture_c_5cm_avg")], na.rm=T)


holder5 = as.data.frame(matrix(nrow = 0, ncol = 3))
colnames(holder5) = c("plot.year", "sm_acf_5cm_24_a", "sm_acf_5cm_24_bc")
for(i in plot.years){
  if(i != "15-2020" & i != "15-2018" & i != "15-2021" & i != "12-2021"){
    tmp = sn01.1[sn01.1$plot.year %in% i,]
    tmp = tmp[tmp$soiltemp_5cm_avg > 0, ]
    acf.a = acf(na_interpolation(tmp$soilmoisture_a_5cm_avg), lag.max = 100)[24]
    acf.bc = acf(na_interpolation(tmp$soilmoisture_bc_5cm_avg), lag.max = 100)[24]
    print(acf)
    holder5[nrow(holder5) + 1, "plot.year"] = i
    holder5[nrow(holder5), "sm_acf_5cm_24_a"] = as.numeric(acf.a[[1]])
    holder5[nrow(holder5), "sm_acf_5cm_24_bc"] = as.numeric(acf.bc[[1]])
  }
}


holder6 = as.data.frame(matrix(nrow = 0, ncol = 2))
colnames(holder6) = c("plot.year", "st_acf_5cm_24")
for(i in plot.years){
  if(i != "15-2020" & i != "15-2018" & i != "15-2021" & i != "12-2021"){
    tmp = sn01.1[sn01.1$plot.year %in% i,]
    acf = acf(na_interpolation(tmp$soiltemp_5cm_avg), lag.max = 100)[24]
    print(acf)
    holder6[nrow(holder6) + 1, "plot.year"] = i
    holder6[nrow(holder6), "st_acf_5cm_24"] = as.numeric(acf[[1]])
  }
}


acf_holder = merge(holder1, holder2, all = T)
acf_holder = merge(acf_holder, holder3, all = T)
acf_holder = merge(acf_holder, holder4, all = T)

acf_holder = tidyr::separate(data = acf_holder, col = "plot.year", c("plot", "year"), sep = "-")

acf_holder2 = merge(holder5, holder6, all = T)
acf_holder2 = tidyr::separate(data = acf_holder2, col = "plot.year", c("plot", "year"), sep = "-")


acf_total = aggregate(acf_holder[,c(3:6)], by = list(acf_holder$plot), FUN = mean, na.rm = T)


env01 = merge(sn.avgs.year, acf_holder2, by = c("plot", "year"))
env01 = env01[, c("plot", "year", "soilmoisture_a_5cm_avg", "soilmoisture_a_5cm_sd", "sm_acf_5cm_24_a", 
                  "soiltemp_5cm_avg", "soiltemp_5cm_sd", "st_acf_5cm_24")]

colnames(env01) = c("plot", "year", "MEAN_SM_5", "VAR_SM_5_tc", "PRED_SM_5", "MEAN_ST_5", "VAR_ST_5_tc", "PRED_ST_5")


env02 = merge(sn.avgs.all, acf_total, by = "Group.1", all = T)
colnames(env02) = c("plot", "airtemp_avg", "soiltemp_5cm_avg", "soiltemp_30cm_avg", 
                    "soilmoisture_a_5cm_avg", "soilmoisture_a_30cm_avg", "airtemp_sd", 
                    "soiltemp_5cm_sd", "soiltemp_30cm_sd", "soilmoisture_a_5cm_sd", 
                    "soilmoisture_a_30cm_sd", "soilmoisture_a_30cm_dr", "soiltemp_30cm_dr", 
                    "soilmoisture_a_5cm_dr", "soiltemp_5cm_dr", "st_acf_5cm_14", "st_acf_30cm_14", "sm_acf_5cm_14", 
                    "sm_acf_30cm_14")

write.csv(env01, "data/env01.csv", row.names = F)
write.csv(env02, "data/env02.csv", row.names = F)


print(colnames(sn.avgs.year))
sn.avgs = sn.avgs.year[, c("plot", "year", "soilmoisture_a_30cm_avg","soilmoisture_a_5cm_avg", "soiltemp_30cm_avg", "soiltemp_5cm_avg")]

xx = merge(sn.avgs, df, by.x = c("plot", "year"), by.y = c("node", "year"))
xx = merge(xx, df2, by.x = c("plot", "year"), by.y = c("node", "year"))
xx = merge(xx, df3, by.x = c("plot", "year"), by.y = c("node", "year"))
xx = merge(xx, df4, by.x = c("plot", "year"), by.y = c("node", "year"))
xx = merge(xx, acf_holder)

names(xx) = c("node", "year", "MEAN_SM_30", "MEAN_SM_5", "MEAN_ST_30", "MEAN_ST_5", "VAR_SM_30_tc", "VAR_SM_5_tc", "VAR_ST_30_tc", "VAR_ST_5_tc", "VAR_SM_30_cv", "VAR_SM_5_cv", "VAR_ST_30_cv", "VAR_ST_5_cv", "PRED_ST_5", "PRED_ST_30", "PRED_SM_5", "PRED_SM_30")
xx2 = aggregate(xx[,3:18], by = list(xx$node), FUN = mean, na.rm = T)

write.csv(xx, "data/env03.csv", row.names = FALSE)
write.csv(xx2, "data/env04.csv", row.names = FALSE)

rm(list = ls())
