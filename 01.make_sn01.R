## 01.make_sn01.R ####
## Will Reed
## 
## This script loads each data file for the nodes in the NWT LTER sensor network
## and combines them into one data frame. It also removes any flagged or unusual 
## data

## OUTPUT: sn.01.csv


## Get list of files in the data subfolder
files1 = list.files("data/knb-lter-nwt.210.4.2/")
## Read in one file to get the columns names / Uncomment if need to modify
# xx = read.csv("data/knb-lter-nwt.210.3/sn_01_tenminute.jm.data.csv")
# dput(colnames(xx))

## read all csvs (from first locale)
for (i in 1:length(files1)){
  ## Loop through files to read in
  file1 = read.csv(paste0("data/knb-lter-nwt.210.4.2/", files1[i]))
  ## Adjust column names because they are messed up.
  file1 = file1[, c("LTER_site", "local_site", "sensornode", "date", "airtemp_max", 
                    "flag_airtemp_max", "airtemp_min", "flag_airtemp_min", "airtemp_avg", 
                    "flag_airtemp_avg", "rh_max", "flag_rh_max", "rh_min", "flag_rh_min", 
                    "rh_avg", "flag_rh_avg", "soiltemp_5cm_avg", "flag_soiltemp_5cm_avg", 
                    "soiltemp_30cm_avg", "flag_soiltemp_30cm_avg", "soilmoisture_a_5cm_avg", 
                    "flag_soilmoisture_a_5cm_avg", "soilmoisture_a_30cm_avg", "flag_soilmoisture_a_30cm_avg", 
                    "soilmoisture_b_5cm_avg", "flag_soilmoisture_b_5cm_avg", "soilmoisture_b_30cm_avg", 
                    "flag_soilmoisture_b_30cm_avg", "soilmoisture_c_5cm_avg", "flag_soilmoisture_c_5cm_avg", 
                    "soilmoisture_c_30cm_avg", "flag_soilmoisture_c_30cm_avg", "batt_volt_min", 
                    "flag_batt_volt_min", "dts_sample", "flag_dts_sample")]
  
  ## Make a new data frame for them to be combined into on i==1 
  if (i == 1){
    holder1 = as.data.frame(matrix(nrow = 0, ncol = dim(file1)[2]))
    colnames(holder1) = colnames(file1)
    
  }
  print('PASS')
  holder1 = rbind(holder1, file1)
}

## Convert date
holder1$date = as.POSIXct(holder1$date, format ="%Y-%m-%d %H:%M:%S")

## VISUAL DATA INSPECTION ####
## Look at soil moisture (5cm)


## There are clearly some weird things with a few of the sensors.. 
## Run some general quality checks here first. 
sn.2 = holder1

## the QA/QC (flag) column for each record indicates that the sensor detection is between -0.03 and 0.60
## 5cm
sn.2$soilmoisture_a_5cm_avg = ifelse(sn.2$soilmoisture_a_5cm_avg > -0.03 & sn.2$soilmoisture_a_5cm_avg < 0.6, sn.2$soilmoisture_a_5cm_avg, NA)              
sn.2$soilmoisture_b_5cm_avg = ifelse(sn.2$soilmoisture_b_5cm_avg > -0.03 & sn.2$soilmoisture_b_5cm_avg < 0.6, sn.2$soilmoisture_b_5cm_avg, NA) 
sn.2$soilmoisture_c_5cm_avg = ifelse(sn.2$soilmoisture_c_5cm_avg > -0.03 & sn.2$soilmoisture_c_5cm_avg < 0.6, sn.2$soilmoisture_c_5cm_avg, NA)

## 30 cm 
sn.2$soilmoisture_a_30cm_avg = ifelse(sn.2$soilmoisture_a_30cm_avg > -0.03 & sn.2$soilmoisture_a_30cm_avg < 0.7, sn.2$soilmoisture_a_30cm_avg, NA)              
sn.2$soilmoisture_b_30cm_avg = ifelse(sn.2$soilmoisture_b_30cm_avg > -0.03 & sn.2$soilmoisture_b_30cm_avg < 0.7, sn.2$soilmoisture_b_30cm_avg, NA) 
sn.2$soilmoisture_c_30cm_avg = ifelse(sn.2$soilmoisture_c_30cm_avg > -0.03 & sn.2$soilmoisture_c_30cm_avg < 0.7, sn.2$soilmoisture_c_30cm_avg, NA)

#### 2. SOIL TEMP CLEANING  ####
sn.2$soiltemp_5cm_avg = ifelse(sn.2$soiltemp_5cm_avg > -35 & sn.2$soiltemp_5cm_avg < 50, sn.2$soiltemp_5cm_avg, NA)
sn.2$soiltemp_30cm_avg = ifelse(sn.2$soiltemp_30cm_avg > -35 & sn.2$soiltemp_30cm_avg < 50, sn.2$soiltemp_30cm_avg, NA)


#### 2.1 AIR Temp Cleaning
sn.2$airtemp_avg = ifelse(sn.2$airtemp_avg > -50 & sn.2$airtemp_avg < 50, sn.2$airtemp_avg, NA)


#### 3. REMOVE FLAG COLUMNS ####
# dput(names(sn.2))

include = c("LTER_site", "local_site", "sensornode", "date","airtemp_avg","soiltemp_5cm_avg", 
            "soiltemp_30cm_avg", "soilmoisture_a_5cm_avg", "soilmoisture_a_30cm_avg", 
            "soilmoisture_b_5cm_avg", "soilmoisture_b_30cm_avg", "soilmoisture_c_5cm_avg", 
            "soilmoisture_c_30cm_avg")


sn.2 = sn.2[, include]

write.csv(sn.2, "data/sn01.csv", row.names = FALSE)

rm(list = ls())
