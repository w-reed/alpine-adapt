## Load packages
library(MDMR)
library(phytools)
library(vegan)
library(ggplot2)
# library(ggeffects) ## deprecated [--doesn't need -- my code, not the package :)]

## load in env01 [daily measures] and env03 [seasonal measures]
env.daily = read.csv("data/env01.csv")
env.seaso = read.csv("data/env03.csv")

veg = read.csv("data/veg01.csv")

## Give plot-year combo name
rownames(env.daily) = paste(env.daily$plot, env.daily$year, sep = "-")
rownames(env.seaso) = paste(env.seaso$node, env.seaso$year, sep = "-")

gps = c("10-2018", "10-2019", "10-2020", "10-2021", "11-2018", "11-2019", 
    "11-2020", "11-2021", "12-2018", "12-2019", "12-2020", "13-2018", 
    "13-2019", "13-2020", "13-2021", "14-2018", "14-2019", "14-2020", 
    "14-2021", "16-2018", "16-2019", "16-2020", "16-2021", "17-2019", 
    "17-2020", "17-2021", "19-2018", "19-2019", "19-2020", "19-2021", 
    "20-2018", "20-2019", "20-2020", "20-2021", "6-2018", "6-2019", 
    "6-2020", "6-2021", "7-2018", "7-2019", "7-2020", "7-2021", "8-2018", 
    "8-2019", "8-2020", "8-2021", "9-2018", "9-2019", "9-2020", "9-2021")


env.daily = env.daily[row.names(env.daily) %in% gps, ]
env.seaso = env.seaso[row.names(env.seaso) %in% gps, ]

## Now subset to the interesting columns
env.means = env.seaso[, c("MEAN_SM_5", "MEAN_ST_5")]
env.daily = env.daily[, c("MEAN_SM_5", "MEAN_ST_5", "VAR_ST_5_tc", "VAR_SM_5_tc", "PRED_SM_5", "PRED_ST_5")]
env.seaso = env.seaso[, c("MEAN_SM_5", "MEAN_ST_5", "VAR_ST_5_tc", "VAR_SM_5_tc", "PRED_SM_5", "PRED_ST_5")]


veg.sub = veg[, c("sensor_node", "abundance", "species", "year")]
veg.sub$name = paste(veg.sub$sensor_node, veg.sub$year, sep = "-")
veg.sub = veg.sub[, c(5, 2, 3)]
matrix = picante::sample2matrix(veg.sub)
#matrix = matrix[c(2:10, 12, 14:16),]

matrix = matrix[rownames(matrix) %in% gps, ]
matrix = decostand(matrix, method = "hellinger")


r_mod1 = rda(matrix ~ ., data = env.means)
r_mod2 = rda(matrix ~ ., data = env.daily)
r_mod3 = rda(matrix ~., data = env.seaso)

set.seed(123)

RsquareAdj(r_mod1)
summary(r_mod1) #58.741
anova.cca(r_mod1, step = 1000)
anova.cca(r_mod1, step = 1000, by = "term")
anova.cca(r_mod1, step = 1000, by = "axis")
coef(r_mod1)
RsquareAdj(r_mod2) # 0.58741
summary(r_mod2)
anova.cca(r_mod2, step = 1000)
anova.cca(r_mod2, step = 1000, by = "term")

RsquareAdj(r_mod3) # 0.58741
summary(r_mod3)
anova.cca(r_mod3, step = 1000)
anova.cca(r_mod3, step = 1000, by = "term")

species = c("ACOROS", "CARSCO", "DESCAE", "KOBMYO", "TRIPAR", "SEDLAN")
cols = c("red", "orange", "yellow", "blue", "green", "purple")

rda_pretty_plot = function(rda_obj, displayText = FALSE){
  perc <- round(100*(summary(rda_obj)$cont$importance[2, 1:2]), 2)
  sc_bp <- scores.rda(rda_obj, display="bp", choices=c(1, 2), scaling=2)
  sc_sites <- scores.rda(rda_obj, display="sites", choices=c(1, 2), scaling=2)
  sc_species = scores.rda(rda_obj, display = "species", choices = c(1,2), scaling = 3)
  sc_species = sc_species[row.names(sc_species) %in% c("ACOROS", "CARSCO", "DESCAE", "KOBMYO", "TRIPAR", "SEDLAN"),]
  cols = c("red", "orange", "yellow", "green", "blue", "purple")
  
  plot.new()
  plot.window(xlim = c(-1.2,1.2), ylim = c(-1.2,1.2))
  box()
  axis(1)
  axis(2)
  
  abline(h = 0, lwd = 0.3)
  abline(v = 0, lwd = 0.3)
  abline(h = c(-1, -0.5, 0.5, 1), lwd = 0.3, lty = 2)
  abline(v = c(-1, -0.5, 0.5, 1), lwd = 0.3, lty = 2)
  
  
  points(sc_sites[,1], sc_sites[,2], pch = 21, bg = "gray", col = "black", cex = 0.8)

  
  for(i in 1:nrow(sc_bp)){
    print(names(sc_bp[i, ]))
    if(i %in% c(1,3,5)){
      arrows(0,0,sc_bp[i,1], sc_bp[i,2], lwd = 1.8, length = 0.1, col = "blue")
    } else {
      arrows(0,0,sc_bp[i,1], sc_bp[i,2], lwd = 1.8, length = 0.1, col = "red")
    }
    
  }
  points(sc_species[,1], sc_species[,2], pch = 23, lwd = 1.1 ,cex = 1.5, col = "black",bg = "white")
  points(sc_species[,1], sc_species[,2], pch = 23, lwd = 2 ,cex = 1.5, col = "black", bg = alpha(cols, 0.6))
  
  
  title(xlab = paste("RDA 1,", perc[1], "%", sep = " "), ylab = paste("RDA 2,", perc[2], "%", sep = " "),)
  
  if(displayText == TRUE){
    for(i in 1:nrow(sc_bp)){
      text(sc_bp[i,1], sc_bp[i,2], labels = row.names(sc_bp)[i], cex = 0.4, pos = 4)

    }
    
  }
  

  
}

par(family = "Times New Roman")
str(r_mod1)

png("/output/fig1a.png", height = 4, width = 4, res = 300, units = "in")
par(mar = c(4,4,1,1), family = "Times New Roman")
rda_pretty_plot(r_mod1)
dev.off()

png("/output/fig1b.png", height = 4, width = 4, res = 300, units = "in")
par(mar = c(4,4,1,1), family = "Times New Roman")
rda_pretty_plot(r_mod2)
dev.off()

png("/output/fig1c.png", height = 4, width = 4, res = 300, units = "in")
par(mar = c(4,4,1,1), family = "Times New Roman")
rda_pretty_plot(r_mod3)
dev.off()

rda_pretty_plot(r_mod1, displayText = T)
rda_pretty_plot(r_mod2, displayText = T)
rda_pretty_plot(r_mod3, displayText = T)

png("/output/fig1_legend_alt.png", height = 8, width = 5, res = 200, units = "in")
plot.new()
plot.window(xlim = c(0,10), ylim = c(0,1))
cols = c("red", "orange", "yellow", "green", "blue", "purple")
species_gs = c("G. rossii", "C. scopulorum", "D. caespitosa", "K. myosuroides", "T. parryi", "S. lanceolatum")
legend(x = 0, y = 1, pch=23, legend = c(species_gs), pt.bg = alpha(cols, 0.6), bty = "n", ncol = 1, cex = 2)
dev.off()

library(PerformanceAnalytics)

env.daily = env.daily[,c(1,2,4,3,5,6)]
env.seaso = env.seaso[,c(1,2,4,3,5,6)]

colnames(env.daily) = c("MSM", "MST", "VSM", "VST", "PSM", "PST")
colnames(env.seaso) = c("MSM", "MST", "VSM", "VST", "PSM", "PST")


png("/output/A1.1.png", height = 8, width = 8, res = 200, units = "in")
chart.Correlation(env.daily, histogram = TRUE, method = "pearson")
dev.off()

png("/output/A1.2.png", height = 8, width = 8, res = 200, units = "in")
chart.Correlation(env.seaso, histogram = TRUE, method = "pearson")
dev.off()
