library(ggplot2)
library(MASS)
library(lme4)
library(MuMIn)
library(ggeffects)
library(lmerTest)
library(sjPlot)
library(merTools)

## Important species
species_gs = c("G. rossii", "C. scopulorum", "D. caespitosa", "K. myosuroides", "T. parryi", "S. lanceolatum")

### DAILY
env1 = read.csv("data/env01.csv")
colnames(env1) = c("node", "year", "MEAN_SM_5", "VAR_SM_5_tc", "PRED_SM_5", "MEAN_ST_5", 
                   "VAR_ST_5_tc", "PRED_ST_5")
env1 = env1[env1$node != 15, ]
env1 = env1[env1$node != 12 | env1$year != 2021, ]



### SEASONAL
env3 = read.csv("data/env03.csv")

env1 = env1[c(1:23, 25:51), ]
env3 = env3[c(1:23, 25:51), ]

env1[, 3:8] = scale(env1[, 3:8])
env3[, 3:18] = scale(env3[, 3:18])

matrix2df = function(species, matrix){
  t1 = matrix[, colnames(matrix) %in% species]
  t2 = data.frame(rownames(matrix), t1)
  colnames(t2) = c("name", "abundance")
  t2$species = species
  t2$node = matrix(unlist(strsplit(t2$name, split = "-")), ncol = 2, byrow = T)[, 1]
  t2$year = matrix(unlist(strsplit(t2$name, split = "-")), ncol = 2, byrow = T)[, 2]
  t2 = t2[, c(4,5,3,2)]
  return(t2)
}

make.df = function(veg, env){
  all = merge(veg, env, by.x = c("year", "node"), by.y = c("year", "node"), all.y = T)
  all$year = as.numeric(all$year)
  return(all)
  
}

make.all.veg = function(matrix, env, presence = FALSE){
  species = unique(colnames(matrix))
  species = species[!(species %in% c("Brassicaceae_sp", 
                                      "Carex_sp", "Draba_sp", "Forb_sp","JUNI_sp","Lichen", "Litter", "Moss", "Pedicularis_sp",  "POT_SP", 
                                      "Potentilla_sp1", "Potentilla_sp2", "Rock", "Soil", "Unknown_graminoid", 
                                      "Unknown_seedling", "Vaccinium_sp"))]
  
  veg_long = as.data.frame(matrix(nrow = 0, ncol = 10))
  colnames(veg_long) = c("node", "year", "species", "abundance", "MEAN_SM_5", "MEAN_ST_5",
                         "VAR_SM_5_tc", "VAR_ST_5_tc", "PRED_SM_5", "PRED_ST_5")
  
  for(i in species){
    veg_long  = rbind(veg_long, make.df(matrix2df(i, matrix), env))
  }
  
  if(presence == TRUE){
    veg_long$abundance = ifelse(veg_long$abundance != 0, 1, 0)
  } else if (presence == FALSE){
    veg_long = veg_long[veg_long$abundance != 0, ]
  }
  
  
  return(veg_long)
}

one.factor.model = function(matrix, env, factor, presence = FALSE){
  veg_long = make.all.veg(matrix, env, presence = presence)
  veg_long$year = as.factor(veg_long$year)
  ind = grep(factor,  colnames(veg_long))
  
  if(presence == FALSE){
    x = glmer(abundance ~ veg_long[, ind] + year + (veg_long[,ind]|species) + (1|node) + year, family = poisson(link='log'), data = veg_long)
  } else if (presence == TRUE){
    x = glmer(abundance ~  veg_long[, ind] + (veg_long[,ind]|species) + (1|node) + year, family = binomial,data = veg_long)
  }

  return(x)
  
  
  
}

run.models = function(matrix, env, presence = FALSE){
  p.terms = c("MEAN_SM_5", "MEAN_ST_5", "VAR_SM_5_tc", "VAR_ST_5_tc", "PRED_SM_5", "PRED_ST_5")
  mod.list = list()
  
  for(i in 1:length(p.terms)){
    mod = one.factor.model(matrix, env, p.terms[i], presence)
    mod.list[[i]] = mod
  }
  
  return(mod.list)
  
}

abundance.daily = run.models(round(matrix), env1, presence = FALSE)
abundance.seaso = run.models(round(matrix), env3, presence = FALSE)

presence.daily = run.models(matrix, env1, presence = TRUE)
presence.seaso = run.models(matrix, env3, presence = TRUE)

new_plot = function(presence = TRUE){
  plot.new()
  if(presence == TRUE){
    plot.window(xlim = c(-4, 4), ylim = c(0.5,6.5))
  } else if (presence == FALSE){
    plot.window(xlim = c(-25, 25), ylim = c(0.5,6.5))
  }
  
  box(bty = "l", lwd = 2)
  axis(1)
  #axis(2)
  
}

get_reff_species = function(mod){
    reff = ranef(mod)
    return(reff$species$`veg_long[, ind]`)
}

get_reff_species(abundance.daily)

add_distributions = function(model, term){
  names = c("MEAN_SM_5",  "MEAN_ST_5", "VAR_ST_5_tc", "VAR_SM_5_tc", "PRED_SM_5", "PRED_ST_5")
  shift = c(5,4,3,2,1,0)
  reff = ranef(model)
  x = density(reff$species$`veg_long[, ind]`)
  lines(x$y + shift[which(names %in% term)] ~ x$x, lwd = 2)
}

add_species = function(model, term){
  reff = ranef(model)
  x = tt(REsim(model))
  print(x)
  species = c("ACOROS", "CARSCO", "DESCAE", "KOBMYO", "TRIPAR", "SEDLAN")
  cols = c("red", "orange", "yellow", "green", "blue", "purple")
  names = c("MEAN_SM_5",  "MEAN_ST_5", "VAR_SM_5_tc", "VAR_ST_5_tc", "PRED_SM_5", "PRED_ST_5")
  shift = c(1,2,3,4,5,6)

  if(isSingular(model)){
    text(x = 0, y = shift[which(names %in% term)], labels = c("NO SUPPORT"), col = "gray", cex = 1.5)
  } else{
    for(i in 1:6){
      xx = x[x$groupID %in% species[i] & x$term == "veg_long[, ind]", ]
      arrows(xx$mean, shift[which(names %in% term)] + 0.05 * i, xx$ymax, shift[which(names %in% term)] + 0.05 * i, lwd = 2, col = alpha(cols[i], 0.7), angle = 90, length = 0.05)
      arrows(xx$mean, shift[which(names %in% term)] + 0.05 * i, xx$ymin, shift[which(names %in% term)] + 0.05 * i, lwd = 2, col = alpha(cols[i], 0.7), angle = 90, length = 0.05)
      points(xx$mean, shift[which(names %in% term)] + 0.05 * i, pch = 21, bg = cols[i], col = "black", cex = 1.2)
    }
  }
  

  
  
}

plot.slopes = function(mod.list, presence = FALSE){
  other.list = list()
  
  for(i in 1:length(mod.list)){
    print(mod.list[[i]])
    if(!isSingular(mod.list[[i]])){
      print(get_reff_species(mod.list[[i]]))
      other.list[[i]] = get_reff_species(mod.list[[i]])
    }else{
      other.list[[i]] = NA
    }
  }
  print(other.list)
  
  par(las = 1, bty = "l", mar = c(4,4,0.5,0.5), cex.axis = 1.5)
  new_plot(presence = presence)
  abline(v = 0, col = "gray", lty = 2, lwd = 2)
  boxplot(other.list,
          horizontal = T,
          range = 0, bty = "l", boxwex = 0.5,
          names = c("MSM", "MST", "VSM", "VST", "PSM", "PST"), 
          ylim = c(-4, 4), lwd = 1.5, col = "white", add = T)
  
  add_species(mod.list[[1]], "MEAN_SM_5")
  add_species(mod.list[[2]], "MEAN_ST_5")
  add_species(mod.list[[3]], "VAR_SM_5_tc")
  add_species(mod.list[[4]], "VAR_ST_5_tc")
  add_species(mod.list[[5]], "PRED_SM_5")
  add_species(mod.list[[6]], "PRED_ST_5")
  
}

plot.slopes(abundance.daily)
plot.slopes(abundance.seaso)
plot.slopes(presence.daily, presence = TRUE)
plot.slopes(presence.seaso, presence = TRUE)


cols = c("red", "orange", "yellow", "green", "blue", "purple")
png("/output/Figure4_daily_occurence", res = 200, width = 6, height = 6, units = "in")
plot.slopes(presence.daily, presence = TRUE)
legend(1.5, 6.5, pch = 21, col = "black", legend = c(rev(c("G. rossii", "C. scopulorum", "D. caespitosa", "K. myosuroides", "T. parryi", "S. lanceolatum"))), pt.bg = rev(cols), bty = "n")
dev.off()

png("/output/Figure4_seasonal_occurence", res = 200, width = 6, height = 6, units = "in")
plot.slopes(presence.seaso, presence = TRUE)
#legend(1.5, 6.5, pch = 21, col = "black", legend = c(species_gs), pt.bg = cols, bty = "n")
dev.off()

png("/output/Figure4_daily_abundance", res = 200, width = 6, height = 6, units = "in")
plot.slopes(abundance.daily)
#legend(1.5, 6.5, pch = 21, col = "black", legend = c(species_gs), pt.bg = cols, bty = "n")
dev.off()

png("/output/Figure4_seasonal_abundance", res = 200, width = 6, height = 6, units = "in")
plot.slopes(abundance.seaso)
#legend(1.5, 6.5, pch = 21, col = "black", legend = c(species_gs), pt.bg = cols, bty = "n")
dev.off()

make_tab = function(modlists, names1, names2){
  tmp = as.data.frame(matrix(nrow = length(names1), ncol = length(names2)))
  row.names(tmp) = names1
  colnames(tmp) = names2
  
  for(i in 1:length(modlists)){
    tmp.mods = modlists[[i]]
    for(j in 1:length(tmp.mods)){
      tmp[i, j] = paste(round(as.numeric(performance::r2(tmp.mods[[j]])[2]), digits = 2),
                        round(as.numeric(performance::r2(tmp.mods[[j]])[1]), digits = 2), sep = "/") 
    }
    
    
  }
  return(tmp)
}

xx = make_tab(list(abundance.daily, abundance.seaso,
              presence.daily, presence.seaso), 
         names1 = c("abundance_daily", "abundance_seasonal", "occurence_daily", "occurence_seasonal"),
         names2 = c("MSM", "MST", "VSM", "VST", "PSM", "PST"))

write.csv(xx, "output/figures/table1.csv")

count_sig = function(models){
  set.seed(12)
  vect = NULL
  vect2 = NULL
  for(i in 1:length(models)){
    xx = tt(REsim(models[[i]]))
    xx = xx[xx$groupFctr == "species" & xx$term != "(Intercept)", ]
    if(!isSingular(models[[i]])){
      vect2= c(vect2, xx[which(xx$sig == TRUE), "groupID"])  
    }
     
    vect[i] = as.numeric(table(xx$sig)[2])    
    
  }
  print(length(unique(vect2)))
  return(vect)
}
count_sig(abundance.daily)
count_sig(abundance.seaso)
count_sig(presence.daily)
count_sig(presence.seaso)

cols = c("dark blue", "dark red", alpha("dark blue", 0.66), alpha("dark red", 0.66), alpha("dark blue", 0.33), alpha("dark red", 0.33))

png("/output/Fig2a.png", height = 5, width = 4.25, res =150, units = "in")
par(mar = c(3,3,3,3), cex.axis = 2)
barplot(count_sig(presence.daily), ylim = c(0,40), beside = T, space = 0, col = cols)
box(bty = "l", lwd = 2)
dev.off()

png("/output/Fig2b.png", height = 5, width = 4.25, res =150, units = "in")
par(mar = c(3,3,3,3), cex.axis = 2)
barplot(count_sig(abundance.daily), ylim = c(0,40), beside = T, space = 0, col = cols)
box(bty = "l", lwd = 2)
dev.off()

png("/output/Fig2c.png", height = 5, width = 4.25, res =150, units = "in")
par(mar = c(3,3,3,3), cex.axis = 2)
barplot(count_sig(presence.seaso), ylim = c(0,40), beside = T, space = 0, col = cols)
box(bty = "l", lwd = 2)
dev.off()

png("/output/Fig2d.png", height = 5, width = 4.25, res =150, units = "in")
par(mar = c(3,3,3,3), cex.axis = 2)
barplot(count_sig(abundance.seaso), ylim = c(0,40), beside = T, space = 0, col = cols)
box(bty = "l", lwd = 2)
dev.off()



tab_model(presence.daily[[1]], presence.daily[[2]], file = "/Users/Will/Desktop/t1.doc")

tab_model(presence.daily[[1]], presence.daily[[2]],presence.daily[[3]], presence.daily[[4]], presence.daily[[5]], presence.daily[[6]]) #,file = "/Users/Will/Desktop/t1.doc")
tab_model(presence.seaso[[1]], presence.seaso[[2]],presence.seaso[[3]], presence.seaso[[4]], presence.seaso[[5]], presence.seaso[[6]]) #,file = "/Users/Will/Desktop/t2.doc")

tab_model(abundance.daily[[1]], abundance.daily[[2]],abundance.daily[[3]], abundance.daily[[4]], abundance.daily[[5]], abundance.daily[[6]]) #,file = "/Users/Will/Desktop/t3.doc")
tab_model(abundance.seaso[[1]], abundance.seaso[[2]],abundance.seaso[[3]], abundance.seaso[[4]], abundance.seaso[[5]], abundance.seaso[[6]]) #,file = "/Users/Will/Desktop/t4.doc")

tab_model(presence.seaso[[3]], presence.seaso[[4]])
tab_model(presence.seaso[[5]], presence.seaso[[6]])


tab_model(presence.daily[[1]], presence.daily[[2]])

tab_model(presence.daily[[3]], presence.daily[[4]])
tab_model(presence.daily[[5]], presence.daily[[6]])

tab_model(abundance.daily[[3]], abundance.daily[[4]])
tab_model(abundance.daily[[5]], abundance.daily[[6]])

xx = xx[xx$soiltemp_5cm_avg != -34.99,]
min(xx$soiltemp_5cm_avg, na.rm = T)

xx = xx[xx$sensornode !=12, ]

min(xx$soiltemp_5cm_avg, na.rm = T)

xxx = xx[xx$sensornode == 12, ]

plot(y = xxx$soiltemp_5cm_avg, x = 1:503584, type = "l")



##########################
library(merTools)
library(ggplot2)
biggest.response = function(model, term){
  reff = ranef(model)
  x = tt(REsim(model))
  
  reff.species = reff$species
  
  reff = reff.species[order(reff.species[, 'veg_long[, ind]']), ]
  print(head(reff))
  print(tail(reff))
}

biggest.response(abundance.daily[[1]], "MSM")


total.appearances = colSums(matrix2)
total.appearances = total.appearances[total.appearances > 21]

sort(total.appearances)

names(total.appearances)

matrix_sub = matrix[, names(total.appearances)]

colSums(matrix_sub)
sort(colSums(matrix_sub))



