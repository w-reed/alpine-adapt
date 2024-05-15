
library(ggplot2)
library(MASS)
library(lme4)
library(MuMIn)
library(ggeffects)
library(lmerTest)
library(sjPlot)
library(merTools)
library(overdisp)
library(aods3)

#adds tt function for plotting model coefficients.
source('00.add_function.R')
#generates the abundance matrix and presence/absence matrix for glmms.
source('make_matrix.R')
set.seed(1)


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
env3[,3:20] = scale(env3[, 3:20])



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
    #veg_long$abundance = ifelse(veg_long$abundance != 0, 1, 0)
  } else if (presence == FALSE){
    veg_long = veg_long[veg_long$abundance != 0, ]
    
    ind_vec <- 1:1046
    veg_long$inds <- ind_vec
  }
  veg_long$year = as.factor(veg_long$year)
  return(veg_long)
}


one.factor.model = function(matrix, env, factor, presence = FALSE){
  veg_long = make.all.veg(matrix, env, presence = presence)
  print(factor)
  ind = grep(factor,  colnames(veg_long))
  print(ind)
  print(head(veg_long[,ind]))
  
  if(presence == FALSE){
    

    print(paste('Poisson', colnames(veg_long[,ind])))
    pois_rs = glmer(abundance ~ veg_long[, ind]  + (veg_long[,ind]|species)+ (1|node)  +(1|inds)+ year, family=poisson(link = "log"), data = veg_long)
    pois_nrs = glmer(abundance ~ veg_long[, ind] + (1|species)+ (1|node) +(1|inds) + year, family=poisson(link="log"),data = veg_long)
    pois_ovd_rs = glmer(abundance ~ veg_long[, ind]  + (veg_long[,ind]|species)+ (1|node)  + year, family=poisson(link = "log"), data = veg_long)
    pois_ovd_nrs = glmer(abundance ~ veg_long[, ind] + (1|species)+ (1|node) + year, family=poisson(link="log"),data = veg_long)
    
    print(paste('NB', colnames(veg_long[,ind])))
    nb_rs = glmer.nb(abundance ~ veg_long[, ind]  + (veg_long[,ind]|species)+ (1|node) + year, data = veg_long)
    nb_nrs = glmer.nb(abundance ~ veg_long[, ind] + (1|species)+ (1|node)  + year,data = veg_long)
    print(paste('LM', colnames(veg_long[,ind])))
    lm_rs = lmer(abundance ~ veg_long[, ind]  + (veg_long[,ind]|species)+ (1|node)  + year, data = veg_long)
    lm_nrs = lmer(abundance ~ veg_long[, ind] + (1|species)+ (1|node)  + year, data = veg_long)
    # oops <- names(which(table(veg_long$species)< 5))
    # 
    # vshortest <- veg_long %>%
    #   filter(!species %in% oops)
    # print('short pois')
    # short_pois = glmer(abundance ~ vshortest[, ind]  + (vshortest[,ind]|species)+ (1|node)  + year, family=poisson(link = "log"), data = vshortest)
    # print('short nb')
    # short_nb =glmer.nb(abundance ~ vshortest[, ind]  + (vshortest[,ind]|species)+ (1|node)   + year, data = vshortest)
    # short_nb_nrs =glmer.nb(abundance ~ vshortest[, ind]  + (1|species)+ (1|node)   + year, data = vshortest)
    # 
    #print('log norm')
   # log_lm_rs = lmer(log(abundance) ~ veg_long[, ind]  + (veg_long[,ind]|species)+ (1|node)  + year, data = veg_long)
    
    #These lines calculate prediction intervals, so median fit and upper and lower bounds.
    #These will all throw warnings because the function is apparently written for binomial
    #glms. These still return values but the warnings can make it seem like the above model
    #fits have lots of issues when its really just these functions.
    preds_pois_olre_rs <- predictInterval(merMod = pois_rs, newdata = veg_long,
                                     level = 0.95, n.sims = 1000,
                                     stat = "median", type="linear.prediction",
                                     include.resid.var = TRUE)
    preds_pois_olre_nrs <- predictInterval(merMod = pois_nrs, newdata = veg_long,
                                      level = 0.95, n.sims = 1000,
                                      stat = "median", type="linear.prediction",
                                      include.resid.var = TRUE)
    preds_pois_rs <- predictInterval(merMod = pois_ovd_rs, newdata = veg_long,
                                     level = 0.95, n.sims = 1000,
                                     stat = "median", type="linear.prediction",
                                     include.resid.var = TRUE)
    preds_pois_nrs <- predictInterval(merMod = pois_ovd_nrs, newdata = veg_long,
                                      level = 0.95, n.sims = 1000,
                                      stat = "median", type="linear.prediction",
                                      include.resid.var = TRUE)
    preds_nb_rs <- predictInterval(merMod = nb_rs, newdata = veg_long,
                                   level = 0.95, n.sims = 1000,
                                   stat = "median", type="linear.prediction",
                                   include.resid.var = TRUE)
    preds_nb_nrs <- predictInterval(merMod = nb_nrs, newdata = veg_long,
                                    level = 0.95, n.sims = 1000,
                                    stat = "median", type="linear.prediction",
                                    include.resid.var = TRUE)

    preds_lm_rs <- predictInterval(merMod = lm_rs, newdata = veg_long,
                                   level = 0.95, n.sims = 1000,
                                   stat = "median", type="linear.prediction",
                                   include.resid.var = TRUE)
    preds_lm_nrs <- predictInterval(merMod = lm_nrs, newdata = veg_long,
                                    level = 0.95, n.sims = 1000,
                                    stat = "median", type="linear.prediction",
                                    include.resid.var = TRUE)
    # 
    # short_preds_pois <- predictInterval(merMod = short_pois, newdata = vshortest,
    #                                     level = 0.95, n.sims = 1000,
    #                                     stat = "median", type="linear.prediction",
    #                                     include.resid.var = TRUE)
    # short_preds_nb <- predictInterval(merMod = short_nb, newdata = vshortest,
    #                                   level = 0.95, n.sims = 1000,
    #                                   stat = "median", type="linear.prediction",
    #                                   include.resid.var = TRUE)
    # 
    # log_preds_lm_rs <- predictInterval(merMod = log_lm_rs, newdata = veg_long,
    #                                    level = 0.95, n.sims = 1000,
    #                                    stat = "median", type="linear.prediction",
    #                                    include.resid.var = TRUE)
    # 
    # 
    mods = list(pois_rs = pois_rs, pois_nrs = pois_nrs, 
                pois_ovd_rs= pois_ovd_rs, pois_ovd_nrs=pois_ovd_nrs,
                nb_rs = nb_rs, nb_nrs = nb_nrs, 
                lm_rs = lm_rs, lm_nrs = lm_nrs,
                preds_pois_olre = preds_pois_olre_rs, preds_pois_olre_nrs= preds_pois_olre_nrs,
                preds_pois_rs = preds_pois_rs, preds_pois_nrs = preds_pois_nrs,
                preds_nb_rs = preds_nb_rs, preds_nb_nrs = preds_nb_nrs,
                preds_lm_rs = preds_lm_rs, preds_lm_nrs = preds_lm_nrs,
                veg_long = veg_long)
    
    
  } else if (presence == TRUE){
    x = glmer(abundance ~  veg_long[, ind] + (veg_long[,ind]|species)+ (1|node) + year, family = binomial,data = veg_long)
    y = glmer(abundance ~  veg_long[, ind] + (1|species)+ (1|node) + year, family = binomial,data = veg_long)
    mods = list(rs = x, no_rs = y,
                veg_long = veg_long)
  }

  return(mods)
  
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


#The abundance model objects contain a lot of separate things. They have:

#1) Poisson models with an observation level random effect: pois_rs and pois_nrs for
#   random slopes and non-random slopes models respectively. Prediction intervals
#   for both models with original data - preds_pois_olre_rs and preds_pois_olre_nrs

#2) Poisson models without an observation level random effect: pois_ovd_rs and pois_ovd_nrs
#   for random slopes and non-random-slopes. Prediction intervals for both models: preds_pois_rs
#   and preds_pois_nrs.

#3) Negative binomial models (not in manuscript): nb_rs and nb_nrs. Prediction intervals for
#   both models: preds_nb_rs and preds_nb_nrs.

#4) Gaussian models (not in manuscript): lm_rs and lm_nrs. Prediction intervals for both:
#   preds_lm_rs and preds_lm_nrs

#5) the final data frame used to fit all sets of models (returned from make.all veg): veg_long.

abundance.daily = run.models(matrix_round, env1, presence = FALSE)
abundance.seaso = run.models(matrix_round, env3, presence = FALSE)



presence.daily = run.models(matrix2, env1, presence = TRUE)
presence.seaso = run.models(matrix2, env3, presence = TRUE)




##### Likelihood Ratio Tests and BIC comparing random slopes models vs no random slopes models ####


ab_d_ps <- numeric(6)
ab_s_ps <- numeric(6)
r2_pois_s_rs <- numeric(6)
r2_pois_s_nrs<- numeric(6)
r2_pois_d_rs <- numeric(6)
r2_pois_s_nrs <- numeric(6)
nb_d_ps <- numeric(6)
nb_s_ps <- numeric(6)
pr_d_ps <- numeric(6)
pr_s_ps <- numeric(6)
lm_s_ps <- numeric(6)
lm_d_ps <- numeric(6)
ab_d_delta_aic <- numeric(6)
ab_s_delta_aic <- numeric(6)
nb_d_delta_aic <- numeric(6)
nb_s_delta_aic <- numeric(6)
lm_s_delta_aic <- numeric(6)
lm_d_delta_aic <- numeric(6)
pr_s_delta_aic <- numeric(6)
pr_d_delta_aic <- numeric(6)

overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  
  #the residuals function is a bit funky - for lme objects (and presumably similarly for glmer objects),
  #it calculates the residuals and scales them by the estimated standard deviation - which in a Poisson
  #should be the sqrt(lambda). So basically just the number of lambdas each data point is from its
  #predicted labmda. Hence why we then square it for the pearson.chisq variable - puts it back on the
  #scale of squared residuals rather than on the scale of data points. If Poisson is adequate for
  #the data, then by definition lambda should approximate the variance of data around the mean, so each
  #data point should be on average 1 lambda away from its predicted value. hence the rough pearson.chisq/rdf
  #metric, seeing if each 'free' data point is about 1 lambda away from its prediction.
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

for(i in 1:6){
   ab_d_ps[i] <- anova(abundance.daily[[i]]$pois_rs, abundance.daily[[i]]$pois_nrs)$`Pr(>Chisq)`[2]
   ab_s_ps[i] <- anova(abundance.seaso[[i]]$pois_rs, abundance.seaso[[i]]$pois_nrs)$`Pr(>Chisq)`[2]
   nb_d_ps[i] <- anova(abundance.daily[[i]]$nb_rs, abundance.daily[[i]]$nb_nrs)$`Pr(>Chisq)`[2]
   nb_s_ps[i] <- anova(abundance.seaso[[i]]$nb_rs, abundance.seaso[[i]]$nb_nrs)$`Pr(>Chisq)`[2]
   lm_s_ps[i] <- anova(abundance.seaso[[i]]$lm_rs, abundance.seaso[[i]]$lm_nrs)$`Pr(>Chisq)`[2]
   lm_d_ps[i] <- anova(abundance.daily[[i]]$lm_rs, abundance.daily[[i]]$lm_nrs)$`Pr(>Chisq)`[2]
   pr_d_ps[i] <- anova(presence.daily[[i]]$rs, presence.daily[[i]]$no_rs, refit=F)$`Pr(>Chisq)`[2]
   pr_s_ps[i] <- anova(presence.seaso[[i]]$rs, presence.seaso[[i]]$no_rs,refit=F)$`Pr(>Chisq)`[2]
  # 
   r2_pois_s_rs[i] <- r2(abundance.seaso[[i]]$pois_rs)[1]
   r2_pois_s_nrs[i]<-r2(abundance.seaso[[i]]$pois_nrs)[1]
   r2_pois_d_rs[i] <- r2(abundance.daily[[i]]$pois_rs)[1]
   r2_pois_s_nrs[i] <- r2(abundance.daily[[i]]$pois_nrs)[1]
   
  # # 
   print(i)
   ab_d_delta_aic[i] <- AIC(abundance.daily[[i]]$pois_rs)-AIC(abundance.daily[[i]]$pois_nrs)
   ab_s_delta_aic[i] <- AIC(abundance.seaso[[i]]$pois_rs)-AIC(abundance.seaso[[i]]$pois_nrs)
   nb_d_delta_aic[i] <- AIC(abundance.daily[[i]]$nb_rs)-AIC(abundance.daily[[i]]$nb_nrs)
   nb_s_delta_aic[i] <- AIC(abundance.seaso[[i]]$nb_rs)-AIC(abundance.seaso[[i]]$nb_nrs)
   lm_s_delta_aic[i] <- AIC(abundance.seaso[[i]]$lm_rs)-AIC(abundance.seaso[[i]]$lm_nrs)
   lm_d_delta_aic[i] <- AIC(abundance.daily[[i]]$lm_rs)-AIC(abundance.daily[[i]]$lm_nrs)
  # 
  pr_d_delta_aic[i] <- AIC(presence.daily[[i]]$rs)- AIC(presence.daily[[i]]$no_rs)
  pr_s_delta_aic[i] <- AIC(presence.seaso[[i]]$rs)- AIC(presence.seaso[[i]]$no_rs)
  
  # 
  

  
}





names(ab_d_ps) <- c("MEAN_SM_5", 'MEAN_ST_5', 'VAR_SM_5', 'VAR_ST_5', 'PRED_SM_5', 'PRED_ST_5')
names(ab_s_ps) <- c("MEAN_SM_5", 'MEAN_ST_5', 'VAR_SM_5', 'VAR_ST_5', 'PRED_SM_5', 'PRED_ST_5')
names(nb_d_ps) <- c("MEAN_SM_5", 'MEAN_ST_5', 'VAR_SM_5', 'VAR_ST_5', 'PRED_SM_5', 'PRED_ST_5')
names(nb_s_ps) <- c("MEAN_SM_5", 'MEAN_ST_5', 'VAR_SM_5', 'VAR_ST_5', 'PRED_SM_5', 'PRED_ST_5')
names(lm_s_ps) <- c("MEAN_SM_5", 'MEAN_ST_5', 'VAR_SM_5', 'VAR_ST_5', 'PRED_SM_5', 'PRED_ST_5')
names(lm_d_ps) <- c("MEAN_SM_5", 'MEAN_ST_5', 'VAR_SM_5', 'VAR_ST_5', 'PRED_SM_5', 'PRED_ST_5')
names(pr_d_ps) <- c("MEAN_SM_5", 'MEAN_ST_5', 'VAR_SM_5', 'VAR_ST_5', 'PRED_SM_5', 'PRED_ST_5')
names(pr_s_ps) <-c("MEAN_SM_5", 'MEAN_ST_5', 'VAR_SM_5', 'VAR_ST_5', 'PRED_SM_5', 'PRED_ST_5')
names(ab_d_delta_aic) <- c("MEAN_SM_5", 'MEAN_ST_5', 'VAR_SM_5', 'VAR_ST_5', 'PRED_SM_5', 'PRED_ST_5')
names(ab_s_delta_aic ) <- c("MEAN_SM_5", 'MEAN_ST_5', 'VAR_SM_5', 'VAR_ST_5', 'PRED_SM_5', 'PRED_ST_5')
names(nb_d_delta_aic) <- c("MEAN_SM_5", 'MEAN_ST_5', 'VAR_SM_5', 'VAR_ST_5', 'PRED_SM_5', 'PRED_ST_5')
names(nb_s_delta_aic) <- c("MEAN_SM_5", 'MEAN_ST_5', 'VAR_SM_5', 'VAR_ST_5', 'PRED_SM_5', 'PRED_ST_5') 
names(lm_s_delta_aic) <- c("MEAN_SM_5", 'MEAN_ST_5', 'VAR_SM_5', 'VAR_ST_5', 'PRED_SM_5', 'PRED_ST_5')
names(lm_d_delta_aic) <- c("MEAN_SM_5", 'MEAN_ST_5', 'VAR_SM_5', 'VAR_ST_5', 'PRED_SM_5', 'PRED_ST_5')
names(pr_d_delta_aic) <- c("MEAN_SM_5", 'MEAN_ST_5', 'VAR_SM_5', 'VAR_ST_5', 'PRED_SM_5', 'PRED_ST_5')
names(pr_s_delta_aic) <- c("MEAN_SM_5", 'MEAN_ST_5', 'VAR_SM_5', 'VAR_ST_5', 'PRED_SM_5', 'PRED_ST_5')


#p values for LTRs comparing random slopes to no random slopes - a couple in the abundance models
#are not statistically significant
ab_d_ps
ab_s_ps
nb_d_ps
nb_s_ps
lm_d_ps
nb_s_ps
pr_d_ps
pr_s_ps

# delta AIC for comparing random slopes to no random slopes
ab_d_delta_aic
ab_s_delta_aic
nb_d_delta_aic
nb_s_delta_aic
lm_d_delta_aic
lm_s_delta_aic
pr_s_delta_aic
pr_d_delta_aic



for(i in 1:6){
  print(paste('NB daily AIC for model',i,AIC(abundance.daily[[i]]$nb_rs)))
  print(paste('Pois daily AIC for model',i,AIC(abundance.daily[[i]]$pois_rs)))
  print('')
  print(paste('NB seasonal AIC for model',i,AIC(abundance.seaso[[i]]$nb_rs)))
  print(paste('Pois seasonal AIC for model',i,AIC(abundance.seaso[[i]]$pois_rs)))
  print('')
}

overdisp_fun(abundance.daily[[1]]$pois_rs)
overdisp_fun(abundance.daily[[2]]$pois_rs)
overdisp_fun(abundance.daily[[3]]$pois_rs)
overdisp_fun(abundance.daily[[4]]$pois_rs)
overdisp_fun(abundance.daily[[5]]$pois_rs)
overdisp_fun(abundance.daily[[6]]$pois_rs)

overdisp_fun(abundance.daily[[1]]$pois_ovd_rs)
overdisp_fun(abundance.daily[[2]]$pois_ovd_rs)
overdisp_fun(abundance.daily[[3]]$pois_ovd_rs)
overdisp_fun(abundance.daily[[4]]$pois_ovd_rs)
overdisp_fun(abundance.daily[[5]]$pois_ovd_rs)
overdisp_fun(abundance.daily[[6]]$pois_ovd_rs)


overdisp_fun(abundance.seaso[[1]]$pois_rs)
overdisp_fun(abundance.seaso[[2]]$pois_rs)
overdisp_fun(abundance.seaso[[3]]$pois_rs)
overdisp_fun(abundance.seaso[[4]]$pois_rs)
overdisp_fun(abundance.seaso[[5]]$pois_rs)
overdisp_fun(abundance.seaso[[6]]$pois_rs)


overdisp_fun(abundance.seaso[[1]]$pois_ovd_rs)
overdisp_fun(abundance.seaso[[2]]$pois_ovd_rs)
overdisp_fun(abundance.seaso[[3]]$pois_ovd_rs)
overdisp_fun(abundance.seaso[[4]]$pois_ovd_rs)
overdisp_fun(abundance.seaso[[5]]$pois_ovd_rs)
overdisp_fun(abundance.seaso[[6]]$pois_ovd_rs)



## CHANGE S

new_plot = function(presence = TRUE){
  plot.new()
  if(presence == TRUE){
    plot.window(xlim = c(-4.5, 4.5), ylim = c(0.5,6.5))
  } else if (presence == FALSE){
    plot.window(xlim = c(-2, 2), ylim = c(0.5,6.5))
  }
  
  box(bty = "l", lwd = 2)
  axis(1)
  #axis(2)
  
}

get_reff_species = function(mod){
    reff = ranef(mod)
    return(reff$species$`veg_long[, ind]`)
}

#get_reff_species(abundance.daily)

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

  if(abs(attr(VarCorr(model)$species, 'correlation')[2])>0.99|
     abs(attr(VarCorr(model)$species, 'correlation')[2])<0.06){
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
  if(presence==T){
    for(i in 1:length(mod.list)){
      print(mod.list[[i]]$rs)
      if(!isSingular(mod.list[[i]]$rs)){
        print(get_reff_species(mod.list[[i]]$rs))
        other.list[[i]] = get_reff_species(mod.list[[i]]$rs)
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
    
    add_species(mod.list[[1]]$rs, "MEAN_SM_5")
    add_species(mod.list[[2]]$rs, "MEAN_ST_5")
    add_species(mod.list[[3]]$rs, "VAR_SM_5_tc")
    add_species(mod.list[[4]]$rs, "VAR_ST_5_tc")
    add_species(mod.list[[5]]$rs, "PRED_SM_5")
    add_species(mod.list[[6]]$rs, "PRED_ST_5")
  } else if(presence ==F){
    for(i in 1:length(mod.list)){
      print(mod.list[[i]]$pois_rs)
      if(abs(attr(VarCorr(mod.list[[i]]$pois_rs)$species, 'correlation')[2])<0.99&
         abs(attr(VarCorr(mod.list[[i]]$pois_rs)$species, 'correlation')[2])>0.06){
        print(get_reff_species(mod.list[[i]]$pois_rs))
        other.list[[i]] = get_reff_species(mod.list[[i]]$pois_rs)
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
    
    add_species(mod.list[[1]]$pois_rs, "MEAN_SM_5")
    add_species(mod.list[[2]]$pois_rs, "MEAN_ST_5")
    add_species(mod.list[[3]]$pois_rs, "VAR_SM_5_tc")
    add_species(mod.list[[4]]$pois_rs, "VAR_ST_5_tc")
    add_species(mod.list[[5]]$pois_rs, "PRED_SM_5")
    add_species(mod.list[[6]]$pois_rs, "PRED_ST_5")
  }
 
  
}


plot.slopes(abundance.daily)
plot.slopes(abundance.seaso)
plot.slopes(presence.daily, presence = TRUE)
plot.slopes(presence.seaso, presence = TRUE)

!file.exists('output')
if(!file.exists('output')){
  dir.create('output')
}

cols = c("red", "orange", "yellow", "green", "blue", "purple")
png("output/Figure4_daily_occurence.png", res = 200, width = 6, height = 6, units = "in")
plot.slopes(presence.daily, presence = TRUE)
legend(1.5, 6.5, pch = 21, col = "black", legend = c(expression(italic("G. rossii")), 
                                                                  expression(italic("C. scopulorum")), 
                                                                  expression(italic("D. caespitosa")), 
                                                                  expression(italic("K. myosuroides")), 
                                                                  expression(italic("T. parryi")), 
                                                                  expression(italic("S. lanceolatum"))), 
                                                       pt.bg = cols, bty = "n", cex=1.12)
dev.off()

png("output/Figure4_seasonal_occurence.png", res = 200, width = 6, height = 6, units = "in")
plot.slopes(presence.seaso, presence = TRUE)
#legend(1.5, 6.5, pch = 21, col = "black", legend = c(species_gs), pt.bg = cols, bty = "n")
dev.off()

png("output/Figure4_daily_abundance_OLRE_Poisson.png", res = 200, width = 6, height = 6, units = "in")
plot.slopes(abundance.daily)
#legend(1.5, 6.5, pch = 21, col = "black", legend = c(species_gs), pt.bg = cols, bty = "n")
dev.off()

png("output/Figure4_seasonal_abundance_OLRE_Poisson.png", res = 200, width = 6, height = 6, units = "in")
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
      tmp[i, j] = paste(round(as.numeric(performance::r2(tmp.mods[[j]]$spp)[2]), digits = 3),
                        round(as.numeric(performance::r2(tmp.mods[[j]]$spp)[1]), digits = 3), sep = "/") 
    }
    
    
  }
  return(tmp)
}




count_sig = function(models, presence=F){
  set.seed(12)
  vect = NULL
  vect2 = NULL
  if(presence==T){
    for(i in 1:length(models)){
      xx = tt(REsim(models[[i]]$rs))
      xx = xx[xx$groupFctr == "species" & xx$term != "(Intercept)", ]
      if(!isSingular(models[[i]]$rs)){
        vect2= c(vect2, xx[which(xx$sig == TRUE), "groupID"]) 
      }
      vect[i] = as.numeric(table(xx$sig)[2])    
    }
  }else if(presence==F){
    for(i in 1:length(models)){
      if(attr(VarCorr(models[[i]]$pois_rs)$species, 'correlation')[2]<0.99){
        xx = tt(REsim(models[[i]]$pois_rs))
        xx = xx[xx$groupFctr == "species" & xx$term != "(Intercept)", ]
        vect[i] = as.numeric(table(xx$sig)[2])    
        
        vect2= c(vect2, xx[which(xx$sig == TRUE), "groupID"]) 
        
      }else{
        vect[i] <- 0
      }
      }
    }
  
  
  return(list(count=vect,spp=vect2))
}
ad_sig <- count_sig(abundance.daily)
as_sig <- count_sig(abundance.seaso)
pd_sig <- count_sig(presence.daily, presence=T)
ps_sig <- count_sig(presence.seaso, presence=T)

#number of statistically significant terms in each models

ad_sig$count
as_sig$count
pd_sig$count
ps_sig$count


#percentage of species that had at least one statistically significant term

length(unique(ad_sig$spp))/87
length(unique(as_sig$spp))/87

#there are ten more species in the presence/absence data
length(unique(pd_sig$spp))/87
length(unique(ps_sig$spp))/87

table(ad_sig$spp)
table(as_sig$spp)
table(pd_sig$spp)
table(ps_sig$spp)


cols = c("dark blue", "dark red", alpha("dark blue", 0.66), alpha("dark red", 0.66), alpha("dark blue", 0.33), alpha("dark red", 0.33))



png("output/Fig3a_Presence_daily.png", height = 5, width = 4.25, res =150, units = "in")
par(mar = c(3,3,3,3), cex.axis = 2)
barplot(count_sig(presence.daily, presence=T)$count, ylim = c(0,40), beside = T, space = 0, col = cols)
box(bty = "l", lwd = 2)
dev.off()

png("output/Fig3b_Presence_seasonal.png", height = 5, width = 4.25, res =150, units = "in")
par(mar = c(3,3,3,3), cex.axis = 2)
barplot(count_sig(presence.seaso, presence=T)$count, ylim = c(0,40), beside = T, space = 0, col = cols)
box(bty = "l", lwd = 2)
dev.off()

png("output/Fig3c_Abundance_daily.png", height = 5, width = 4.25, res =150, units = "in")
par(mar = c(3,3,3,3), cex.axis = 2)
barplot(count_sig(abundance.daily)$count, ylim = c(0,20), beside = T, space = 0, col = cols)
box(bty = "l", lwd = 2)
dev.off()

png("output/Fig3d_Abundance_seasonal.png", height = 5, width = 4.25, res =150, units = "in")
par(mar = c(3,3,3,3), cex.axis = 2)
barplot(count_sig(abundance.seaso)$count, ylim = c(0,20), beside = T, space = 0, col = cols)
box(bty = "l", lwd = 2)
dev.off()



tab_model(presence.daily[[1]]$rs, presence.daily[[2]]$rs,presence.daily[[3]]$rs,
          presence.daily[[4]]$rs, presence.daily[[5]]$rs, presence.daily[[6]]$rs,
          file = "Models/Table_S3_Random_slopes_presence_daily_tabs_sep.html", show.aic = F, show.r2 = T,
          dv.labels = c("MSM", "MST", "VSM", "VST", "PSM", "PST"))
tab_model(presence.daily[[1]]$no_rs, presence.daily[[2]]$no_rs,presence.daily[[3]]$no_rs,
          presence.daily[[4]]$no_rs, presence.daily[[5]]$no_rs, presence.daily[[6]]$no_rs,
          file = "Models/No_random_slopes_presence_daily_tabs_sep.html",show.aic = F, show.r2 = T,
          dv.labels = c("MSM", "MST", "VSM", "VST", "PSM", "PST"))



tab_model(presence.seaso[[1]]$rs, presence.seaso[[2]]$rs,presence.seaso[[3]]$rs,
          presence.seaso[[4]]$rs, presence.seaso[[5]]$rs, presence.seaso[[6]]$rs,
          file = "Models/Table_S4_Random_slopes_presence_seaso_tabs_sep.html",show.aic = F, show.r2 = T,
          dv.labels = c("MSM", "MST", "VSM", "VST", "PSM", "PST"))
tab_model(presence.seaso[[1]]$no_rs, presence.seaso[[2]]$no_rs,presence.seaso[[3]]$no_rs, 
          presence.seaso[[4]]$no_rs, presence.seaso[[5]]$no_rs, presence.seaso[[6]]$no_rs,
          file = "No_random_slopes_presence_seaso_tabs_sep.html",show.aic = F, show.r2 = T,
          dv.labels = c("MSM", "MST", "VSM", "VST", "PSM", "PST"))



tab_model(abundance.daily[[1]]$pois_rs, abundance.daily[[2]]$pois_rs,abundance.daily[[3]]$pois_rs, 
          abundance.daily[[4]]$pois_rs, abundance.daily[[5]]$pois_rs, abundance.daily[[6]]$pois_rs ,
          file = "Models/Table_S5_Poisson_random_slopes_abundance_daily_tabs_no_AIC.html",show.aic = F, show.r2 = T,
          dv.labels = c("MSM", "MST", "VSM", "VST", "PSM", "PST"))
tab_model(abundance.daily[[1]]$pois_nrs, abundance.daily[[2]]$pois_nrs,abundance.daily[[3]]$pois_nrs, 
          abundance.daily[[4]]$pois_nrs, abundance.daily[[5]]$pois_nrs, abundance.daily[[6]]$pois_nrs ,
          file = "Models/Poisson_no_random_slopes_abundance_daily_tabs_no_AIC.html",show.aic = F, show.r2 = T,
          dv.labels = c("MSM", "MST", "VSM", "VST", "PSM", "PST"))


tab_model(abundance.seaso[[1]]$pois_rs, abundance.seaso[[2]]$pois_rs,abundance.seaso[[3]]$pois_rs, abundance.seaso[[4]]$pois_rs, 
          abundance.seaso[[5]]$pois_rs, abundance.seaso[[6]]$pois_rs,
          file = "Models/Table_S6_Poisson_random_slopes_abundance_seaso_tabs_no_AIC.html", show.aic = F, show.r2 = T,
          dv.labels = c("MSM", "MST", "VSM", "VST", "PSM", "PST"))
 
tab_model(abundance.seaso[[1]]$pois_nrs, abundance.seaso[[2]]$pois_nrs,abundance.seaso[[3]]$pois_nrs, 
               abundance.seaso[[4]]$pois_nrs, abundance.seaso[[5]]$pois_nrs, abundance.seaso[[6]]$pois_nrs,
               file = "Models/Poisson_no_random_slopes_abundance_seaso_tabs_no_AIC.html",show.aic = F, show.r2 = T,
          dv.labels = c("MSM", "MST", "VSM", "VST", "PSM", "PST"))


####Quick exam of how species variance component is stable regardless of including fixed predictor ###

veg_long_d <- abundance.daily[[1]]$veg_long
veg_long_s <- abundance.seaso[[1]]$veg_long
species_node_nb <- glmer.nb(abundance~ (1|species)+(1|node)+year, data=veg_long_d)


full_mod_nrs_nb <- glmer.nb(abundance~ MEAN_SM_5+(1|species)+(1|node)+year, data=veg_long_d)

summary(species_node_nb)$varcor
summary(full_mod_nrs_nb)$varcor


species_nod_pois <- glmer(abundance ~ (1|species)+(1|node)+year, data=veg_long_d, family=poisson())

full_mod_nrs_poiss <- glmer(abundance ~ MEAN_SM_5 + (1|species)+(1|node)+year, data=veg_long_d, family = poisson())

summary(species_nod_pois)$varcor
summary(full_mod_nrs_poiss)$varcor


#### Quick exam of how species variance component changes when we add random slopes ###

full_mod_rs_nb <- glmer.nb(abundance~ MEAN_SM_5+(MEAN_SM_5|species)+(1|node)+year, data=veg_long_d)

summary(species_node_nb)$varcor
summary(full_mod_nrs_nb)$varcor
summary(full_mod_rs_nb)$varcor

full_mod_rs_poiss <- glmer(abundance ~ MEAN_SM_5 + (MEAN_SM_5|species)+(1|node)+year, data=veg_long_d, family = poisson())

summary(species_nod_pois)$varcor
summary(full_mod_nrs_poiss)$varcor
summary(full_mod_rs_poiss)$varcor


# 
# ####### OLD CODE #########
# 
# 
# xx = xx[xx$soiltemp_5cm_avg != -34.99,]
# min(xx$soiltemp_5cm_avg, na.rm = T)
# 
# xx = xx[xx$sensornode !=12, ]
# 
# min(xx$soiltemp_5cm_avg, na.rm = T)
# 
# xxx = xx[xx$sensornode == 12, ]
# 
# plot(y = xxx$soiltemp_5cm_avg, x = 1:503584, type = "l")
# 
# 
# 
# ##########################
# library(merTools)
# library(ggplot2)
# biggest.response = function(model, term){
#   reff = ranef(model$spp)
#   x = tt(REsim(model$spp))
#   
#   reff.species = reff$species
#   
#   reff = reff.species[order(reff.species[, 'veg_long[, ind]']), ]
#   print(head(reff))
#   print(tail(reff))
# }
# 
# biggest.response(abundance.daily[[1]], "MSM")
# 
# 
# total.appearances = colSums(matrix2)
# total.appearances = total.appearances[total.appearances > 21]
# 
# sort(total.appearances)
# 
# names(total.appearances)
# 
# matrix_sub = matrix[, names(total.appearances)]
# 
# colSums(matrix_sub)
# sort(colSums(matrix_sub))
# 
# 

