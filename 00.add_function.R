tt = function (data, level = 0.95, stat = "median", sd = TRUE, sigmaScale = NULL, 
          oddsRatio = FALSE, labs = FALSE, facet = TRUE) 
{
  #plot_sim_error_chks(type = "RE", level = level, stat = stat, 
  #                    sd = sd, sigmaScale = sigmaScale, oddsRatio = oddsRatio, 
  #                    labs = labs, facet = facet)
  facet_logical <- is.logical(facet)
  if (!facet_logical) {
    data <- data[data$groupFctr == facet[[1]] & data$term == 
                   facet[[2]], ]
  }
  if (!missing(sigmaScale)) {
    data[, "sd"] <- data[, "sd"]/sigmaScale
    data[, stat] <- data[, stat]/sigmaScale
  }
  data[, "sd"] <- data[, "sd"] * qnorm(1 - ((1 - level)/2))
  data[, "ymax"] <- data[, stat] + data[, "sd"]
  data[, "ymin"] <- data[, stat] - data[, "sd"]
  data[, "sig"] <- data[, "ymin"] > 0 | data[, "ymax"] < 0
  hlineInt <- 0
  if (oddsRatio == TRUE) {
    data[, "ymax"] <- exp(data[, "ymax"])
    data[, stat] <- exp(data[, stat])
    data[, "ymin"] <- exp(data[, "ymin"])
    hlineInt <- 1
  }
  data <- data[order(data[, "groupFctr"], data[, "term"], data[, 
                                                               stat]), ]
  rownames(data) <- 1:nrow(data)
  data[, "xvar"] <- factor(paste(data$groupFctr, data$groupID, 
                                 sep = ""), levels = unique(paste(data$groupFctr, data$groupID, 
  
                                                                                                                                                                                               sep = "")), ordered = TRUE)
  if (labs == TRUE) {
    xlabs.tmp <- element_text(face = "bold", angle = 90, 
                              vjust = 0.5)
  }
  else {
    data[, "xvar"] <- as.numeric(data[, "xvar"])
    xlabs.tmp <- element_blank()
  }
  
  return(data)
  p <- ggplot(data, aes_string(x = "xvar", y = stat, ymax = "ymax", 
                               ymin = "ymin")) + geom_hline(yintercept = hlineInt, color = I("red"), 
                                                            size = I(1.1)) + geom_point(color = "gray75", alpha = 1/(nrow(data)^0.33), 
                                                                                        size = I(0.5)) + geom_point(data = subset(data, sig == 
                                                                                                                                    TRUE), size = I(3)) + labs(x = "Group", y = "Effect Range", 
                                                                                                                                                               title = "Effect Ranges") + theme_bw() + theme(panel.grid.major = element_blank(), 
                                                                                                                                                                                                             panel.grid.minor = element_blank(), axis.text.x = xlabs.tmp, 
                                                                                                                                                                                                             axis.ticks.x = element_blank())
  if (sd) {
    p <- p + geom_pointrange(alpha = 1/(nrow(data)^0.33)) + 
      geom_pointrange(data = subset(data, sig == TRUE), 
                      alpha = 0.25)
  }
  if (facet_logical) {
    return(p + facet_grid(term ~ groupFctr, scales = "free_x"))
  }
  else {
    return(p)
  }
}
