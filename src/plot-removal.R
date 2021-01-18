####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# plot-removal.R
# Created January 2021
# Last Updated January 2021

####### Import Libraries and External Files #######

library(MASS)
library(ggplot2)
library(GGally)
library(ggpubr)
theme_set(theme_pubclean())

source("../utilities/order-taxo.R")
source("../utilities/rm-non-sp.R")

####### Read Data #################################

rem <- rm_non_sp(order_taxo(read.csv("data/removal.csv")))
family <- read.csv("../utilities/NACC_list_species.csv")[, c("common_name",
                                                             "family")]
ibp <- read.csv("../utilities/IBP-Alpha-Codes20.csv")[, c("SPEC",
                                                          "COMMONNAME")]
load("data/var-covar/rem_vcv_best.rda")

####### Simulate phi ##############################

jd <- seq(91, 212)
tssr_values <- seq(-2, 6)
tssr <- rep(tssr_values, each = length(jd))

sim_data <- data.frame(Species = rep(rem$Species,
                                     each = length(tssr)),
                       Model = rep(rem$best_model, each = length(tssr)),
                       Intercept = rep(1, times = length(rem$Species) * length(tssr)),
                       TSSR = rep(tssr, length(rem$Species)),
                       JD = rep(jd, length(tssr_values) * length(rem$Species)))

phi <- NULL
phi_low <- NULL
phi_high <- NULL

design <- sim_data[which(sim_data$Species == rem$Species[1]),
                   c("Intercept", "TSSR", "JD")]
design$TSSR <- design$TSSR / 24
design$TSSR2 <- design$TSSR ^ 2
design$JD <- design$JD / 365
design$JD2 <- design$JD ^ 2

design <- design[, c("Intercept", "TSSR", "TSSR2", "JD", "JD2")]

for (sp in rem$Species)
{
  coefficients <- as.numeric(rem[which(rem$Species == sp), 
                      c("int_best", "tssr_best", "tssr2_best", "jd_best", "jd2_best")])
  
  zeros_indices <- which(is.na(coefficients)) - 1
  if (length(zeros_indices) > 0)
  {
    coefficients <- coefficients[-which(is.na(coefficients))]    
  }
  
  vcv <- rem_vcv_best[[sp]]
  
  # Simulate a bunch of possible coefficients
  # Try-catch surrounding for the odd non positive definite var-covar
  sim_coef <-tryCatch(
    {
      rbind(coefficients, MASS::mvrnorm(10^4, coefficients, vcv))
      #sim_coef <- rbind(coefficients, MASS::mvrnorm(10^4, coefficients, vcv))      
    },
    error = function(e)
    {
      return(NA)
    }
  )

  if (is.na(sim_coef))
  {
    # In the case of an error, just output the calculated phi and NA
    # for the upper and lower
    if (length(zeros_indices) > 0)
    {
      coef_zeros <- c(coefficients, rep(0, length(zeros_indices)))
      id <- c(seq_along(coefficients), zeros_indices + 0.5)
      coef_zeros <- coef_zeros[order(id)]    
    }else
    {
      coef_zeros <- coefficients
    }
    
    phi_pred <- exp(as.matrix(design) %*% (coef_zeros))
    phi <- c(phi, as.numeric(phi_pred[,1]))
    phi_low <- c(phi_low, rep(NA, nrow(design)))
    phi_high <- c(phi_high, rep(NA, nrow(design)))    
  }else
  {
    # Add columns of zeros back in to where NA coefficients were previously
    # See https://stackoverflow.com/a/1495204/5665609 for explanation
    if (length(zeros_indices) > 0)
    {
      coef_zeros <- cbind(sim_coef, matrix(0,
                                           ncol = length(zeros_indices),
                                           nrow = nrow(sim_coef)))
      id <- c(seq_along(sim_coef[1,]), zeros_indices + 0.5)
      coef_zeros <- coef_zeros[,order(id)]    
    }else
    {
      coef_zeros <- sim_coef
    }
    
    phi_pred <- exp(as.matrix(design) %*% t(coef_zeros))
    phi <- c(phi, as.numeric(phi_pred[,1]))
    
    # Calculate quantiles
    phi_pred <- phi_pred[,-1]
    phi_low <- c(phi_low,
                 as.numeric(apply(phi_pred,
                                  1,
                                  quantile,
                                  probs = c(0.025),
                                  na.rm = TRUE)))
    phi_high <- c(phi_high,
                  as.numeric(apply(phi_pred,
                                   1,
                                   quantile,
                                   probs = c(0.975),
                                   na.rm = TRUE)))       
  }

}

sim_data$phi <- phi
sim_data$phi_2.5 <- phi_low
sim_data$phi_97.5 <- phi_high

# 
# 
# coefficients <- rem[, c("int_best", "tssr_best", "tssr2_best", "jd_best", "jd2_best")]
# coefficients <- coefficients[rep(seq_len(nrow(coefficients)),
#                                  each = length(tssr)), ]
# coefficients[is.na(coefficients)] <- 0
# 
# sim_data$phi <- coefficients[,"int_best"] +
#   (coefficients[, "tssr_best"] * (sim_data[, "TSSR"] / 24)) +
#   (coefficients[, "tssr2_best"] * ((sim_data[, "TSSR"] / 24))^2) +
#   (coefficients[, "jd_best"] * (sim_data[, "JD"] / 365)) +
#   (coefficients[, "jd2_best"] * ((sim_data[, "JD"] / 365))^2)
# sim_data$phi <- exp(sim_data$phi)

write.table(x = sim_data, file = "tables/phi_sim.csv", sep = ",", row.names = FALSE)

####### Simulate p ################################

time_values <- c(1, 3, 5, 10)
time <- rep(time_values, times = nrow(sim_data))

sim_data <- sim_data[rep(seq_len(nrow(sim_data)),
                         each = length(time_values)), ]
sim_data$Time <- time
sim_data$p <- 1 - exp(-(sim_data$Time * sim_data$phi))
sim_data$p_2.5 <- 1 - exp(-(sim_data$Time * sim_data$phi_2.5))
sim_data$p_97.5 <- 1 - exp(-(sim_data$Time * sim_data$phi_97.5))

####### Plot All Species with Mean (JD) ###########

# Hold TSSR (we'll call it sr here) constant at 1
sr <- 1

# Empty plot list
plot_list <- vector(mode = "list", length = length(time_values))

i <- 1

for (tv in time_values)
{
  plot_list[[i]] <- 
    ggplot(data = sim_data[which(sim_data$TSSR == sr & 
                                   sim_data$Time == tv),]) +
    geom_line(aes(x = JD, y = p, group = Species), alpha = 0.05) +
    stat_summary(aes(x = JD, y = p), fun = mean, geom = "smooth", size = 1.25) +
    ylim(0, 1) +
    #theme(legend.position = "none") +
    NULL
  i <- i + 1
}

plot_matrix <- ggmatrix(
  plot_list,
  ncol = length(time_values),
  nrow = 1,
  xAxisLabels = c("1 min", "3 min", "5 min", "10 min")
)

png(filename = "plots/removal/removal_jd_all_species.png",
    width = 7, height = 3, units = "in", res = 300)
print(plot_matrix)
dev.off()

####### Plot All Species with Mean (TSSR) ###########

# Hold JD (we'll call it day here) constant at 152 (i.e. June 1)
day <- 152

# Empty plot list
plot_list <- vector(mode = "list", length = length(time_values))

i <- 1

for (tv in time_values)
{
  plot_list[[i]] <- 
    ggplot(data = sim_data[which(sim_data$JD == day & 
                                   sim_data$Time == tv),]) +
    geom_line(aes(x = TSSR, y = p, group = Species), alpha = 0.05) +
    stat_summary(aes(x = TSSR, y = p), fun = mean, geom = "smooth", size = 1.25) +
    ylim(0, 1) +
    #theme(legend.position = "none") +
    NULL
  i <- i + 1
}

plot_matrix <- ggmatrix(
  plot_list,
  ncol = length(time_values),
  nrow = 1,
  xAxisLabels = c("1 min", "3 min", "5 min", "10 min")
)

png(filename = "plots/removal/removal_tssr_all_species.png",
    width = 7, height = 3, units = "in", res = 300)
print(plot_matrix)
dev.off()

####### Plot by Species (JD) ######################

sm_list <- vector(mode = "list", length = length(unique(sim_data$Species)))
sm <- 1

for (sp in unique(sim_data$Species))
{
  # Empty plot list
  plot_list <- vector(mode = "list", length = length(time_values))
  
  i <- 1
  
  for (tv in time_values)
  {
    plot_list[[i]] <- 
      ggplot(data = sim_data[which(sim_data$TSSR == sr & 
                                     sim_data$Time == tv &
                                     sim_data$Species == sp),]) +
      geom_line(aes(x = JD, y = p)) +
      geom_ribbon(aes(x = JD, ymin = p_2.5, ymax = p_97.5),
                  alpha = 0.25) +
      #stat_summary(aes(x = JD, y = p), fun = mean, geom = "smooth", size = 1.25) +
      ylim(0, 1) +
      theme(legend.position = "none")
    i <- i + 1
  }
  
  plot_matrix <- ggmatrix(
    plot_list,
    ncol = length(time_values),
    nrow = 1,
    xAxisLabels = c("1 min", "3 min", "5 min", "10 min"),
    title = paste0("Species ",
                   sp)
  )
  
  sm_list[[sm]] <- plot_matrix
  sm <- sm + 1
}

pdf(file = paste0("plots/removal/removal_jd_species.pdf"), width = 7, height = 3)
print(sm_list)
dev.off() 

####### Plot by Family (TSSR) #####################

sm_list <- vector(mode = "list", length = length(unique(sim_data$Species)))
sm <- 1

for (sp in unique(sim_data$Species))
{
  # Empty plot list
  plot_list <- vector(mode = "list", length = length(time_values))
  
  i <- 1
  
  for (tv in time_values)
  {
    plot_list[[i]] <- 
      ggplot(data = sim_data[which(sim_data$JD == day & 
                                     sim_data$Time == tv &
                                     sim_data$Species == sp),]) +
      geom_line(aes(x = TSSR, y = p)) +
      geom_ribbon(aes(x = TSSR, ymin = p_2.5, ymax = p_97.5),
                  alpha = 0.25) +
     # stat_summary(aes(x = TSSR, y = p), fun = mean, geom = "smooth", size = 1.25) +
      ylim(0, 1) +
      theme(legend.position = "none")
    i <- i + 1
  }
  
  plot_matrix <- ggmatrix(
    plot_list,
    ncol = length(time_values),
    nrow = 1,
    xAxisLabels = c("1 min", "3 min", "5 min", "10 min"),
    title = paste0("Species ",
                   sp)
  )
  
  sm_list[[sm]] <- plot_matrix
  sm <- sm + 1
}

pdf(file = paste0("plots/removal/removal_tssr_species.pdf"), width = 7, height = 3)
print(sm_list)
dev.off() 

####### Plot by Family (JD) #######################

# Add Family grouping
family <- merge(x = family, y = ibp,
                by.x = "common_name", by.y = "COMMONNAME")
sim_data <- merge(x = sim_data, y = family[, c("SPEC", "family")],
                  by.x = "Species", by.y = "SPEC")

pm_list <- vector(mode = "list", length = length(unique(sim_data$family)))
pm <- 1

for (f in sort(unique(sim_data$family)))
{
  # Empty plot list
  plot_list <- vector(mode = "list", length = length(time_values))
  
  i <- 1
  n_sp <- length(unique(sim_data[which(sim_data$family == f), "Species"]))
  
  for (tv in time_values)
  {
    plot_list[[i]] <- 
      ggplot(data = sim_data[which(sim_data$TSSR == sr & 
                                     sim_data$Time == tv &
                                     sim_data$family == f),]) +
      geom_line(aes(x = JD, y = p, group = Species), alpha = 0.2) +
      stat_summary(aes(x = JD, y = p), fun = mean, geom = "smooth", size = 1.25) +
      ylim(0, 1) +
      theme(legend.position = "none")
    i <- i + 1
  }
  
  plot_matrix <- ggmatrix(
    plot_list,
    ncol = length(time_values),
    nrow = 1,
    xAxisLabels = c("1 min", "3 min", "5 min", "10 min"),
    title = paste0("Family ",
                   f,
                   " (n = ",
                   n_sp,
                   ")")
  )
  
  pm_list[[pm]] <- plot_matrix
  pm <- pm + 1
}

pdf(file = paste0("plots/removal/removal_jd_families.pdf"), width = 7, height = 3)
print(pm_list)
dev.off() 

####### Plot by Family (TSSR) #####################

pm_list <- vector(mode = "list", length = length(unique(sim_data$family)))
pm <- 1

for (f in sort(unique(sim_data$family)))
{
  # Empty plot list
  plot_list <- vector(mode = "list", length = length(time_values))
  
  i <- 1
  n_sp <- length(unique(sim_data[which(sim_data$family == f), "Species"]))
  
  for (tv in time_values)
  {
    plot_list[[i]] <- 
      ggplot(data = sim_data[which(sim_data$JD == day & 
                                     sim_data$Time == tv &
                                     sim_data$family == f),]) +
      geom_line(aes(x = TSSR, y = p, group = Species), alpha = 0.2) +
      stat_summary(aes(x = TSSR, y = p), fun = mean, geom = "smooth", size = 1.25) +
      ylim(0, 1) +
      theme(legend.position = "none")
    i <- i + 1
  }
  
  plot_matrix <- ggmatrix(
    plot_list,
    ncol = length(time_values),
    nrow = 1,
    xAxisLabels = c("1 min", "3 min", "5 min", "10 min"),
    title = paste0("Family ",
                   f,
                   " (n = ",
                   n_sp,
                   ")")
  )
  
  pm_list[[pm]] <- plot_matrix
  pm <- pm + 1
}

pdf(file = paste0("plots/removal/removal_tssr_families.pdf"), width = 7, height = 3)
print(pm_list)
dev.off() 
