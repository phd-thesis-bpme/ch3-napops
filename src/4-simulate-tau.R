####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# 4-simulate-tau.R
# Created January 2021
# Last Updated April 2021

####### Import Libraries and External Files #######

library(MASS)

####### Read Data #################################

dis <- read.csv("output/tables/distance_best_reduced.csv")
load("results/var-covar/dis_vcv_list.rda")

####### Simulate tau ##############################

forest_coverage <- seq(0, 1, by = 0.1)
roadside <- c(rep(1, length(forest_coverage)),
              rep(0, length(forest_coverage)))

sim_data <- data.frame(Species = rep(dis$Species,
                                     each = length(roadside)),
                       Intercept = rep(1,
                                       times = length(dis$Species) *
                                         length(roadside)),
                       Roadside = rep(roadside, length(dis$Species)),
                       Forest = rep(forest_coverage, 2 * length(dis$Species)))
sim_data$Interaction <- sim_data$Forest * sim_data$Roadside

tau <- NULL
tau_low <- NULL
tau_high <- NULL

for (sp in unique(dis$Species))
{
  model <- dis[which(dis$Species == sp), "model"]
  design <- sim_data[which(sim_data$Species == sp),
                     c("Intercept", "Roadside", "Forest", "Interaction")]
  
  coefficients <- as.numeric(dis[which(dis$Species == sp),
                                 c("intercept", "road",
                                   "forest", "roadforest")])
  zeros_indices <- which(is.na(coefficients)) - 1
  if (length(zeros_indices) > 0)
  {
    coefficients <- coefficients[-which(is.na(coefficients))]    
  }
  
  vcv <- dis_vcv_list[[model]][[sp]]
  
  # Simulate a bunch of possible coefficients
  sim_coef <- rbind(coefficients, MASS::mvrnorm(9999, coefficients, vcv))

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

  tau_pred <- exp(as.matrix(design) %*% t(coef_zeros))
  tau <- c(tau, as.numeric(tau_pred[,1]))

  # Calculate quantiles
  tau_pred <- tau_pred[,-1]
  tau_low <- c(tau_low,
               as.numeric(apply(tau_pred,
                                1,
                                quantile,
                                probs = c(0.025),
                                na.rm = TRUE)))
  tau_high <- c(tau_high,
                as.numeric(apply(tau_pred,
                                 1,
                                 quantile,
                                 probs = c(0.975),
                                 na.rm = TRUE)))

}
sim_data$tau <- tau
sim_data$tau_2.5 <- tau_low
sim_data$tau_97.5 <- tau_high

####### Save Results ############################## 

write.table(x = sim_data,
            file = "output/tables/tau_sim.csv",
            sep = ",",
            row.names = FALSE)
