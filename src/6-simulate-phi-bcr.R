####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# 6-simulate-phi-bcr.R
# Created January 2021
# Last Updated April 2021

####### Import Libraries and External Files #######

library(MASS)
library(lqmm)
library(Matrix)

####### Read Data #################################

rem <- read.csv("output/tables/removal_best_reduced.csv")
rem <- rem[which(rem$model >= 10), ]
rem_bcr <- read.csv("results-master/coefficients/removal_bcr.csv")
rem_bcr <- rem_bcr[which(rem_bcr$Species %in% rem$Species), ]
rem_bcr_jd <- read.csv("results-master/coefficients/removal_bcr_jd.csv")
rem_bcr_jd <- rem_bcr_jd[which(rem_bcr_jd$Species %in% rem$Species), ]
rem_bcr_jd2 <- read.csv("results-master/coefficients/removal_bcr_jd2.csv")
rem_bcr_jd2 <- rem_bcr_jd2[which(rem_bcr_jd2$Species %in% rem$Species), ]
load("results-master/var-covar/rem_vcv_list.rda")

####### Simulate phi ##############################

jd <- seq(91, 212)
tssr_values <- seq(-2, 6)
tssr <- rep(tssr_values, each = length(jd))
sim_data_list <- vector(mode = "list", length = length(unique(rem$Species)))
names(sim_data_list) <- unique(rem$Species)

for (sp in unique(rem$Species))
{
  sim_data <- data.frame(Intercept = rep(1, times = length(tssr)),
                         TSSR = tssr,
                         JD = rep(jd, length(tssr_values)))
  model <- rem[which(rem$Species == sp), "model"]
  

  bcr_sp <- which(!is.na(rem_bcr[which(rem_bcr$Species == sp & 
                                         rem_bcr$model == model), 1:40]))
  bcr <- rep(bcr_sp, times = nrow(sim_data))
  sim_data <- sim_data[rep(seq_len(nrow(sim_data)),
                           each = length(bcr_sp)), ]
  sim_data$BCR <- bcr
  
  phi <- NULL
  phi_low <- NULL
  phi_high <- NULL
  
  #' In the case where there is a BCR interaction effect, we're first going to proceed
  #' as normal and take all of the main effects that are NOT factor effects. In this case,
  #' just the intercept, tssr, and jd coefficients. And we'll do the thing where we keep track
  #' of where the NAs are for the 0 terms.
  coefficients <- as.numeric(rem[which(rem$Species == sp), 
                                 c("intercept", "tssr", "tssr2", 
                                   "jd", "jd2")])
  zeros_indices <- which(is.na(coefficients)) - 1
  if (length(zeros_indices) > 0)
  {
    coefficients <- coefficients[-which(is.na(coefficients))]    
  }
  
  #' Now let's add all of the BCR coefficients. Recall that I was clever before and not
  #' only made it so that bcr_sp tracks the BCRs that were modelled for this species,
  #' but it is also the indices where each of the BCR coefficients live in the table.
  #' Good job, past Brando!
  
  #' First just the BCR effects that change the intercept. Recall from the past that I artificially
  #' added 0 to the "base" BCR, so that I could keep track of which BCRs were being modelled. But,
  #' we need to rremove that from the possible BCRs here, so we'll go from 2:length(bcr_sp)
  coefficients <- c(coefficients,
                    as.numeric(rem_bcr[which(rem_bcr$Species == sp & 
                                               rem_bcr$model == model), bcr_sp[2:length(bcr_sp)]]))
  #' Now the BCR effects that affect the slope
  coefficients <- c(coefficients,
                    as.numeric(rem_bcr_jd[which(rem_bcr_jd$Species == sp & 
                                                  rem_bcr_jd$model == model), bcr_sp[2:length(bcr_sp)]]))
  if (model %in% c(11, 14, 15))
  {
    coefficients <- c(coefficients,
                      as.numeric(rem_bcr_jd2[which(rem_bcr_jd2$Species == sp & 
                                                     rem_bcr_jd2$model == model), bcr_sp[2:length(bcr_sp)]]))
  }
  
  vcv <- rem_vcv_list[[model]][[sp]]
  
  sim_coef <- rbind(coefficients, MASS::mvrnorm(9999, coefficients, vcv))
  
  # Add columns of zeros back in to where NA coefficients were previously
  # See https://stackoverflow.com/a/1495204/5665609 for explanation
  if (length(zeros_indices) > 0)
  {
    sim_coef_main <- sim_coef[,1:(5 - length(zeros_indices))]
    coef_zeros <- cbind(sim_coef_main, matrix(0,
                                                ncol = length(zeros_indices),
                                                nrow = nrow(sim_coef)))
    id <- c(seq_along(sim_coef_main[1,]), zeros_indices + 0.5)
    coef_zeros <- coef_zeros[,order(id)]
    coef_zeros <- cbind(coef_zeros, sim_coef[, (5 - length(zeros_indices) + 1):ncol(sim_coef)])
  }else
  {
    coef_zeros <- sim_coef
  }
  
  #' Calculate log phi for the main effects first. That is, grab the main effect covariates
  #' from design matrix, and grab the first 5 columns of
  design <- sim_data[,c("Intercept", "TSSR", "JD")]
  design$TSSR <- design$TSSR / 24
  design$TSSR2 <- design$TSSR ^ 2
  design$JD <- design$JD / 365
  design$JD2 <- design$JD ^ 2
  
  design_main <- design[, c("Intercept", "TSSR", "TSSR2", "JD", "JD2")]

  log_phi <- as.matrix(design_main) %*%
    t(coef_zeros[,1:5])
  
  #' Now create sparse matrix of 1s in the column that pertains to the
  #' BCR for that particular data point
  design_main$BCR <- sim_data$BCR
  design_bcr <- as.matrix(sparseMatrix(i = seq(1:nrow(design_main)),j = sim_data$BCR)) * 1
  design_bcr <- design_bcr[,which(colSums(design_bcr) > 0)]
  
  bcr_sim_coefs <- cbind(rep(0, nrow(coef_zeros)), coef_zeros[, 6:(6 + length(bcr_sp) - 2)])
  bcr_effects <- as.matrix(design_bcr) %*%
    t(bcr_sim_coefs)
  
  log_phi <- log_phi + bcr_effects
  
  #' Same thing for JD BCR interactions, just using JD instead of 1s for data
  design_bcr_jd <- as.matrix(sparseMatrix(i = seq(1:nrow(design_main)),
                                       j = sim_data$BCR,
                                       x = design_main$JD))
  design_bcr_jd <- design_bcr_jd[,which(colSums(design_bcr_jd) > 0)]
  
  bcr_jd_sim_coefs <- cbind(rep(0, nrow(coef_zeros)), coef_zeros[, (6 + length(bcr_sp) - 1):(6 + 2*length(bcr_sp) - 3)])
  bcr_jd_effects <- as.matrix(design_bcr_jd) %*%
    t(bcr_jd_sim_coefs)
  
  log_phi <- log_phi + bcr_jd_effects
  
  #' Conditional same thing if there is a JD2 term
  if (model %in% c(11, 14, 15))
  {
    design_bcr_jd2 <- as.matrix(sparseMatrix(i = seq(1:nrow(design_main)),
                                            j = sim_data$BCR,
                                            x = design_main$JD2))
    design_bcr_jd2 <- design_bcr_jd2[,which(colSums(design_bcr_jd2) > 0)]
    
    bcr_jd2_sim_coefs <- cbind(rep(0, nrow(coef_zeros)), 
                               coef_zeros[, (6 + 2*length(bcr_sp) - 2):ncol(coef_zeros)])
    bcr_jd2_effects <- as.matrix(design_bcr_jd2) %*%
      t(bcr_jd2_sim_coefs)    
    
    log_phi <- log_phi + bcr_jd2_effects
  }
  
  phi_pred <- exp(log_phi)

  #phi_pred <- exp(as.matrix(design) %*% t(coef_zeros))
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
  
  sim_data$phi <- phi
  sim_data$phi_2.5 <- phi_low
  sim_data$phi_97.5 <- phi_high
  
  sim_data_list[[sp]] <- sim_data
}


# 
# ####### Simulate p ################################
# 
# time_values <- c(1, 3, 5, 10)
# time <- rep(time_values, times = nrow(sim_data))
# 
# sim_data <- sim_data[rep(seq_len(nrow(sim_data)),
#                          each = length(time_values)), ]
# sim_data$Time <- time
# sim_data$p <- 1 - exp(-(sim_data$Time * sim_data$phi))
# sim_data$p_2.5 <- 1 - exp(-(sim_data$Time * sim_data$phi_2.5))
# sim_data$p_97.5 <- 1 - exp(-(sim_data$Time * sim_data$phi_97.5))
# 
# phi_df <- sim_data
# save(x = phi_df, file = "../results/simulations/phi.rda")
