####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# plot-distance.R
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

dis <- rm_non_sp(order_taxo(read.csv("data/distance.csv")))
family <- read.csv("../utilities/NACC_list_species.csv")[, c("common_name",
                                                             "family")]
ibp <- read.csv("../utilities/IBP-Alpha-Codes20.csv")[, c("SPEC",
                                                          "COMMONNAME")]
load("data/var-covar/dis_vcv_best.rda")

####### Simulate tau ##############################

forest_coverage <- seq(0, 1, by = 0.1)
roadside <- c(rep(1, length(forest_coverage)),
              rep(0, length(forest_coverage)))

sim_data <- data.frame(Species = rep(dis$Species,
                                     each = length(roadside)),
                       Model = rep(dis$best_model, each = length(roadside)),
                       Intercept = rep(1, times = length(dis$Species) * length(roadside)),
                       Roadside = rep(roadside, length(dis$Species)),
                       Forest = rep(forest_coverage, 2 * length(dis$Species)))
sim_data$Interaction <- sim_data$Forest * sim_data$Roadside

tau <- NULL
tau_low <- NULL
tau_high <- NULL

for (sp in dis$Species)
{
  design <- sim_data[which(sim_data$Species == sp),
                     c("Intercept", "Roadside", "Forest", "Interaction")]
  
  coefficients <- as.numeric(dis[which(dis$Species == sp),
                      c("int_best", "road_best", "forest_best", "roadforest_best")])
  zeros_indices <- which(is.na(coefficients)) - 1
  if (length(zeros_indices) > 0)
  {
    coefficients <- coefficients[-which(is.na(coefficients))]    
  }

  vcv <- dis_vcv_best[[sp]]
  
  # Simulate a bunch of possible coefficients
  sim_coef <- rbind(coefficients, MASS::mvrnorm(10^4, coefficients, vcv))
  
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

write.table(x = sim_data, file = "tables/tau_sim.csv", sep = ",", row.names = FALSE)

####### Simulate q ################################

radius_values <- seq(50, 400, by = 50)
radius <- rep(radius_values, times = nrow(sim_data))

sim_data <- sim_data[rep(seq_len(nrow(sim_data)),
                         each = length(radius_values)), ]
sim_data$Radius <- radius

sim_data$q <- ifelse(sim_data$Radius == "Inf",
                     1,
                     ((sim_data$tau ^ 2) / (sim_data$Radius ^ 2)) *
                       (1 - exp(-(sim_data$Radius ^ 2) /
                                  sim_data$tau ^ 2)))

sim_data$q_2.5 <- ifelse(sim_data$Radius == "Inf",
                         1,
                         ((sim_data$tau_2.5 ^ 2) / (sim_data$Radius ^ 2)) *
                           (1 - exp(-(sim_data$Radius ^ 2) /
                                      sim_data$tau_2.5 ^ 2)))

sim_data$q_97.5 <- ifelse(sim_data$Radius == "Inf",
                          1,
                          ((sim_data$tau_97.5 ^ 2) / (sim_data$Radius ^ 2)) *
                            (1 - exp(-(sim_data$Radius ^ 2) /
                                       sim_data$tau_97.5 ^ 2)))

####### Plot All Species with Mean ################

forest_level <- c(1.0, 0.0)
sim_data$SpeciesRoad <- paste0(sim_data$Species, sim_data$Roadside)

# Empty plot list
plot_list <- vector(mode = "list", length = length(forest_level))
i <- 1

for (fc in c(1.0, 0.0))
{
  plot_list[[i]] <- 
    ggplot(data = sim_data[which(sim_data$Forest == fc),]) +
    geom_line(aes(x = Radius, y = q, group = SpeciesRoad, color = as.factor(Roadside)), alpha = 0.05) +
    stat_summary(aes(x = Radius, y = q, group = as.factor(Roadside), color = as.factor(Roadside)), fun = mean, geom = "smooth", size = 1.25) +
    ylim(0, 1) +
    #theme(legend.position = "none") +
    NULL
  i <- i + 1  
}

plot_matrix <- ggmatrix(
  plot_list,
  ncol = length(forest_level),
  nrow = 1,
  xAxisLabels = c("Forest", "Non-forest")
)
png(filename = "plots/distance/distance_all_species.png",
    width = 7, height = 4, units = "in", res = 300)
print(plot_matrix)
dev.off()

####### Plot by Species ###########################

sm_list <- vector(mode = "list", length = length(unique(sim_data$Species)))
sm <- 1

for (sp in unique(sim_data$Species))
{
  # Empty plot list
  plot_list <- vector(mode = "list", length = length(forest_level))
  i <- 1
  
  for (fc in c(1.0, 0.0))
  {
    plot_list[[i]] <- 
      ggplot(data = sim_data[which(sim_data$Forest == fc &
                                     sim_data$Species == sp),]) +
      geom_line(aes(x = Radius, y = q, color = as.factor(Roadside))) +
      geom_ribbon(aes(x = Radius, ymin = q_2.5, ymax = q_97.5, color = as.factor(Roadside)),
                  alpha = 0.25) +
      #stat_summary(aes(x = Radius, y = q, group = as.factor(Roadside), color = as.factor(Roadside)), fun = mean, geom = "smooth", size = 1.25) +
      ylim(0, 1) +
      #theme(legend.position = "none") +
      NULL
    i <- i + 1  
  }
  
  plot_matrix <- ggmatrix(
    plot_list,
    ncol = length(forest_level),
    nrow = 1,
    xAxisLabels = c("Forest", "Non-forest"),
    title = paste0("Species: ",
                   sp)
  )
  
  sm_list[[sm]] <- plot_matrix
  sm <- sm + 1  
}

pdf(file = paste0("plots/distance/distance_species.pdf"))
print(sm_list)
dev.off() 


# ####### Plot by Family ############################
# 
# # Add Family grouping
# family <- merge(x = family, y = ibp,
#                 by.x = "common_name", by.y = "COMMONNAME")
# sim_data <- merge(x = sim_data, y = family[, c("SPEC", "family")],
#                   by.x = "Species", by.y = "SPEC")
# 
# pm_list <- vector(mode = "list", length = length(unique(sim_data$family)))
# pm <- 1
# 
# for (f in sort(unique(sim_data$family)))
# {
#   # Empty plot list
#   plot_list <- vector(mode = "list", length = length(radius_values) * length(unique(roadside)))
#   
#   i <- 1
#   n_sp <- length(unique(sim_data[which(sim_data$family == f), "Species"]))
#   for (rad in radius_values)
#   {
#     for (road in c(1, 0))
#     {
#       plot_list[[i]] <- 
#         ggplot(data = sim_data[which(sim_data$Roadside == road & 
#                                        sim_data$Radius == rad &
#                                        sim_data$family == f),]) +
#         geom_line(aes(x = Forest, y = q, group = Species), alpha = 0.2) +
#         stat_summary(aes(x = Forest, y = q), fun = mean, geom = "smooth", size = 1.25) +
#         ylim(0, 1) +
#         theme(legend.position = "none")
#       i <- i + 1
#     }
#   }
#   
#   plot_matrix <- ggmatrix(
#     plot_list,
#     ncol = length(unique(roadside)),
#     nrow = length(radius_values),
#     xAxisLabels = c("On-Road Survey", "Off-road Survey"),
#     yAxisLabels = c("50m", "100m", "200m", "400m"),
#     title = paste0("Family ",
#                    f,
#                    " (n = ",
#                    n_sp,
#                    ")")
#   )
#   
#   pm_list[[pm]] <- plot_matrix
#   pm <- pm + 1
# }
# 
# pdf(file = paste0("plots/distance/distance_families.pdf"))
# print(pm_list)
# dev.off() 
