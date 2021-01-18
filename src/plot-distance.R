####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# plot-distance.R
# Created January 2021
# Last Updated January 2021

####### Import Libraries and External Files #######

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

####### Simulate tau ##############################

forest_coverage <- seq(0, 1, by = 0.1)
roadside <- c(rep(1, length(forest_coverage)),
              rep(0, length(forest_coverage)))

sim_data <- data.frame(tau = NA,
                       Species = rep(dis$Species,
                                     each = length(roadside)),
                       Forest = rep(forest_coverage, 2 * length(dis$Species)),
                       Roadside = rep(roadside, length(dis$Species)),
                       Model = rep(dis$best_model, each = length(roadside)))

coefficients <- dis[, c("int_best", "road_best", "forest_best", "roadforest_best")]
coefficients <- coefficients[rep(seq_len(nrow(coefficients)),
                                 each = length(roadside)), ]
coefficients[is.na(coefficients)] <- 0

sim_data$tau <- coefficients[,"int_best"] +
  (coefficients[, "road_best"] * sim_data[, "Roadside"]) +
  (coefficients[, "forest_best"] * sim_data[, "Forest"]) +
  (coefficients[, "roadforest_best"] * (sim_data[, "Roadside"] * sim_data[, "Forest"]))
sim_data$tau <- exp(sim_data$tau)

write.table(x = sim_data, file = "tables/tau_sim.csv", sep = ",", row.names = FALSE)

####### Simulate q ################################

radius_values <- c(50, 100, 200, 400)
radius <- rep(radius_values, times = nrow(sim_data))

sim_data <- sim_data[rep(seq_len(nrow(sim_data)),
                         each = length(radius_values)), ]
sim_data$Radius <- radius
sim_data$q <- ifelse(sim_data$Radius == "Inf",
                     1,
                     ((sim_data$tau ^ 2) / (sim_data$Radius ^ 2)) *
                       (1 - exp(-(sim_data$Radius ^ 2) /
                                  sim_data$tau ^ 2)))

####### Plot All Species with Mean ################

# Remove SEOW as it seems to be having some...issues
sim_data <- sim_data[-which(sim_data$Species == "SEOW"), ]

# Empty plot list
plot_list <- vector(mode = "list", length = length(radius_values) * length(unique(roadside)))

i <- 1

for (rad in radius_values)
{
  for (road in c(1, 0))
  {
    plot_list[[i]] <- 
      ggplot(data = sim_data[which(sim_data$Roadside == road & 
                                   sim_data$Radius == rad),]) +
      geom_line(aes(x = Forest, y = q, group = Species), alpha = 0.05) +
      stat_summary(aes(x = Forest, y = q), fun = mean, geom = "smooth", size = 1.25) +
      ylim(0, 1) +
      theme(legend.position = "none")
    i <- i + 1
  }
}

plot_matrix <- ggmatrix(
  plot_list,
  ncol = length(unique(roadside)),
  nrow = length(radius_values),
  xAxisLabels = c("On-Road Survey", "Off-road Survey"),
  yAxisLabels = c("50m", "100m", "200m", "400m")
)
png(filename = "plots/distance/distance_all_species.png",
    width = 7, height = 9, units = "in", res = 300)
print(plot_matrix)
dev.off()
####### Plot by Family ############################

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
  plot_list <- vector(mode = "list", length = length(radius_values) * length(unique(roadside)))
  
  i <- 1
  n_sp <- length(unique(sim_data[which(sim_data$family == f), "Species"]))
  for (rad in radius_values)
  {
    for (road in c(1, 0))
    {
      plot_list[[i]] <- 
        ggplot(data = sim_data[which(sim_data$Roadside == road & 
                                       sim_data$Radius == rad &
                                       sim_data$family == f),]) +
        geom_line(aes(x = Forest, y = q, group = Species), alpha = 0.2) +
        stat_summary(aes(x = Forest, y = q), fun = mean, geom = "smooth", size = 1.25) +
        ylim(0, 1) +
        theme(legend.position = "none")
      i <- i + 1
    }
  }
  
  plot_matrix <- ggmatrix(
    plot_list,
    ncol = length(unique(roadside)),
    nrow = length(radius_values),
    xAxisLabels = c("On-Road Survey", "Off-road Survey"),
    yAxisLabels = c("50m", "100m", "200m", "400m"),
    title = paste0("Family ",
                   f,
                   " (n = ",
                   n_sp,
                   ")")
  )
  
  pm_list[[pm]] <- plot_matrix
  pm <- pm + 1
}

pdf(file = paste0("plots/distance/distance_families.pdf"))
print(pm_list)
dev.off() 
