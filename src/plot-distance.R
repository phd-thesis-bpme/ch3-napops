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

####### Read Data #################################

# "simulate-tau.R" MUST BE RUN before trying to read in tau_sim.csv!!
sim_data <- read.csv("tables/tau_sim.csv")
family <- read.csv("../utilities/NACC_list_species.csv")[, c("common_name",
                                                             "family")]
ibp <- read.csv("../utilities/IBP-Alpha-Codes20.csv")[, c("SPEC",
                                                          "COMMONNAME")]

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
