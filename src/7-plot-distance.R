####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# 7-plot-distance.R
# Created January 2021
# Last Updated April 2021

####### Import Libraries and External Files #######

library(ggplot2)
library(GGally)
library(ggpubr)
library(viridis)
theme_set(theme_pubclean())

####### Read Data #################################

# "4-simulate-tau.R" MUST BE RUN before trying to read in tau_sim.csv!!
sim_data <- read.csv("output/tables/tau_sim.csv")

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



# ####### Plot All Species with Mean ################
# 
# forest_level <- c(1.0, 0.0)
# 
# # Create a variable where you can separate roadside status but species
# sim_data$SpeciesRoad <- paste0(sim_data$Species, sim_data$Roadside)
# 
# # Empty plot list
# plot_list <- vector(mode = "list", length = length(forest_level))
# i <- 1
# 
# for (fc in c(1.0, 0.0))
# {
#   plot_list[[i]] <- 
#     ggplot(data = sim_data[which(sim_data$Forest == fc),]) +
#     geom_line(aes(x = Radius, y = q, group = SpeciesRoad, color = as.factor(Roadside)), alpha = 0.05) +
#     stat_summary(aes(x = Radius, y = q, group = as.factor(Roadside), color = as.factor(Roadside)), fun = mean, geom = "smooth", size = 1.25) +
#     ylim(0, 1) +
#     #theme(legend.position = "none") +
#     NULL
#   i <- i + 1  
# }
# 
# plot_matrix <- ggmatrix(
#   plot_list,
#   ncol = length(forest_level),
#   nrow = 1,
#   xAxisLabels = c("Forest", "Non-forest")
# )
# png(filename = "plots/distance/distance_all_species.png",
#     width = 7, height = 4, units = "in", res = 300)
# print(plot_matrix)
# dev.off()

####### Plot by Species ###########################
forest_level <- c(1.0, 0.0)
sim_data$Roadside_Status <- ifelse(sim_data$Roadside == 1, "On-Road", "Off-road")
sm_list <- vector(mode = "list", length = length(unique(sim_data$Species)))
names(sm_list) <- unique(sim_data$Species)

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
      geom_line(aes(x = Radius, y = q, color = Roadside_Status)) +
      geom_ribbon(aes(x = Radius, ymin = q_2.5, ymax = q_97.5, color = Roadside_Status),
                  alpha = 0.25) +
      #stat_summary(aes(x = Radius, y = q, group = as.factor(Roadside), color = as.factor(Roadside)), fun = mean, geom = "smooth", size = 1.25) +
      ylim(0, 1) +
      scale_color_viridis(discrete=TRUE) +
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
                   sp),
    legend = 2
  )
  
  png(filename = paste0("output/plots/distance/",
                        sp,
                        ".png"),
      width = 6, height = 6, units = "in", res = 300)
  print(plot_matrix)
  dev.off()
  
  sm_list[[sp]] <- plot_matrix
}

save(sm_list, file = "output/plots/distance/distance_plot_list.rda")

pdf(file = paste0("output/plots/distance/distance_species.pdf"))
print(sm_list)
dev.off() 
